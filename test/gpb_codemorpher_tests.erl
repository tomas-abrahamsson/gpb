%%% Copyright (C) 2017  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%%% MA  02110-1301  USA

-module(gpb_codemorpher_tests).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------

-define(dummy_mod, list_to_atom(lists:concat([?MODULE, "-test"]))).

remove_unused_record_fields_test() ->
    {module,M} =
        ls(?dummy_mod,
           ["-record(rr,{a,b}).\n",
            {fun gpb_codemorpher:underscore_unused_vars/1,
             ["x(1, #rr{a=A,b=B}=R) -> x(2, R#rr{a=2});\n"
              "x(2, R) -> R."]}]),
    {rr,2,17} = M:x(1, {rr,18,17}).

remove_unused_record_var_test() ->
    {module,M} =
        ls(?dummy_mod,
           ["-record(rr,{a,b}).\n",
            {fun gpb_codemorpher:underscore_unused_vars/1,
             "x(#rr{a=A,b=B}=R) -> {B,A}."}]),
    {17,18} = M:x({rr,18,17}).

remove_unused_fn_parameters_test() ->
    {module,M} =
        ls(?dummy_mod,
           [{fun gpb_codemorpher:underscore_unused_vars/1,
             ["x(<<1:1, N:7, Rest/binary>>, Z1,Z2,A,B) when Z2 < 57 ->\n",
              "    x(Rest, 0, 0, N, B);\n",
              "x(<<>>, Z1, Z2, A, B) ->\n",
              "    {A,B}."]}]),
    {3,2} = M:x(<<1:1, 3:7>>, 0, 0, 1, 2).

remove_unused_case_clause_test() ->
    {module,M} =
        ls(?dummy_mod,
           [{fun gpb_codemorpher:underscore_unused_vars/1,
             ["x(A) ->\n"
              "    case A of\n"
              "        {x,X2} -> 49;\n"
              "        {y,X2} -> X2+1\n"
              "    end."]}]),
    49 = M:x({x,z}),
    18 = M:x({y,17}).

finds_record_param_test() ->
    %% Direct match for record
    1 = gpb_codemorpher:locate_record_param(parse_form(["f(#r{}) -> ok."])),
    %% Matches field too:
    1 = gpb_codemorpher:locate_record_param(parse_form(["f(#r{a=1}) -> ok."])),
    1 = gpb_codemorpher:locate_record_param(parse_form(["f(#r{a=X}) -> X."])),
    %% Match record + bind to variable:
    2 = gpb_codemorpher:locate_record_param(
          parse_form(["f(_, #r{}=M) -> M."])),
    %% Not all clauses match, but same pos for those that do:
    2 = gpb_codemorpher:locate_record_param(
          parse_form(["f(_, #r{a=1}=M) -> M;\n"
                      "f(_, #r{b=2}=M) -> M;\n"
                      "f(X, Y) -> {X,Y}."])),
    %% No param matches for record at all:
    ?assertError(badarg, gpb_codemorpher:locate_record_param(
                           parse_form(["f() -> ok."]))),
    ?assertError(badarg, gpb_codemorpher:locate_record_param(
                           parse_form(["f(_) -> ok."]))),
    %% More than one position:
    ?assertError(badarg,
                 gpb_codemorpher:locate_record_param(
                   parse_form(["f(#r{}, #r{}) -> ok."]))),
    %% Not same position for all function clauses:
    ?assertError(badarg,
                 gpb_codemorpher:locate_record_param(
                   parse_form(["f(#r{}, _) -> ok;\n"
                               "f(_, #r{}) -> nok."]))),
    ok.

map_tail_expr_test() ->
    %% Turn any expression <E> into {x,<E>}
    TailExprF = fun(X) -> erl_syntax:tuple([erl_syntax:atom(x), X]) end,
    FnClausePatternF = fun(Params) -> {Params, TailExprF} end,
    F = fun(FnSTree) ->
                gpb_codemorpher:map_tail_exprs(
                  FnClausePatternF,
                  FnSTree)
        end,
    {module,M} = ls(?dummy_mod, [{F, ["x(A) -> A."]},
                                 {F, ["y(B) -> case B of\n
                                                   1 -> -17;
                                                   _ -> B+1
                                               end."]},
                                 {F, ["z(C) -> if C == 1 -> -17;
                                                  true   -> {C+2}
                                               end.\n"]}]),
    {x,33} = M:x(33),
    {x,-17} = M:y(1),
    {x,3}   = M:y(2),
    {x,-17} = M:z(1),
    {x,{4}} = M:z(2),
    ok.

explode_record_fields_to_params_test() ->
    FieldNames = [a,b,c],
    F1 = fun(FnSTree) ->
                 gpb_codemorpher:explode_record_fields_to_params_init(
                   FnSTree,
                   2,
                   {r, [{a,erl_syntax:integer(1)},
                        {b,erl_syntax:integer(2)},
                        {b,erl_syntax:nil()}]})
         end,
    F2 = fun(FnSTree) ->
                 gpb_codemorpher:explode_record_fields_to_params(
                   FnSTree,
                   2,
                   {r, FieldNames})
         end,
    {module,M} = ls(?dummy_mod,
                    [["-record(r, {",
                      string:join([atom_to_list(N) || N <- FieldNames],","),
                      "})."],
                     {F1, ["fn_1(Bin) ->
                                 fn_x(Bin, #r{})."]},
                     {F2, ["fn_x(<<N,Rest/binary>>, #r{b=B}=M) ->
                                 NewB = N + B,
                                 fn_x(Rest, M#r{b=NewB});
                            fn_x(<<>>, #r{c=C}=M) ->
                                 M#r{c=lists:reverse(C)}."]}]),
    {r,4711,6,[42,17]} = M:fn_x(<<1,2,3>>, 4711, 0, [17,42]),
    {r,1,5,[]}         = M:fn_1(<<0,3>>).

explode_record_fields_to_params_with_passthrough_test() ->
    FieldNames = [a,b],
    F1 = fun(FnSTree) ->
                 gpb_codemorpher:explode_record_fields_to_params_init(
                   FnSTree,
                   2,
                   {r, [{a,erl_syntax:integer(1)},
                        {b,erl_syntax:integer(2)}]})
         end,
    F2 = fun(FnSTree) ->
                 gpb_codemorpher:underscore_unused_vars(
                   gpb_codemorpher:explode_record_fields_to_params(
                     FnSTree,
                     2,
                     {r, FieldNames}))
         end,
    {module,M} = ls(?dummy_mod,
                    [["-record(r, {",
                      string:join([atom_to_list(N) || N <- FieldNames],","),
                      "})."],
                     {F1, ["fn_1(Bin) ->
                                 fastpath(Bin, #r{})."]},
                     {F2, ["fastpath(<<17,Rest/binary>>, M) ->
                                 fastpath(Rest, M#r{b=17});
                            fastpath(<<>>, #r{}=M) ->
                                 M;
                            fastpath(Other, #r{}=M) ->
                                 general(Other, M)."]},
                     {F2, ["general(<<N,Rest/binary>>, #r{b=B}=M) ->
                                 NewB = N + B,
                                 general(Rest, M#r{b=NewB});
                            general(<<>>, #r{c=C}=M) ->
                                 M#r{c=lists:reverse(C)}."]}]),
    {r,4711,6} = M:fastpath(<<1,2,3>>, 4711, 0),
    {r,1,5}    = M:fn_1(<<0,3>>).

-ifndef(NO_HAVE_MAPS).
implode_to_map_exprs_test() ->
    F = fun(FnSTree) ->
                gpb_codemorpher:marked_map_expr_to_map_expr(
                  gpb_codemorpher:underscore_unused_vars(
                    gpb_codemorpher:implode_to_map_exprs(
                      FnSTree, 2,
                      [{a, optional},
                       {b, optional},
                       {c, required}],
                      '$novalue')))
        end,
    {module,M} = ls(?dummy_mod,
                    [{F, ["fn_f(<<>>, A, B, C) ->
                               #r{a = A-1,
                                  b = {b,B},
                                  c = lists:reverse(C)}."]}]),
    #{a := 0,
      b := {b,b_b},
      c := [3,2,1]} = Map1 = M:fn_f(<<>>, 1, b_b, [1,2,3]),
    3 = maps:size(Map1),

    #{c := [3,2,1]} = Map2 = M:fn_f(<<>>, '$novalue', '$novalue', [1,2,3]),
    1 = maps:size(Map2).
-endif. % NO_HAVE_MAPS

analyze_case_clauses_test() ->
    S = "case M of
             #r{a=undefined} -> V;
             #r{a=N} -> merge(P,V);
             #r{a={u1,U}} -> {u,merge(P,U)};
             #r{a=A} when A =:= undefined -> V;
             #r{a=A} when A =/= undefined -> V;
             #r{a=A} when A == undefined -> V;
             #r{a=A} when A /= undefined -> V;
             _ -> V
         end.\n",
    CaseExpr = parse_expr(S),
    [{{match_undefined,a}, [_]},
     {{match_variable,a,'N'}, [_]},
     {{match_tagged_variable,a,u1,'U'}, [_]},
     {{match_undefined,a}, [_]},
     {{match_not_undefined,a}, [_]},
     {{match_undefined,a}, [_]},
     {{match_not_undefined,a}, [_]},
     {'_', [_]}] =
        gpb_codemorpher:analyze_case_clauses(CaseExpr, undefined).

analyze_if_clauses_test() ->
    PatS = "#r{a=A}.",
    IfS  = "if A == undefined-> V;
               true -> V
            end.\n",
    [Pat, If] = [parse_expr(S) || S <- [PatS, IfS]],
    [{{match_undefined,a}, [_]},
     {'_', [_]}] =
        gpb_codemorpher:analyze_if_clauses(Pat, If, undefined).

ls(Mod, FormStrs) ->
    Forms = parse_transform_form_strs(FormStrs),
    l(Mod, Forms).

l(Mod, Forms) ->
    l(Mod, get_exports_from_forms(Forms), Forms).

l(Mod, Exports, Forms) ->
    File = atom_to_list(Mod)++".erl",
    Program = [mk_attr(file,{File,1}),
               mk_attr(module,Mod),
               mk_export_attr(Exports)
               | Forms],
    try compile:noenv_forms(Program, [binary, return]) of
        {ok, Mod, Bin, _Warnings=[]} ->
            unload_code(Mod),
            code:load_binary(Mod, File, Bin);
        Error ->
            ?debugFmt("~nCompilation Error:~n~s~n  ~p~n",
                      [format_forms_debug(Forms), Error]),
            Error
    catch error:Error ->
            ST = erlang:get_stacktrace(),
            ?debugFmt("~nCompilation crashed (malformed parse-tree?):~n"
                      ++ "~s~n"
                      ++ "  ~p~n",
                      [format_forms_debug(Forms), {Error,ST}]),
            {error, {compiler_crash,Error}}
    end.

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

format_forms_debug(Forms) ->
    PrettyForm = [try [erl_prettypr:format(Form),"\n"]
                  catch error:_ -> "(Failed to pretty-print form)"
                  end
                  || Form <- Forms],
    io_lib:format("~nForm=~p~n"
                  ++ "-->~n"
                  ++ "~s~n"
                  ++ "^^^^",
                  [Forms, PrettyForm]).

mk_attr(AttrName, AttrValue) ->
    erl_syntax:revert(
      erl_syntax:attribute(erl_syntax:atom(AttrName),
                           [erl_syntax:abstract(AttrValue)])).

mk_export_attr(Exports) ->
    Fns = [io_lib:format("~p/~w", [F,A]) || {F,A} <- Exports],
    S = ["-export([", string:join(Fns, ","), "])."],
    {ok,Tokens,_} = erl_scan:string(lists:flatten(S)),
    {ok,Form} = erl_parse:parse_form(Tokens),
    Form.

parse_transform_form_strs(Strs) ->
    {Forms, _Endline} = lists:mapfoldl(fun parse_x/2, 1, Strs),
    Forms.

parse_x({Transform, S}, EndLine) ->
    {Form, EndLine1} = parse_x(S, EndLine),
    {erl_syntax:revert(Transform(Form)), EndLine1};
parse_x(S, EndLine) ->
    {ok, Tokens, EndLine1} = erl_scan:string(lists:flatten(S), EndLine),
    {ok, Form} = erl_parse:parse_form(Tokens),
    {Form, EndLine1}.

parse_form(S) ->
    {Form, _EndLine} = parse_x(S, 1),
    Form.

parse_expr(S) ->
    {ok, Toks, _End} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Toks),
    Expr.

get_exports_from_forms(Forms) ->
    lists:append([get_exports_from_form(Form) || Form <- Forms]).

get_exports_from_form(Form) ->
    case erl_syntax_lib:analyze_form(Form) of
        {function,{_Name,_Arity}=Export} -> [Export];
        _ -> []
    end.
