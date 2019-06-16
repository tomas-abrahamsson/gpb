%%% Copyright (C) 2013  Tomas Abrahamsson
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

-module(gpb_codegen_tests).

-include("../src/gpb_codegen.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R:St ->).
-else. % -ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R -> St = erlang:get_stacktrace(),).
-endif. % -ifdef(OTP_RELEASE).

%-compile(export_all).

-define(dummy_mod, list_to_atom(lists:concat([?MODULE, "-test"]))).
-define(debug,true).

-define(current_function,
        begin
            {current_function,{_M___,_F___,_Arity___}} =
                erlang:process_info(self(), current_function),
            _F___
        end).

plain_parse_transform_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(),
    {module,M} = l(M, gpb_codegen:mk_fn(FnName, fun(a) -> {ok, 1} end)),
    {ok, 1} = M:FnName(a),
    ?assertError(_, M:FnName(b)).

term_replacements_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(),
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(a) -> {ok, b} end,
                                        [{replace_term,a,1},
                                         {replace_term,b,2}])),
    ?assertError(_, M:FnName(a)),
    {ok, 2} = M:FnName(1).

tree_replacements_1_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(),
    Var = ?expr(V),
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(a) -> {ok, b} end,
                                        [{replace_tree,a,Var},
                                         {replace_tree,b,Var}])),
    {ok, x} = M:FnName(x),
    {ok, z} = M:FnName(z).

tree_replacements_2_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(?current_function),
    Add = ?expr(V + V),
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(V, V) -> ret end,
                                        [{replace_tree,ret,Add}])),
    8 = M:FnName(4, 4).

tree_splicing_1_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(?current_function),
    Vars = [?expr(V), ?expr(V)],
    Ret  = [?expr(V), ?expr(V)],
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(p) -> {ret} end,
                                        [{splice_trees,p,Vars},
                                         {splice_trees,ret,Ret}])),
    {x,x} = M:FnName(x, x),
    {z,z} = M:FnName(z, z),
    ?assertError(_, M:FnName(x, z)).

tree_splicing_2_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(?current_function),
    Vars = [?expr(V), ?expr(V)],
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(p) -> V + V end,
                                        [{splice_trees,p,Vars}])),
    4 = M:FnName(2, 2).

replaces_function_name_after_splicings_test() ->
    M = ?dummy_mod,
    FnName = fn,
    Vars = [?expr(V), ?expr(V)],
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(p) -> V + V end,
                                        [{splice_trees,p,Vars}])),
    4 = M:FnName(2, 2).

recursive_call_test() ->
    M = ?dummy_mod,
    {module,M} = l(M, gpb_codegen:mk_fn(fact,
                                        fun(N) when N == 0 -> 1;
                                           (N) -> N * call_self(N-1)
                                        end)),
    120 = M:fact(5).

can_add_case_clause_test() ->
    M = ?dummy_mod,
    FnName = p,
    CaseClauses = [?case_clause(1 -> one),
                   ?case_clause(2 -> two)],
    MoreClauses = [?case_clause(b -> 2),
                   ?case_clause(_ -> other)],
    {module,M} = l(M, gpb_codegen:mk_fn(
                        FnName,
                        fun(X, Y) ->
                                case X of
                                    cx -> dummy;
                                    3  -> three;
                                    99 -> case Y of
                                              a  -> 1;
                                              cy -> dummy
                                          end
                                end
                        end,
                        [{splice_clauses, cx, CaseClauses},
                         {splice_clauses, cy, MoreClauses}])),
    one   = M:FnName(1, 55),
    two   = M:FnName(2, 55),
    three = M:FnName(3, 55),
    ?assertError({case_clause,_}, M:FnName(cx, 55)),
    1     = M:FnName(99, a),
    2     = M:FnName(99, b),
    other = M:FnName(99, c),
    other = M:FnName(99, cy).

can_runtime_transform_case_clause_test() ->
    M = ?dummy_mod,
    FnName = p,
    CaseClause = ?case_clause(mm -> one, [{replace_term, mm, 1}]),
    {module,M} = l(M, gpb_codegen:mk_fn(
                        FnName,
                        fun(X) ->
                                case X of
                                    rr -> dummy
                                end
                        end,
                        [{splice_clauses, rr, [CaseClause]}])),
    one   = M:FnName(1),
    ok.

runtime_transforms_for_expr_test() ->
    E1 = ?expr(a, [{replace_term, a, 1}]),
    E2 = ?expr(b, [{replace_tree, b, ?expr(bee)}]),
    E3 = ?expr({c}, [{splice_trees, c, [?expr(see), ?expr(X)]}]),
    E4 = ?expr(case Y of
                   1      -> one;
                   marker -> dummy;
                   _      -> other
               end,
               [{splice_clauses, marker, [?case_clause(2 -> two)]}]),
    M = ?dummy_mod,
    FnName = p,
    {module, M} = l(M, gpb_codegen:mk_fn(
                         FnName,
                         fun(e1) -> '<e1>';
                            (e2) -> '<e2>';
                            ({e3, X}) -> '<e3>';
                            ({e4, Y}) -> '<e4>'
                         end,
                         [{replace_tree, '<e1>', E1},
                          {replace_tree, '<e2>', E2},
                          {replace_tree, '<e3>', E3},
                          {replace_tree, '<e4>', E4}])),
    1         = M:FnName(e1),
    bee       = M:FnName(e2),
    {see,sea} = M:FnName({e3, sea}),
    one       = M:FnName({e4, 1}),
    two       = M:FnName({e4, 2}),
    other     = M:FnName({e4, 3}),
    ok.

runtime_transforms_for_exprs_test() ->
    E1s = ?exprs(a, [{replace_term, a, 1}]),
    E2s = ?exprs(a, b, [{replace_term, a, 1}, {replace_term, b, 2}]),
    true = is_list(E1s),
    true = is_list(E2s),
    M = ?dummy_mod,
    FnName = p,
    {module, M} = l(M, gpb_codegen:mk_fn(
                         FnName,
                         fun(e1) -> {'<e1>'};
                            (e2) -> {'<e2>'}
                         end,
                         [{splice_trees, '<e1>', E1s},
                          {splice_trees, '<e2>', E2s}])),
    {1}   = M:FnName(e1),
    {1,2} = M:FnName(e2),
    ok.

splice_trees_of_exprs_at_top_level_test() ->
    Es = ?exprs(b, cd, e, [{splice_trees, cd, ?exprs(c, d, [])}]),
    M = ?dummy_mod,
    FnName = p,
    Fn = gpb_codegen:mk_fn(
           FnName,
           fun() -> {a, exprs_go_here, f} end,
           [{splice_trees, exprs_go_here, Es}]),
    {module, M} = l(M, Fn),
    {a, b, c, d, e, f} = M:FnName(),
    ok.

format_fn_no_rt_transforms_test() ->
    FnName = p,
    IoList = gpb_codegen:format_fn(FnName, fun(33) -> yes end),
    true = io_lib:deep_char_list(IoList),
    $\n = lists:last(lists:flatten(IoList)),
    M = ?dummy_mod,
    {module, M} = ls(M, IoList),
    yes = M:FnName(33).

format_fn_with_transforms_test() ->
    FnName = p,
    IoList = gpb_codegen:format_fn(FnName, fun(zz) -> yes end,
                                   [{replace_term, zz, 88}]),
    true = io_lib:deep_char_list(IoList),
    $\n = lists:last(lists:flatten(IoList)),
    M = ?dummy_mod,
    {module, M} = ls(M, IoList),
    yes = M:FnName(88).

splice_fn_clause_test() ->
    FnClause = ?fn_clause(fun(x, 1) -> a_one end,
                          [{replace_term, x, a}]),
    FnName = p,
    FT = gpb_codegen:mk_fn(FnName, fun(y, 2) -> y_two;
                                      (mm, _) -> dummy
                                   end,
                           [{splice_clauses, mm, [FnClause]}]),
    M = ?dummy_mod,
    {module, M} = l(M, FT),
    a_one = M:FnName(a, 1),
    y_two = M:FnName(y, 2),
    ?assertError(function_clause, M:FnName(mm, dummy)),
    ok.

splice_if_clause_test() ->
    IfClause = ?if_clause(X == 2 -> r_me, [{replace_term, r_me, two}]),
    FnName = p,
    FT = gpb_codegen:mk_fn(FnName, fun(X) ->
                                           if X == 1 -> one;
                                              mm -> to_be_replaced
                                           end
                                   end,
                           [{splice_clauses, mm, [IfClause]}]),
    M = ?dummy_mod,
    {module, M} = l(M, FT),
    one = M:FnName(1),
    two = M:FnName(2),
    ?assertError(if_clause, M:FnName(mm)),
    ok.

splice_receive_clause_test() ->
    RClause1 = ?receive_clause(a -> x, [{replace_term, x, 1}]),
    RClause2 = ?receive_clause(b -> y, [{replace_term, y, 2}]),
    FnName = p,
    FT = gpb_codegen:mk_fn(FnName, fun(_) ->
                                           receive mm -> dummy end
                                   end,
                           [{splice_clauses, mm, [RClause1, RClause2]}]),
    M = ?dummy_mod,
    {module, M} = l(M, FT),
    self() ! a,
    1 = M:FnName(a),
    self() ! b,
    2 = M:FnName(b),
    ok.

can_replace_binary_fields_test() ->
    F2 = erl_syntax:binary_field(?expr(2), []),
    F3 = erl_syntax:binary_field(?expr(3), []),
    FnName = p,
    FT = gpb_codegen:mk_fn(FnName, fun() -> <<1, morefields>> end,
                           [{splice_trees, morefields, [F2, F3]}]),
    M = ?dummy_mod,
    {module, M} = l(M, FT),
    <<1,2,3>> = M:FnName().


repeat_clauses_test() ->
    FnName = p,
    FT = gpb_codegen:mk_fn(FnName,
                           fun(n, m) -> {m, n} end,
                           [{repeat_clauses,n,
                             [[{replace_term, n, N}, {replace_term, m, N+1}]
                              || N <- [1,2,3]]}]),
    M = ?dummy_mod,
    {module, M} = l(M, FT),
    {2,1} = M:FnName(1, 2),
    {3,2} = M:FnName(2, 3),
    {4,3} = M:FnName(3, 4),
    ?assertError(function_clause, M:FnName(0,1)),
    ?assertError(function_clause, M:FnName(1,1)),
    ?assertError(function_clause, M:FnName(4,5)),
    ok.

can_insert_program_fragments_as_text_test() ->
    %% This can be useful for program constructs that erl_syntax
    %% does not yet support. At the time of this writing (pre-R17),
    %% this includes map, but I still want to be able to generate
    %% code for supporting maps.
    FnName = p,
    XTimes2Text = erl_syntax:text("X * 2"),
    IoList = gpb_codegen:format_fn(FnName, fun(X) -> marker end,
                                   [{replace_tree, marker, XTimes2Text}]),
    true = io_lib:deep_char_list(IoList),
    M = ?dummy_mod,
    {module, M} = ls(M, IoList),
    128 = M:FnName(64).

%% -- helpers ---------------------------

mk_test_fn_name() ->
    %% Make different names (for testability),
    %% but don't exhaust the atom table.
    list_to_atom(lists:concat([test_, small_random_number()])).

mk_test_fn_name(FnName) ->
    %% Make different names (for testability),
    %% but don't exhaust the atom table.
    list_to_atom(lists:concat([test_, FnName, "_", small_random_number()])).

small_random_number() ->
    integer_to_list(erlang:phash2(make_ref()) rem 17).

l(Mod, Form) ->
    l(Mod, get_exports_from_form(Form), Form).

l(Mod, Exports, Form) ->
    File = atom_to_list(Mod)++".erl",
    Program = [mk_attr(file,{File,1}),
               mk_attr(module,Mod),
               mk_export_attr(Exports),
               Form],
    try compile:noenv_forms(Program, []) of
        {ok, Mod, Bin} ->
            unload_code(Mod),
            code:load_binary(Mod, File, Bin);
        Error ->
            ?debugFmt("~nCompilation Error:~n~s~n  ~p~n",
                      [format_form_debug(Form), Error]),
            Error
    catch ?STACKTRACE(error,Error,ST) % ->
            ?debugFmt("~nCompilation crashed (malformed parse-tree?):~n"
                      ++ "~s~n"
                      ++ "  ~p~n",
                      [format_form_debug(Form), {Error,ST}]),
            {error, {compiler_crash,Error}}
    end.

mk_attr(AttrName, AttrValue) ->
    erl_syntax:revert(
      erl_syntax:attribute(erl_syntax:atom(AttrName),
                           [erl_syntax:abstract(AttrValue)])).

mk_export_attr(Exports) ->
    Fns = [io_lib:format("~p/~w", [F,A]) || {F,A} <- Exports],
    S = ["-export([", gpb_lib:comma_join(Fns), "])."],
    {ok,Tokens,_} = erl_scan:string(lists:flatten(S)),
    {ok,Form} = erl_parse:parse_form(Tokens),
    Form.

ls(Mod, FormAsIoList) ->
    {ok, Tokens, _} = erl_scan:string(lists:flatten(FormAsIoList)),
    {ok, Form} = erl_parse:parse_form(Tokens),
    l(Mod, Form).

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

format_form_debug(Form) ->
    PrettyForm = try erl_prettypr:format(Form)
                 catch error:_ -> "(Failed to pretty-print form)"
                 end,
    io_lib:format("~nForm=~p~n"
                  ++ "-->~n"
                  ++ "~s~n"
                  ++ "^^^^",
                  [Form, PrettyForm]).

get_exports_from_form(Form) ->
    case erl_syntax_lib:analyze_form(Form) of
        {function,{_Name,_Arity}=Export} -> [Export];
        _ -> []
    end.
