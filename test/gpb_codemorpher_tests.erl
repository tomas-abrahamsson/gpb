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

get_exports_from_forms(Forms) ->
    lists:append([get_exports_from_form(Form) || Form <- Forms]).

get_exports_from_form(Form) ->
    case erl_syntax_lib:analyze_form(Form) of
        {function,{_Name,_Arity}=Export} -> [Export];
        _ -> []
    end.
