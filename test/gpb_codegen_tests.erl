%%% Copyright (C) 2013  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-module(gpb_codegen_tests).

-include("../src/gpb_codegen.hrl").
-include_lib("eunit/include/eunit.hrl").

%-compile(export_all).
-export([debug_form/1, pp/1]).

-define(dummy_mod, list_to_atom(lists:concat([?MODULE, "-test"]))).

-ifdef(debug).
debug_form(Form) -> pp(Form).
-else.   %% debug
debug_form(_Form) -> ok.
-endif.  %% debug



generates_code_with_no_replacements_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(),
    {module,M} = l(M, gpb_codegen:mk_fn(FnName, fun(a) -> {ok, 1} end)),
    {ok, 1} = M:FnName(a).

replacements_during_codegen_test() ->
    M = ?dummy_mod,
    FnName = mk_test_fn_name(),
    {module,M} = l(M, gpb_codegen:mk_fn(FnName,
                                        fun(a) -> {ok, b} end,
                                        [{replace,a,1},
                                         {replace,b,2}])),
    {ok, 2} = M:FnName(1).

mk_test_fn_name() ->
    %% Make different names (for testability),
    %% but don't exhaust the atom table.
    list_to_atom(
      lists:concat(
        [test_, integer_to_list(erlang:phash2(make_ref()) rem 17)])).

l(Mod, Form) ->
    debug_form(Form),
    File = atom_to_list(Mod)++".erl",
    Program = [mk_attr(file,{File,1}),
               mk_attr(module,Mod),
               mk_attr(compile,export_all),
               Form],
    {ok, Mod, Bin} = compile:forms(Program),
    unload_code(Mod),
    code:load_binary(Mod, File, Bin).

mk_attr(AttrName, AttrValue) ->
    erl_syntax:revert(
      erl_syntax:attribute(erl_syntax:atom(AttrName),
                           [erl_syntax:abstract(AttrValue)])).

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

%% for debugging
pp(Form) ->
    ?debugFmt("~n~s~n", [erl_prettypr:format(Form)]).
