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

-module(gpb_codegen).
-export([parse_transform/2]).

-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

%% This parse transform provides the following re-writes:
%%
%% <dl>
%%   <dt>`gpb_codegen:mk_fn(FnName, fun(Args) -> Body end)'</dt>
%%   <dd><p>will be replaced by a parse-tree for a function `FnName',
%%         with `Args' and `Body' as in the specified.</p>
%%       <p>The `Body' may contain meta variables---variables that end in
%%         at-character, for example `Var@'.
%%         These are not variables in the parse-tree, but the values
%%         they have in the surrounding code is used. Since the value
%%         will end up in a parse tree, it cannot be a pid, a
%%         reference or a fun. It must be a value you could write in
%%         source code.</p>
%%       <p>The `Body' may also cotain parse-tree-meta-variables---variables
%%         that end in two at-characters, for example `Var@@'.
%%         These de note parse-tree values that are replaced in verbatim.
%%   </dd>
%% </dl>

parse_transform(Forms, Opts) ->
    transform_forms(Forms, Opts).

transform_forms(Forms, Opts) ->
    Mapper = mk_transform_fn(Forms),
    [debug_form(erl_syntax:revert(transform_form(Mapper, Form)), Opts)
     || Form <- Forms].

debug_form(NewForm, Opts) ->
    case debug_form_generation_p(Opts) of
        true ->
            try io:format("~s~n", [erl_prettypr:format(NewForm)])
            catch _:_ -> io:format("Non-pretty-printable:~n  ~p", [NewForm])
            end,
            NewForm;
        false ->
            NewForm
    end.

debug_form_generation_p(Opts) ->
    proplists:get_bool(debug_pt, proplists:unfold(Opts)).

mk_transform_fn(Forms) ->
    fun(Node) ->
            Type = erl_syntax:type(Node),
            transform_node(Type, Node, Forms)
    end.

transform_form(Mapper, Form) ->
    try
        erl_syntax_lib:map(Mapper, Form)
    catch error:Reason ->
            ST = erlang:get_stacktrace(),
            io:format("~p: parse transform failed:~n"
                      ++ "  for form:~n"
                      ++ "    ~p~n"
                      ++ "  error:~n"
                      ++ "    ~p~n"
                      ++ "    ~p~n",
                      [?MODULE, Form, Reason, ST]),
            erlang:error({parse_transform_error, Reason, ST})
    end.

transform_node(application, Node, Forms) ->
    case erl_syntax_lib:analyze_application(Node) of
        {?MODULE, {mk_fn, 2}} ->
            [FnNameExpr, FnDef] = erl_syntax:application_arguments(Node),
            transform_mk_fn(FnNameExpr, FnDef, Forms);
        _X ->
            Node
    end;
transform_node(_Type, Node, _Forms) ->
    Node.

%% transform a "call" to gpb_codegen:mk_fn(Name, Def)
%% return a parse-tree for an expression that produces a
%% a parse-tree for a function definition
transform_mk_fn(FnNameExpr, DefAsFun, AllForms) ->
    case erl_syntax:type(DefAsFun) of
        fun_expr ->
            FnClauses = erl_syntax:fun_expr_clauses(DefAsFun),
            mk_partly_abstract_function(FnNameExpr, FnClauses);
        implicit_fun ->
            case analyze_implicit_fun_name(DefAsFun) of
                {DFnName, Arity} when is_integer(Arity) ->
                    FnClauses = find_function_clauses(AllForms, DFnName, Arity),
                    mk_partly_abstract_function(FnNameExpr, FnClauses);
                {Module, {FnName, Arity}} ->
                    erlang:error({?MODULE,not_supported,mk_fn,remote_fn,
                                  ?ff("~p:~p/~w", [Module, FnName, Arity])})
            end
    end.

%% Return a the function's parse-tree in abstract form, except for the
%% expression representing the function name
mk_partly_abstract_function(FnNameExpr, FnClauses) ->
    Placeholder = lists:concat(["-<",?MODULE,"-placeholder>-"]), % odd enough
    FnNameExprPlaceholder = erl_syntax:atom(Placeholder),
    %% Couldn't find anything useful neither in erl_syntax nor in
    %% erl_syntax_lib for making abstract a tree except for some
    %% subterm of it.
    %%
    %% Substituting was the best I could come up with,
    %% while still being agnostic about the erl_parse tree format.
    %% I think I only rely on same representation of atoms
    %% (which do have a line number though...)
    subst_meta_var_values(
      substitute(
        erl_parse:abstract(
          erl_syntax:revert(
            erl_syntax:function(FnNameExprPlaceholder, FnClauses))),
        erl_syntax:revert(FnNameExprPlaceholder),
        FnNameExpr)).

find_function_clauses([Form | Rest], FnName, Arity) ->
    case erl_syntax:type(Form) of
        function ->
            case analyze_function_name(Form) of
                {FnName, Arity} ->
                    erl_syntax:function_clauses(Form);
                _X ->
                    find_function_clauses(Rest, FnName, Arity)
            end;
        _ ->
            find_function_clauses(Rest, FnName, Arity)
    end;
find_function_clauses([], FnName, Arity) ->
    erlang:error({reference_to_undefined_function,FnName,Arity}).

substitute(Term, TermToBeChanged, NewTerm) ->
    map_term(fun(Node) ->
                     if Node =:= TermToBeChanged -> NewTerm;
                        true                     -> Node
                     end
             end,
             Term).

subst_meta_var_values(Term) ->
    map_term(fun try_subst_meta_var_value/1, Term).

map_term(F, [Term | Rest]) ->
    case F(Term) of
        Term -> [map_term(F, Term) | map_term(F, Rest)];
        New  -> [New | map_term(F, Rest)]
    end;
map_term(F, Tuple) when is_tuple(Tuple) ->
    case F(Tuple) of
        Tuple -> list_to_tuple(map_term(F, tuple_to_list(Tuple)));
        New   -> New
    end;
map_term(F, Other) ->
    F(Other).

try_subst_meta_var_value(Term) ->
    try begin C = erl_syntax:concrete(Term), {erl_syntax:type(C), C} end of
        {variable, ConcreteTerm} ->
            case var_metaness(erl_syntax:variable_name(ConcreteTerm)) of
                meta       -> mk_apply(erl_parse, abstract, [ConcreteTerm]);
                parse_meta -> ConcreteTerm;
                ordinary   -> Term
            end;
        _ ->
            Term
    catch _:_ ->
            Term
    end.

var_metaness(VarName) ->
    VarNameAsStr = atom_to_list(VarName),
    case lists:suffix("@@", VarNameAsStr) of
        true  -> parse_meta;
        false -> case lists:suffix("@", VarNameAsStr) of
                     true  -> meta;
                     false -> ordinary
                 end
    end.

mk_apply(M, F, Args) when is_atom(M), is_atom(F) ->
    erl_syntax:revert(
      erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), Args)).

%% -> {Name,Arity} | {Module,{Name,Arity}}
analyze_implicit_fun_name(Tree) ->
    erl_syntax_lib:analyze_function_name(erl_syntax:implicit_fun_name(Tree)).

analyze_function_name(Tree) ->
    Name = erl_syntax_lib:analyze_function_name(erl_syntax:function_name(Tree)),
    Arity = erl_syntax:function_arity(Tree),
    %% Return a format like that of analyze_implicit_fun_name (no module)
    {Name, Arity}.
