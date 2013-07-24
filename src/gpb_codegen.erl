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
-export([runtime_transform/1, runtime_transform/2]).

-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

%% @doc
%% This parse transform provides the following re-writes:
%%
%% <dl>
%%   <dt>`gpb_codegen:mk_fn(FnName, fun(Args) -> Body end)'</dt>
%%   <dd>Will be replaced by a parse-tree for a function `FnName',
%%       with `Args' and `Body' as in the specified fun.
%%       The `FnName' is evaluated at run-time, not at compile-time.
%%   </dd>
%%   <dt>`gpb_codegen:mk_fn(FnName, fun(Args) -> Body end, RtTransforms)'</dt>
%%   <dd><p>Like gpb_codegen:mk_fn/2, but apply `RtTransforms' at run-time
%%         before returning the parse-tree.</p>
%%       <p>The following `RtTransforms' are available:</p>
%%       <dl>
%%         <dt>`{replace_term,Marker::atom(),Replacement::term()}'</dt>
%%         <dd>Replace any occurrences of `Marker' with the syntax tree
%%           representing `Replacement', which must be something that could
%%           have occurred as a literal term in some program text,
%%           thus it must not contain any funs, pids, ports, references or such.
%%         </dd>
%%         <dt>`{replace_tree,Marker::atom(),Replacement::syntaxtree()}'</dt>
%%         <dd>Replace any occurrences of `Marker' with the syntax tree
%%           `Replacement'.
%%         </dd>
%%         <dt>`{splice_trees,Marker::atom(),Replacements::[syntaxtree()]}'</dt>
%%         <dd>For any list that contains `Marker', insert the `Replacements'
%%           syntax trees instead. Use with care since usage assumes
%%           the `erl_syntax'-internal representation the `Marker'-containing
%%           structure is a list.
%%           <!-- It is difficult to be entirely parse-tree-format agnostic -->
%%         </dd>
%%       </dl>
%%   </dd>
%%   <dt>`gpb_codegen:expr(Expr)'</dt>
%%   <dd>Will be replaced by the parse-tree for a `Expr'.</dd>
%%   <dt>`gpb_codegen:exprs(Expr, ...)'</dt>
%%   <dd>Will be replaced by a list of parse-trees, one for each `Expr'.</dd>
%% </dl>
%% @end
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
            transform_mk_fn(FnNameExpr, FnDef, [], Forms);
        {?MODULE, {mk_fn, 3}} ->
            [FnNameExpr, FnDef, RuntimeTransforms] =
                erl_syntax:application_arguments(Node),
            transform_mk_fn(FnNameExpr, FnDef, [RuntimeTransforms], Forms);
        {?MODULE, {expr, 1}} ->
            [Expr] = erl_syntax:application_arguments(Node),
            erl_parse:abstract(erl_syntax:revert(Expr));
        {?MODULE, {exprs, _Arity}} ->
            Exprs = erl_syntax:application_arguments(Node),
            erl_parse:abstract([erl_syntax:revert(Expr) || Expr <- Exprs]);
        _X ->
            Node
    end;
transform_node(_Type, Node, _Forms) ->
    Node.


%% transform a "call" to gpb_codegen:mk_fn(Name, Def)
%% return a parse-tree for an expression that produces a
%% a parse-tree for a function definition
transform_mk_fn(FnNameExpr, DefAsFun, RuntimeTransforms, AllForms) ->
    case erl_syntax:type(DefAsFun) of
        fun_expr ->
            FnClauses = erl_syntax:fun_expr_clauses(DefAsFun),
            mk_apply(
              ?MODULE, runtime_transform,
              [mk_partly_abstract_function(FnNameExpr, FnClauses)
               | RuntimeTransforms]);
        implicit_fun ->
            case analyze_implicit_fun_name(DefAsFun) of
                {DFnName, Arity} when is_integer(Arity) ->
                    FnClauses = find_function_clauses(AllForms, DFnName, Arity),
                    mk_apply(
                      ?MODULE, runtime_transform,
                      [mk_partly_abstract_function(FnNameExpr, FnClauses)
                       | RuntimeTransforms]);
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
    subst_atom(
      erl_parse:abstract(
        erl_syntax:revert(
          erl_syntax:function(FnNameExprPlaceholder, FnClauses))),
      erl_syntax:revert(FnNameExprPlaceholder),
      FnNameExpr).

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

subst_atom(Term, AtomToBeChanged, NewExpr) ->
    AtomLiteralToBeChanged = erl_syntax:atom_name(AtomToBeChanged),
    map_term(fun(Node) -> try_subst_atom(Node, AtomLiteralToBeChanged, NewExpr)
             end,
             Term).

try_subst_atom(Node, AtomLiteralToBeChanged, NewExpr) ->
    case safe_analyze_atom_as_name(Node) of
        {atom, AtomLiteralToBeChanged} -> NewExpr;
        {atom, _Other}                 -> Node;
        non_atom                       -> Node
    end.

map_term(F, List) when is_list(List) ->
    case F(List) of
        List -> [map_term(F, Elem) || Elem <- List];
        New  -> New
    end;
map_term(F, Tuple) when is_tuple(Tuple) ->
    case F(Tuple) of
        Tuple -> list_to_tuple(map_term(F, tuple_to_list(Tuple)));
        New   -> New
    end;
map_term(F, Other) ->
    F(Other).

mk_apply(M, F, Args) when is_atom(M), is_atom(F) ->
    erl_syntax:revert(
      erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), Args)).

%% Main entry point at runtime
runtime_transform(ParseTree) ->
    runtime_transform(ParseTree, []).

runtime_transform(ParseTree, Transforms) ->
    erl_syntax:revert(lists:foldl(fun apply_transform/2, ParseTree, Transforms)).

apply_transform(Transform, ParseTree) ->
    map_term(transform_to_mapper(Transform), ParseTree).

transform_to_mapper({replace_term, Marker, Replacement}) ->
    term_replacing_mapper(Marker, Replacement);
transform_to_mapper({replace_tree, Marker, Replacement}) ->
    tree_replacing_mapper(Marker, Replacement);
transform_to_mapper({splice_trees, Marker, Replacements}) ->
    param_splicing_mapper(Marker, Replacements).

term_replacing_mapper(Marker, Replacement) ->
    ReplacementTree = erl_parse:abstract(Replacement),
    tree_replacing_mapper(Marker, ReplacementTree).

tree_replacing_mapper(Marker, Replacement) ->
    fun(Node) ->
            case safe_analyze_atom_as_value(Node) of
                {atom, Marker} -> Replacement;
                {atom, _Other} -> Node;
                non_atom       -> Node
            end
    end.

param_splicing_mapper(Marker, Replacements) ->
    %% Handle splicing specially, because if adding params to a
    %% parameter list, then there's also an arity that needs to be
    %% updated. It is set properly when reconstructing a function
    %% definition using erl_syntax:function/2, but not when
    %% adding parameters to a parameter list.
    fun(Term) ->
            case safe_analyze_function(Term) of
                {function, Name, Clauses} ->
                    Mapper = tree_splicing_mapper(Marker, Replacements),
                    NewClauses = [map_clause(Mapper, C) || C <- Clauses],
                    erl_syntax:revert(erl_syntax:function(Name, NewClauses));
                non_function ->
                    Term
            end
    end.

tree_splicing_mapper(Marker, Replacements) ->
    fun(Term) when is_list(Term) ->
            case safe_split_list_on_marker(Term, Marker) of
                marker_not_found ->
                    Term;
                {BeforeMarker, _MarkerTree, AfterMarker} ->
                    Mapper = tree_splicing_mapper(Marker, Replacements),
                    TermsBefore = map_term(Mapper, BeforeMarker),
                    TermsAfter  = map_term(Mapper, AfterMarker),
                    TermsBefore ++ Replacements ++ TermsAfter
            end;
       (OtherTerm) ->
            OtherTerm
    end.

map_clause(Mapper, Clause) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    Guard    = erl_syntax:clause_guard(Clause),
    Body     = erl_syntax:clause_body(Clause),
    erl_syntax:clause(map_term(Mapper, Patterns),
                      map_term(Mapper, Guard),
                      map_term(Mapper, Body)).

safe_split_list_on_marker(Elems, Marker) -> safe_split_aux(Elems, Marker, []).

safe_split_aux([X | Rest], Marker, Acc) ->
    case safe_analyze_atom_as_value(X) of
        {atom, Marker} -> {lists:reverse(Acc), X, Rest};
        {atom, _Other} -> safe_split_aux(Rest, Marker, [X | Acc]);
        non_atom       -> safe_split_aux(Rest, Marker, [X | Acc])
    end;
safe_split_aux([], _Marker, _Acc) ->
    marker_not_found.

%% -> {Name,Arity} | {Module,{Name,Arity}}
analyze_implicit_fun_name(Tree) ->
    erl_syntax_lib:analyze_function_name(erl_syntax:implicit_fun_name(Tree)).

analyze_function_name(Tree) ->
    Name = erl_syntax_lib:analyze_function_name(erl_syntax:function_name(Tree)),
    Arity = erl_syntax:function_arity(Tree),
    %% Return a format like that of analyze_implicit_fun_name (no module)
    {Name, Arity}.

%% -> {atom, atom()} | non_atom
safe_analyze_atom_as_value(Node) ->
    try {erl_syntax:type(Node), erl_syntax:atom_value(Node)} of
        {atom, Value} -> {atom, Value};
        _             -> non_atom
    catch error:_ ->
            non_atom
    end.

%% -> {atom, string()} | non_atom %% the string is not single-quoted
safe_analyze_atom_as_name(Node) ->
    try {erl_syntax:type(Node), erl_syntax:atom_name(Node)} of
        {atom, Name} -> {atom, Name};
        _            -> non_atom
    catch error:_ ->
            non_atom
    end.

safe_analyze_function(Term) ->
    try {erl_syntax:type(Term),
         erl_syntax:function_arity(Term),
         erl_syntax:function_name(Term),
         erl_syntax:function_clauses(Term)} of
        {function, Arity, Name, Clauses} when is_integer(Arity) ->
            {function, Name, Clauses};
        _ ->
            non_function
    catch error:_ ->
            non_function
    end.
