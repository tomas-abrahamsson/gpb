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
-export([runtime_fn_transform/2, runtime_fn_transform/3]).

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
%%           syntax trees instead.
%%         </dd>
%%         <dt>`{splice_clauses,Marker::atom(),Replacements::[syntaxtree()]}'</dt>
%%         <dd>For case clauses (and function clauses), where the pattern is a
%%           single atom, `Marker', insert the case clauses (or function
%%           clauses) in `Replacements' instead.
%%           See also the `?case_clause/1' macro.
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
        {?MODULE, {case_clause, 1}} ->
            [Expr] = erl_syntax:application_arguments(Node),
            transform_case_expr_to_parse_tree_for_clause(Expr);
        _X ->
            Node
    end;
transform_node(_Type, Node, _Forms) ->
    Node.


%% transform a "call" to gpb_codegen:mk_fn(Name, Def, RtTransforms)
%% into a real (run-time) call to:
%%
%%    ?MODULE:runtime_fn_transform(Name, <parse tree for Def>, RtTransforms)
%%
transform_mk_fn(FnNameExpr, DefAsFun, RtTransforms, AllForms) ->
    case erl_syntax:type(DefAsFun) of
        fun_expr ->
            FnClauses = erl_syntax:fun_expr_clauses(DefAsFun),
            mk_runtime_fn_transform_invoker(FnNameExpr, FnClauses, RtTransforms);
        implicit_fun ->
            case analyze_implicit_fun_name(DefAsFun) of
                {DFnName, Arity} when is_integer(Arity) ->
                    FnClauses = find_function_clauses(AllForms, DFnName, Arity),
                    mk_runtime_fn_transform_invoker(FnNameExpr, FnClauses,
                                                    RtTransforms);
                {Module, {FnName, Arity}} ->
                    erlang:error({?MODULE,not_supported,mk_fn,remote_fn,
                                  ?ff("~p:~p/~w", [Module, FnName, Arity])})
            end
    end.

mk_runtime_fn_transform_invoker(FnNameExpr, FnClauses, RtTransforms) ->
    DummyFnName = erl_syntax:atom(fn_name_to_be_replaced_at_runtime),
    mk_apply(?MODULE, runtime_fn_transform,
             [FnNameExpr,
              erl_parse:abstract(
                erl_syntax:revert(
                  erl_syntax:function(DummyFnName, FnClauses)))
              | RtTransforms]).

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

mk_apply(M, F, Args) when is_atom(M), is_atom(F) ->
    erl_syntax:revert(
      erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), Args)).

transform_case_expr_to_parse_tree_for_clause(Expr) ->
    case erl_syntax:type(Expr) of
        case_expr ->
            [Clause | _] = erl_syntax:case_expr_clauses(Expr),
            erl_parse:abstract(erl_syntax:revert(Clause));
        _X ->
            Expr
    end.

%% Main entry point at runtime.
%%@hidden
runtime_fn_transform(FnName, FnParseTree) ->
    runtime_fn_transform(FnName, FnParseTree, []).

%%@hidden
runtime_fn_transform(FnName, FnParseTree, Transforms) ->
    Clauses = erl_syntax:function_clauses(FnParseTree),
    erl_syntax:revert(
      erl_syntax:function(
        erl_syntax:atom(FnName),
        [lists:foldl(fun apply_transform/2, C, Transforms) || C <- Clauses])).

apply_transform({replace_term, Marker, Replacement}, ParseTree) ->
    erl_syntax_lib:map(term_replacing_mapper(Marker, Replacement),
                       ParseTree);
apply_transform({replace_tree, Marker, Replacement}, ParseTree) ->
    erl_syntax_lib:map(tree_replacing_mapper(Marker, Replacement),
                       ParseTree);
apply_transform({splice_trees, Marker, Replacements}, ParseTree) ->
    splice_trees(Marker, Replacements, ParseTree);
apply_transform({splice_clauses, CaseMarker, Replacements}, ParseTree) ->
    splice_clauses(CaseMarker, Replacements, ParseTree).


term_replacing_mapper(Marker, Replacement) ->
    ReplacementTree = erl_parse:abstract(Replacement),
    tree_replacing_mapper(Marker, ReplacementTree).

tree_replacing_mapper(Marker, Replacement) ->
    fun(Node) ->
            case analyze_atom_as_value(Node) of
                {atom, Marker} -> Replacement;
                {atom, _Other} -> Node;
                non_atom       -> Node
            end
    end.

splice_trees(Marker, Replacements, Tree)   ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            Tree;
        Gs ->
            F = fun(SubTree) -> splice_trees(Marker, Replacements, SubTree) end,
            Gs1 = [case split_list_on_marker(G, Marker) of
                       marker_not_found ->
                           [F(T) || T <- G];
                       {BeforeMarker, _MarkerTree, AfterMarker} ->
                           Before = [F(T) || T <- BeforeMarker],
                           After = [F(T) || T <- AfterMarker],
                           Before ++ Replacements ++ After
                   end
                   || G <- Gs],
            Tree1 = erl_syntax:make_tree(erl_syntax:type(Tree), Gs1),
            erl_syntax:copy_attrs(Tree, Tree1)
    end.

split_list_on_marker(Elems, Marker) -> split_aux(Elems, Marker, []).

split_aux([X | Rest], Marker, Acc) ->
    case analyze_atom_as_value(X) of
        {atom, Marker} -> {lists:reverse(Acc), X, Rest};
        {atom, _Other} -> split_aux(Rest, Marker, [X | Acc]);
        non_atom       -> split_aux(Rest, Marker, [X | Acc])
    end;
split_aux([], _Marker, _Acc) ->
    marker_not_found.

splice_clauses(CMarker, Replacements, Tree) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:type(Node) of
                  case_expr ->
                      Arg = erl_syntax:case_expr_argument(Node),
                      Cs  = erl_syntax:case_expr_clauses(Node),
                      case split_clauses_on_marker(Cs, CMarker) of
                          marker_not_found ->
                              Node;
                          {Before, _MarkerClause, After} ->
                              Cs1 = Before ++ Replacements ++ After,
                              erl_syntax:case_expr(Arg, Cs1)
                      end;
                  _Other ->
                      Node
              end
      end,
      Tree).

split_clauses_on_marker(Clauses, CMarker) ->
    csplit_aux(Clauses, CMarker, []).

csplit_aux([CC | Rest], CMarker, Acc) ->
    case erl_syntax:clause_patterns(CC) of
        [CPattern] ->
            case analyze_atom_as_value(CPattern) of
                {atom, CMarker} -> {lists:reverse(Acc), CC, Rest};
                {atom, _Other}  -> csplit_aux(Rest, CMarker, [CC | Acc]);
                non_atom        -> csplit_aux(Rest, CMarker, [CC | Acc])
            end;
        _CPatterns ->
            csplit_aux(Rest, CMarker, [CC | Acc])
    end;
csplit_aux([], _CMarker, _Acc) ->
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
analyze_atom_as_value(Node) ->
    case erl_syntax:type(Node) of
        atom -> {atom, erl_syntax:atom_value(Node)};
        _    -> non_atom
    end.
