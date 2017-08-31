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

%%% @doc Changing shape of functions
%%%
%%% @private

-module(gpb_codemorpher).

-export([underscore_unused_vars/1]).
-export([locate_record_param/1]).

-export([map_tail_exprs/2]). % intended for testing

-type syntax_tree() :: erl_parse:abstract_form() | % for an af_function_decl()
                       erl_syntax:syntaxTree().
-type pos() :: non_neg_integer().

%% @doc Replace unused function params and case clause patterns with
%% underscore Remove record field match patterns that are unused.
%% Example:
%% ```
%%     f(Bin, Z1, Z2, #{a=A, b=B, c=C}=M, Tr) ->
%%         ...use of Bin...
%%         ...use of B...
%% '''
%% gets turned into:
%% ```
%%     f(Bin, _, _, #{b=B}, Tr) ->
%%         ...use of Bin...
%%         ...use of B...
%% '''
%% Similarly for case clauses.
-spec underscore_unused_vars(syntax_tree()) -> syntax_tree().
underscore_unused_vars(FnSTree) ->
    function = erl_syntax:type(FnSTree), % assert
    FnName = erl_syntax:function_name(FnSTree),
    Clauses = erl_syntax:function_clauses(FnSTree),
    B0 = ordsets:from_list([]),
    Clauses1 = [underscore_aux1(erl_syntax_lib:annotate_bindings(Clause, B0))
                || Clause <- Clauses],
    erl_syntax:copy_pos(
      FnSTree,
      erl_syntax:function(FnName, Clauses1)).

underscore_aux1(STree) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:type(Node) of
                  clause ->
                      Patterns = erl_syntax:clause_patterns(Node),
                      Body = erl_syntax:clause_body(Node),
                      Guard = erl_syntax:clause_guard(Node),
                      UsedVars = ordsets:union(get_used_vars(Guard),
                                               get_used_vars_l(Body)),
                      Patterns1 = [reduce_match_underscore(
                                     underscore_if_unused(Pattern,UsedVars))
                                   || Pattern <- Patterns],
                      erl_syntax:copy_pos(
                        Node,
                        erl_syntax:clause(Patterns1, Guard, Body));
                  _ ->
                      Node
              end
      end,
      STree).

get_used_vars(Node) ->
    case proplists:get_value(free, erl_syntax:get_ann(Node)) of
        undefined -> ordsets:new();
        Used      -> Used
    end.

get_used_vars_l(Nodes) ->
    lists:foldl(fun(N, Acc) -> ordsets:union(get_used_vars(N), Acc) end,
                ordsets:new(),
                Nodes).

underscore_if_unused(STree, UsedVars) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case is_unused_var(Node, UsedVars) of
                  true -> erl_syntax:underscore();
                  _ -> Node
              end
      end,
      STree).

is_unused_var(Node, UsedVars) ->
    case erl_syntax:type(Node) of
        variable ->
            Name = erl_syntax:variable_name(Node),
            not ordsets:is_element(Name, UsedVars);
        _Type ->
            false
    end.

reduce_match_underscore(STree) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case test_match_underscore(Node) of
                  {match, {'_', R}} -> R;
                  {match, {L, '_'}} -> L;
                  record_expr -> reduce_fields_matching_underscore(Node);
                  _ -> Node
              end
      end,
      STree).

test_match_underscore(Node) ->
    case erl_syntax:type(Node) of
        match_expr ->
            P = erl_syntax:match_expr_pattern(Node),
            B = erl_syntax:match_expr_body(Node),
            {match, {test_underscore(P), test_underscore(B)}};
        record_expr ->
            record_expr;
        _ ->
            other
    end.

reduce_fields_matching_underscore(Node) ->
    Arg = erl_syntax:record_expr_argument(Node),
    T = erl_syntax:record_expr_type(Node),
    Fs = erl_syntax:record_expr_fields(Node),
    Fs1 = [F || F <- Fs,
                test_underscore(erl_syntax:record_field_value(F)) /= '_'],
    erl_syntax:copy_pos(
      Node,
      erl_syntax:record_expr(Arg, T, Fs1)).

test_underscore(Node) ->
    case erl_syntax:type(Node) of
        underscore -> '_';
        _ -> Node
    end.

%% @doc Given a syntax tree for a function, locate the parameter that is a
%% record.  Example: For `fn(Bin, Z1, Z2, #r{f=F}=M, Tr) -> ...', return 4.
%% If no such parameter is found, fail with badarg.
-spec locate_record_param(Function::syntax_tree()) -> pos().
locate_record_param(FnSTree) ->
    function = erl_syntax:type(FnSTree), % assert
    Clauses = erl_syntax:function_clauses(FnSTree),
    case lists:usort(lists:append([find_r_params(C) || C <- Clauses])) of
        [N] when is_integer(N) ->
            N;
        _ ->
            error(badarg)
    end.

find_r_params(Clause) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    [I || {I, P} <- index_seq(Patterns),
          is_r_param(P)].

is_r_param(Pattern) ->
    erl_syntax_lib:fold(
      fun(Node, B) -> B orelse erl_syntax:type(Node) == record_expr end,
      false,
      Pattern).

%% @doc Transform tail expressions, possibly based on params.
%% Takes a map-function and a syntax tree for a function-to-transform.  For
%% each function clause in the function-to-transform, the map-function is
%% called with the list of parameters. It must return both a (possibly
%% changed) list of parameters, and a new map-function that is called for
%% each tail expressions of the function clause.
%%
%% Example:
%% ```
%%   F1 = fun(Params) when is_list(Params) ->
%%              F2 = fun(TailExpressions) ->
%%                        ...transform tail expression depending on Params...
%%                   end,
%%              {Params, F2}
%%        end
%%   map_tail_exprs(F1, SyntaxTreeForFunctionToTransform) -> NewFunction.
%% '''
-spec map_tail_exprs(F1, Funcion::syntax_tree()) -> syntax_tree() when
      F1 :: fun((Params::[syntax_tree()]) -> {Params1::[syntax_tree()], F2}),
      F2 :: fun((TailExpr::syntax_tree()) -> TailExpr1),
      TailExpr1 ::syntax_tree() | [syntax_tree()].
map_tail_exprs(F1, FnSTree) ->
    function = erl_syntax:type(FnSTree), % assert
    FnName = erl_syntax:function_name(FnSTree),
    Clauses = erl_syntax:function_clauses(FnSTree),
    Clauses1 = [map_fn_clause_tails(F1, C) || C <- Clauses],
    erl_syntax:copy_pos(
      FnSTree,
      erl_syntax:function(FnName, Clauses1)).

map_fn_clause_tails(F1, Clause) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    Body = erl_syntax:clause_body(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    {Patterns1, F2} = F1(Patterns),
    Clause1 = erl_syntax:copy_pos(
                Clause,
                erl_syntax:clause(Patterns1, Guard, Body)),
    map_tails(F2, Clause1).

map_tails(F, Clause) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    Body = erl_syntax:clause_body(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    Body1 = map_tails2(F, Body),
    erl_syntax:copy_pos(
      Clause,
      erl_syntax:clause(Patterns, Guard, Body1)).

map_tails2(F, Exprs) ->
    [E | Preceding] = lists:reverse(Exprs),
    case erl_syntax:type(E) of
        if_expr ->
            Cs = erl_syntax:if_expr_clauses(E),
            Cs1 = [map_tails(F, C) || C <- Cs],
            E1 = erl_syntax:copy_pos(E, erl_syntax:if_expr(Cs1)),
            lists:reverse([E1 | Preceding]);
        case_expr ->
            A = erl_syntax:case_expr_argument(E),
            Cs = erl_syntax:case_expr_clauses(E),
            Cs1 = [map_tails(F, C) || C <- Cs],
            E1 = erl_syntax:copy_pos(E, erl_syntax:case_expr(A, Cs1)),
            lists:reverse([E1 | Preceding]);
        _ ->
            case F(E) of
                E1 when not is_list(E1) ->
                    lists:reverse([E1 | Preceding]);
                E1s when is_list(E1s) ->
                    lists:reverse(lists:reverse(E1s, Preceding))
            end
    end.

index_seq(L) ->
    lists:zip(lists:seq(1,length(L)), L).
