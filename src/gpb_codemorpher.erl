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
-export([explode_record_fields_to_params_init/3]).
-export([explode_record_fields_to_params/3]).
-export([implode_to_map_exprs/5]).
-export([implode_to_map_expr/2]).
-export([change_undef_marker_in_clauses/2]).
-export([locate_record_param/1]).
-export([rework_records_to_maps/5]).
-export([rework_records_to_tuples/3]).

-export([rework_clauses_for_records_to_maps/4]). % intended for testing
-export([analyze_case_clauses/2]). % intended for testing
-export([analyze_if_clauses/3]). % intended for testing
-export([map_tail_exprs/2]). % intended for testing
-export([get_call_arg/2]). % intended for testing
-export([splice/4]). % intended for testing
-export([splice_call_arg/4]). % intended for testing

-include("gpb_codegen.hrl").

-import(gpb_lib, [replace_tree/2, replace_term/2, splice_trees/2]).

-type syntax_tree() :: erl_parse:abstract_form() | % for an af_function_decl()
                       erl_syntax:syntaxTree().
-type param_pos() :: non_neg_integer().

-type clause_analysis() :: {clause_meaning(), body()}.
-type body()            :: [syntax_tree()].
-type clause_meaning()  :: match_undefined |
                           {match_tagged_variable,
                            Tag::atom(),
                            Var::syntax_tree()} |
                           {match_undefined, FieldName::atom()} |
                           {match_not_undefined, FieldName::atom()} |
                           {match_tagged_variable,
                            FieldName::atom(),
                            Tag::atom(),
                            Var::syntax_tree()} |
                           {match_variable,
                            FieldName::atom(),
                            Var::syntax_tree()} |
                           '_'.

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
                  record_expr -> reduce_record_fields_matching_underscore(Node);
                  map_expr -> reduce_map_fields_matching_underscore(Node);
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
        map_expr ->
            map_expr;
        _ ->
            other
    end.

reduce_record_fields_matching_underscore(Node) ->
    Arg = erl_syntax:record_expr_argument(Node),
    T = erl_syntax:record_expr_type(Node),
    Fs = erl_syntax:record_expr_fields(Node),
    Fs1 = [F || F <- Fs,
                test_underscore(erl_syntax:record_field_value(F)) /= '_'],
    erl_syntax:copy_pos(
      Node,
      erl_syntax:record_expr(Arg, T, Fs1)).

reduce_map_fields_matching_underscore(Node) ->
    Arg = erl_syntax:map_expr_argument(Node),
    Fs = erl_syntax:map_expr_fields(Node),
    Fs1 = [F || F <- Fs,
                erl_syntax:type(F) == map_field_exact,
                test_underscore(erl_syntax:map_field_exact_value(F)) /= '_'],
    erl_syntax:copy_pos(
      Node,
      erl_syntax:map_expr(Arg, Fs1)).

test_underscore(Node) ->
    case erl_syntax:type(Node) of
        underscore -> '_';
        _ -> Node
    end.

%% @doc Explode record-arguments to parameters-per-field in a tail call, like
%% below by turning code like this (assuming record `#r{}' has fields
%% `a', `b' and `c'):
%% ```
%%     fn(...) -> fn_x(..., #r{b=InitExpr2}, ...);
%% '''
%% into:
%% ```
%%     fn(...) -> fn_x(..., InitA, InitExpr2, InitC, ...);
%% '''
-spec explode_record_fields_to_params_init(
        Function,
        param_pos(),
        {RName::atom(), InitFieldExprs}) -> Function when
      Function :: syntax_tree(),
      InitFieldExprs::[{FieldName::atom(), Expr::syntax_tree()}].
explode_record_fields_to_params_init(FnSTree, ArgPos, {RName, InitExprs}) ->
    function = erl_syntax:type(FnSTree), % assert
    map_tail_exprs(
      fun(Patterns) ->
              {Patterns, mk_tail_expr_exploder(ArgPos, RName, InitExprs)}
      end,
      FnSTree).

%% @doc
%% Explode record-params/arguments to parameters-per-field like below
%% by turning code like this:
%% ```
%%     fn(..., #r{b=B}=M, ...) ->
%%         NewB = ...,
%%         fn(..., M#r{b=NewB}, ...);
%%     fn(..., M ...) ->
%%         M#r{b=lists:reverse(B)}.
%% '''
%% into:
%% ```
%%     fn(..., F1, B, F3, ...) ->
%%          NewB = ...,
%%          fn(..., A, NewB, F3, ...);
%%     fn(..., F1, F2, F3, ...) ->
%%          #r{a=F1, b=lists:reverse(B), c=F3}.
%% '''
%% (for performance reasons, typically)
-spec explode_record_fields_to_params(Function,
                                      param_pos(),
                                      {atom(), [atom()]}) -> Function when
      Function :: syntax_tree().
explode_record_fields_to_params(FnSTree, ArgPos, {RName, FieldNames}) ->
    function = erl_syntax:type(FnSTree), % assert
    map_tail_exprs(
      fun(Patterns) ->
              P = lists:nth(ArgPos, Patterns),
              case is_r_param(P) of
                  true ->
                      %% We have eg: fn(..., #r{b=B}=M, ...) ->
                      %% Change to:  fn(..., F1, B, F3, ...) ->
                      Bs0 = get_record_field_bindings(P),
                      Bs1 = fill_bindings(Bs0, FieldNames),
                      {_, Params} = lists:unzip(Bs1),
                      F2 = mk_tail_expr_exploder(ArgPos, RName, Bs1),
                      {splice(Patterns, ArgPos, 1, Params), F2};
                  false ->
                      %% We have eg: fn(..., M ...) ->
                      %% Change to:  fn(..., F1, F2, F3, ...) ->
                      Bs1 = fill_bindings([], FieldNames),
                      {_, Params} = lists:unzip(Bs1),
                      F2 = mk_tail_expr_exploder(ArgPos, RName, Bs1),
                      {splice(Patterns, ArgPos, 1, Params), F2}
              end
      end,
      FnSTree).

mk_tail_expr_exploder(ArgPos, RName, Binds) ->
    fun(Expr) ->
            case erl_syntax:type(Expr) of
                application ->
                    %% We have eg: call_to(..., M#r{b=NewB}, ...)
                    %% Change to:  call_to(..., F1, NewB, F3, ...)
                    Arg = get_call_arg(Expr, ArgPos),
                    Bs0 = case erl_syntax:type(Arg) of
                              record_expr -> get_record_field_updates(Arg);
                              _           -> [] % eg pass through var
                          end,
                    Args = fill_args(Bs0, Binds),
                    splice_call_arg(Expr, ArgPos, 1, Args);
                record_expr ->
                    %% We have eg: M#r{b=lists:reverse(B)}
                    %% Change to:  #r{a=F1, b=lists:reverse(B), c=F3}
                    T = erl_syntax:atom(RName),
                    Bs0 = get_record_field_updates(Expr),
                    Bs1 = fill_updates(Bs0, Binds),
                    erl_syntax:copy_pos(
                      Expr,
                      erl_syntax:record_expr(none, T, Bs1));
                variable ->
                    %% We have eg: M
                    %% Change to:  #r{a=F1, b=F2, c=F3}
                    Bs1 = fill_updates([], Binds),
                    T = erl_syntax:atom(RName),
                    erl_syntax:copy_pos(
                      Expr,
                      erl_syntax:record_expr(none, T, Bs1))
            end
    end.

fill_bindings(NVs, Names) ->
    [case lists:keyfind(Name, 1, NVs) of
         {Name, Value} ->
             {Name, Value};
         false ->
             {Name, mk_var("F@_", I)}
     end
     || {I, Name} <- index_seq(Names)].

fill_args(Updates, Binds) ->
    [case lists:keyfind(Name, 1, Updates) of
         {Name, Expr} ->
             Expr;
         false ->
             Var
     end
     || {Name, Var} <- Binds].

fill_updates(Updates, Binds) ->
    [case lists:keyfind(Name, 1, Updates) of
         {Name, Expr} ->
             erl_syntax:record_field(erl_syntax:atom(Name), Expr);
         false ->
             erl_syntax:record_field(erl_syntax:atom(Name), Var)
     end
     || {Name, Var} <- Binds].


%% @doc Given a syntax tree for a function, locate the parameter that is a
%% record.  Example: For `fn(Bin, Z1, Z2, #r{f=F}=M, Tr) -> ...', return 4.
%% If no such parameter is found, fail with badarg.
-spec locate_record_param(Function::syntax_tree()) -> param_pos().
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

%% @doc From a syntax tree for a call, retrieve the nth argument.
-spec get_call_arg(Call::syntax_tree(), param_pos()) -> syntax_tree().
get_call_arg(CallSTree, Pos) ->
    application = erl_syntax:type(CallSTree), % assert
    Args = erl_syntax:application_arguments(CallSTree),
    lists:nth(Pos, Args).

%% @doc Given a list and a position, replace a number of elements with
%% elements from a new list.
%% Examples:
%% ```
%%    splice([a,b,c,d,e], 2, 1, [b1, b2]) -> [a,b1,b2,c,d,e]
%%    splice([a,b,c,d,e], 2, 1, [])       -> [a,c,d,e]
%%    splice([a,b,c,d,e], 2, 2, [b3, c3]) -> [a,b3,c3,d,e]
%% '''
-spec splice(L::list(), param_pos(), NToReplace, NewElems::list()) -> L2 when
      NToReplace :: non_neg_integer(),
      L2 :: list().
splice(List,       1, N, NewElems) -> NewElems ++ drop_n(N, List);
splice([H | Rest], P, N, NewElems) -> [H | splice(Rest, P-1, N, NewElems)].

drop_n(0, List)     -> List;
drop_n(N, [_ | Tl]) -> drop_n(N-1, Tl).

%% @doc Given a syntax tree for a call and a position, replace a number of
%% call arguments with a list of syntax trees for new call arguments.
%% See also {@link splice/4}.
-spec splice_call_arg(Call::syntax_tree(),
                      param_pos(), non_neg_integer(),
                      NewArgs::[syntax_tree()]) -> Call1 when
      Call1::syntax_tree().
splice_call_arg(CallSTree, Pos, NumToReplace, NewArgs) ->
    application = erl_syntax:type(CallSTree), % assert
    Op = erl_syntax:application_operator(CallSTree),
    Args = erl_syntax:application_arguments(CallSTree),
    Args1 = splice(Args, Pos, NumToReplace, NewArgs),
    erl_syntax:copy_pos(
      CallSTree,
      erl_syntax:application(Op, Args1)).

%% Example: Given function parameter pattern like "#r{a=F1, b=F2}",
%% or possibly "#r{a=F1, b=F2}=M",
%% return [{a,<F1>}, {b,<F2>}]
%% where <F1> and <F2> are the syntax trees
get_record_field_bindings(STree) ->
    case erl_syntax:type(STree) of
        record_expr ->
            get_record_fields(STree, [variable]);
        match_expr ->
            P = erl_syntax:match_expr_pattern(STree),
            record_expr = erl_syntax:type(P), % assert
            get_record_fields(P, [variable])
    end.

%% Example: Given expression "M#r{a=OldA + NewA}",
%% return [{a,<OldA + NewA>}]
%% where <OldA + NewA> is a syntax tree for the expression.
get_record_field_updates(STree) ->
    get_record_fields(STree, []).

get_record_fields(STree, Opts) ->
    [{FName, Expr}
     || F <- erl_syntax:record_expr_fields(STree),
        FName <- [erl_syntax:atom_value(erl_syntax:record_field_name(F))],
        Expr <- [erl_syntax:record_field_value(F)],
        test_record_field_expr(Expr, Opts)].

test_record_field_expr(Expr, Opts) ->
    case proplists:get_bool(variable, Opts) of
        true ->
            erl_syntax:type(Expr) == variable;
        false ->
            true
    end.

%% @doc
%% Given records that have been translated to exploded parameters, convert
%% such use to map creating expressions, ie collapse it back, but to a map
%% instead of to a record.
%%
%% The construction of the map is complicated somewhat by the fact that some
%% parameter values -- for optional fields -- represent a value indicating that
%% the field is undefined and must be omitted from the map.
%%
%% So construct a map creation expression that first sets the mandatory fields,
%% then adds to the map each optional field, one at a time, unless it has the
%% special value indicating that it is unset.
%%
-spec implode_to_map_exprs(Function, param_pos(), FieldInfos, Undef, Opts) ->
                                  Function1 when
      Function   :: syntax_tree(),
      FieldInfos :: [{FieldName :: atom(),
                      Info :: required | repeated | optional | flatten_oneof}],
      Undef      :: term(),
      Opts       :: gpb_compile:opts(),
      Function1  :: syntax_tree().
implode_to_map_exprs(FnSTree, Field1ArgPos, FieldInfos, Undef, Opts) ->
    map_tail_exprs(
      fun(Params) ->
              FieldParams = lists:sublist(Params,
                                          Field1ArgPos,
                                          length(FieldInfos)),
              F = fun(TailNode) ->
                          record_creation_to_map_exprs(
                            FieldParams, TailNode, FieldInfos, Undef, Opts)
                  end,
              {Params, do_if_tail_is_record_creation(F)}
      end,
      FnSTree).

%% For the maps_unset_optional = present_undefined case, convert records that
%% have been translated to exploded parameters to a map creation expression.
%% All fields can be considered mandatory/present, so just convert it to
%% a map creation expression.
-spec implode_to_map_expr(Function, Opts) -> Function1 when
      Function :: syntax_tree(),
      Opts :: gpb_compile:opts(),
      Function1 :: syntax_tree().
implode_to_map_expr(FnSTree, Opts) ->
    map_tail_exprs(
      fun(Params) ->
              F = fun(Node) ->
                          record_creation_to_map_expr_all_mandatory(Node, Opts)
                  end,
              {Params, do_if_tail_is_record_creation(F)}
      end,
      FnSTree).

record_creation_to_map_expr_all_mandatory(Node, Opts) ->
    erl_syntax:copy_pos(
      Node,
      gpb_lib:map_create(get_record_field_updates(Node), Opts)).

do_if_tail_is_record_creation(F) ->
    fun(TailNode) ->
            case erl_syntax:type(TailNode) of
                record_expr ->
                    F(TailNode);
                variable ->
                    F(TailNode);
                _ ->
                    TailNode
            end
    end.

record_creation_to_map_exprs(FieldParams, Node, FInfos, Undef, Opts) ->
    Updates = get_record_field_updates(Node),
    FIPVs = [begin
                {FName, NewVExpr} = lists:keyfind(FName, 1, Updates),
                {FName, Info, Param, NewVExpr}
            end
            || {{FName,Info}, Param} <- lists:zip(FInfos, FieldParams)],
    {MandFIPVs, OptFIPVs} =
        lists:partition(
          fun({_FName, Info, _Param, _Expr}) ->
                  case Info of
                      required -> true;
                      repeated -> true;
                      optional -> false;
                      flatten_oneof -> false
                  end
          end,
          FIPVs),
    MandFIVs = [{FName, V} || {FName, _Info, _Param, V} <- MandFIPVs],
    InitExpr = erl_syntax:copy_pos(Node, gpb_lib:map_create(MandFIVs, Opts)),
    gpb_lib:do_exprs(
      fun({FName, optional, Param, Expr}, Var) ->
              ?expr(if 'Param' == '$undef' -> 'Var';
                       true -> 'Var#{FName => Expr}'
                    end,
                    [replace_tree('Param', Param),
                     replace_tree('Expr', Expr),
                     replace_term('$undef', Undef),
                     replace_tree('Var', Var),
                     replace_tree('Var#{FName => Expr}',
                                  gpb_lib:map_set(Var, [{FName, Expr}],
                                                  Opts))]);
         ({FName, flatten_oneof, Param, Expr}, Var) ->
              TagVar = gpb_lib:var("Tag~s", [FName]),
              ValueVar = gpb_lib:var("Value~s", [FName]),
              ?expr(if 'Param' == '$undef' -> 'Var';
                       true ->
                            {'Tag', 'Value'} = 'Expr',
                            'Var#{Tag => Value}'
                    end,
                    [replace_tree('Param', Param),
                     replace_tree('Expr', Expr),
                     replace_term('$undef', Undef),
                     replace_tree('Tag', TagVar),
                     replace_tree('Value', ValueVar),
                     replace_tree('Var', Var),
                     replace_tree('Var#{Tag => Value}',
                                  gpb_lib:map_set(Var, [{TagVar, ValueVar}],
                                                  Opts))])
      end,
      InitExpr,
      OptFIPVs).

%% @doc Change a marker for indicating that an optional value is not defined.
%% For records, `undefined' is used, but this is also a valid value for enums,
%% and for maps, we can use something else, such as the atom `$undef'.
-spec change_undef_marker_in_clauses(Function, atom()) -> Function when
      Function :: syntax_tree().
change_undef_marker_in_clauses(FnSTree, NewUndef) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:type(Node) of
                  if_expr ->
                      Cs = erl_syntax:if_expr_clauses(Node),
                      Cs1 = [ch_undef(C, NewUndef) || C <- Cs],
                      erl_syntax:copy_pos(Node, erl_syntax:if_expr(Cs1));
                  case_expr ->
                      A = erl_syntax:case_expr_argument(Node),
                      Cs = erl_syntax:case_expr_clauses(Node),
                      Cs1 = [ch_undef(C, NewUndef) || C <- Cs],
                      erl_syntax:copy_pos(Node, erl_syntax:case_expr(A, Cs1));
                  _ ->
                      Node
              end
      end,
      FnSTree).

ch_undef(Clause, NewUndef) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    Body = erl_syntax:clause_body(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    Patterns1 = [erl_syntax_lib:map(atom_changer(undefined, NewUndef), P)
                 || P <- Patterns],
    Guard1 = safe_tree_map(atom_changer(undefined, NewUndef), Guard),
    erl_syntax:copy_pos(
      Clause,
      erl_syntax:clause(Patterns1, Guard1, Body)).

safe_tree_map(_F, none) ->
    none;
safe_tree_map(F, Tree) ->
    erl_syntax_lib:map(F, Tree).

atom_changer(Old, New) ->
    fun(Node) ->
            case erl_syntax:type(Node) of
                atom ->
                    case erl_syntax:atom_value(Node) of
                        Old ->
                            erl_syntax:copy_pos(Node, erl_syntax:atom(New));
                        _ ->
                            Node
                    end;
                _ ->
                    Node
            end
    end.

%% @doc Transform records expressions to map expressions in a function.
%% Existing field update expressions are recognized, and are converted to
%% corresponding expressions for maps.
%%
%% NB: An `Undef' value of `undefined' assumes the context is
%% maps_unset_optional = `present_undefined', otherwise assumes `omitted'.
-spec rework_records_to_maps(Function, param_pos(), FieldInfos,
                             atom(), Opts) -> Function1 when
      Function :: syntax_tree(),
      FieldInfos :: [{FieldName :: atom(),
                      Info :: required | repeated | optional | flatten_oneof}],
      Opts :: gpb_compile:opts(),
      Function1 :: syntax_tree().
rework_records_to_maps(FnSTree, RecordParamPos, FieldInfos, Undef, Opts) ->
    function = erl_syntax:type(FnSTree), % assert
    FnName = erl_syntax:function_name(FnSTree),
    Clauses = erl_syntax:function_clauses(FnSTree),
    Clauses1 = [rework_records_to_maps_aux(C, RecordParamPos, FieldInfos,
                                           Undef, Opts)
                || C <- Clauses],
    erl_syntax:copy_pos(
      FnSTree,
      erl_syntax:function(FnName, Clauses1)).

rework_records_to_maps_aux(Clause, RecordParamPos, FieldInfos, Undef, Opts) ->
    Params = erl_syntax:clause_patterns(Clause),
    RParam = nth_or_none(RecordParamPos, Params),
    Body = erl_syntax:clause_body(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    {Body1, MaybeNewRecordParam} =
        mapfold_exprs(
          fun(Node, Acc) ->
                  case erl_syntax:type(Node) of
                      record_expr ->
                          rework_body_records_to_maps_2(Node, RParam, Acc,
                                                        Undef, Opts);
                      _ ->
                          {Node, Acc}
                  end
          end,
          none,
          Body),
    IsParam1EmptyBinary = test_is_empty_binary_as_param1(Params),
    if MaybeNewRecordParam == none,
       IsParam1EmptyBinary ->
            %% The function-clause is the one that will return the
            %% record-reworked-to-map
            Params1 = [rework_param_records_to_maps_aux2(P, Opts)
                       || P <- Params],
            FieldsToFlatten = [FName || {FName, flatten_oneof} <- FieldInfos],
            Body2 = records_to_maps_flatten_oneof(FieldsToFlatten, Body1, Opts),
            erl_syntax:copy_pos(
              Clause,
              erl_syntax:clause(Params1, Guard, Body2));
       MaybeNewRecordParam == none ->
            Params1 = [rework_param_records_to_maps_aux2(P, Opts)
                       || P <- Params],
            erl_syntax:copy_pos(
              Clause,
              erl_syntax:clause(Params1, Guard, Body1));
       MaybeNewRecordParam /= none ->
            Params1 = set_nth(RecordParamPos, MaybeNewRecordParam, Params),
            erl_syntax:copy_pos(
              Clause,
              erl_syntax:clause(Params1, Guard, Body1))
    end.

nth_or_none(Pos, L) when length(L) >= Pos -> lists:nth(Pos, L);
nth_or_none(_Pos, _L) -> none.

set_nth(1, New, [_ | Rest]) -> [New | Rest];
set_nth(P, New, [H | Rest]) -> [H | set_nth(P-1, New, Rest)].

test_is_empty_binary_as_param1([Param1 | _]) ->
    erl_syntax:type(Param1) =:= binary
        andalso erl_syntax:binary_fields(Param1) =:= [];
test_is_empty_binary_as_param1(_Params) ->
    false.

mapfold_exprs(F, InitAcc, Exprs) ->
    lists:mapfoldl(
      fun(Expr, LAcc) -> erl_syntax_lib:mapfold(F, LAcc, Expr) end,
      InitAcc,
      Exprs).

rework_body_records_to_maps_2(RExpr, RParam, InitAcc, Undef, Opts) ->
    Arg = erl_syntax:record_expr_argument(RExpr),
    Fields = get_record_fields(RExpr, []),
    {Fields1, MaybeNewFParam} =
        lists:mapfoldl(
          fun({FName, Expr}, Acc) ->
                  case erl_syntax:type(Expr) of
                      if_expr ->
                          rework_rtom_3(FName, Expr, RParam, Undef, Opts);
                      case_expr ->
                          rework_rtom_3(FName, Expr, RParam, Undef, Opts);
                      _ ->
                          {{FName, Expr}, Acc}
                  end
          end,
          InitAcc,
          Fields),
    RExpr1 = if Arg == none -> gpb_lib:map_create(Fields1, Opts);
                Arg /= none -> gpb_lib:map_set(Arg, Fields1, Opts)
             end,
    {erl_syntax:copy_pos(RExpr, RExpr1), MaybeNewFParam}.

rework_rtom_3(FName, Expr, RParam, Undef, Opts) ->
    {NewPattern, Expr1} =
        rework_clauses_for_records_to_maps(RParam, Expr, Undef, Opts),
    {{FName, Expr1}, NewPattern}.

rework_param_records_to_maps_aux2(ParamSTree, Opts) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:type(Node) of
                  record_expr ->
                      Fields = get_record_fields(Node, []),
                      ResNode = gpb_lib:map_match(Fields, Opts),
                      erl_syntax:copy_pos(Node, ResNode);
                  _ ->
                      Node
              end
      end,
      ParamSTree).

records_to_maps_flatten_oneof([], BodyExprs, _Opts) ->
    %% Also called with length(BodyExprs) > 1,
    %% for maps omitted, but then there are no oneof fields to flatten
    BodyExprs;
records_to_maps_flatten_oneof(NamesOfOneofFieldsToFlatten, [BodyExpr], Opts) ->
    gpb_lib:do_exprs(
      fun(FName, Var) ->
              TagVar = gpb_lib:var("Tag@~s", [FName]),
              ValueVar = gpb_lib:var("Value@~s", [FName]),
              TagValue = ?expr({'Tag','Value'},
                               [replace_tree('Tag', TagVar),
                                replace_tree('Value', ValueVar)]),
               ?expr(case 'Var' of
                        '#{field := {Tag, Value}}' ->
                            maps:remove(field, 'Var#{Tag => Value}');
                        _ ->
                            'Var'
                    end,
                    [replace_term(field, FName),
                     replace_tree('Var', Var),
                     replace_tree('#{field := {Tag, Value}}',
                                  gpb_lib:map_match([{FName, TagValue}], Opts)),
                     replace_tree('Var#{Tag => Value}',
                                  gpb_lib:map_set(Var, [{TagVar, ValueVar}],
                                                  Opts))])
      end,
      BodyExpr,
      NamesOfOneofFieldsToFlatten).

%% @doc Analyze case clauses for record field match meanings,
%% so that they can be reworked for maps.
-spec analyze_case_clauses(syntax_tree(), atom()) -> [clause_analysis()].
analyze_case_clauses(CaseExpr, Undef) ->
    [begin
         [Pattern] = erl_syntax:clause_patterns(C),
         analyze_clause(Pattern, C, Undef)
     end
     || C <- erl_syntax:case_expr_clauses(CaseExpr)].

%% @doc Analyze if clauses for record field match meanings, so that they
%% can be reworked for maps. An extra pattern from the function record
%% parameter must be supplied. (A reason not to rewrite such if clauses to
%% case clauses is performance; there are differences in the generated beam
%% assembler that are significant enough to warrant special handling of if
%% clause analysis.)
-spec analyze_if_clauses(syntax_tree(), syntax_tree(), atom()) ->
                                [clause_analysis()].
analyze_if_clauses(Pattern, IfExpr, Undef) ->
    [analyze_clause(Pattern, C, Undef)
     || C <- erl_syntax:if_expr_clauses(IfExpr)].

analyze_clause(Pattern, Clause, Undef) ->
    Body = erl_syntax:clause_body(Clause),
    case clause_pattern_contains_record_match(Pattern) of
        {true, {K,VTree}} ->
            Guard = erl_syntax:clause_guard(Clause),
            {analyze_clause_record_match_vtree(K, VTree, Guard, Undef), Body};
        false ->
            case erl_syntax:type(Pattern) of
                underscore ->
                    {'_', Body};
                atom ->
                    case erl_syntax:atom_value(Pattern) of
                        Undef ->
                            {match_undefined, Body};
                        A ->
                            error({unexpected_atom_case_clause,Clause,A})
                    end;
                tuple ->
                    Elems = erl_syntax:tuple_elements(Pattern),
                    case lists:zip([erl_syntax:type(Elem) || Elem <- Elems],
                                   Elems) of
                        [{atom,E1}, {variable,E2}] ->
                            Tag = erl_syntax:atom_value(E1),
                            Var = erl_syntax:variable_name(E2),
                            {{match_tagged_variable, Tag, Var}, Body};
                        Other ->
                            error({unexpected_tuple_match,Other})
                    end;
                X ->
                    error({unexpected_case_clause,Clause,X})
            end
    end.

clause_pattern_contains_record_match(P) ->
    erl_syntax_lib:fold(
      fun(_Node, {true,X}) ->
              {true,X};
         (Node, false) ->
              case erl_syntax:type(Node) of
                  record_expr ->
                      case erl_syntax:record_expr_fields(Node) of
                          [F] ->
                              Name = erl_syntax:atom_value(
                                       erl_syntax:record_field_name(F)),
                              VTree = erl_syntax:record_field_value(F),
                              {true, {Name, VTree}};
                          [] ->
                              false;
                          Fs ->
                              error({unexpected_multifield_match,Fs})
                      end;
                  _ ->
                      false
              end
      end,
      false,
      P).

analyze_clause_record_match_vtree(K, VTree, Guard, Undef) ->
    case erl_syntax:type(VTree) of
        atom ->
            case erl_syntax:atom_value(VTree) of
                Undef ->
                    {match_undefined, K};
                Other ->
                    error({unexpected_record_field_atom_match,K,Other})
            end;
        variable ->
            Var = erl_syntax:variable_name(VTree),
            if Guard =:= none ->
                    {match_variable, K, Var};
               true ->
                    case is_guard_cmp_undefined(Var, Guard, Undef) of
                        {true, eq} -> {match_undefined, K};
                        {true, ne} -> {match_not_undefined, K};
                        false ->
                            case is_guard_true(Guard) of
                                true ->
                                    '_'; % a catch-all if-clause, likely
                                false ->
                                    error({unexpected_guard,K,Guard,VTree})
                            end
                    end
            end;
        tuple ->
            Elems = erl_syntax:tuple_elements(VTree),
            case lists:zip([erl_syntax:type(Elem) || Elem <- Elems], Elems) of
                [{atom,E1}, {variable,E2}] ->
                    Tag = erl_syntax:atom_value(E1),
                    Var = erl_syntax:variable_name(E2),
                    {match_tagged_variable, K, Tag, Var};
                Other ->
                    error({unexpected_record_field_tuple_match,K,Other})
            end;
        Other ->
            error({unexpected_record_field_match,K,Other,VTree,Guard})
    end.

is_guard_cmp_undefined(Var, Guard, Undef) ->
    case erl_syntax:disjunction_body(Guard) of
        [D] ->
            case erl_syntax:conjunction_body(D) of
                [C] -> is_conjunction_cmp_undefined(Var, C, Undef);
                _   -> false
            end;
        _ ->
            false
    end.

is_guard_true(Guard) ->
    case erl_syntax:disjunction_body(Guard) of
        [D] ->
            case erl_syntax:conjunction_body(D) of
                [C] ->
                    case erl_syntax:type(C) of
                        atom -> erl_syntax:atom_value(C) == true;
                        _    -> false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

is_conjunction_cmp_undefined(Var, C, U) ->
    case erl_syntax:type(C) of
        infix_expr ->
            L = erl_syntax:infix_expr_left(C),
            R = erl_syntax:infix_expr_right(C),
            Op = erl_syntax:operator_name(erl_syntax:infix_expr_operator(C)),
            case {erl_syntax:type(L), Op, erl_syntax:type(R)} of
                {variable, '=:=', atom} -> is_cmp_var_undefined(L,Var,R,U, eq);
                {variable, '=/=', atom} -> is_cmp_var_undefined(L,Var,R,U, ne);
                {variable, '==', atom}  -> is_cmp_var_undefined(L,Var,R,U, eq);
                {variable, '/=', atom}  -> is_cmp_var_undefined(L,Var,R,U, ne);
                {atom, '=:=', variable} -> is_cmp_var_undefined(R,Var,L,U, eq);
                {atom, '=/=', variable} -> is_cmp_var_undefined(R,Var,L,U, ne);
                {atom, '==', variable}  -> is_cmp_var_undefined(R,Var,L,U, eq);
                {atom, '/=', variable}  -> is_cmp_var_undefined(R,Var,L,U, ne);
                _ -> false
            end;
        _ ->
            false
    end.

is_cmp_var_undefined(VarTree, Var, ATree, Undef, CmpHow) ->
    case {erl_syntax:variable_name(VarTree), erl_syntax:atom_value(ATree)} of
        {Var, Undef} ->
            {true, CmpHow};
        _ ->
            false
    end.

rework_clauses_for_records_to_maps(Pattern, Expr, Undef, Opts) ->
    match_expr = erl_syntax:type(Pattern), % assert,
    MsgVar = erl_syntax:match_expr_body(Pattern), % the variable part
    CAns = case erl_syntax:type(Expr) of
               if_expr   -> {'if',analyze_if_clauses(Pattern, Expr, Undef)};
               case_expr -> {'case',analyze_case_clauses(Expr, Undef)}
           end,
    case CAns of
        {'if',[{{match_undefined,FName}, UndefBody}, {'_', DefBody}]} ->
            %% Handle merge of message field (same for required and
            %% optional message fields)
            %%
            %% d_read_field_<msg>_<field>(..., #r{field=Prev}=Msg, ...) ->
            %%    New = ...<decode-sub-msg>(...),
            %%    NewMsg = Msg#r{field = if Prev =:= undefined -> New;
            %%                              true -> merge_<msg2>(Prev,New)
            %%                           end},
            %%    d_read_field_def_<msg>(..., NewMsg, ...).
            %%
            [{FName,Prev}] = get_record_field_bindings(Pattern),
            if Undef == undefined ->
                    %% maps_unset_optional = present_undefined
                    NewPattern = erl_syntax:match_expr(
                                   gpb_lib:map_match([{FName, Prev}], Opts),
                                   MsgVar),
                    {NewPattern, Expr};
               Undef /= undefined ->
                    %% maps_unset_optional = omitted
                    E2 = mk_case_expr(MsgVar,
                                      [mk_matches_map_field_clause(
                                         FName, Prev, DefBody, Opts),
                                       mk_orelse_clause(UndefBody)]),
                    {MsgVar, erl_syntax:revert(E2)}
            end;
        {'case',[{match_undefined, _UndefBody},
                 {{match_tagged_variable,Tag,VarName}, TaggedBody},
                 {'_', OtherBody}]} ->
            %% Handle merge of oneof message alternative:
            %%
            %% d_read_field_<msg>_<f>(..., #r{f=FPrev}=Msg, ...) ->
            %%    New = ...<decode-sub-msg>(...),
            %%    Msg2 = Msg#r{f = case FPrev of
            %%                         undefined -> {t,New};
            %%                         {t,Old}   -> {t,merge(Old,New)};
            %%                         _         -> {t,New}
            %%                     end},
            %%    d_read_field_def_<msg>(..., NewMsg, ...).
            %%
            [{FName,_Prev}] = get_record_field_bindings(Pattern),
            Var = erl_syntax:variable(VarName),
            E2 = mk_case_expr(MsgVar,
                              [mk_matches_tagged_map_field_clause(
                                 FName, Tag, Var, TaggedBody, Opts),
                               mk_orelse_clause(OtherBody)]),
            {MsgVar, erl_syntax:revert(E2)};
        {'case',[{{match_tagged_variable,Tag,VarName}, TaggedBody},
                 {'_', OtherBody}]} ->
            [{FName,_Prev}] = get_record_field_bindings(Pattern),
            Var = erl_syntax:variable(VarName),
            E2 = mk_case_expr(MsgVar,
                              [mk_matches_tagged_map_field_clause(
                                 FName, Tag, Var, TaggedBody, Opts),
                               mk_orelse_clause(OtherBody)]),
            {MsgVar, erl_syntax:revert(E2)}
    end.

-spec rework_records_to_tuples(Function, atom(), InitExprs) -> Function1 when
      Function :: syntax_tree(),
      InitExprs :: [{FName :: atom(), InitExpr :: syntax_tree()}],
      Function1 :: syntax_tree().
rework_records_to_tuples(FnSTree, MsgName, InitExprs) ->
    AnnotatedFnSTree = annotate_record_nodes(FnSTree),
    rework_records_to_tuples2(AnnotatedFnSTree, MsgName, InitExprs).

annotate_record_nodes(FnSTree) ->
    map_record_exprs(
      fun(Node, #{position := {param, _},
                  returns_record := true}=Ctxt) ->
              case test_match_expr_binding(Node) of
                  {true, _Pattern, Var} ->
                      {Node, Ctxt#{msg_var => erl_syntax:variable_name(Var)}};
                  false ->
                      {Node, Ctxt}
              end;
         (Node, #{position := body,
                  returns_record := true,
                  is_return_expr := true,
                  msg_var := MsgVar}=Ctxt) ->
              case erl_syntax:type(Node) of
                  variable ->
                      case erl_syntax:variable_name(Node) of
                          MsgVar ->
                              Node1 = erl_syntax:add_ann(record_expr, Node),
                              {Node1, Ctxt};
                          _Other ->
                              {Node, Ctxt}
                      end;
                  _ ->
                      {Node, Ctxt}
              end;
         (Node, Ctxt) ->
              {Node, Ctxt}
      end,
      FnSTree).

rework_records_to_tuples2(FnSTree, MsgName, InitExprs) ->
    {TPosByFName, TVarByFName, _} =
        lists:foldl(
          fun({FName, _InitExpr}, {AccPos, AccVar, I}) ->
                  AccPos1 = AccPos#{FName => I},
                  AccVar1 = AccVar#{FName => gpb_lib:var_n("T@@", I)},
                  {AccPos1, AccVar1, I + 1}
          end,
          {#{}, #{}, 1},
          InitExprs),
    FNames = [FName || {FName, _InitExpr} <- InitExprs],
    Type = erl_syntax:atom(MsgName),
    map_record_exprs(
      only_for_record_nodes(
        fun(Node, #{position := {param, _},
                    returns_record := IsReturnClause}=Ctxt) ->
                Fields = record_kv_fields(Node),
                if not IsReturnClause ->
                        TElems = [case lists:keyfind(FName, 1, Fields) of
                                      {FName, Var} -> Var;
                                      false -> erl_syntax:underscore()
                                  end
                                  || FName <- FNames],
                        Tuple = erl_syntax:tuple(TElems),
                        {Tuple, Ctxt};
                   IsReturnClause ->
                        TElems = [case lists:keyfind(FName, 1, Fields) of
                                      {FName, Var} -> Var;
                                      false -> maps:get(FName, TVarByFName)
                                  end
                                  || FName <- FNames],
                        Tuple = erl_syntax:tuple(TElems),
                        Ctxt1 = Ctxt#{f_vars => lists:zip(FNames, TElems)},
                        Ctxt2 = add_action(fun remove_match_expr_binding/2,
                                           Ctxt1),
                        {Tuple, Ctxt2}
                end;
           (Node, #{position := guard}=Ctxt) ->
                {Node, Ctxt};
           (Node, #{position := body,
                    returns_record := true,
                    f_vars := FVars}=Ctxt) ->
                case erl_syntax:type(Node) of
                    record_expr ->
                        Fields = record_kv_fields(Node),
                        Fields1 = [case lists:keyfind(FName, 1, Fields) of
                                       false -> lists:keyfind(FName,1, FVars);
                                       {FName, Expr} -> {FName, Expr}
                                   end
                                   || FName <- FNames],
                        Node1 = record_kv_expr(Type, Fields1),
                        {Node1, Ctxt};
                    variable ->
                        Node1 = record_kv_expr(Type, FVars),
                        {Node1, Ctxt}
                end;
           (Node, #{position := body,
                    returns_record := false}=Ctxt) ->
                Arg = erl_syntax:record_expr_argument(Node),
                Fields = record_kv_fields(Node),
                if Arg == none ->
                        TElems = [case lists:keyfind(FName, 1, Fields) of
                                      {FName, Var} -> Var;
                                      false -> InitExpr
                                  end
                                  || {FName, InitExpr} <- InitExprs],
                        Node1 = erl_syntax:tuple(TElems),
                        {Node1, Ctxt};
                   Arg /= none ->
                        %% Record update expression.
                        %% Translate to Var#{a = A} to
                        %% setelement(TPos, Var, A)
                        Node1 = lists:foldl(
                                  fun({FName, Val}, Acc) ->
                                          #{FName := TPos} = TPosByFName,
                                          mk_setelement_call(TPos, Acc, Val)
                                  end,
                                  Arg,
                                  Fields),
                        {Node1, Ctxt}
                end
        end),
      FnSTree).

map_record_exprs(Fn, FnSTree) ->
     function = erl_syntax:type(FnSTree), % assert
     FnName = erl_syntax:function_name(FnSTree),
     Clauses = erl_syntax:function_clauses(FnSTree),
     Clauses1 = [map_record_exprs_clause(Fn, C) || C <- Clauses],
     erl_syntax:copy_pos(
       FnSTree,
       erl_syntax:function(FnName, Clauses1)).

map_record_exprs_clause(Fn, Clause) ->
    Params = erl_syntax:clause_patterns(Clause),
    Guard = erl_syntax:clause_guard(Clause),
    Body = erl_syntax:clause_body(Clause),

    IsParam1EmptyBinary = test_is_empty_binary_as_param1(Params),
    Ctxt0 = #{returns_record => IsParam1EmptyBinary},
    {Params1, Ctxt1} = map_record_expr_params(Fn, Params, Ctxt0),
    {Guard1, Ctxt2} = map_record_expr_guard(Fn, Guard, Ctxt1),
    {Body1, _Ctxt3} = map_record_expr_body(Fn, Body, Ctxt2),
    erl_syntax:clause(Params1, Guard1, Body1).

map_record_expr_params(Fn, Params, Ctxt0) ->
    lists:mapfoldl(
      fun({I, Param}, Ctxt) ->
              Ctxt1 = Ctxt#{position => {param, I}},
              {Param1, Ctxt2} = erl_syntax_lib:mapfold(Fn, Ctxt1, Param),
              Actions = maps:get(actions, Ctxt2, []),
              Ctxt3 = maps:remove(actions, Ctxt2),
              run_actions(Param1, Ctxt3, Actions) % -> {Param2, Ctxt4}
      end,
      Ctxt0,
      gpb_lib:index_seq(Params)).

map_record_expr_guard(_Fn, none, Ctxt0) ->
    {none, Ctxt0};
map_record_expr_guard(Fn, Guard, Ctxt0) ->
    Ctxt1 = Ctxt0#{position => guard},
    erl_syntax_lib:mapfold(Fn, Ctxt1, Guard).

map_record_expr_body(Fn, Body, Ctxt0) ->
    BodyLen = length(Body),
    lists:mapfoldl(
      fun({I, Expr}, Ctxt) ->
              Ctxt1 = Ctxt#{is_return_expr => I == BodyLen},
              {Expr1, Ctxt2} = erl_syntax_lib:mapfold(Fn, Ctxt1, Expr),
              Actions = maps:get(actions, Ctxt2, []),
              Ctxt3 = maps:remove(actions, Ctxt2),
              run_actions(Expr1, Ctxt3, Actions)
      end,
      Ctxt0#{position => body},
      index_seq(Body)).

only_for_record_nodes(Fn) ->
    fun(Node, Ctxt) ->
            case test_record_node(Node) of
                true -> Fn(Node, Ctxt);
                false ->  {Node, Ctxt}
            end
    end.

add_action(ActionFn, #{actions := Actions}=Ctxt) ->
    Ctxt#{actions := Actions ++ [ActionFn]};
add_action(ActionFn, Ctxt) ->
    Ctxt#{actions => [ActionFn]}.

run_actions(Node0, Ctxt0, Actions) ->
    lists:foldl(
      fun(Action, {Node, Ctxt}) -> Action(Node, Ctxt) end,
      {Node0, Ctxt0},
      Actions).

%% Test if an expression is <Pattern> = Var
test_match_expr_binding(Node) ->
    case erl_syntax:type(Node) of
        match_expr ->
            Pattern = erl_syntax:match_expr_pattern(Node),
            Body = erl_syntax:match_expr_body(Node),
            case erl_syntax:type(Body) of
                variable ->
                    {true, Pattern, Body};
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% If an expression is <Pattern> = Var, replace it with just <Pattern>
remove_match_expr_binding(Param, Ctxt) ->
    case test_match_expr_binding(Param) of
        {true, Pattern, _Var} -> {Pattern, Ctxt};
        false                 -> {Param, Ctxt}
    end.

%% True if it s record_expr or has been annotated with a record_expr.
test_record_node(Node) ->
    case erl_syntax:type(Node) of
        record_expr ->
            true;
        _ ->
            Annotations = erl_syntax:get_ann(Node),
            lists:member(record_expr, Annotations)
    end.

%% Return a 2-tuple list with field names as atoms, not syntax trees
record_kv_fields(RecordExpr) ->
    [{erl_syntax:atom_value(erl_syntax:record_field_name(Field)),
      erl_syntax:record_field_value(Field)}
     || Field <- erl_syntax:record_expr_fields(RecordExpr)].

record_kv_expr(Type, Fields) ->
    Fields1 = [erl_syntax:record_field(erl_syntax:atom(FName), Val)
               || {FName, Val} <- Fields],
    erl_syntax:record_expr(none, Type, Fields1).


mk_setelement_call(Index, TupleExpr, NewValueExpr) ->
    erl_syntax:application(
      none,
      erl_syntax:atom(setelement),
      [erl_syntax:integer(Index),
       TupleExpr,
       NewValueExpr]).

mk_case_expr(ArgExpr, Clauses) ->
    erl_syntax:case_expr(ArgExpr, Clauses).

mk_matches_map_field_clause(FName, Var, Body, Opts) ->
    ?case_clause('#{fname := Var}' -> 'Body',
                 [replace_tree('#{fname := Var}',
                               gpb_lib:map_match([{FName,Var}], Opts)),
                  splice_trees('Body', Body)]).

mk_matches_tagged_map_field_clause(FName, Tag, Var, Body, Opts) ->
    TaggedVar = ?expr({tag,'Var'},
                      [replace_term(tag, Tag),
                       replace_tree('Var', Var)]),
    ?case_clause('#{fname := {tag,Var}}' -> 'Body',
                 [replace_tree('#{fname := {tag,Var}}',
                               gpb_lib:map_match([{FName, TaggedVar}], Opts)),
                  splice_trees('Body', Body)]).

mk_orelse_clause(Body) ->
    ?case_clause(_ -> 'Body',
                 [splice_trees('Body', Body)]).

mk_var(Base, Suffix) ->
    erl_syntax:variable(lists:concat([Base, Suffix])).

index_seq(L) ->
    lists:zip(lists:seq(1,length(L)), L).

