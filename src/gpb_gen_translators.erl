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

%%% @doc Generation of translation functions.
%%% This can translate google.protobuf.Any messages into more
%%% useful values, eg by doing yet another interior decoding or
%%% encoding.
%%%
%%% @private

-module(gpb_gen_translators).

-export([format_translators/3]).
-export([format_aux_transl_helpers/1]).
-export([format_merge_translators/3]).

-export([mk_find_tr_fn/3]).
-export([mk_find_tr_fn_elem/4]).
-export([mk_find_tr_fn_elem_or_default/3]).
-export([mk_find_tr_fn_elem_or_default/4]).
-export([mk_elempath_elem/3]).
-export([find_translation/3, find_translation/4]).
-export([default_fn_by_op/2]).
-export([default_any_merge_translator/0]).
-export([default_any_verify_translator/0]).
-export([exists_tr_for_msg/3]).
-export([args_by_op2/1]).
-export([maybe_userdata_param/2]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2, splice_trees/2]).

format_translators(_Defs, #anres{translations=Ts}=AnRes, Opts) ->
    [[[format_field_op_translator(ElemPath, Op, CallTemplate, Opts)
       || {Op, CallTemplate} <- OpTransls]
      || {ElemPath, OpTransls} <- dict:to_list(Ts)],
     format_default_translators(AnRes, Opts)].

format_merge_translators(_Defs, #anres{translations=Ts}=AnRes, Opts) ->
    [[[format_field_op_translator(ElemPath, Op, CallTemplate, Opts)
       || {Op, CallTemplate} <- OpTransls,
          Op == merge]
      || {ElemPath, OpTransls} <- dict:to_list(Ts)],
     format_default_merge_translators(AnRes, Opts)].

format_field_op_translator(ElemPath, Op, CallTemplate, Opts) ->
    ArgTemplate = last_tuple_element(CallTemplate),
    FnName = mk_tr_fn_name(ElemPath, Op),
    {InArgs, OutArgs0, Relations0} =
        if Op /= verify ->
                Ins = Outs = tr_in_args_by_op(Op),
                InOutNames = [Name || {Name,_Value} <- Outs],
                {Ins, Outs, mk_pass_straight_through_rel(InOutNames)};
           Op == verify ->
                [{_,V},{_,Path},{_,UserData}] = Ins = tr_in_args_by_op(Op),
                ErrorF = ?expr(fun(Reason) ->
                                       mk_type_error(Reason, 'Actual', 'Path')
                               end,
                               [replace_tree('Actual', V),
                                replace_tree('Path', Path)]),
                Outs = [{'$1',V},{'$errorf',ErrorF},{'$user_data',UserData}],
                Rels = [{'$1',['$1']},
                        %% $errorf uses $1,$2 if present, else $1,$2 are used
                        {'$errorf',['$1','$2'], ['$1','$2']},
                        {'$user_data',['$user_data']}],
                {Ins, Outs, Rels}
        end,
    OutArgs = OutArgs0 ++ [{'$op', erl_syntax:abstract(Op)}],
    Relations = Relations0 ++ [{'$op', []}],
    {InPatterns, OutParams, _UsedInNames, UsedOutNames} =
        process_tr_params(InArgs, Relations, OutArgs, ArgTemplate),
    Call = case CallTemplate of
               {Fn, ArgTemplate} ->
                   ?expr('$$Fn'('$$OutParams'),
                         [replace_term('$$Fn', Fn),
                          splice_trees('$$OutParams', OutParams)]);
               {Mod, Fn, ArgTemplate} ->
                   ?expr('$$Mod':'$$Fn'('$$OutParams'),
                         [replace_term('$$Mod', Mod),
                          replace_term('$$Fn', Fn),
                          splice_trees('$$OutParams', OutParams)])
           end,
    UsesErrorF = lists:member('$errorf', UsedOutNames),
    Body = if Op == verify, not UsesErrorF ->
                   [Actual,EPath|_] = InPatterns,
                   ?expr(try '$$Call', ok
                         catch _:Reason ->
                                 mk_type_error(Reason,'Actual','Path')
                         end,
                         [replace_tree('$$Call', Call),
                          replace_tree('Actual', Actual),
                          replace_tree('Path', EPath)]);
              true ->
                   Call
           end,
    [inline_attr(FnName,length(InArgs)),
     if Op == verify ->
             %% Dialyzer might complain that "The created fun has no
             %% local return", for a $errorf, which is true, but also
             %% not surprising, so shut this warning down.
             gpb_lib:nowarn_dialyzer_attr(FnName,length(InArgs),Opts);
        true ->
             ""
     end,
     gpb_codegen:format_fn(
       FnName,
       fun('$$InPatterns') ->
               '$$Body'
       end,
       [splice_trees('$$InPatterns', InPatterns),
        replace_tree('$$Body', Body)])].


last_tuple_element(Tuple) ->
    element(tuple_size(Tuple), Tuple).

%% InArgs = [{Name, SyntaxTree}] % eg: [{'$1',?expr(ToPackForEncode)}, ...]
%%     Name = atom()
%% InOutArgRelations = [{OutParamName, [InArg1, InArg2, ...]}]
%%     Example if InOutArgRelations (for verify translators):
%%          [{'$1',['$1']},
%%           {'$errorf', ['$1','$2']},
%%           {'$user_data', ['$3']}]
%% Outs = [{Name, SyntaxTree}] % eg: [{'$1',?expr(ToPackForEncode)}, ...]
%%     Name = atom()
%% ArgsTemplate = [term()]     % eg: ['$1', '$2']
%%          ff                 % or  ['$user_data', ['$1', '$2', a, 4711]]
%% -> {InParams      :: [syntax_tree()],
%%     OutArgs       :: [syntax_tree()],
%%     UsedInArgs    :: [atom()],
%%     UsedOutParams :: [atom()]}
process_tr_params(InArgs, InOutArgRelations, Outs, ArgsTemplate) ->
    {OutArgs, UsedOutNames} =
        lists:mapfoldl(
          fun(ArgTempl, Used) ->
                  {Out, MoreUsed} = abstractify_tr_param(ArgTempl, Outs),
                  {Out, lists:usort(MoreUsed ++ Used)}
          end,
          [],
          ArgsTemplate),
    UnusedOutNames = [N || {N,_} <- Outs] -- UsedOutNames,
    UsedInArgs = compute_used_in_args(UsedOutNames, UnusedOutNames,
                                      InOutArgRelations),
    InParams = underscore_unused_params(InArgs, UsedInArgs),
    {InParams, OutArgs, UsedInArgs, UsedOutNames}.

abstractify_tr_param([H|T], Outs) ->
    {AbsH, UsedH} = abstractify_tr_param(H, Outs),
    {AbsT, UsedT} = abstractify_tr_param(T, Outs),
    {erl_syntax:cons(AbsH, AbsT), lists:usort(UsedH ++ UsedT)};
abstractify_tr_param([], _Outs) ->
    {erl_syntax:nil(), []};
abstractify_tr_param(Tuple, Outs) when is_tuple(Tuple) ->
    {AElems, AUsed} = lists:unzip([abstractify_tr_param(Elem, Outs)
                                   || Elem <- tuple_to_list(Tuple)]),
    {erl_syntax:tuple(AElems), lists:usort(lists:append(AUsed))};
abstractify_tr_param(I, _Outs) when is_integer(I) ->
    {erl_syntax:integer(I), []};
abstractify_tr_param(F, _Outs) when is_float(F) ->
    {erl_syntax:float(F), []};
abstractify_tr_param(A, Outs) when is_atom(A) ->
    case lists:keyfind(A, 1, Outs) of
        {A,Abstr} -> {Abstr, [A]};
        false     -> {erl_syntax:atom(A), []}
    end;
abstractify_tr_param(B, _Outs) when is_binary(B) ->
    {erl_syntax:abstract(B), []};
abstractify_tr_param(B, _Outs) when is_bitstring(B) ->
    %% Current version of erl_syntax (Erlang-18.3) can't do bitstrings,
    %% but erl_parse can. Maybe future erl_syntax versions will...
    try erl_syntax:abstract(B) of
        STree -> {STree, []}
    catch error:{badarg,_} ->
            erl_parse:abstract(B)
    end;
abstractify_tr_param(X, Outs) ->
    abstractify_tr_param_check_for_map(X, Outs).

-ifdef(NO_HAVE_MAPS).
abstractify_tr_param_check_for_map(X, _Outs) ->
    error({translator,cant_make_abstraxt_code_for,X}).
-else.
abstractify_tr_param_check_for_map(M, Outs) when is_map(M) ->
    {MItems, MUsed} =
        lists:unzip([begin
                         {AK,UK} = abstractify_tr_param(K, Outs),
                         {AV,UV} = abstractify_tr_param(V, Outs),
                         {erl_syntax:map_field_assoc(AK, AV), UK ++ UV}
                     end
                     || {K,V} <- maps:to_list(M)]),
    {erl_syntax:map_expr(MItems), lists:usort(lists:append(MUsed))};
abstractify_tr_param_check_for_map(X, _Outs) ->
    error({translator,cant_make_abstraxt_code_for,X}).
-endif. % NO_HAVE_MAPS.

mk_pass_straight_through_rel(Names) ->
    [{Name,[Name]} || Name <- Names].

compute_used_in_args(Used, Unused, InOutArgRelations) ->
    lists:usort(
      lists:append(
        [lists:append(
           [case lists:keyfind(U, 1, InOutArgRelations) of
                {U, Ins}         -> Ins;
                {U, Ins, _Elses} -> Ins;
                false            -> []
            end
            || U <- Used]),
         lists:append(
           [case lists:keyfind(Uu, 1, InOutArgRelations) of
                {Uu, _Ins}        -> [];
                {Uu, _Ins, Elses} -> Elses;
                false             -> []
            end
            || Uu <- Unused])])).

underscore_unused_params(InArgs, UsedInArgs) ->
    [case lists:member(InName, UsedInArgs) of
         true  -> InExpr;
         false -> ?expr(_)
     end
     || {InName, InExpr} <- InArgs].

dollar_i(Name) ->
    list_to_atom(?ff("$~w", [Name])).

tr_in_args_by_op(Op) ->
    [{dollar_i(I), A} || {I,A} <- gpb_lib:index_seq(args_by_op2(Op))]
        ++ [{'$user_data', ?expr(TrUserData)}].

args_by_op2(encode)                   -> [?expr(X)];
args_by_op2(decode)                   -> [?expr(X)];
args_by_op2(decode_init_default)      -> [?expr(InitialValue)];
args_by_op2(decode_repeated_add_elem) -> [?expr(Elem), ?expr(L)];
args_by_op2(decode_repeated_finalize) -> [?expr(L)];
args_by_op2(merge)                    -> [?expr(X1), ?expr(X2)];
args_by_op2(verify)                   -> [?expr(V), ?expr(Path)].

format_aux_transl_helpers(#anres{default_transls=UsedDefaultTransls}) ->
    IsNeeded = fun(F,A) -> sets:is_element({F,A}, UsedDefaultTransls) end,
    [[[inline_attr(id,2),
       "id(X, _TrUserData) -> X.\n",
       "\n"] || IsNeeded(id,2)],
     [[inline_attr(cons,3),
       "cons(Elem, Acc, _TrUserData) -> [Elem | Acc].\n",
       "\n"] || IsNeeded(cons,3)],
     [[inline_attr('lists_reverse',2),
       "'lists_reverse'(L, _TrUserData) -> lists:reverse(L)."
       "\n"] || IsNeeded(lists_reverse,2)],
     [[inline_attr('erlang_++',3),
       "'erlang_++'(A, B, _TrUserData) -> A ++ B."
       "\n"] || IsNeeded('erlang_++',3)]].

format_default_translators(AnRes, Opts) ->
    [format_default_map_translators(AnRes, Opts),
     format_default_any_translators(AnRes, Opts)].

format_default_map_translators(#anres{map_types=MapTypes,
                                      map_value_types=MVT}=AnRes, Opts) ->
    HaveMaps = sets:size(MapTypes) > 0,
    {HaveMapSubmsgs, HaveMapNonSubmsgs} = MVT,
    [%% Auxiliary helpers in case of fields of type map<_,_>
     [case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
          '2tuples' ->
              [inline_attr(mt_maptuple_to_pseudomsg_r,2),
               gpb_codegen:format_fn(
                 mt_maptuple_to_pseudomsg_r,
                 fun({K,V},RName) -> {RName,K,V} end),
               "\n",
               inline_attr(mt_empty_map_r,0),
               gpb_codegen:format_fn(
                 mt_empty_map_r,
                 fun() -> dict:new() end),
               [[inline_attr(mt_add_item_r,2),
                 gpb_codegen:format_fn(
                   mt_add_item_r,
                   fun({_RName,K,V}, D) -> dict:store(K,V,D) end),
                 "\n"]
                || HaveMapNonSubmsgs],
               [[inline_attr(mt_add_item_r_verify_value,2),
                 gpb_codegen:format_fn(
                   mt_add_item_r_verify_value,
                   fun({_,_,undefined}, _) -> error({gpb_error, missing_value});
                      ({_RName,K,V}, D) -> dict:store(K,V,D)
                   end),
                 "\n"]
                || HaveMapSubmsgs],
               inline_attr(mt_finalize_items_r,1),
               gpb_codegen:format_fn(
                 mt_finalize_items_r,
                 fun(D) -> dict:to_list(D) end),
               "\n"];
          maps ->
              {M,K,V} = {?expr(M), ?expr(K), ?expr(V)},
              [inline_attr(mt_maptuple_to_pseudomsg_m,1),
               gpb_codegen:format_fn(
                 mt_maptuple_to_pseudomsg_m,
                 fun({K,V}) -> '#{key => K, value => V}' end,
                 [replace_tree('#{key => K, value => V}',
                               gpb_lib:map_create([{key,K}, {value,V}]))]),
               "\n",
               inline_attr(mt_map_to_list_m,1),
               gpb_codegen:format_fn(
                 mt_map_to_list_m,
                 fun(M) -> maps:to_list(M) end),
               "\n",
               inline_attr(mt_empty_map_m,0),
               gpb_codegen:format_fn(
                 mt_empty_map_m,
                 fun() -> '#{}' end,
                 [replace_tree('#{}', gpb_lib:map_create([]))]),
               "\n",
               [[inline_attr(mt_add_item_m,2),
                 case gpb_lib:is_target_major_version_at_least(18, Opts) of
                     true ->
                         gpb_codegen:format_fn(
                           mt_add_item_m,
                           fun('#{key := K,value := V}', M) -> 'M#{K => V}' end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}])),
                            replace_tree(
                              'M#{K => V}',
                              gpb_lib:map_set(M, [{K,V}]))]);
                     false ->
                         gpb_codegen:format_fn(
                           mt_add_item_m,
                           fun('#{key := K,value := V}', M) ->
                                   maps:put('K', 'V', 'M')
                           end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}])),
                            replace_tree('K', K),
                            replace_tree('V', V),
                            replace_tree('M', M)])
                 end]
                || HaveMapNonSubmsgs],
               [[inline_attr(mt_add_item_m_verify_value,2),
                 case gpb_lib:is_target_major_version_at_least(18, Opts) of
                     true ->
                         gpb_codegen:format_fn(
                           mt_add_item_m_verify_value,
                           fun('#{key := K,value := V}', M) ->
                                   if V =:= '$undef' ->
                                           error({gpb_error, missing_value});
                                      true ->
                                           'M#{K => V}'
                                   end
                           end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}])),
                            replace_tree(
                              'M#{K => V}',
                              gpb_lib:map_set(M, [{K,V}]))]);
                     false ->
                         gpb_codegen:format_fn(
                           mt_add_item_m_verify_value,
                           fun('#{key := K,value := V}', M) ->
                                   if V =:= '$undef' ->
                                           error({gpb_error, missing_value});
                                      true ->
                                           maps:put('K', 'V', 'M')
                                   end
                           end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}])),
                            replace_tree('K', K),
                            replace_tree('V', V),
                            replace_tree('M', M)])
                 end]
                || HaveMapSubmsgs],
               "\n"]
      end,
      format_default_merge_translators(AnRes, Opts)]
     || HaveMaps].

format_default_merge_translators(#anres{map_types=MapTypes}, Opts) ->
    HaveMaps = sets:size(MapTypes) > 0,
    [%% Auxiliary helpers in case of fields of type map<_,_>
     case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
         '2tuples' ->
             [inline_attr(mt_merge_maptuples_r,2),
              gpb_codegen:format_fn(
                mt_merge_maptuples_r,
                fun(L1, L2) ->
                        dict:to_list(dict:merge(fun(_Key, _V1, V2) -> V2 end,
                                                dict:from_list(L1),
                                                dict:from_list(L2)))
                end),
              "\n"];
         maps ->
             [inline_attr(mt_merge_maps_m,2),
              gpb_codegen:format_fn(
                mt_merge_maps_m,
                fun(M1, M2) -> maps:merge(M1,M2) end),
              "\n"]
     end
     || HaveMaps].

format_default_any_translators(#anres{translations=Translations}, _Opts) ->
    Defaults = [{merge, default_any_merge_translator()},
                {verify, default_any_verify_translator()}],
    Needs = compute_needed_default_translations(Translations, Defaults),
    [[[inline_attr(any_m_overwrite,2),
       gpb_codegen:format_fn(
         any_m_overwrite,
         fun(Any2,_) -> Any2 end),
       "\n"] || sets:is_element(merge, Needs)],
     [[gpb_codegen:format_fn(
         any_v_no_check,
         fun(_,_) -> ok end),
       "\n"] || sets:is_element(verify, Needs)]].

compute_needed_default_translations(Translations, Defaults) ->
    dict:fold(
      fun(_ElemPath, Ops, Acc) ->
              lists:foldl(
                fun({Op, Call}, Acc2) ->
                        case lists:member({Op, Call}, Defaults) of
                            true  -> sets:add_element(Op, Acc2);
                            false -> Acc2
                        end
                end,
                Acc,
                Ops)
      end,
      sets:new(),
      Translations).

inline_attr(FnName,Arity) ->
    ?f("-compile({inline,~p/~w}).~n", [FnName,Arity]).

%% -- auxiliary helpers, also used from encoders/decoders/... ------------

mk_find_tr_fn(MsgName, #?gpb_field{name=FName}, AnRes) ->
    ElemPath = [MsgName,FName],
    fun(Op) -> find_translation(ElemPath, Op, AnRes) end;
mk_find_tr_fn(MsgName, #gpb_oneof{name=CFName}, AnRes) ->
    fun({update_elem_path,FName}) ->
            fun(Op) ->
                    ElemPath = [MsgName,CFName,FName],
                    find_translation(ElemPath, Op, AnRes)
            end
    end.

mk_find_tr_fn_elem(MsgName, Field, IsOneof, AnRes) ->
    ElemPath = mk_elempath_elem(MsgName, Field, IsOneof),
    fun(Op) -> find_translation(ElemPath, Op, AnRes) end.

mk_find_tr_fn_elem_or_default(MsgName, {Field, IsOneof}=_XField, AnRes) ->
    mk_find_tr_fn_elem_or_default(MsgName, Field, IsOneof, AnRes).

mk_find_tr_fn_elem_or_default(MsgName, Field, IsOneof, AnRes) ->
    ElemPath = mk_elempath_elem(MsgName, Field, IsOneof),
    fun(Op, Default) -> find_translation(ElemPath, Op, AnRes, Default) end.

mk_elempath_elem(MsgName, #?gpb_field{name=FName,occurrence=Occ}, false) ->
    case Occ of
        repeated -> [MsgName,FName,[]];
        _        -> [MsgName,FName]
    end;
mk_elempath_elem(MsgName, #?gpb_field{name=FName}, {true, CFName}) ->
    [MsgName,CFName,FName].

find_translation(ElemPath, Op, AnRes) ->
    find_translation(ElemPath, Op, AnRes, undefined).
find_translation(ElemPath, Op, #anres{translations=Ts}, Default) ->
    case dict:find(ElemPath, Ts) of
        {ok, OpTransls} ->
            case lists:keyfind(Op, 1, OpTransls) of
                {Op, _Fn} ->
                    mk_tr_fn_name(ElemPath, Op);
                false ->
                    default_fn_by_op(Op, Default)
            end;
        error ->
            default_fn_by_op(Op, Default)
    end.

mk_tr_fn_name([MsgName,FieldName,[]], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s[x]", [Op, MsgName,FieldName]));
mk_tr_fn_name([MsgName,FName,OneofName], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s.~s", [Op, MsgName,FName,OneofName]));
mk_tr_fn_name([MsgName,FieldName], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s", [Op, MsgName,FieldName])).

default_fn_by_op(decode_repeated_add_elem, undefined) ->
    cons;
default_fn_by_op(decode_repeated_finalize, undefined) ->
    lists_reverse;
default_fn_by_op(_, undefined) ->
    id;
default_fn_by_op(_, Fn) ->
    Fn.

default_any_merge_translator() -> {any_m_overwrite,['$2','$user_data']}.

default_any_verify_translator() -> {any_v_no_check,['$1', '$user_data']}.

exists_tr_for_msg(MsgName, Op, #anres{translations=Translations}) ->
    dict:fold(fun(_Key, _OpCalls, true) ->
                      true;
                 ([Name,_Field|_], OpCalls, false) when Name == MsgName ->
                      lists:keymember(Op, 1, OpCalls);
                 (_Key, _OpCalls, false) ->
                      false
              end,
              false,
              Translations).

maybe_userdata_param(Field, Expr) ->
    case is_primitive_type(Field) of
        true -> [];
        false -> [Expr]
    end.

is_primitive_type(#?gpb_field{type={msg,_}}) -> false;
is_primitive_type(#?gpb_field{type={group,_}}) -> false;
is_primitive_type(#?gpb_field{type={map,_,_}}) -> false;
is_primitive_type(_) -> true.

