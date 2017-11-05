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

%%% @doc Generation of msg-decoding functions.
%%%
%%% Merging is an integral part of decoding: optional and required
%%% messages that occur multiple times on the wire are merged
%%% recursively. Scalar optional or required fields a merged by
%%% overwriting. Repeated fields are merged by appending.
%%%
%%% @private

-module(gpb_gen_decoders).

-export([format_decoders_top_function/2]).
-export([format_msg_decoders/3]).
-export([format_map_decoders/3]).
-export([format_aux_decoders/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

%% Varints are processed 7 bits at a time.
%% We can expect that we have processed this number of bits before
%% we expect to see the last varint byte, which must have msb==0.
%% 64 - 7 = 57.
-define(NB, 57).
-define(is_msg_or_group(X),
        is_tuple(X)
        andalso tuple_size(X) =:= 2
        andalso (element(1, X) =:= msg orelse element(1, X) =:= group)).

-record(fn, {
          name :: atom(),
          initializes_fields   = false :: boolean(),
          has_finalizer        = false :: boolean(),
          fields_in_tail_calls = false :: boolean(),
          passes_msg           = false :: boolean(),
          tree   % the syntax tree
         }).

format_decoders_top_function(Defs, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_decoders_top_function_msgs(Defs, Opts);
        false -> format_decoders_top_function_no_msgs(Opts)
    end.

format_decoders_top_function_no_msgs(Opts) ->
    ["-spec decode_msg(binary(), atom()) -> no_return().\n",
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, _MsgName) when is_binary(Bin) ->
               erlang:error({gpb_error, no_messages})
       end),
     "-spec decode_msg(binary(), atom(), list()) -> no_return().\n",
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, _MsgName, _Opts) when is_binary(Bin) ->
               erlang:error({gpb_error, no_messages})
       end),
     "\n",
     [["%% epb compatibility\n",
       ?f("-spec decode(atom(), binary()) -> no_return().\n"),
       gpb_codegen:format_fn(
         decode,
         fun(MsgName, Bin) when is_atom(MsgName), is_binary(Bin) ->
                 erlang:error({gpb_error, no_messages})
         end)]
      || gpb_lib:get_epb_functions_by_opts(Opts)]].

format_decoders_top_function_msgs(Defs, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    [if DoNif -> "";
        true ->
             gpb_codegen:format_fn(
               decode_msg,
               fun(Bin, MsgName) when is_binary(Bin) ->
                       call_self(Bin, MsgName, [])
               end,
               [])
     end,
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, MsgName, 'Opts') when is_binary(Bin) ->
               'TrUserData = proplists:get_value(user_data, Opts)',
               case MsgName of
                   '<MsgName>' ->
                       try '<decode-call>'(Bin, 'TrUserData')
                       catch Class:Reason ->
                               StackTrace = erlang:get_stacktrace(),
                               error({gpb_error,
                                      {decoding_failure,
                                       {Bin, '<MsgName>',
                                        {Class, Reason, StackTrace}}}})
                       end
               end
       end,
       [splice_trees('Opts', if DoNif -> [];
                                true  -> [?expr(Opts)]
                             end),
        splice_trees(
          'TrUserData = proplists:get_value(user_data, Opts)',
          if DoNif -> [];
             true  -> [?expr(TrUserData = proplists:get_value(user_data, Opts))]
          end),
        repeat_clauses('<MsgName>',
                       [[replace_term('<MsgName>', MsgName),
                         replace_term('<decode-call>',
                                      gpb_lib:mk_fn(d_msg_, MsgName))]
                        || {{msg,MsgName}, _Fields} <- Defs]),
        splice_trees('TrUserData', if DoNif -> [];
                                      true  -> [?expr(TrUserData)]
                                   end)]),
     [["\n",
       "%% epb compatibility\n",
       gpb_codegen:format_fn(
         decode,
         fun(MsgName, Bin) when is_atom(MsgName), is_binary(Bin) ->
                 decode_msg(Bin, MsgName)
         end),
       [gpb_codegen:format_fn(
          gpb_lib:mk_fn(decode_, MsgName),
          fun(Bin) when is_binary(Bin) ->
                  decode_msg(Bin, 'MsgName')
          end,
          [replace_term('MsgName', MsgName)])
        || {{msg,MsgName}, _Fields} <- Defs]]
      || gpb_lib:get_epb_functions_by_opts(Opts)]].

format_aux_decoders(Defs, AnRes, _Opts) ->
    [format_enum_decoders(Defs, AnRes),
     [format_read_group_fn() || gpb_lib:contains_messages(Defs)]].

format_enum_decoders(Defs, #anres{used_types=UsedTypes}) ->
    %% FIXME: enum values can be negative, but "raw" varints are positive
    %%        insert a 2-complement in the mapping in order to move computations
    %%        from run-time to compile-time??
    [gpb_codegen:format_fn(
       gpb_lib:mk_fn(d_enum_, EnumName),
       fun('<EnumValue>') -> '<EnumSym>';
          (V) -> V % for yet unknown enums
       end,
       [repeat_clauses('<EnumValue>',
                       [[replace_term('<EnumValue>', EnumValue),
                         replace_term('<EnumSym>', EnumSym)]
                        || {EnumSym, EnumValue} <- gpb_lib:unalias_enum(
                                                     EnumDef)])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        gpb_lib:smember({enum,EnumName}, UsedTypes)].

format_map_decoders(Defs, AnRes, Opts0) ->
    Opts1 = case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts0)
            of
                '2tuples' -> [{msgs_as_maps, false} | Opts0];
                maps      -> [{msgs_as_maps, true} | Opts0]
            end,
    format_msg_decoders(Defs, AnRes, Opts1).

format_msg_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoder(Name, MsgDef, Defs, AnRes, Opts)
     || {_Type, Name, MsgDef} <- gpb_lib:msgs_or_groups(Defs)].

format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    %% The general idea here is:
    %%
    %% - The code layout function can assume pass_as_records
    %%   and only generate code for records.
    %% - Code generation for maps is done post-generation
    %%   as additional steps using the code morpher functionality
    %% - The pass_as_params feature (improves performance),
    %%   is also done post-generation using the code-morpher.
    %%
    ROpts = [{msgs_as_maps,false},
             {mapfields_as_maps,false}
             | lists:keydelete(maps_unset_optional, 1, Opts)],
    TrUserDataVar = ?expr(TrUserData),
    InitExprs = init_exprs(MsgName, MsgDef, Defs, TrUserDataVar, AnRes, Opts),
    #anres{d_field_pass_method=FPass} = AnRes,
    RFPass = dict:store(MsgName, pass_as_record, FPass),
    RAnRes = AnRes#anres{d_field_pass_method=RFPass},
    Fns = lists:flatten(
            [format_msg_decoder_read_field(MsgName, MsgDef, InitExprs,
                                           RAnRes),
             format_field_decoders(MsgName, MsgDef, RAnRes, ROpts),
             format_field_skippers(MsgName)]),
    FieldPass = gpb_lib:get_field_pass(MsgName, AnRes),
    MappingUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    FNames = [FName || {FName, _InitExpr} <- InitExprs],
    FOccurrences = field_occurrences(MsgDef),
    %% Compute extra post-generation operations needed for maps
    %% and pass_as_params
    Ops = case {MappingUnset, FieldPass} of
              {records, pass_as_record} ->
                  [underscore_unused_vars()];
              {records, pass_as_params} ->
                  [explode_param_init(MsgName, InitExprs, 4),
                   explode_param_pass(MsgName, FNames, 4),
                   underscore_unused_vars()];
              {{maps, present_undefined},pass_as_record} ->
                  [rework_records_to_maps(4, undefined),
                   underscore_unused_vars(),
                   finalize_marked_map_exprs()];
              {{maps, present_undefined},pass_as_params} ->
                  [explode_param_init(MsgName, InitExprs, 4),
                   explode_param_pass(MsgName, FNames, 4),
                   implode_to_map_exprs_all_mandatory(),
                   underscore_unused_vars(),
                   finalize_marked_map_exprs()];
              {{maps, omitted}, pass_as_record} ->
                  [change_undef_marker_in_clauses('$undef'),
                   rework_records_to_maps(4, '$undef'),
                   underscore_unused_vars(),
                   finalize_marked_map_exprs()];
              {{maps, omitted}, pass_as_params} ->
                  [change_undef_marker_in_clauses('$undef'),
                   explode_param_init(MsgName, InitExprs, 4),
                   explode_param_pass(MsgName, FNames, 4),
                   implode_to_map_exprs(4, FOccurrences, '$undef'),
                   underscore_unused_vars(),
                   finalize_marked_map_exprs()]
          end,
    run_morph_ops(Ops, Fns).

init_exprs(MsgName, MsgDef, Defs, TrUserDataVar, AnRes, Opts)->
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    IsMapMsg = is_map_msg(MsgName, AnRes),
    UseDefaults = proplists:get_bool(defaults_for_omitted_optionals, Opts),
    UseTypeDefaults = proplists:get_bool(type_defaults_for_omitted_optionals,
                                         Opts),
    ExprInfos1 =
        [case Field of
             #?gpb_field{name=FName, occurrence=Occurrence0, type=Type,
                         opts=FOpts} ->
                 HasDefault = lists:keymember(default, 1, FOpts),
                 SubMsgType = is_msg_type(Type),
                 Occurrence = if IsMapMsg, not SubMsgType -> optional;
                                 true -> Occurrence0
                              end,
                 {Undefined, Undef, P} =
                     if IsProto3 ->
                             TD = gpb_lib:proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, m};
                        IsMapMsg, not SubMsgType ->
                             TD = gpb_lib:proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, o};
                        UseDefaults, HasDefault ->
                             {default,D} = lists:keyfind(default, 1, FOpts),
                             AD = erl_syntax:abstract(D),
                             {AD, AD, m};
                        UseTypeDefaults ->
                             TD = gpb_lib:proto2_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, m};
                        true ->
                             Pr = if HasDefault -> d;
                                     true -> o
                                  end,
                             {?expr(undefined), ?expr('$undef'), Pr}
                     end,
                 case Occurrence of
                     repeated -> {FName, m, ?expr([]),        ?expr([])};
                     required -> {FName, o, ?expr(undefined), ?expr('$undef')};
                     optional -> {FName, P, Undefined,        Undef}
                 end;
             #gpb_oneof{name=FName} ->
                 {FName, o, ?expr(undefined), ?expr('$undef')}
         end
         || Field <- MsgDef],
    ExprInfos2 =
        [begin
             ElemPath = [MsgName, FName],
             TranslFn = gpb_gen_translators:find_translation(
                          ElemPath,
                          decode_init_default,
                          AnRes),
             TrInitExpr = ?expr('Tr'('InitExpr', 'TrUserData'),
                                [replace_tree('InitExpr', InitExpr),
                                 replace_term('Tr', TranslFn),
                                 replace_tree('TrUserData', TrUserDataVar)]),
             TrMOExpr = ?expr('Tr'('MOExpr', 'TrUserData'),
                              [replace_tree('MOExpr', MOExpr),
                               replace_term('Tr', TranslFn),
                               replace_tree('TrUserData', TrUserDataVar)]),
             {FName, Presence, TrInitExpr, TrMOExpr}
         end
         || {FName, Presence, InitExpr, MOExpr} <- ExprInfos1],
    case gpb_lib:get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    [{FName, Expr} || {FName, _, Expr, _MOExpr} <- ExprInfos2];
                {maps, omitted} ->
                    [{FName, MapsOmittedExpr}
                     || {FName, _, _Expr, MapsOmittedExpr} <- ExprInfos2]
            end;
        pass_as_record ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    [{FName, Expr} || {FName, P, Expr, _} <- ExprInfos2,
                                      P == m orelse P == d];
                {maps, present_undefined} ->
                    [{FName, Expr} || {FName, _, Expr, _} <- ExprInfos2];
                {maps, omitted} ->
                    [{FName, Expr} || {FName, m, Expr, _} <- ExprInfos2]
            end
    end.

field_occurrences(MsgDef) ->
    [{gpb_lib:get_field_name(Field), gpb_lib:get_field_occurrence(Field)}
     || Field <- MsgDef].

run_morph_ops([Op | Rest], Fns) ->
    run_morph_ops(Rest, Op(Fns));
run_morph_ops([], Fns) ->
    [gpb_codegen:erl_prettypr_format_nl(Tree) || #fn{tree=Tree} <- Fns].

loop_fns(MapFun, Filter, Fns) ->
    [case matches_filter(Fn, Filter) of
         true  -> Fn#fn{tree = MapFun(FnTree)};
         false -> Fn
     end
     || #fn{tree=FnTree}=Fn <- Fns].

matches_filter(#fn{}=Fn, Filter) ->
    Filter(Fn).

process_all() -> fun(#fn{}) -> true end.

process_initializer() -> fun(#fn{initializes_fields=Bool}) -> Bool end.

process_finalizers() -> fun(#fn{has_finalizer=Bool}) -> Bool end.

process_msg_passers() -> fun(#fn{passes_msg=Bool}) -> Bool end.

process_initializers_finalizers_and_msg_passers() ->
    fun(#fn{initializes_fields=B1,
            has_finalizer=B2,
            passes_msg=B3}) ->
            B1 or B2 or B3
    end.

underscore_unused_vars() ->
    fun(Fns) ->
            loop_fns(fun gpb_codemorpher:underscore_unused_vars/1,
                     process_all(),
                     Fns)
    end.

explode_param_init(MsgName, InitExprs, ArgPos) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:explode_record_fields_to_params_init(
                        FnTree, ArgPos, {MsgName, InitExprs})
              end,
              process_initializer(),
              Fns)
    end.

explode_param_pass(MsgName, FNames, ArgPos) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:explode_record_fields_to_params(
                        FnTree, ArgPos, {MsgName, FNames})
              end,
              process_msg_passers(),
              Fns)
    end.

change_undef_marker_in_clauses(Undef) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:change_undef_marker_in_clauses(
                        FnTree, Undef)
              end,
              process_all(),
              Fns)
    end.

implode_to_map_exprs_all_mandatory() ->
    fun(Fns) ->
            loop_fns(fun gpb_codemorpher:implode_to_map_expr/1,
                     process_finalizers(),
                     Fns)
    end.

implode_to_map_exprs(F1Pos, FOccurrences, Undef) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:implode_to_map_exprs(
                        FnTree, F1Pos, FOccurrences, Undef)
              end,
              process_finalizers(),
              Fns)
    end.

rework_records_to_maps(RecordParamPos, Undef) ->
    fun(Fns) ->
            loop_fns(
              fun(FnTree) ->
                      gpb_codemorpher:rework_records_to_maps(
                        FnTree, RecordParamPos, Undef)
              end,
              process_initializers_finalizers_and_msg_passers(),
              Fns)
    end.

finalize_marked_map_exprs() ->
    fun(Fns) ->
            loop_fns(fun gpb_codemorpher:marked_map_expr_to_map_expr/1,
                     process_initializers_finalizers_and_msg_passers(),
                     Fns)
    end.

format_msg_decoder_read_field(MsgName, MsgDef, InitExprs, AnRes) ->
    Key = ?expr(Key),
    Rest = ?expr(Rest),
    {Param, FParam, FParamBinds} =
        decoder_read_field_param(MsgName, MsgDef),
    Bindings = new_bindings([{'<Param>', Param},
                             {'<FParam>', FParam},
                             {'<FFields>', FParamBinds},
                             {'<Key>', Key},
                             {'<Rest>', Rest},
                             {'<TrUserData>', ?expr(TrUserData)}]),
    [format_msg_init_decoder(MsgName, InitExprs),
     format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes),
     format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes)].

format_msg_init_decoder(MsgName, InitExprs) ->
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_msg_, MsgName),
          fun(Bin, TrUserData) ->
                  '<decode-field-fp>'(Bin, 0, 0, '<init>', TrUserData)
          end,
          [replace_term('<decode-field-fp>',
                        gpb_lib:mk_fn(dfp_read_field_def_, MsgName)),
           replace_tree('<init>',
                        gpb_lib:record_create(MsgName, InitExprs))]),
    #fn{name = boot,
        initializes_fields = true,
        tree = T}.

format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes) ->
    %% The fast-path decoder directly matches the minimal varint form
    %% of the field-number combined with the wiretype.
    %% Unrecognized fields fall back to the more generic decoder-loop
    Param = fetch_binding('<Param>', Bindings),
    FParam = fetch_binding('<FParam>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
          fun('<precomputed-binary-match>', Z1, Z2, '<Param>', TrUserData) ->
                  '<calls-to-field-decoding>';
             (<<>>, 0, 0, '<FParam>', TrUserData) ->
                  '<finalize-result>';
             (Other, Z1, Z2, '<Param>', TrUserData) ->
                  '<decode-general>'(Other, Z1, Z2, '<Param>', TrUserData)
          end,
          [replace_tree('<Param>', Param),
           replace_tree('<FParam>', FParam),
           repeat_clauses(
             '<precomputed-binary-match>',
             [[replace_tree('<precomputed-binary-match>', BinMatch),
               replace_tree('<calls-to-field-decoding>', FnCall)]
              || {BinMatch, FnCall} <- decoder_fp(Bindings, MsgName, MsgDef)]),
           replace_tree('<finalize-result>',
                        decoder_finalize_result(
                          Param, FFields,
                          MsgName, ?expr(TrUserData),
                          AnRes)),
           replace_term('<decode-general>',
                        gpb_lib:mk_fn(dg_read_field_def_, MsgName))]),
    #fn{name = fastpath,
        has_finalizer = true,
        passes_msg = true,
        tree = T}.

format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes) ->
    %% The more general field selecting decoder
    %% Stuff that ends up here: non-minimal varint forms and field to skip
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    FParam = fetch_binding('<FParam>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(dg_read_field_def_, MsgName),
          fun(<<1:1, X:7, '<Rest>'/binary>>, N, Acc, '<Param>', TrUserData)
                when N < (32-7) ->
                  call_self('<Rest>', N+7, X bsl N + Acc, '<Param>',
                            TrUserData);
             (<<0:1, X:7, '<Rest>'/binary>>, N, Acc, '<Param>', TrUserData) ->
                  '<Key>' = X bsl N + Acc,
                  '<calls-to-field-decoding-or-skip>';
             (<<>>, 0, 0, '<FParam>', TrUserData) ->
                  '<finalize-result>'
          end,
          [replace_tree('<Key>', Key),
           replace_tree('<Rest>', Rest),
           replace_tree('<Param>', Param),
           replace_tree('<FParam>', FParam),
           replace_tree('<calls-to-field-decoding-or-skip>',
                        decoder_field_calls(Bindings, MsgName, MsgDef, AnRes)),
           replace_tree('<finalize-result>',
                        decoder_finalize_result(Param, FFields,
                                                MsgName, ?expr(TrUserData),
                                                AnRes))]),
    #fn{name = generic,
        has_finalizer = true,
        passes_msg = true,
        tree = T}.

is_map_msg(MsgName, #anres{maps_as_msgs=MapsAsMsgs}) ->
    lists:keymember({msg,MsgName}, 1, MapsAsMsgs).

is_msg_type({msg,_}) -> true;
is_msg_type(_)       -> false.

decoder_read_field_param(MsgName, MsgDef) ->
    %% Maps currently don't support single value access, ie: M#{f},
    %% so when passing as records/maps, in the end, we must reverse
    %% repeated fields to get a linear amortized cost of
    %% reading/adding elements)
    %%
    %% So instead of generating code that looks
    %% like below for the maps case (similar for records):
    %%
    %%    d_read_field_m_f(<<>>, _, _, M) ->
    %%      M#{f1 = lists:reverse(M#{f1})
    %%
    %% we generate code like this:
    %%
    %%    d_read_field_m_f(<<>>, _, _, #{f1 := F1}=M) ->
    %%      M#{f1 := lists:reverse(F1)
    %%
    %% Here we must provide enough info to generate
    %% the finalizing code (ie: the function body in the example above)
    %%
    MappingVar = ?expr(Msg),
    FFields = [{FName, gpb_lib:var_n("R", I)}
               || {I,FName} <- gpb_lib:index_seq(
                                 repeated_field_names(MsgDef))],
    FMatch = gpb_lib:record_match(MsgName, FFields),
    FParam = ?expr(matching = '<Var>',
                   [replace_tree(matching, FMatch),
                    replace_tree('<Var>', MappingVar)]),
    {MappingVar, FParam, FFields}.

repeated_field_names(MsgDef) ->
    [FName || #?gpb_field{name=FName, occurrence=repeated} <- MsgDef].

%% compute info for the fast-path field recognition/decoding-call
decoder_fp(Bindings, MsgName, MsgDef) ->
    Rest = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    [begin
         BMatch = ?expr(<<'<field-and-wiretype-bytes>', '<Rest>'/binary>>,
                        [splice_trees('<field-and-wiretype-bytes>',
                                      gpb_lib:varint_to_binary_fields(
                                        Selector)),
                         replace_tree('<Rest>', Rest)]),
         FnCall = ?expr('decode_field'('<Rest>', Z1, Z2, '<Param>',
                                       'TrUserData'),
                        [replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         replace_tree('<Param>', Param),
                         replace_tree('TrUserData', TrUserDataVar)]),
         {BMatch, FnCall}
     end
     || {Selector, DecodeFn} <- decoder_field_selectors(MsgName, MsgDef)].

decoder_field_calls(Bindings, MsgName, []=_MsgDef, _AnRes) ->
    Key = fetch_binding('<Key>', Bindings),
    WiretypeExpr = ?expr('<Key>' band 7, [replace_tree('<Key>', Key)]),
    Bindings1 = add_binding({'<wiretype-expr>', WiretypeExpr}, Bindings),
    decoder_skip_calls(Bindings1, MsgName);
decoder_field_calls(Bindings, MsgName, MsgDef, AnRes) ->
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    SkipCalls = decoder_field_calls(Bindings, MsgName, [], AnRes),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    FieldSelects = decoder_field_selectors(MsgName, MsgDef),
    ?expr(case '<Key>' of
              '<selector>' -> 'decode_field'('<Rest>', 0, 0, '<Param>',
                                             'TrUserData');
              _            -> '<skip-calls>'
       end,
       [replace_tree('<Key>', Key),
        repeat_clauses('<selector>',
                       [[replace_term('<selector>', Selector),
                         replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         replace_tree('<Param>', Param)]
                        || {Selector, DecodeFn} <- FieldSelects]),
        replace_tree('<skip-calls>', SkipCalls),
        replace_tree('TrUserData', TrUserDataVar)]).

decoder_skip_calls(Bindings, MsgName) ->
    KeyExpr = fetch_binding('<Key>', Bindings),
    FieldNumExpr = ?expr('<Key>' bsr 3, [replace_tree('<Key>', KeyExpr)]),
    WiretypeExpr = fetch_binding('<wiretype-expr>', Bindings),
    RestExpr = fetch_binding('<Rest>', Bindings),
    Param = fetch_binding('<Param>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    ?expr(case '<wiretype-expr>' of
              0 -> skip_vi('<Rest>', 0, 0, '<Param>', 'TrUserData');
              1 -> skip_64('<Rest>', 0, 0, '<Param>', 'TrUserData');
              2 -> skip_ld('<Rest>', 0, 0, '<Param>', 'TrUserData');
              3 -> skip_gr('<Rest>', 'FNum', 0, '<Param>', 'TrUserData');
              5 -> skip_32('<Rest>', 0, 0, '<Param>', 'TrUserData')
          end,
          [replace_tree('<wiretype-expr>', WiretypeExpr),
           replace_tree('<Rest>', RestExpr),
           replace_tree('<Param>', Param),
           replace_tree('TrUserData', TrUserDataVar),
           replace_term(skip_vi, gpb_lib:mk_fn(skip_varint_, MsgName)),
           replace_term(skip_64, gpb_lib:mk_fn(skip_64_, MsgName)),
           replace_term(skip_ld, gpb_lib:mk_fn(skip_length_delimited_,
                                               MsgName)),
           replace_term(skip_gr, gpb_lib:mk_fn(skip_group_, MsgName)),
           replace_tree('FNum', FieldNumExpr),
           replace_term(skip_32, gpb_lib:mk_fn(skip_32_, MsgName))]).

decoder_field_selectors(MsgName, MsgDef) ->
    lists:append(
      map_msgdef_fields_o(
        fun(#?gpb_field{name=FName, fnum=FNum, type=Type, occurrence=Occ},
            _IsOneof) ->
                case Occ == repeated andalso gpb:is_type_packable(Type) of
                    true ->
                        %% "Protocol buffer parsers must be able to parse
                        %% repeated fields that were compiled as packed
                        %% as if they were not packed, and vice versa."
                        %%
                        %% So generate selectors for recognizing both
                        %% the packed and unpacked case.
                        PWiretype = gpb:encode_wiretype(bytes),
                        UWiretype = gpb:encode_wiretype(Type),
                        [begin
                             Selector = (FNum bsl 3) bor Wiretype,
                             DecodeFn = gpb_lib:mk_fn(Prefix, MsgName, FName),
                             {Selector, DecodeFn}
                         end
                         || {Wiretype, Prefix} <- [{PWiretype, d_pfield_},
                                                   {UWiretype, d_field_}]];
                    false ->
                        Wiretype = case Type of
                                       {group, _} ->
                                           gpb:encode_wiretype(group_start);
                                       _ ->
                                           gpb:encode_wiretype(Type)
                                   end,
                        Selector = (FNum bsl 3) bor Wiretype,
                        DecodeFn = gpb_lib:mk_fn(d_field_, MsgName, FName),
                        [{Selector, DecodeFn}]
                end
        end,
        MsgDef)).

decoder_finalize_result(MsgVar, FFields, MsgName, TrUserDataVar, AnRes) ->
    gpb_lib:record_update(
      MsgVar,
      MsgName,
      [begin
           ElemPath = [MsgName, FName],
           Finalizer = gpb_gen_translators:find_translation(
                         ElemPath,
                         decode_repeated_finalize,
                         AnRes),
           FValueExpr = ?expr('lists:reverse'('<FVar>', 'TrUserData'),
                              [replace_term('lists:reverse',Finalizer),
                               replace_tree('<FVar>', FVar),
                               replace_tree('TrUserData',
                                            TrUserDataVar)]),
           {FName, FValueExpr}
       end
       || {FName, FVar} <- FFields]).

format_field_decoders(MsgName, MsgDef, AnRes, Opts) ->
    map_msgdef_fields_o(
      fun(Field, IsOneof) ->
              format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts)
      end,
      MsgDef).

format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts) ->
    XField = {Field, IsOneof},
    case Field of
        #?gpb_field{occurrence=repeated, type=Type} ->
            case gpb:is_type_packable(Type) of
                true ->
                    %% Generate decoder functions for both the packed
                    %% and unpacked case
                    [format_non_packed_field_decoder(MsgName, XField, AnRes,
                                                     Opts),
                     %% A packed field can never be one of a `oneof' fields
                     %% So pass Field and not XField
                     format_packed_field_decoder(MsgName, Field, AnRes, Opts)];
                false ->
                    format_non_packed_field_decoder(MsgName, XField, AnRes,
                                                    Opts)
            end;
        _ ->
            format_non_packed_field_decoder(MsgName, XField, AnRes, Opts)
    end.

format_non_packed_field_decoder(MsgName, XField, AnRes, Opts) ->
    {#?gpb_field{type=Type}, _IsOneof} = XField,
    case Type of
        sint32   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        sint64   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        int32    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        int64    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        uint32   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        uint64   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bool     -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {enum,_} -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        fixed32  -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        sfixed32 -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        float    -> format_floating_point_field_decoder(MsgName, XField,
                                                        float, AnRes);
        fixed64  -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        sfixed64 -> format_fixlen_field_decoder(MsgName, XField, AnRes);
        double   -> format_floating_point_field_decoder(MsgName, XField,
                                                        double, AnRes);
        string   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {map,_,_}-> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {group,_}-> format_group_field_decoder(MsgName, XField, AnRes)
    end.

format_packed_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #?gpb_field{name=FName} = FieldDef,
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_pfield_, MsgName, FName),
          fun(<<1:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData)
                when N < ?NB ->
                  call_self(Rest, N + 7, X bsl N + Acc, Msg, TrUserData);
             (<<0:1, X:7, Rest/binary>>, N, Acc, #'MsgName'{field=E}=Msg,
              TrUserData) ->
                  Len = X bsl N + Acc,
                  <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
                  NewSeq = decode_packed(PackedBytes, 0, 0, E,
                                         'MaybeTrUserData'),
                  '<call-read-field>'(Rest2, 0, 0,
                                      Msg#'MsgName'{field=NewSeq},
                                      TrUserData)
          end,
          [replace_term(decode_packed,
                        gpb_lib:mk_fn(d_packed_field_, MsgName, FName)),
           replace_term('<call-read-field>',
                        gpb_lib:mk_fn(dfp_read_field_def_, MsgName)),
           replace_term('MsgName', MsgName),
           replace_term(field, FName),
           splice_trees('MaybeTrUserData',
                        gpb_gen_translators:maybe_userdata_param(
                          FieldDef,
                          ?expr(TrUserData)))]),
    [#fn{name = packed_field,
         passes_msg = true,
         tree = T},
     format_packed_field_seq_decoder(MsgName, FieldDef, AnRes, Opts)].

format_packed_field_seq_decoder(MsgName, #?gpb_field{type=Type}=Field,
                                AnRes, Opts) ->
    case Type of
        fixed32  -> format_dpacked_nonvi(MsgName, Field, 32, [little]);
        sfixed32 -> format_dpacked_nonvi(MsgName, Field, 32, [little,signed]);
        float    -> format_dpacked_nonvi(MsgName, Field, 32, float);
        fixed64  -> format_dpacked_nonvi(MsgName, Field, 64, [little]);
        sfixed64 -> format_dpacked_nonvi(MsgName, Field, 64, [little,signed]);
        double   -> format_dpacked_nonvi(MsgName, Field, 64, double);
        _        -> format_dpacked_vi(MsgName, Field, AnRes, Opts)
    end.

format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 32, float) ->
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [infinity | AccSeq]);
             (<<0:16,128,255, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, ['-infinity' | AccSeq]);
             (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [nan | AccSeq]);
             (<<Value:32/little-float, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [Value | AccSeq]);
             (<<>>, _, _, AccSeq) ->
                  AccSeq
          end,
          []),
    #fn{name = packed_float,
        tree = T};
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 64, double) ->
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [infinity | AccSeq]);
             (<<0:48,240,255, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, ['-infinity' | AccSeq]);
             (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [nan | AccSeq]);
             (<<Value:64/little-float, Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [Value | AccSeq]);
             (<<>>, _, _, AccSeq) ->
                  AccSeq
          end,
          []),
    #fn{name = packed_double,
        tree = T};
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, BitLen, BitTypes) ->
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, AccSeq) ->
                  call_self(Rest, Z1, Z2, [Value | AccSeq]);
             (<<>>, _, _, AccSeq) ->
                  AccSeq
          end,
          [replace_term('<N>', BitLen),
           splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes])]),
    #fn{name = packed_fixint,
        tree = T}.

format_dpacked_vi(MsgName, #?gpb_field{name=FName}=FieldDef, AnRes, Opts) ->
    ExtValue = ?expr(X bsl N + Acc),
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, false,
                                                AnRes),
    DExpr = decode_int_value(ExtValue, Rest, TrUserDataVar, FieldDef, Tr, Opts),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
          fun(<<1:1, X:7, Rest/binary>>, N, Acc, AccSeq, 'MaybeTrUserData')
                when N < ?NB ->
                  call_self(Rest, N + 7, X bsl N + Acc, AccSeq,
                            'MaybeTrUserData');
             (<<0:1, X:7, Rest/binary>>, N, Acc, AccSeq, 'MaybeTrUserData') ->
                  {NewFValue, RestF} = '<decode-expr>',
                  call_self(RestF, 0, 0, [NewFValue | AccSeq],
                            'MaybeTrUserData');
             (<<>>, 0, 0, AccSeq, 'MaybeTrUserData') ->
                  AccSeq
          end,
          [replace_tree('<decode-expr>', DExpr),
           splice_trees('MaybeTrUserData',
                        gpb_gen_translators:maybe_userdata_param(
                          FieldDef,
                          TrUserDataVar))]),
    #fn{name = packed_vi_based,
        tree = T}.

format_vi_based_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName}=FieldDef, IsOneof}=XFieldDef,
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    MsgVar =?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    ReadFieldDefFn = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    OutParam = updated_merged_param(MsgName, XFieldDef, AnRes,
                                    FVar, PrevValue, MsgVar,
                                    TrUserDataVar),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof,
                                                AnRes),
    DExpr = decode_int_value(ExtValue, Rest, TrUserDataVar, FieldDef, Tr, Opts),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_field_, MsgName, FName),
          fun(<<1:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData)
                when N < ?NB ->
                  call_self(Rest, N + 7, X bsl N + Acc, Msg, TrUserData);
             (<<0:1, X:7, Rest/binary>>, N, Acc, 'InParam', TrUserData) ->
                  {'FVar', RestF} = '<decode-expr>',
                  '<call-read-field>'(RestF, 0, 0, 'OutParam', 'TrUserData')
          end,
          [replace_tree('InParam', InParam),
           replace_term('<call-read-field>', ReadFieldDefFn),
           replace_tree('FVar', FVar),
           replace_tree('<decode-expr>', DExpr),
           replace_tree('OutParam', OutParam),
           replace_tree('TrUserData', TrUserDataVar)]),
    #fn{name = vi_based,
        passes_msg = true,
        tree = T}.

decode_int_value(ExtValueExpr, Rest, TrUserDataVar, FieldDef, Tr, Opts) ->
    #?gpb_field{type=Type}=FieldDef,
    StringsAsBinaries = gpb_lib:get_strings_as_binaries_by_opts(Opts),
    case Type of
        sint32 ->
            tuplify(decode_zigzag(ExtValueExpr), Rest);
        sint64 ->
            tuplify(decode_zigzag(ExtValueExpr), Rest);
        int32 ->
            tuplify(decode_uint_to_int(ExtValueExpr, 32), Rest);
        int64 ->
            tuplify(decode_uint_to_int(ExtValueExpr, 64), Rest);
        uint32 ->
            tuplify(ExtValueExpr, Rest);
        uint64 ->
            tuplify(ExtValueExpr, Rest);
        bool ->
            tuplify(?expr(('ExtValueExpr') =/= 0,
                          [replace_tree('ExtValueExpr', ExtValueExpr)]),
                    Rest);
        {enum, EnumName} ->
            EnumDecodeFn = gpb_lib:mk_fn(d_enum_, EnumName),
            UintToIntExpr = decode_uint_to_int(ExtValueExpr, 32),
            ToSym = ?expr('decode-enum'('decode-uint-to-int'),
                          [replace_term('decode-enum', EnumDecodeFn),
                           replace_tree('decode-uint-to-int', UintToIntExpr)]),
            tuplify(ToSym, Rest);
        string when StringsAsBinaries ->
            unpack_bytes(ExtValueExpr, Rest, Opts);
        string when not StringsAsBinaries ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Utf8:Len/binary, Rest2/binary>> = 'Rest',
                      {unicode:characters_to_list(Utf8, unicode), Rest2}
                  end,
                  [replace_tree('ExtValueExpr', ExtValueExpr),
                   replace_tree('Rest', Rest)]);
        bytes ->
            unpack_bytes(ExtValueExpr, Rest, Opts);
        {msg, Msg2Name} ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bs:Len/binary, Rest2/binary>> = 'Rest',
                      {'Tr'('d-msg-X'(Bs, 'TrUserData'), 'TrUserData'), Rest2}
                  end,
                  [replace_tree('ExtValueExpr', ExtValueExpr),
                   replace_tree('Rest', Rest),
                   replace_term('d-msg-X', gpb_lib:mk_fn(d_msg_, Msg2Name)),
                   replace_term('Tr', Tr(decode)),
                   replace_tree('TrUserData', TrUserDataVar)]);
        {map, KeyType, ValueType} ->
            MapAsMsgMame = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
            F2 = FieldDef#?gpb_field{type={msg,MapAsMsgMame}},
            decode_int_value(ExtValueExpr, Rest, TrUserDataVar, F2, Tr, Opts)
    end.

unpack_bytes(ExtValueExpr, Rest, Opts) ->
    CompilerHasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    Copy = case proplists:get_value(copy_bytes, Opts, auto) of
               auto when not CompilerHasBinary -> false;
               auto when CompilerHasBinary     -> true;
               true                            -> true;
               false                           -> false;
               N when is_integer(N)            -> N;
               N when is_float(N)              -> N
           end,
    Transforms = [replace_tree('ExtValueExpr', ExtValueExpr),
                  replace_tree('Rest', Rest)],
    if Copy == false ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bytes:Len/binary, Rest2/binary>> = 'Rest',
                      {Bytes, Rest2}
                  end,
                  Transforms);
       Copy == true ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bytes:Len/binary, Rest2/binary>> = 'Rest',
                      {binary:copy(Bytes), Rest2}
                  end,
                  Transforms);
       is_integer(Copy); is_float(Copy) ->
            ?expr(begin
                      Len = 'ExtValueExpr',
                      <<Bytes:Len/binary, Rest2/binary>> = 'Rest',
                      Res = case binary:referenced_byte_size(Bytes) of
                                LB when LB >= byte_size(Bytes) * 'Copy' ->
                                    binary:copy(Bytes);
                                _ ->
                                    Bytes
                            end,
                      {Res, Rest2}
                  end,
                  [replace_term('Copy', Copy) | Transforms])
       end.

format_group_field_decoder(MsgName, XFieldDef, AnRes) ->
    {#?gpb_field{name=FName, fnum=FNum, type={group,GroupName}}=FieldDef,
     IsOneof}=XFieldDef,
    ResVar = ?expr(NewFValue), %% result is to be put in this variable
    TrUserDataVar = ?expr(TrUserData),
    MsgVar = ?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    OutParam = updated_merged_param(MsgName, XFieldDef, AnRes,
                                    ResVar, PrevValue, MsgVar,
                                    TrUserDataVar),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof,
                                                AnRes),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_field_, MsgName, FName),
          fun(Bin, _, _, 'InParam', TrUserData) ->
                  {GroupBin, Rest} = read_group(Bin, 'FieldNum'),
                  'Res' = 'Tr'('d_msg_X'(GroupBin, TrUserData), TrUserData),
                  'call-read-field'(Rest, 0, 0, 'OutParam', TrUserData)
          end,
          [replace_tree('InParam', InParam),
           replace_term('call-read-field', gpb_lib:mk_fn(dfp_read_field_def_,
                                                         MsgName)),
           replace_term('Tr', Tr(decode)),
           replace_tree('Res', ResVar),
           replace_tree('FieldNum', erl_syntax:integer(FNum)),
           replace_term('d_msg_X', gpb_lib:mk_fn(d_msg_, GroupName)),
           replace_tree('OutParam', OutParam)]),
    #fn{name = group,
        passes_msg = true,
        tree = T}.

updated_merged_param(MsgName, XFieldDef, AnRes, NewValue, PrevValue,
                     MsgVar, TrUserDataVar) ->
    Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(MsgName, XFieldDef,
                                                           AnRes),
    MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                   MsgName, Tr, TrUserDataVar,
                                   AnRes),
    case XFieldDef of
        {#?gpb_field{name=FName}, false} ->
            gpb_lib:record_update(MsgVar, MsgName, [{FName, MergedValue}]);
        {_OField, {true, CFName}} ->
            gpb_lib:record_update(MsgVar, MsgName, [{CFName, MergedValue}])
    end.

merge_field_expr({FieldDef, false}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes) ->
    case gpb_lib:classify_field_merge_action(FieldDef) of
        overwrite ->
            NewValue;
        seqadd ->
            ElemPath = [MsgName, gpb_lib:get_field_name(FieldDef)],
            Cons = gpb_gen_translators:find_translation(
                     ElemPath,
                     decode_repeated_add_elem,
                     AnRes),
            ?expr('[New|Acc]'('<New>', '<Acc>', 'TrUserData'),
                  [replace_term('[New|Acc]', Cons),
                   replace_tree('<New>', NewValue),
                   replace_tree('<Acc>', PrevValue),
                   replace_tree('TrUserData', TrUserDataVar)]);
        msgmerge ->
            FMsgName = case FieldDef of
                           #?gpb_field{type={msg,Nm}} -> Nm;
                           #?gpb_field{type={group,Nm}} -> Nm
                       end,
            MergeFn = gpb_lib:mk_fn(merge_msg_, FMsgName),
            ?expr(if 'Prev' == undefined -> 'New';
                     true -> 'merge_msg_X'('Prev', 'New', 'TrUserData')
                  end,
                  [replace_term('merge_msg_X', Tr(merge, MergeFn)),
                   replace_tree('Prev', PrevValue),
                   replace_tree('New', NewValue),
                   replace_tree('TrUserData', TrUserDataVar)])
    end;
merge_field_expr({FieldDef, {true, _CFName}}, PrevValue, NewValue,
                 _MsgName, Tr, TrUserDataVar, _AnRes)->
    #?gpb_field{name=FName, type=Type} = FieldDef,
    if ?is_msg_or_group(Type) ->
            {_, FMsgName} = Type,
            MergeFn = gpb_lib:mk_fn(merge_msg_, FMsgName),
            MVPrev = gpb_lib:prefix_var("MV", PrevValue),
            ?expr(case 'Prev' of
                      undefined ->
                          {'tag', 'New'};
                      {'tag', 'MVPrev'} ->
                          {'tag', 'merge_msg_X'('MVPrev', 'New',
                                                'TrUserData')};
                      _ ->
                          {'tag', 'New'}
                  end,
                  [replace_tree('Prev', PrevValue),
                   replace_term('tag', FName),
                   replace_tree('New', NewValue),
                   replace_term('merge_msg_X', Tr(merge, MergeFn)),
                   replace_tree('MVPrev', MVPrev),
                   replace_tree('TrUserData', TrUserDataVar)]);
       true ->
            %% Replace
            ?expr({'fieldname', '<expr>'},
                  [replace_term('fieldname', FName),
                   replace_tree('<expr>', NewValue)])
    end.

decoder_in_param(MsgVar, MsgName, {FieldDef, false}) ->
    #?gpb_field{name=FName}=FieldDef,
    Prev = erl_syntax:variable('Prev'),
    InParam = gpb_lib:match_bind_var(
                gpb_lib:record_match(MsgName, [{FName, Prev}]),
                MsgVar),
    {InParam, Prev};
decoder_in_param(MsgVar, MsgName, {FieldDef, {true, CFName}}) ->
    #?gpb_field{type=Type} = FieldDef,
    if ?is_msg_or_group(Type) ->
            %% oneof fields that of message type may need merging
            Prev = erl_syntax:variable('Prev'),
            InParam = gpb_lib:match_bind_var(
                        gpb_lib:record_match(MsgName, [{CFName, Prev}]),
                        MsgVar),
            {InParam, Prev};
       true ->
            {MsgVar, erl_syntax:variable('Prev')}
    end.

format_fixlen_field_decoder(MsgName, XFieldDef, AnRes) ->
    {#?gpb_field{name=FName, type=Type}, _IsOneof} = XFieldDef,
    {BitLen, BitTypes} = case Type of
                             fixed32  -> {32, [little]};
                             sfixed32 -> {32, [little,signed]};
                             float    -> {32, [little,float]};
                             fixed64  -> {64, [little]};
                             sfixed64 -> {64, [little,signed]};
                             double   -> {64, [little,float]}
                         end,
    MsgVar = ?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    Value = ?expr(Value),
    TrUserDataVar = ?expr(TrUserData),
    Param2 = updated_merged_param(MsgName, XFieldDef, AnRes,
                                  Value, PrevValue, MsgVar,
                                  TrUserDataVar),
    ReadFieldDefFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    T = gpb_codegen:mk_fn(
          gpb_lib:mk_fn(d_field_, MsgName, FName),
          fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, '<InParam>',
              'TrUserData') ->
                  '<call-read-field>'(Rest, Z1, Z2, '<OutParam>', 'TrUserData')
          end,
          [replace_term('<N>', BitLen),
           splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes]),
           replace_tree('<InParam>', InParam),
           replace_term('<call-read-field>', ReadFieldDefFnName),
           replace_tree('<OutParam>', Param2),
           replace_tree('TrUserData', TrUserDataVar)]),
    #fn{name = fixlen,
        passes_msg = true,
        tree = T}.

format_floating_point_field_decoder(MsgName, XFieldDef, Type, AnRes) ->
    {#?gpb_field{name=FName}, _IsOneof} = XFieldDef,
    TrUserDataVar = ?expr(TrUserData),
    MsgVar = ?expr(Msg),
    {InParam, PrevValue} = decoder_in_param(MsgVar, MsgName, XFieldDef),
    OutParamReplacements =
        [replace_tree(Marker, updated_merged_param(MsgName, XFieldDef, AnRes,
                                                   OutExpr, PrevValue, MsgVar,
                                                   TrUserDataVar))
         || {Marker, OutExpr} <- [{'OutParam', ?expr(Value)},
                                  {'InfinityOutParam', ?expr(infinity)},
                                  {'-InfinityOutParam', ?expr('-infinity')},
                                  {'NanOutParam', ?expr(nan)}]],
    ReadFieldDefFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    Replacements =
        [replace_tree('InParam', InParam),
         replace_term('<call-read-field>', ReadFieldDefFnName),
         replace_tree('TrUserData', TrUserDataVar)] ++
        OutParamReplacements,
    T = case Type of
            float ->
                gpb_codegen:mk_fn(
                  gpb_lib:mk_fn(d_field_, MsgName, FName),
                  fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'InfinityOutParam',
                                              'TrUserData');
                     (<<0:16,128,255, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              '-InfinityOutParam',
                                              'TrUserData');
                     (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'NanOutParam',
                                              'TrUserData');
                     (<<Value:32/little-float, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'OutParam',
                                              'TrUserData')
                  end,
                  Replacements);
            double ->
                gpb_codegen:mk_fn(
                  gpb_lib:mk_fn(d_field_, MsgName, FName),
                  fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'InfinityOutParam',
                                              'TrUserData');
                     (<<0:48,240,255, Rest/binary>>, Z1, Z2, 'InParam',
                      'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              '-InfinityOutParam',
                                              'TrUserData');
                     (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'NanOutParam',
                                              'TrUserData');
                     (<<Value:64/little-float, Rest/binary>>, Z1, Z2,
                      'InParam', 'TrUserData') ->
                          '<call-read-field>'(Rest, Z1, Z2,
                                              'OutParam',
                                              'TrUserData')
                  end,
                  Replacements)
        end,
    #fn{name = float,
        passes_msg = true,
        tree = T}.

decode_zigzag(ExtValueExpr) ->
    ?expr(begin
              ZValue = 'ExtValueExpr',
              if ZValue band 1 =:= 0 -> ZValue bsr 1;
                 true                -> -((ZValue + 1) bsr 1)
              end
          end,
          [replace_tree('ExtValueExpr', ExtValueExpr)]).

decode_uint_to_int(ExtValueExpr, NumBits) ->
    %% Contrary to the 64 bit encoding done for int32 (and enum),
    %% decode the value as 32 bits, so we decode negatives
    %% given both as 32 bits and as 64 bits wire encodings
    %% to the same integer.
    ?expr(begin
              <<Res:'N'/signed-native>> =
                  <<('ExtValueExpr'):'N'/unsigned-native>>,
              Res
          end,
          [replace_term('N', NumBits),
           replace_tree('ExtValueExpr', ExtValueExpr)]).

format_read_group_fn() ->
    ["read_group(Bin, FieldNum) ->\n"
     "    {NumBytes, EndTagLen} = read_gr_b(Bin, 0, 0, 0, 0, FieldNum),\n"
     "    <<Group:NumBytes/binary, _:EndTagLen/binary, Rest/binary>> = Bin,\n"
     "    {Group, Rest}.\n"
     "\n"
     "%% Like skipping over fields, but record the total length,\n"
     "%% Each field is <(FieldNum bsl 3) bor FieldType> ++ <FieldValue>\n"
     "%% Record the length because varints may be non-optimally encoded.\n"
     "%%\n"
     "%% Groups can be nested, but assume the same FieldNum cannot be nested\n"
     "%% because group field numbers are shared with the rest of the fields\n"
     "%% numbers. Thus we can search just for an group-end with the same\n"
     "%% field number.\n"
     "%%\n"
     "%% (The only time the same group field number could occur would\n"
     "%% be in a nested sub message, but then it would be inside a\n"
     "%% length-delimited entry, which we skip-read by length.)\n"
     "read_gr_b(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum)\n"
     "  when N < (32-7) ->\n"
     "    read_gr_b(Tl, N+7, X bsl N + Acc, NumBytes, TagLen+1, FieldNum);\n"
     "read_gr_b(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen,\n"
     "          FieldNum) ->\n"
     "    Key = X bsl N + Acc,\n"
     "    TagLen1 = TagLen + 1,\n"
     "    case {Key bsr 3, Key band 7} of\n"
     "        {FieldNum, 4} -> % 4 = group_end\n"
     "            {NumBytes, TagLen1};\n"
     "        {_, 0} -> % 0 = varint\n"
     "            read_gr_vi(Tl, 0, NumBytes + TagLen1, FieldNum);\n"
     "        {_, 1} -> % 1 = bits64\n"
     "            <<_:64, Tl2/binary>> = Tl,\n"
     "            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 8, 0, FieldNum);\n"
     "        {_, 2} -> % 2 = length_delimited\n"
     "            read_gr_ld(Tl, 0, 0, NumBytes + TagLen1, FieldNum);\n"
     "        {_, 3} -> % 3 = group_start\n"
     "            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);\n"
     "        {_, 4} -> % 4 = group_end\n"
     "            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);\n"
     "        {_, 5} -> % 5 = bits32\n"
     "            <<_:32, Tl2/binary>> = Tl,\n"
     "            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 4, 0, FieldNum)\n"
     "    end.\n"
     "\n"
     "read_gr_vi(<<1:1, _:7, Tl/binary>>, N, NumBytes, FieldNum)\n"
     "  when N < (64-7) ->\n"
     "    read_gr_vi(Tl, N+7, NumBytes+1, FieldNum);\n"
     "read_gr_vi(<<0:1, _:7, Tl/binary>>, _, NumBytes, FieldNum) ->\n"
     "    read_gr_b(Tl, 0, 0, NumBytes+1, 0, FieldNum).\n"
     "\n"
     "read_gr_ld(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum)\n"
     "  when N < (64-7) ->\n"
     "    read_gr_ld(Tl, N+7, X bsl N + Acc, NumBytes+1, FieldNum);\n"
     "read_gr_ld(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum) ->\n"
     "    Len = X bsl N + Acc,\n"
     "    NumBytes1 = NumBytes + 1,\n"
     "    <<_:Len/binary, Tl2/binary>> = Tl,\n"
     "    read_gr_b(Tl2, 0, 0, NumBytes1 + Len, 0, FieldNum).\n"].

format_field_skippers(MsgName) ->
    SkipVarintFnName = gpb_lib:mk_fn(skip_varint_, MsgName),
    SkipLenDelimFnName = gpb_lib:mk_fn(skip_length_delimited_, MsgName),
    SkipGroupFnName = gpb_lib:mk_fn(skip_group_, MsgName),
    ReadFieldFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    Ts = [%% skip_varint_<MsgName>/2,4
          gpb_codegen:mk_fn(
            SkipVarintFnName,
            fun(<<1:1, _:7, Rest/binary>>, Z1, Z2, Msg, TrUserData) ->
                    call_self(Rest, Z1, Z2, Msg, TrUserData);
               (<<0:1, _:7, Rest/binary>>, Z1, Z2, Msg, TrUserData) ->
                    '<call-read-field>'(Rest, Z1,Z2, Msg, TrUserData)
            end,
            [replace_term('<call-read-field>', ReadFieldFnName)]),
          %% skip_length_delimited_<MsgName>/4
          gpb_codegen:mk_fn(
            SkipLenDelimFnName,
            fun(<<1:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData)
                  when N < ?NB ->
                    call_self(Rest, N+7, X bsl N + Acc, Msg,
                              TrUserData);
               (<<0:1, X:7, Rest/binary>>, N, Acc, Msg, TrUserData) ->
                    Length = X bsl N + Acc,
                    <<_:Length/binary, Rest2/binary>> = Rest,
                    '<call-read-field>'(Rest2, 0, 0, Msg, TrUserData)
            end,
            [replace_term('<call-read-field>', ReadFieldFnName)]),
          %% skip_group_<MsgName>/4
          gpb_codegen:mk_fn(
            SkipGroupFnName,
            fun(Bin, FNum, Z2, Msg, TrUserData) ->
                    {_, Rest} = read_group(Bin, FNum),
                    '<call-read-field>'(Rest, 0, Z2, Msg, TrUserData)
            end,
            [replace_term('<call-read-field>', ReadFieldFnName)]),
          %% skip_32_<MsgName>/2,4
          %% skip_64_<MsgName>/2,4
          [gpb_codegen:mk_fn(
             gpb_lib:mk_fn(skip_, NumBits, MsgName),
             fun(<<_:'<NumBits>', Rest/binary>>, Z1, Z2, Msg, TrUserData) ->
                     '<call-read-field>'(Rest, Z1, Z2, Msg, TrUserData)
             end,
             [replace_term('<call-read-field>', ReadFieldFnName),
              replace_term('<NumBits>', NumBits)])
           || NumBits <- [32, 64]]],
    [#fn{name = skipper,
         passes_msg = true,
         tree=T}
     || T <- lists:flatten(Ts)].

new_bindings(Tuples) ->
    lists:foldl(fun add_binding/2, new_bindings(), Tuples).

new_bindings() ->
    dict:new().

add_binding({Key, Value}, Bindings) ->
    dict:store(Key, Value, Bindings).

fetch_binding(Key, Bindings) ->
    dict:fetch(Key, Bindings).

%% The fun takes two args: Fun(#?gpb_field{}, IsOneofField) -> term()
map_msgdef_fields_o(Fun, Fields) ->
    lists:reverse(
      lists:foldl(
        fun(#?gpb_field{}=Field, Acc) ->
                [Fun(Field, false) | Acc];
           (#gpb_oneof{name=CFName, fields=OFields}, Acc) ->
                IsOneOf = {true, CFName},
                lists:foldl(fun(OField, OAcc) -> [Fun(OField, IsOneOf) | OAcc]
                            end,
                            Acc,
                            OFields)
        end,
        [],
        Fields)).

tuplify(Expr, Rest) ->
    ?expr({'Expr', 'Rest'},
          [replace_tree('Expr', Expr),
           replace_tree('Rest', Rest)]).
