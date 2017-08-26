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
                   '<MsgName>' -> '<decode-call>'(Bin, 'TrUserData')
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
    [format_msg_decoder_read_field(MsgName, MsgDef, Defs, AnRes, Opts),
     format_field_decoders(MsgName, MsgDef, AnRes, Opts),
     format_field_skippers(MsgName, AnRes)].

format_msg_decoder_read_field(MsgName, MsgDef, Defs, AnRes, Opts) ->
    Key = ?expr(Key),
    Rest = ?expr(Rest),
    {Params, FParams, FParamBinds} =
        decoder_read_field_params(MsgName, MsgDef, AnRes, Opts),
    Bindings = new_bindings([{'<Params>', Params},
                             {'<FParams>', FParams},
                             {'<FFields>', FParamBinds},
                             {'<Key>', Key},
                             {'<Rest>', Rest},
                             {'<TrUserData>', ?expr(TrUserData)}]),
    [format_msg_init_decoder(MsgName, MsgDef, Defs, AnRes, Opts),
     format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts),
     format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts)].

format_msg_init_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(d_msg_, MsgName),
      fun(Bin, TrUserData) ->
              '<decode-field-fp>'(Bin, 0, 0, '<initial-params>', TrUserData)
      end,
      [replace_term('<decode-field-fp>',
                    gpb_lib:mk_fn(dfp_read_field_def_, MsgName)),
       splice_trees('<initial-params>',
                    msg_decoder_initial_params(MsgName, MsgDef, Defs,
                                               ?expr(TrUserData),
                                               AnRes, Opts))]).

format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts) ->
    %% The fast-path decoder directly matches the minimal varint form
    %% of the field-number combined with the wiretype.
    %% Unrecognized fields fall back to the more generic decoder-loop
    Params = fetch_binding('<Params>', Bindings),
    FParams = fetch_binding('<FParams>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
      fun('<precomputed-binary-match>', Z1, Z2, '<Params>', TrUserData) ->
              '<calls-to-field-decoding>';
         (<<>>, 0, 0, '<FParams>', 'MaybeTrUserData') ->
              '<finalize-result>';
         (Other, Z1, Z2, '<Params>', TrUserData) ->
              '<decode-general>'(Other, Z1, Z2, '<Params>', TrUserData)
      end,
      [splice_trees('<Params>', Params),
       splice_trees('<FParams>', FParams),
       repeat_clauses(
         '<precomputed-binary-match>',
         [[replace_tree('<precomputed-binary-match>', BinMatch),
           replace_tree('<calls-to-field-decoding>', FnCall)]
          || {BinMatch, FnCall} <- decoder_fp(Bindings, MsgName, MsgDef)]),
       splice_trees('<finalize-result>',
                    decoder_finalize_result(Params, FFields,
                                            MsgName, MsgDef, ?expr(TrUserData),
                                            AnRes, Opts)),
       replace_term('<decode-general>',
                    gpb_lib:mk_fn(dg_read_field_def_, MsgName)),
       replace_tree('MaybeTrUserData',
                    case decode_finalizer_needs_tr_userdata(
                           MsgName, MsgDef, AnRes) of
                        true  -> ?expr(TrUserData);
                        false -> ?expr(_)
                    end)]).

decode_finalizer_needs_tr_userdata(MsgName, Fields, AnRes) ->
    gpb_lib:any_field_is_repeated(Fields) orelse
        gpb_gen_translators:exists_tr_for_msg(MsgName, decode_repeated_finalize,
                                              AnRes).

format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts) ->
    %% The more general field selecting decoder
    %% Stuff that ends up here: non-minimal varint forms and field to skip
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    FParams = fetch_binding('<FParams>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(dg_read_field_def_, MsgName),
      fun(<<1:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>', TrUserData)
            when N < (32-7) ->
              call_self('<Rest>', N+7, X bsl N + Acc, '<Params>', TrUserData);
         (<<0:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>', TrUserData) ->
              '<Key>' = X bsl N + Acc,
              '<calls-to-field-decoding-or-skip>';
         (<<>>, 0, 0, '<FParams>', 'MaybeTrUserData') ->
              '<finalize-result>'
      end,
      [replace_tree('<Key>', Key),
       replace_tree('<Rest>', Rest),
       splice_trees('<Params>', Params),
       splice_trees('<FParams>', FParams),
       replace_tree('<calls-to-field-decoding-or-skip>',
                    decoder_field_calls(Bindings, MsgName, MsgDef, AnRes)),
       splice_trees('<finalize-result>',
                    decoder_finalize_result(Params, FFields,
                                            MsgName, MsgDef, ?expr(TrUserData),
                                            AnRes, Opts)),
       replace_tree('MaybeTrUserData',
                   case decode_finalizer_needs_tr_userdata(
                          MsgName,MsgDef,AnRes) of
                       true  -> ?expr(TrUserData);
                       false -> ?expr(_)
                   end)]).

msg_decoder_initial_params(MsgName, MsgDef, Defs, TrUserDataVar, AnRes, Opts)->
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
                    [Expr || {_FName, _, Expr, _MOExpr} <- ExprInfos2];
                {maps, omitted} ->
                    [MapsOmittedExpr
                     || {_FName, _, _Expr, MapsOmittedExpr} <- ExprInfos2]
            end;
        pass_as_record ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    [gpb_lib:record_create(
                       MsgName,
                       [{FName, Expr} || {FName, P, Expr, _} <- ExprInfos2,
                                         P == m orelse P == d])];
                {maps, present_undefined} ->
                    [gpb_lib:map_create(
                       [{FName, Expr} || {FName, _, Expr, _} <- ExprInfos2])];
                {maps, omitted} ->
                    [gpb_lib:map_create(
                       [{FName, Expr} || {FName, m, Expr, _} <- ExprInfos2])]
            end
    end.

is_map_msg(MsgName, #anres{maps_as_msgs=MapsAsMsgs}) ->
    lists:keymember({msg,MsgName}, 1, MapsAsMsgs).

is_msg_type({msg,_}) -> true;
is_msg_type(_)       -> false.

decoder_read_field_params(MsgName, MsgDef, AnRes, Opts) ->
    case gpb_lib:get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            Params = decoder_params(MsgName, AnRes),
            {Params, Params, []};
        pass_as_record ->
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
            Params = decoder_params(MsgName, AnRes),
            MappingVar = hd(Params),
            FFields = [{FName, gpb_lib:var_n("R", I)}
                       || {I,FName} <- gpb_lib:index_seq(
                                         repeated_field_names(MsgDef))],
            FMatch = gpb_lib:mapping_match(MsgName, FFields, Opts),
            FParam = ?expr(matching = '<Var>',
                           [replace_tree(matching, FMatch),
                            replace_tree('<Var>', MappingVar)]),
            {Params, [FParam], FFields}
    end.

repeated_field_names(MsgDef) ->
    [FName || #?gpb_field{name=FName, occurrence=repeated} <- MsgDef].

decoder_params(MsgName, AnRes) ->
    NumFields = gpb_lib:get_num_fields(MsgName, AnRes),
    case gpb_lib:get_field_pass(MsgName, AnRes) of
        pass_as_params -> [gpb_lib:var_f_n(I) || I <- lists:seq(1, NumFields)];
        pass_as_record -> [?expr(Msg)]
    end.

%% compute info for the fast-path field recognition/decoding-call
decoder_fp(Bindings, MsgName, MsgDef) ->
    Rest = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    [begin
         BMatch = ?expr(<<'<field-and-wiretype-bytes>', '<Rest>'/binary>>,
                        [splice_trees('<field-and-wiretype-bytes>',
                                      gpb_lib:varint_to_binary_fields(
                                        Selector)),
                         replace_tree('<Rest>', Rest)]),
         FnCall = ?expr('decode_field'('<Rest>', Z1, Z2, '<Params>',
                                       'TrUserData'),
                        [replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         splice_trees('<Params>', Params),
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
    Params = fetch_binding('<Params>', Bindings),
    SkipCalls = decoder_field_calls(Bindings, MsgName, [], AnRes),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    FieldSelects = decoder_field_selectors(MsgName, MsgDef),
    ?expr(case '<Key>' of
              '<selector>' -> 'decode_field'('<Rest>', 0, 0, '<Params>',
                                             'TrUserData');
              _            -> '<skip-calls>'
       end,
       [replace_tree('<Key>', Key),
        repeat_clauses('<selector>',
                       [[replace_term('<selector>', Selector),
                         replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         splice_trees('<Params>', Params)]
                        || {Selector, DecodeFn} <- FieldSelects]),
        replace_tree('<skip-calls>', SkipCalls),
        replace_tree('TrUserData', TrUserDataVar)]).

decoder_skip_calls(Bindings, MsgName) ->
    KeyExpr = fetch_binding('<Key>', Bindings),
    FieldNumExpr = ?expr('<Key>' bsr 3, [replace_tree('<Key>', KeyExpr)]),
    WiretypeExpr = fetch_binding('<wiretype-expr>', Bindings),
    RestExpr = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    ?expr(case '<wiretype-expr>' of
              0 -> skip_vi('<Rest>', 0, 0, '<Params>', 'TrUserData');
              1 -> skip_64('<Rest>', 0, 0, '<Params>', 'TrUserData');
              2 -> skip_ld('<Rest>', 0, 0, '<Params>', 'TrUserData');
              3 -> skip_gr('<Rest>', 'FNum', 0, '<Params>', 'TrUserData');
              5 -> skip_32('<Rest>', 0, 0, '<Params>', 'TrUserData')
          end,
          [replace_tree('<wiretype-expr>', WiretypeExpr),
           replace_tree('<Rest>', RestExpr),
           splice_trees('<Params>', Params),
           replace_tree('TrUserData', TrUserDataVar),
           replace_term(skip_vi, gpb_lib:mk_fn(skip_varint_, MsgName)),
           replace_term(skip_64, gpb_lib:mk_fn(skip_64_, MsgName)),
           replace_term(skip_ld, gpb_lib:mk_fn(skip_length_delimited_,
                                               MsgName)),
           replace_term(skip_gr, gpb_lib:mk_fn(skip_group_, MsgName)),
           replace_tree('FNum', FieldNumExpr),
           replace_term(skip_32, gpb_lib:mk_fn(skip_32_, MsgName))]).

decoder_field_selectors(MsgName, MsgDef) ->
    map_msgdef_fields_o(
      fun(#?gpb_field{name=FName, fnum=FNum, type=Type}=FieldDef, _IsOneof) ->
              Wiretype = case Type of
                             {group, _} ->
                                 gpb:encode_wiretype(group_start);
                             _ ->
                                 case gpb_lib:is_packed(FieldDef) of
                                     true  -> gpb:encode_wiretype(bytes);
                                     false -> gpb:encode_wiretype(Type)
                                 end
                         end,
              Selector = (FNum bsl 3) bor Wiretype,
              DecodeFn = gpb_lib:mk_fn(d_field_, MsgName, FName),
              {Selector, DecodeFn}
      end,
      MsgDef).

decoder_finalize_result(Params, FFields, MsgName, MsgDef,
                        TrUserDataVar, AnRes, Opts) ->
    case gpb_lib:get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    decoder_finalize_params_all_present(Params, MsgName, MsgDef,
                                                        TrUserDataVar,
                                                        AnRes, Opts);
                {maps, omitted} ->
                    decoder_finalize_params_opt_omitted(Params, MsgName, MsgDef,
                                                        TrUserDataVar,
                                                        AnRes, Opts)
            end;
        pass_as_record ->
            MsgVar = hd(Params),
            [gpb_lib:mapping_update(
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
                || {FName, FVar} <- FFields],
               Opts)]
    end.

decoder_finalize_params_all_present(Params, MsgName, MsgDef, TrUserDataVar,
                                    AnRes, Opts) ->
    [gpb_lib:mapping_create(
       MsgName,
       [decoder_finalize_param_for_mapping(Field, Param, MsgName,
                                           TrUserDataVar, AnRes)
        || {Field, Param} <- lists:zip(MsgDef, Params)],
       Opts)].

decoder_finalize_params_opt_omitted(Params, MsgName, MsgDef, TrUserDataVar,
                                    AnRes, _Opts) ->
    {Optionals, NonOptionals} = gpb_lib:key_partition_on_optionality(
                                  1, lists:zip(MsgDef, Params),
                                  [mapfields_are_required]),
    NonOptionalsMap = gpb_lib:map_create(
                        [decoder_finalize_param_for_mapping(
                           Field, Param, MsgName, TrUserDataVar, AnRes)
                         || {Field, Param} <- NonOptionals]),
    gpb_lib:do_exprs(
      fun({Field, Param}, Var) ->
              FV = decoder_finalize_param_for_mapping(
                     Field, Param, MsgName, TrUserDataVar, AnRes),
              ?expr(if 'Param' == '$undef' -> 'Var';
                       true -> 'Var#{field => Param}'
                    end,
                    [replace_tree('Param', Param),
                     replace_tree('Var', Var),
                     replace_tree('Var#{field => Param}',
                                  gpb_lib:map_set(Var, [FV]))])
      end,
      NonOptionalsMap,
      Optionals).

decoder_finalize_param_for_mapping(Field, Param, MsgName, TrUserDataVar,
                                   AnRes) ->
    FName = gpb_lib:get_field_name(Field),
    ElemPath = [MsgName, FName],
    Finalizer = gpb_gen_translators:find_translation(
                  ElemPath,
                  decode_repeated_finalize,
                  AnRes),
    FValueExpr = case gpb_lib:get_field_occurrence(Field) of
                     required -> Param;
                     optional -> Param;
                     repeated -> ?expr('lists:reverse'('Param', 'TrUserData'),
                                       [replace_term('lists:reverse',Finalizer),
                                        replace_tree('Param', Param),
                                        replace_tree('TrUserData',
                                                     TrUserDataVar)])
                 end,
    {FName, FValueExpr}.

format_field_decoders(MsgName, MsgDef, AnRes, Opts) ->
    map_msgdef_fields_o(
      fun(Field, IsOneof) ->
              [format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts), "\n"]
      end,
      MsgDef).

format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts) ->
    case gpb_lib:is_packed(Field) of
        false ->
            XField = {Field, IsOneof},
            format_non_packed_field_decoder(MsgName, XField, AnRes, Opts);
        true ->
            %% a packed field can never be one of a `oneof' fields
            format_packed_field_decoder(MsgName, Field, AnRes, Opts)
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
        fixed32  -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        sfixed32 -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        float    -> format_floating_point_field_decoder(MsgName, XField,
                                                        float, AnRes, Opts);
        fixed64  -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        sfixed64 -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        double   -> format_floating_point_field_decoder(MsgName, XField,
                                                        double, AnRes, Opts);
        string   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {map,_,_}-> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {group,_}-> format_group_field_decoder(MsgName, XField, AnRes, Opts)
    end.

format_packed_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #?gpb_field{name=FName, rnum=RNum} = FieldDef,
    Params = decoder_params(MsgName, AnRes),
    InParams = case gpb_lib:get_field_pass(MsgName, AnRes) of
                   pass_as_params ->
                       Params;
                   pass_as_record ->
                       MMatch = gpb_lib:mapping_match(MsgName,
                                                      [{FName, ?expr(E)}],
                                                      Opts),
                       [?expr(matching = '<Var>',
                              [replace_tree(matching, MMatch),
                               replace_tree('<Var>', hd(Params))])]
               end,
    Param = case gpb_lib:get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    lists:nth(RNum - 1, Params);
                pass_as_record ->
                    ?expr(E)
            end,
    OutParams = case gpb_lib:get_field_pass(MsgName, AnRes) of
                    pass_as_params ->
                        lists_setelement(RNum - 1, Params, ?expr(NewSeq));
                    pass_as_record ->
                        [gpb_lib:mapping_update(hd(Params), MsgName,
                                                [{FName, ?expr(NewSeq)}],
                                                Opts)]
                end,
    [gpb_codegen:format_fn(
       gpb_lib:mk_fn(d_field_, MsgName, FName),
       fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData)
             when N < ?NB ->
               call_self(Rest, N + 7, X bsl N + Acc, '<Params>', TrUserData);
          (<<0:1, X:7, Rest/binary>>, N, Acc, '<InParams>', TrUserData) ->
               Len = X bsl N + Acc,
               <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
               NewSeq = decode_packed(PackedBytes, 0, 0, '<Param>',
                                      'MaybeTrUserData'),
               '<call-read-field>'(Rest2, 0, 0, '<OutParams>', TrUserData)
       end,
       [splice_trees('<Params>', Params),
        splice_trees('<InParams>', InParams),
        replace_term(decode_packed, gpb_lib:mk_fn(d_packed_field_,
                                                  MsgName, FName)),
        replace_tree('<Param>', Param),
        replace_term('<call-read-field>', gpb_lib:mk_fn(dfp_read_field_def_,
                                                        MsgName)),
        splice_trees('<OutParams>', OutParams),
        splice_trees('MaybeTrUserData',
                     gpb_gen_translators:maybe_userdata_param(
                       FieldDef,
                       ?expr(TrUserData)))]),
     "\n",
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
    gpb_codegen:format_fn(
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
      []);
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 64, double) ->
    gpb_codegen:format_fn(
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
      []);
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, BitLen, BitTypes) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
      fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [Value | AccSeq]);
         (<<>>, _, _, AccSeq) ->
              AccSeq
      end,
      [replace_term('<N>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes])]).

format_dpacked_vi(MsgName, #?gpb_field{name=FName}=FieldDef, AnRes, Opts) ->
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest},
                             {'<TrUserData>', TrUserDataVar}]),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                C = ?exprs(call_self('<Rest2>', 0, 0, ['<Res>' | AccSeq],
                                    'MaybeTrUserData'),
                           [replace_tree('<Rest2>', Rest2Var),
                            replace_tree('<Res>', FVar),
                            splice_trees(
                              'MaybeTrUserData',
                              gpb_gen_translators:maybe_userdata_param(
                                FieldDef,
                                TrUserDataVar))]),
                DecodeExprs ++ C
        end,
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, false,
                                                AnRes),
    Body = decode_int_value(FVar, Bindings, FieldDef, Tr, TrUserDataVar,
                            Opts, BodyTailFn),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(d_packed_field_, MsgName, FName),
      fun(<<1:1, X:7, Rest/binary>>, N, Acc, AccSeq, 'MaybeTrUserData')
            when N < ?NB ->
              call_self(Rest, N + 7, X bsl N + Acc, AccSeq, 'MaybeTrUserData');
         (<<0:1, X:7, Rest/binary>>, N, Acc, AccSeq, 'MaybeTrUserData') ->
              '<body>';
         (<<>>, 0, 0, AccSeq, 'Maybe_TrUserData') ->
              AccSeq
      end,
      [splice_trees('<body>', Body),
       splice_trees('MaybeTrUserData',
                    gpb_gen_translators:maybe_userdata_param(
                      FieldDef,
                      TrUserDataVar)),
       splice_trees('Maybe_TrUserData',
                    gpb_gen_translators:maybe_userdata_param(
                      FieldDef,
                      ?expr(_TrUserData)))]).

format_vi_based_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName}=FieldDef, IsOneof}=XFieldDef,
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest},
                             {'<TrUserData>', TrUserDataVar}]),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef, AnRes,
                                              Opts),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                ReadFieldDefFn = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
                Params2 = updated_merged_params(MsgName, XFieldDef, AnRes,
                                                FVar, PrevValue, Params,
                                                TrUserDataVar, Opts),
                C = ?exprs('<call-read-field>'('<Rest2>', 0, 0, '<Params2>',
                                               'TrUserData'),
                           [replace_term('<call-read-field>', ReadFieldDefFn),
                            replace_tree('<Rest2>', Rest2Var),
                            splice_trees('<Params2>', Params2),
                            replace_tree('TrUserData', TrUserDataVar)]),
                DecodeExprs ++ C
        end,
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof,
                                                AnRes),
    Body = decode_int_value(FVar, Bindings, FieldDef, Tr, TrUserDataVar,
                            Opts, BodyTailFn),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(d_field_, MsgName, FName),
      fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData)
            when N < ?NB ->
              call_self(Rest, N + 7, X bsl N + Acc, '<Params>', TrUserData);
         (<<0:1, X:7, Rest/binary>>, N, Acc, '<InParams>', TrUserData) ->
              '<body>'
      end,
      [splice_trees('<Params>', Params),
       splice_trees('<InParams>', InParams),
       splice_trees('<body>', Body)]).

%% -> {[Expr], Rest2VarExpr}
%% where [Expr] is a list of exprs to calculate the resulting decoded value
decode_int_value(ResVar, Bindings, #?gpb_field{type=Type}=F,
                 Tr, TrUserDataVar, Opts, TailFn) ->
    Value = fetch_binding('<Value>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    StringsAsBinaries = gpb_lib:get_strings_as_binaries_by_opts(Opts),
    case Type of
        sint32 ->
            TailFn(decode_zigzag_to_var(ResVar, Value), Rest);
        sint64 ->
            TailFn(decode_zigzag_to_var(ResVar, Value), Rest);
        int32 ->
            TailFn([uint_to_int_to_var(ResVar, Value, 32)], Rest);
        int64 ->
            TailFn([uint_to_int_to_var(ResVar, Value, 64)], Rest);
        uint32 ->
            TailFn([gpb_lib:assign_to_var(ResVar, Value)], Rest);
        uint64 ->
            TailFn([gpb_lib:assign_to_var(ResVar, Value)], Rest);
        bool ->
            Bool = ?expr('<Res>' = ('<Value>') =/= 0,
                         [replace_tree('<Res>', ResVar),
                          replace_tree('<Value>', Value)]),
            TailFn([Bool], Rest);
        {enum, EnumName} ->
            Tmp = ?expr(Tmp),
            ToSym = [uint_to_int_to_var(Tmp, Value, 32),
                     ?expr('<Res>' = decode_enum('<Int>'),
                           [replace_tree('<Res>', ResVar),
                            replace_term(decode_enum,
                                         gpb_lib:mk_fn(d_enum_, EnumName)),
                            replace_tree('<Int>', Tmp)])],
            TailFn(ToSym, Rest);
        string when StringsAsBinaries ->
            Rest2 = ?expr(Rest2),
            TailFn(unpack_bytes(ResVar, Value, Rest, Rest2, Opts),
                   Rest2);
        string when not StringsAsBinaries ->
            Rest2 = ?expr(Rest2),
            TailFn(?exprs(Len = '<Value>',
                          <<Utf8:Len/binary, Rest2/binary>> = '<Rest>',
                          '<Res>' = unicode:characters_to_list(Utf8, unicode),
                          [replace_tree('<Value>', Value),
                           replace_tree('<Rest>', Rest),
                           replace_tree('<Res>', ResVar)]),
                   Rest2);
        bytes ->
            Rest2 = ?expr(Rest2),
            TailFn(unpack_bytes(ResVar, Value, Rest, Rest2, Opts),
                   Rest2);
        {msg, Msg2Name} ->
            Rest2 = ?expr(Rest2),
            TailFn(?exprs(Len = '<Value>',
                          <<Bs:Len/binary, Rest2/binary>> = '<Rest>',
                          '<Res>' = 'Tr'('d_msg_X'(Bs, 'TrUserData'),
                                         'TrUserData'),
                          [replace_tree('<Value>', Value),
                           replace_tree('<Rest>', Rest),
                           replace_tree('<Res>', ResVar),
                           replace_term('d_msg_X',
                                        gpb_lib:mk_fn(d_msg_, Msg2Name)),
                           replace_term('Tr', Tr(decode)),
                           replace_tree('TrUserData', TrUserDataVar)]),
                   Rest2);
        {map, KeyType, ValueType} ->
            MapAsMsgMame = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
            F2 = F#?gpb_field{type={msg,MapAsMsgMame}},
            decode_int_value(ResVar, Bindings, F2, Tr, TrUserDataVar,
                             Opts, TailFn)
    end.

unpack_bytes(ResVar, Value, Rest, Rest2, Opts) ->
    CompilerHasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    Copy = case proplists:get_value(copy_bytes, Opts, auto) of
               auto when not CompilerHasBinary -> false;
               auto when CompilerHasBinary     -> true;
               true                            -> true;
               false                           -> false;
               N when is_integer(N)            -> N;
               N when is_float(N)              -> N
           end,
    Transforms = [replace_tree('<Value>', Value),
                  replace_tree('<Res>', ResVar),
                  replace_tree('<Rest>', Rest),
                  replace_tree('<Rest2>', Rest2),
                  replace_term('<Copy>', Copy)],
    if Copy == false ->
            ?exprs(Len = '<Value>',
                   <<'<Res>':Len/binary, '<Rest2>'/binary>> = '<Rest>',
                   Transforms);
       Copy == true ->
            ?exprs(Len = '<Value>',
                   <<Bytes:Len/binary, '<Rest2>'/binary>> = '<Rest>',
                   '<Res>' = binary:copy(Bytes),
                   Transforms);
       is_integer(Copy); is_float(Copy) ->
            ?exprs(Len = '<Value>',
                   <<Bytes:Len/binary, '<Rest2>'/binary>> = '<Rest>',
                   '<Res>' = case binary:referenced_byte_size(Bytes) of
                                 LB when LB >= byte_size(Bytes) * '<Copy>' ->
                                     binary:copy(Bytes);
                                 _ ->
                                     Bytes
                             end,
                   Transforms)
    end.

format_group_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName, fnum=FNum, type={group,GroupName}}=FieldDef,
     IsOneof}=XFieldDef,
    ResVar = ?expr(NewFValue), %% result is to be put in this variable
    TrUserDataVar = ?expr(TrUserData),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef,
                                              AnRes, Opts),
    OutParams = updated_merged_params(MsgName, XFieldDef, AnRes,
                                      ResVar, PrevValue, Params,
                                      TrUserDataVar, Opts),
    Tr = gpb_gen_translators:mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof,
                                                AnRes),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(d_field_, MsgName, FName),
      fun(Bin, _, _, 'InParams', TrUserData) ->
              {GroupBin, Rest} = read_group(Bin, 'FieldNum'),
              'Res' = 'Tr'('d_msg_X'(GroupBin, TrUserData), TrUserData),
              'call-read-field'(Rest, 0, 0, 'OutParams', TrUserData)
      end,
      [splice_trees('InParams', InParams),
       replace_term('call-read-field', gpb_lib:mk_fn(dfp_read_field_def_,
                                                     MsgName)),
       replace_term('Tr', Tr(decode)),
       replace_tree('Res', ResVar),
       replace_tree('FieldNum', erl_syntax:integer(FNum)),
       replace_term('d_msg_X', gpb_lib:mk_fn(d_msg_, GroupName)),
       splice_trees('OutParams', OutParams)]).

updated_merged_params(MsgName, XFieldDef, AnRes, NewValue, PrevValue,
                      Params, TrUserDataVar, Opts) ->
    Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(MsgName, XFieldDef,
                                                           AnRes),
    case {gpb_lib:get_field_pass(MsgName, AnRes), XFieldDef} of
        {pass_as_params, {#?gpb_field{rnum=RNum}, _IsOneof}} ->
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, Tr, TrUserDataVar,
                                           AnRes, Opts),
            lists_setelement(RNum - 1, Params, MergedValue);
        {pass_as_record, {#?gpb_field{name=FName}, false}} ->
            MsgVar = hd(Params),
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, Tr, TrUserDataVar,
                                           AnRes, Opts),
            [gpb_lib:mapping_update(MsgVar, MsgName, [{FName, MergedValue}],
                                    Opts)];
        {pass_as_record, {_OField, {true, CFName}}} ->
            MsgVar = hd(Params),
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, Tr, TrUserDataVar,
                                           AnRes, Opts),
            [gpb_lib:mapping_update(MsgVar, MsgName, [{CFName, MergedValue}],
                                    Opts)]
    end.

merge_field_expr({FieldDef, false}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes, Opts) ->
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
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if 'Prev' == undefined -> 'New';
                             true -> 'merge_msg_X'('Prev', 'New', 'TrUserData')
                          end,
                          [replace_term('merge_msg_X', Tr(merge, MergeFn)),
                           replace_tree('Prev', PrevValue),
                           replace_tree('New', NewValue),
                           replace_tree('TrUserData', TrUserDataVar)]);
                {maps, omitted} ->
                    case gpb_lib:get_field_pass(MsgName, AnRes) of
                        pass_as_params ->
                            ?expr(if 'Prev' =:= '$undef' -> 'New';
                                     true -> 'merge_msg_X'('Prev', 'New',
                                                           'TrUserData')
                                  end,
                                  [replace_tree('Prev', PrevValue),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)]);
                        pass_as_record ->
                            FName = gpb_lib:get_field_name(FieldDef),
                            ?expr(case 'Msg' of
                                      '#{fieldname := Prev}' ->
                                          'merge_msg_X'(Prev, 'New',
                                                        'TrUserData');
                                      _ ->
                                          'New'
                                  end,
                                  [replace_tree(
                                     '#{fieldname := Prev}',
                                     gpb_lib:map_match([{FName, ?expr(Prev)}])),
                                   replace_tree('Msg', PrevValue),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)])
                    end
            end
    end;
merge_field_expr({FieldDef, {true, CFName}}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes, Opts)->
    #?gpb_field{name=FName, type=Type} = FieldDef,
    if ?is_msg_or_group(Type) ->
            {_, FMsgName} = Type,
            MergeFn = gpb_lib:mk_fn(merge_msg_, FMsgName),
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
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
                {maps, omitted} ->
                    MsgVar = PrevValue,
                    case gpb_lib:get_field_pass(MsgName, AnRes) of
                        pass_as_params ->
                            ?expr(case 'Prev' of
                                      '$undef' ->
                                          {'tag', 'New'};
                                      {'tag', MVPrev} ->
                                          {'tag', 'merge_msg_X'(MVPrev, 'New',
                                                               'TrUserData')};
                                      _ ->
                                          {'tag', 'New'}
                                  end,
                                  [replace_term('tag', FName),
                                   replace_tree('Prev', PrevValue),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)]);
                        pass_as_record ->
                            OFVal = ?expr({tag, MVPrev},
                                          [replace_term(tag, FName)]),
                            ?expr(case 'Msg' of
                                      '#{fieldname := {tag,MVPrev}}' ->
                                          {'tag', 'merge_msg_X'(MVPrev,'New',
                                                               'TrUserData')};
                                      _ ->
                                          {'tag', 'New'}
                                  end,
                                  [replace_tree('#{fieldname := {tag,MVPrev}}',
                                                gpb_lib:map_match(
                                                  [{CFName, OFVal}])),
                                   replace_term('tag', FName),
                                   replace_tree('Msg', MsgVar),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)])
                    end
            end;
       true ->
            %% Replace
            ?expr({'fieldname', '<expr>'},
                  [replace_term('fieldname', FName),
                   replace_tree('<expr>', NewValue)])
    end.

decoder_in_params(Params, MsgName, {FieldDef, false}, AnRes, Opts) ->
    #?gpb_field{name=FName}=FieldDef,
    Any = ?expr(_),
    case gpb_lib:get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            #?gpb_field{rnum=RNum} = FieldDef,
            Prev = lists:nth(RNum-1, Params),
            case gpb_lib:classify_field_merge_action(FieldDef) of
                overwrite -> {lists_setelement(RNum-1, Params, Any), Any};
                seqadd    -> {Params, Prev};
                msgmerge  -> {Params, Prev}
            end;
        pass_as_record ->
            Prev = ?expr(Prev),
            InParams = [gpb_lib:match_bind_var(
                          gpb_lib:mapping_match(MsgName, [{FName, Prev}], Opts),
                          hd(Params))],
            case gpb_lib:classify_field_merge_action(FieldDef) of
                overwrite -> {Params, Any};
                seqadd    -> {InParams, Prev};
                msgmerge  ->
                    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                        X when X == records;
                               X == {maps, present_undefined} ->
                            {InParams, Prev};
                        {maps, omitted} ->
                            MsgVar = hd(Params),
                            {[MsgVar], MsgVar}
                    end
            end
    end;
decoder_in_params(Params, MsgName, {FieldDef, {true, CFName}}, AnRes, Opts) ->
    #?gpb_field{type=Type, rnum=RNum} = FieldDef,
    if ?is_msg_or_group(Type) ->
            %% oneof fields that of message type may need merging
            case gpb_lib:get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    Prev = lists:nth(RNum-1, Params),
                    {Params, Prev};
                pass_as_record ->
                    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                        X when X == records;
                               X == {maps, present_undefined} ->
                            Prev = ?expr(Prev),
                            InParams = [gpb_lib:match_bind_var(
                                          gpb_lib:mapping_match(
                                            MsgName,
                                            [{CFName, Prev}],
                                            Opts),
                                          hd(Params))],
                            {InParams, Prev};
                        {maps, omitted} ->
                            MsgVar = hd(Params),
                            {[MsgVar], MsgVar}
                    end
            end;
       true ->
            %% Non-messages, treat as an optional field
            Any = ?expr(_),
            case gpb_lib:get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    {lists_setelement(RNum-1, Params, Any), Any};
                pass_as_record ->
                    {Params, Any}
            end
    end.

format_fixlen_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName, type=Type}, _IsOneof} = XFieldDef,
    {BitLen, BitTypes} = case Type of
                             fixed32  -> {32, [little]};
                             sfixed32 -> {32, [little,signed]};
                             float    -> {32, [little,float]};
                             fixed64  -> {64, [little]};
                             sfixed64 -> {64, [little,signed]};
                             double   -> {64, [little,float]}
                         end,
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef, AnRes,
                                              Opts),
    Value = ?expr(Value),
    TrUserDataVar = ?expr(TrUserData),
    Params2 = updated_merged_params(MsgName, XFieldDef, AnRes,
                                    Value, PrevValue, Params,
                                    TrUserDataVar, Opts),
    ReadFieldDefFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(d_field_, MsgName, FName),
      fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, '<InParams>',
          'TrUserData') ->
              '<call-read-field>'(Rest, Z1, Z2, '<OutParams>', 'TrUserData')
      end,
      [replace_term('<N>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes]),
       splice_trees('<InParams>', InParams),
       replace_term('<call-read-field>', ReadFieldDefFnName),
       splice_trees('<OutParams>', Params2),
       replace_tree('TrUserData', TrUserDataVar)]).

format_floating_point_field_decoder(MsgName, XFieldDef, Type, AnRes, Opts) ->
    {#?gpb_field{name=FName}, _IsOneof} = XFieldDef,
    TrUserDataVar = ?expr(TrUserData),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef,
                                              AnRes, Opts),
    OutParamsReplacements =
        [splice_trees(Marker, updated_merged_params(MsgName, XFieldDef, AnRes,
                                                    OutExpr, PrevValue, Params,
                                                    TrUserDataVar, Opts))
         || {Marker, OutExpr} <- [{'OutParams', ?expr(Value)},
                                  {'InfinityOutParams', ?expr(infinity)},
                                  {'-InfinityOutParams', ?expr('-infinity')},
                                  {'NanOutParams', ?expr(nan)}]],
    ReadFieldDefFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    Replacements =
        [splice_trees('InParams', InParams),
         replace_term('<call-read-field>', ReadFieldDefFnName),
         replace_tree('TrUserData', TrUserDataVar)] ++
        OutParamsReplacements,
    case Type of
        float ->
            gpb_codegen:format_fn(
              gpb_lib:mk_fn(d_field_, MsgName, FName),
              fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'InfinityOutParams',
                                          'TrUserData');
                 (<<0:16,128,255, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, '-InfinityOutParams',
                                          'TrUserData');
                 (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'NanOutParams',
                                          'TrUserData');
                 (<<Value:32/little-float, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'OutParams',
                                          'TrUserData')
              end,
              Replacements);
        double ->
            gpb_codegen:format_fn(
              gpb_lib:mk_fn(d_field_, MsgName, FName),
              fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'InfinityOutParams',
                                          'TrUserData');
                 (<<0:48,240,255, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, '-InfinityOutParams',
                                          'TrUserData');
                 (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'NanOutParams',
                                          'TrUserData');
                 (<<Value:64/little-float, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'OutParams',
                                          'TrUserData')
              end,
              Replacements)
    end.

decode_zigzag_to_var(ResVar, ValueExpr) ->
    ?exprs(ZValue = '<Value>',
           '<Res>' = if ZValue band 1 =:= 0 -> ZValue bsr 1;
                        true                -> -((ZValue + 1) bsr 1)
                     end,
           [replace_tree('<Value>', ValueExpr),
            replace_tree('<Res>', ResVar)]).

uint_to_int_to_var(ResVar, ValueExpr, NumBits) ->
    %% Contrary to the 64 bit encoding done for int32 (and enum),
    %% decode the value as 32 bits, so we decode negatives
    %% given both as 32 bits and as 64 bits wire encodings
    %% to the same integer.
    ?expr(
       <<'<Res>':'<N>'/signed-native>> = <<('<Value>'):'<N>'/unsigned-native>>,
       [replace_term('<N>', NumBits),
        replace_tree('<Res>', ResVar),
        replace_tree('<Value>', ValueExpr)]).

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

format_field_skippers(MsgName, AnRes) ->
    SkipVarintFnName = gpb_lib:mk_fn(skip_varint_, MsgName),
    SkipLenDelimFnName = gpb_lib:mk_fn(skip_length_delimited_, MsgName),
    SkipGroupFnName = gpb_lib:mk_fn(skip_group_, MsgName),
    ReadFieldFnName = gpb_lib:mk_fn(dfp_read_field_def_, MsgName),
    Params = decoder_params(MsgName, AnRes),
    [%% skip_varint_<MsgName>/2,4
     gpb_codegen:format_fn(
       SkipVarintFnName,
       fun(<<1:1, _:7, Rest/binary>>, Z1, Z2, '<Params>', TrUserData) ->
               call_self(Rest, Z1, Z2, '<Params>', TrUserData);
          (<<0:1, _:7, Rest/binary>>, Z1, Z2, '<Params>', TrUserData) ->
               '<call-read-field>'(Rest, Z1,Z2, '<Params>', TrUserData)
       end,
       [replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_length_delimited_<MsgName>/4
     gpb_codegen:format_fn(
       SkipLenDelimFnName,
       fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData)
             when N < ?NB ->
               call_self(Rest, N+7, X bsl N + Acc, '<Params>', TrUserData);
          (<<0:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData) ->
               Length = X bsl N + Acc,
               <<_:Length/binary, Rest2/binary>> = Rest,
               '<call-read-field>'(Rest2, 0, 0, '<Params>', TrUserData)
       end,
       [replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_group_<MsgName>/4
     gpb_codegen:format_fn(
       SkipGroupFnName,
       fun(Bin, FNum, Z2, '<Params>', TrUserData) ->
          {_, Rest} = read_group(Bin, FNum),
          '<call-read-field>'(Rest, 0, Z2, '<Params>', TrUserData)
       end,
       [replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_32_<MsgName>/2,4
     %% skip_64_<MsgName>/2,4
     [[gpb_codegen:format_fn(
         gpb_lib:mk_fn(skip_, NumBits, MsgName),
         fun(<<_:'<NumBits>', Rest/binary>>, Z1, Z2, '<Params>', TrUserData) ->
                 '<call-read-field>'(Rest, Z1, Z2, '<Params>', TrUserData)
         end,
         [replace_term('<call-read-field>', ReadFieldFnName),
          replace_term('<NumBits>', NumBits),
          splice_trees('<Params>', Params)]),
       "\n"]
      || NumBits <- [32, 64]]].

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

%% lists_replace(N, List, New) -> NewList
%% Like erlang:setelement, but for a list:
%% Replace the Nth element in List with a New value.
lists_setelement(1, [_ | Rest], New) ->
    [New | Rest];
lists_setelement(N, [X | Rest], New) when N > 1 ->
    [X | lists_setelement(N - 1, Rest, New)].
