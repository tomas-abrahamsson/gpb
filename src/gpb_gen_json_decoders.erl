%%% Copyright (C) 2019  Tomas Abrahamsson
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

%%% @private
%%% @doc Generation of routines for converting JSON to internal format

-module(gpb_gen_json_decoders).

-export([format_exports/2]).
-export([format_top_function/3]).
-export([format_decoders/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").
-include("gpb_decoders_lib.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

format_exports(Defs, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    NoNif = not DoNif,
    [?f("-export([from_json/2"),[", from_json/3" || NoNif], ?f("]).~n"),
     [[[begin
            NoWrapperFnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
            if DoNif ->
                    ?f("-export([~p/1]).~n", [NoWrapperFnName]);
               not DoNif ->
                    ?f("-export([~p/1, ~p/2]).~n",
                       [NoWrapperFnName, NoWrapperFnName])
            end
        end
        || {{msg,MsgName}, _Fields} <- Defs],
       "\n"]
      || gpb_lib:get_bypass_wrappers_by_opts(Opts)]].

format_top_function(Defs, AnRes, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_top_function_msgs(Defs, AnRes, Opts);
        false -> format_top_function_no_msgs(Opts)
    end.

format_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoders(Defs, AnRes, Opts)].

format_top_function_no_msgs(_Opts) ->
    ["-spec from_json(term(), atom()) -> no_return().\n",
     gpb_codegen:format_fn(
       from_json,
       fun(_Json, _MsgName) ->
               erlang:error({gpb_error, no_messages})
       end),
     "-spec from_json(term(), atom(), list()) -> no_return().\n",
     gpb_codegen:format_fn(
       from_json,
       fun(_Json, _MsgName, _Opts) ->
               erlang:error({gpb_error, no_messages})
       end)].

format_top_function_msgs(Defs, AnRes, Opts) ->
    Error = ("error({gpb_error," ++
             ""     "{from_json_failure," ++
             ""     " {Json, MsgName, {Class, Reason, StackTrace}}}})"),
    FromJsonMsg1Catch_GetStackTraceAsPattern =
        ?f("from_json_1_catch(Json, MsgName, TrUserData) ->~n"
           "    try from_json_2_doit(MsgName, Json, TrUserData)~n"
           "    catch Class:Reason:StackTrace -> ~s~n"
           "    end.~n", [Error]),
    FromJsonMsg1Catch_GetStackTraceAsCall =
        ?f("from_json_1_catch(Json, MsgName, TrUserData) ->~n"
           "    try from_json_2_doit(MsgName, Json, TrUserData)~n"
           "    catch Class:Reason ->~n"
           "        StackTrace = erlang:get_stacktrace(),~n"
           "        ~s~n"
           "    end.~n", [Error]),
    DoNif = proplists:get_bool(nif, Opts),
    [gpb_codegen:format_fn(
       from_json,
       fun(Json, MsgName) ->
               call_self(Json, MsgName, [])
       end),
     gpb_codegen:format_fn(
       from_json,
       fun(Json, MsgName, Opts) ->
               TrUserData = proplists:get_value(user_data, Opts),
               from_json_1_catch(Json, MsgName, TrUserData)
       end),
     ["-ifdef('OTP_RELEASE').\n", % This macro appeared in Erlang 21
      FromJsonMsg1Catch_GetStackTraceAsPattern,
      "-else.\n",
      FromJsonMsg1Catch_GetStackTraceAsCall,
      "-endif.\n\n"],
     gpb_codegen:format_fn(
       from_json_2_doit,
       fun('MsgName', Json, TrUserData) ->
               'Tr'('from-json-fn'(Json, 'TrUserData'), TrUserData)
       end,
       [repeat_clauses(
          'MsgName',
          [begin
               ElemPath = [MsgName],
               Transl = gpb_gen_translators:find_translation(
                          ElemPath, decode, AnRes),
               [replace_term('MsgName', MsgName),
                replace_term('Tr', Transl),
                replace_term('from-json-fn',
                             gpb_lib:mk_fn(from_json_msg_, MsgName)),
                splice_trees('TrUserData', if DoNif -> [];
                                              true  -> [?expr(TrUserData)]
                                           end)]
           end
           || {{msg, MsgName}, _Fields} <- Defs])])].

format_msg_decoders(Defs, AnRes, Opts) ->
    [[[gpb_codegen:format_fn(
         gpb_lib:mk_fn(from_json_msg_, MsgName),
         fun(Bin) ->
                 %% The undefined is the default TrUserData
                 call_self(Bin, undefined)
         end)
       || {{msg,MsgName},_Fields} <- Defs]
      || gpb_lib:get_bypass_wrappers_by_opts(Opts)],
     [format_msg_decoder(Name, MsgDef, Defs, AnRes, Opts)
      || {_Type, Name, MsgDef} <- gpb_lib:msgs_or_groups(Defs)],
     format_helpers(Defs, AnRes, Opts)].

format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    %% Compared to decoding from protobuf wire-format (gpb_gen_decoders),
    %% We do not need to do any merging here intertwined with decoding.
    %% In Json, a key can occur only once.
    %%
    %% (In contrast, the protobuf wire-format is a stream of fields,
    %% if a field occurs more than once, the value is to be merged:
    %% - overwritten if it is an an optional or required scalar field,
    %% - appended to the sequence if the field is a repeated,
    %% - recursively merged if the field is another sub message.
    %% )
    %%
    TrUserDataVar = ?expr(TrUserData),
    InitExprs = gpb_decoders_lib:init_exprs(MsgName, MsgDef, Defs,
                                            TrUserDataVar, AnRes, Opts),
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    FieldInfos = gpb_decoders_lib:calc_field_infos(MsgDef, IsProto3, Opts),
    if MsgDef == [] ->
            format_msg_decoder_no_fields(MsgName, Opts);
       MsgDef /= [] ->
            [format_msg_init_decoder(MsgName, InitExprs, FieldInfos,
                                     TrUserDataVar, Opts),
             format_msg_decoder_loop(MsgName, MsgDef,
                                     TrUserDataVar, AnRes, Opts)]
    end.

format_msg_decoder_no_fields(MsgName, Opts) ->
    InitExprs1 = calc_init_exprs(MsgName, [], [], Opts),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(from_json_msg_, MsgName),
      fun(_Json, _TrUserData) ->
              '<init>'
      end,
      [replace_tree('<init>', InitExprs1)]).

format_msg_init_decoder(MsgName, InitExprs, FieldInfos, TrUserDataVar, Opts) ->
    InitExprs1 = calc_init_exprs(MsgName, InitExprs, FieldInfos, Opts),
    FromJAuxFn = gpb_lib:mk_fn(fj_msg_, MsgName),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(from_json_msg_, MsgName),
      fun(Json, 'TrUserData') ->
              '<fj_msg_MsgName>'(fj_next(fj_iter(Json)), '<init>', 'TrUserData')
      end,
      [replace_term('<fj_msg_MsgName>', FromJAuxFn),
       replace_tree('<init>', InitExprs1),
       replace_tree('TrUserData', TrUserDataVar)]).

calc_init_exprs(MsgName, InitExprs, FieldInfos, Opts) ->
    MappingUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    case MappingUnset of
        records ->
            gpb_lib:record_create(MsgName, InitExprs);
        #maps{unset_optional=present_undefined} ->
            gpb_lib:map_create(InitExprs, Opts);
        #maps{unset_optional=omitted, oneof=tuples} ->
            InitExprs1 = repeated_or_required_init_exprs(InitExprs, FieldInfos),
            gpb_lib:map_create(InitExprs1, Opts);
        #maps{unset_optional=omitted, oneof=flat} ->
            InitExprs1 = repeated_or_required_init_exprs(InitExprs, FieldInfos),
            gpb_lib:map_create(InitExprs1, Opts)
    end.

repeated_or_required_init_exprs(InitExprs, FieldInfos) ->
    [{FName, InitExpr}
     || {FName, InitExpr, Occ} <- kzip(InitExprs, FieldInfos),
        Occ == required orelse Occ == repeated].

kzip(L1, L2) ->
    lists:map(fun({{K, V1}, {K, V2}}) -> {K, V1, V2} end,
              lists:zip(L1, L2)).

format_msg_decoder_loop(MsgName, MsgDef, TrUserDataVar, AnRes, Opts) ->
    JValueExpr = ?expr(JValue),
    EMsgVar = ?expr(EMsg),
    Decodings = calc_decodings(MsgName, MsgDef,
                               JValueExpr, EMsgVar, TrUserDataVar,
                               AnRes, Opts),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(fj_msg_, MsgName),
      fun({JKey, JValue, JRest}, EMsg, TrUserData) ->
              EMsg2 = case JKey of
                          '<FieldNamePattern>' ->
                              '<decode-value-update-EMsg>';
                          _ ->
                              EMsg
                      end,
              call_self(fj_next(JRest), EMsg2, TrUserData);
         (none, EMsg, _TrUserData) ->
              EMsg
      end,
      [repeat_clauses(
         '<FieldNamePattern>',
         [[replace_tree('<FieldNamePattern>', KeyPattern),
           replace_tree('<decode-value-update-EMsg>', DecodeExpr)]
          || {KeyPattern, DecodeExpr} <- Decodings])]).

calc_decodings(MsgName, MsgDef, JValueExpr, EMsgVar, TrUserDataVar,
               AnRes, Opts) ->
    %% In Json, the oneof fields are like flat oneofs:
    %%
    %% message Msg {
    %%   oneof c {
    %%     uint32 alt_one = 1;
    %%     string alt_two = 2;
    %%   }
    %%   uint32 outside_oneof = 10;
    %% }
    %%
    %% -> {"alt_one": 17}
    %% or {"alt_two": "x"} in Json
    %%
    %% The Erlang-internal format (if maps) may be either
    %%    #{c => {alt_one, 18}} if option {maps_oneof,flat} is _not_ set, or
    %%    #{alt_one => 18}      if option {maps_oneof,flat} is set.
    %%
    %% Additionally, in Json, "[p]arsers accept both the lowerCamelCase name
    %% (or the one specified by the json_name option) and the original proto
    %% field name."
    %% (src: https://developers.google.com/protocol-buffers/docs/proto3#json)
    %%
    %% So build a mapping from Json field name (binary, atom or string)
    %% to decoding-and-update expressions:
    %%
    KeyFormat = gpb_lib:json_key_format_by_opts(Opts),
    lists:reverse(
      gpb_lib:fold_msgdef_fields_o(
        fun(#?gpb_field{name=FName}=Field, IsOneof, Acc)->
                KeyPatterns = key_patterns(FName, KeyFormat),
                DecExpr = mk_check_null_decode_value_update_field_expr(
                            MsgName, Field, IsOneof,
                            JValueExpr, EMsgVar, TrUserDataVar,
                            AnRes, Opts),
                [{KP, DecExpr} || KP <- KeyPatterns] ++ Acc
        end,
        [],
        MsgDef)).

key_patterns(FName, KeyFormat) ->
    FNameS = atom_to_list(FName),
    LowerCamelCase = gpb_lib:lower_camel_case(FNameS),
    Strs = lists:usort([FNameS, LowerCamelCase]),
    case KeyFormat of
        atom ->
            [erl_syntax:atom(JFName) || JFName <- Strs];
        binary ->
            %% Want <<"fname">> and not <<102,110,97,109,101>>
            %% so make a text node
            [erl_syntax:text(?ff("<<~p>>", [JFName])) || JFName <- Strs];
        string ->
            [erl_syntax:string(JFName) || JFName <- Strs]
    end.

mk_check_null_decode_value_update_field_expr(
  MsgName, #?gpb_field{type=Type, occurrence=Occurrence}=Field, IsOneof,
  JValueExpr, EMsgVar, TrUserDataVar,
  AnRes, Opts) ->
    %% FIXME: handle repeated
    %% FIXME: handle map<_,_>
    %% FIXME: gpb_compile: remove transl gen workaround for no from_json calls
    JNull = gpb_lib:json_null(Opts),
    DecExpr =
        case occurrence_or_mapfield(Occurrence, Type) of
            optional -> type_decode_expr(Type, JValueExpr, TrUserDataVar);
            required -> type_decode_expr(Type, JValueExpr, TrUserDataVar);
            repeated -> repeated_field_decode_expr(MsgName, Field, JValueExpr,
                                                   TrUserDataVar, AnRes);
            mapfield -> mapfield_decode_expr(MsgName, Field, JValueExpr,
                                             TrUserDataVar, AnRes)
        end,
    ?expr(if 'JValueExpr' =:= null ->
                  'EMsgVar';
             true ->
                  '<update-field-expr>'
          end,
          [replace_tree('JValueExpr', JValueExpr),
           replace_term(null, JNull),
           replace_tree('EMsgVar', EMsgVar),
           replace_tree('<update-field-expr>',
                        mk_decode_value_update_field_expr(
                          MsgName, Field, IsOneof,
                          EMsgVar, DecExpr, TrUserDataVar,
                          AnRes, Opts))]).

mk_decode_value_update_field_expr(MsgName, #?gpb_field{name=FName}, IsOneof,
                                  EMsgVar, DecExpr, TrUserDataVar,
                                  AnRes, Opts) ->
    MappingUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    case {MappingUnset, IsOneof} of
        {records, false} ->
            ElemPath = [MsgName, FName],
            FValue = tr_wrap({ElemPath, decode, AnRes},
                             DecExpr,
                             TrUserDataVar),
            ?expr('<EMsg>'#'<MsgName>'{'<field-name>' = '<field-value>'},
                  [replace_tree('<EMsg>', EMsgVar),
                   replace_term('<MsgName>', MsgName),
                   replace_term('<field-name>', FName),
                   replace_tree('<field-value>', FValue)]);
        {records, {true, CFName}} ->
            %% Create CTr({tag, OTr(<decode-expr>, TrUserData)}, TrUserData)
            CElemPath = [MsgName, CFName],
            OElemPath = [MsgName, CFName, FName],
            FValue = tr_wrap({CElemPath, decode, AnRes},
                             tag_wrap(FName,
                                      tr_wrap({OElemPath, decode, AnRes},
                                              DecExpr,
                                              TrUserDataVar)),
                             TrUserDataVar),
            ?expr('<EMsg>'#'<MsgName>'{'<field-name>' = '<field-value>'},
                  [replace_tree('<EMsg>', EMsgVar),
                   replace_term('<MsgName>', MsgName),
                   replace_term('<field-name>', CFName),
                   replace_tree('<field-value>', FValue)]);
        {#maps{oneof=tuples}, false} ->
            ElemPath = [MsgName, FName],
            FValue = tr_wrap({ElemPath, decode, AnRes},
                             DecExpr,
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{FName, FValue}], Opts);
        {#maps{oneof=tuples}, {true, CFName}} ->
            %% Create CTr({tag, OTr(<decode-expr>, TrUserData)}, TrUserData)
            CElemPath = [MsgName, CFName],
            OElemPath = [MsgName, CFName, FName],
            FValue = tr_wrap({CElemPath, decode, AnRes},
                             tag_wrap(FName,
                                      tr_wrap({OElemPath, decode, AnRes},
                                              DecExpr,
                                              TrUserDataVar)),
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{CFName, FValue}], Opts);
        {#maps{oneof=flat}, false} ->
            ElemPath = [MsgName, FName],
            FValue = tr_wrap({ElemPath, decode, AnRes},
                             DecExpr,
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{FName, FValue}], Opts);
        {#maps{oneof=flat}, {true, CFName}} ->
            %% Generate maps:without(...)?  To guarantee at most one oneof
            %% field, also in case the json would be "malformed" containing
            %% for instance both alt_one and alt_two?
            CElemPath = [MsgName, CFName],
            OElemPath = [MsgName, CFName, FName],
            FValue = tr_wrap({CElemPath, decode, AnRes},
                             tr_wrap({OElemPath, decode, AnRes},
                                     DecExpr,
                                     TrUserDataVar),
                             TrUserDataVar),
            gpb_lib:map_set(EMsgVar, [{FName, FValue}], Opts)
    end.

repeated_field_decode_expr(MsgName, #?gpb_field{name=FName, type=Type},
                           JValueExpr, TrUserDataVar, AnRes) ->
    [TrEmptySeq, TrAddElem, TrFinalize] =
        [gpb_gen_translators:find_translation([MsgName, FName], Op, AnRes)
         || Op <- [decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize]],
    ElemPath = [MsgName, FName, []],
    JElemVar = gpb_lib:var("JElem@~s", [FName]),
    ElemTrDecExpr = tr_wrap({ElemPath, decode, AnRes},
                            type_decode_expr(Type, JElemVar, TrUserDataVar),
                            TrUserDataVar),
    ?expr('lists:reverse'(
            lists:foldl(
              fun('JElem@FVar', Acc) ->
                      '[New|Acc]'('<decoded-elem>', Acc, 'TrUserData')
              end,
              '[]'([], 'TrUserData'),
              fj_array('JValue')),
            'TrUserData'),
          [replace_tree('JValue', JValueExpr),
           replace_tree('JElem@FVar', JElemVar),
           replace_tree('<decoded-elem>', ElemTrDecExpr),
           replace_term('lists:reverse', TrFinalize),
           replace_term('[New|Acc]', TrAddElem),
           %% Get it from EMsg instead to avoid translator called twice?
           replace_term('[]', TrEmptySeq),
           replace_tree('TrUserData', TrUserDataVar)]).

mapfield_decode_expr(MsgName, #?gpb_field{name=FName, type={map, KT, VT}=FType},
                     JValueExpr, TrUserDataVar, AnRes) ->
    MapMsgName = gpb_lib:map_type_to_msg_name(KT, VT),
    ElemPath = [MsgName, FName, []],
    [TrEmptySeq, TrAddElem, TrFinalize] =
        [gpb_gen_translators:find_translation([MsgName, FName], Op, AnRes)
         || Op <- [decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize]],
    KElemDecExpr = tr_wrap(
                     {ElemPath, decode, AnRes},
                     map_key_type_decode_expr(KT, ?expr(Key), TrUserDataVar),
                     TrUserDataVar),
    VElemDecExpr = tr_wrap(
                     {ElemPath, decode, AnRes},
                     type_decode_expr(VT, ?expr(Value), TrUserDataVar),
                     TrUserDataVar),
    ElemDecExpr = tr_wrap(
                    {ElemPath, decode, AnRes},
                    type_decode_expr(FType, JValueExpr, TrUserDataVar),
                    TrUserDataVar),
    ?expr(fj_mapfield_fold(
            '<map-name-as-msg>',
            fun 'new-[]'/2,
            fun 'add-elem-[New|Acc]'/3,
            fun 'lists:reverse'/2,
            fun(Key) -> '<ElemDecExpr(Key)>' end,
            fun(Value) -> '<ElemDecExpr(Value)>' end,
            'Tr(JValueExpr,TrUserData)',
            'TrUserData'),
          [replace_term('<map-name-as-msg>', MapMsgName),
           %%  to avoid translator called twice?
           replace_term('new-[]', TrEmptySeq), % Get it from EMsg instead?
           replace_term('add-elem-[New|Acc]', TrAddElem),
           replace_term('lists:reverse', TrFinalize),
           replace_tree('<ElemDecExpr(Key)>', KElemDecExpr),
           replace_tree('<ElemDecExpr(Value)>', VElemDecExpr),
           replace_tree('Tr(JValueExpr,TrUserData)', ElemDecExpr),
           replace_tree('TrUserData', TrUserDataVar)]).

tr_wrap({ElemPath, Op, AnRes}, Expr, TrUserDataVar) ->
    Tr = gpb_gen_translators:find_translation(ElemPath, Op, AnRes),
    ?expr('Tr'('<expr>', 'TrUserData'),
          [replace_term('Tr', Tr),
           replace_tree('<expr>', Expr),
           replace_tree('TrUserData', TrUserDataVar)]).

tag_wrap(Tag, Expr) ->
    ?expr({'<tag>', '<expr>'},
          [replace_term('<tag>', Tag),
           replace_tree('<expr>', Expr)]).

map_key_type_decode_expr(Type, JValueExpr, TrUserDataVar) when Type /= bool ->
    type_decode_expr(Type, JValueExpr, TrUserDataVar);
map_key_type_decode_expr(bool, JValueExpr, TrUserDataVar) ->
    %% All types that can appear as map keys are acceptable
    %% as string on JSON->internal. Except booleans, so
    %% need to make an extra pass for them
    JValueExpr2 = ?expr(case '<JValueExpr>' of
                            <<"true">> -> true;
                            <<"false">> -> false
                        end,
                        [replace_tree('<JValueExpr>', JValueExpr)]),
    type_decode_expr(bool, JValueExpr2, TrUserDataVar).

type_decode_expr(Type, JValueExpr, TrUserDataVar) ->
    case Type of
        Int32 when Int32 == sint32;
                   Int32 == int32;
                   Int32 == uint32 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        Int64 when Int64 == sint64;
                   Int64 == int64;
                   Int64 == uint64 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        bool ->
            ?expr(fj_bool('Var'),
                  [replace_tree('Var', JValueExpr)]);
        {enum,EnumName} ->
            DecodeEnumFn = gpb_lib:mk_fn(fj_enum_, EnumName),
            ?expr('fj_enum_<EName>'('Var'),
                  [replace_tree('Var', JValueExpr),
                   replace_term('fj_enum_<EName>', DecodeEnumFn)]);
        Int32 when Int32 == fixed32;
                   Int32 == sfixed32 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        Int64 when Int64 == fixed64;
                   Int64 == sfixed64 ->
            ?expr(fj_int('Var'),
                  [replace_tree('Var', JValueExpr)]);
        Float when Float == float;
                   Float == double ->
            ?expr(fj_float('Var'),
                  [replace_tree('Var', JValueExpr)]);
        string ->
            ?expr(fj_string('Var'),
                  [replace_tree('Var', JValueExpr)]);
        bytes ->
            ?expr(fj_bytes('Var'),
                  [replace_tree('Var', JValueExpr)]);
        {msg,MsgName} ->
            FnName = gpb_lib:mk_fn(from_json_msg_, MsgName),
            ?expr('from_json_msg_<MsgName>'('Var', 'TrUserData'),
                  [replace_term('from_json_msg_<MsgName>', FnName),
                   replace_tree('Var', JValueExpr),
                   replace_tree('TrUserData', TrUserDataVar)]);
        {map,_KT,_VT} ->
            JValueExpr; % handled on a higher level
        {group,GName} ->
            FnName = gpb_lib:mk_fn(from_json_msg_, GName),
            ?expr('from_json_msg_<GName>'('Var', 'TrUserData'),
                  [replace_term('from_json_msg_<GName>', FnName),
                   replace_tree('Var', JValueExpr),
                   replace_tree('TrUserData', TrUserDataVar)])
    end.

format_helpers(Defs, AnRes, Opts) ->
    [format_json_msg_iterator_helpers(Defs, Opts),
     format_json_array_helpers(Defs, AnRes, Opts),
     format_mapfield_helper(AnRes, Opts),
     format_json_type_helpers(Defs, AnRes, Opts)].

format_json_msg_iterator_helpers(Defs, Opts) ->
    HaveNonemptyMsgs = have_nonempty_msg(Defs),
    HaveMapIterators = gpb_lib:target_has_map_iterators(Opts),
    [[case gpb_lib:json_object_format_by_opts(Opts) of
          eep18 ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun([{}]) -> [];
                    (Proplist) -> Proplist
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)];
          {proplist} ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun({Proplist}) -> Proplist
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)];
          {Tag, proplist} ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun({struct, Proplist}) -> Proplist
                 end,
                 [replace_term(struct, Tag)]),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)];
          map when HaveMapIterators ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun(Map) -> maps:iterator(Map)
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun(Iter) -> maps:next(Iter)
                 end)];
          map when not HaveMapIterators ->
              [gpb_codegen:format_fn(
                 fj_iter,
                 fun(Map) -> maps:to_list(Map)
                 end),
               gpb_codegen:format_fn(
                 fj_next,
                 fun([{Key,Value} | Rest]) -> {Key, Value, Rest};
                    ([]) -> none
                 end)]
      end] || HaveNonemptyMsgs].

format_json_array_helpers(Defs, #anres{map_types=MapTypes}, Opts) ->
    HaveMapfields = sets:size(MapTypes) > 0,
    HaveRepeated = have_repeated_fields(Defs) orelse HaveMapfields,
    [[case gpb_lib:json_array_format_by_opts(Opts) of
          list ->
              [gpb_lib:nowarn_unused_function(fj_array, 1),
               gpb_codegen:format_fn(
                 fj_array,
                 fun(L) -> L end)];
          {Tag, list} ->
              [gpb_lib:nowarn_unused_function(fj_array, 1),
               gpb_codegen:format_fn(
                 fj_array,
                 fun({array, L}) -> L end,
                 [replace_term(array, Tag)])]
      end] || HaveRepeated].

format_mapfield_helper(#anres{map_types=MapTypes}, Opts) ->
    HaveMapfields = sets:size(MapTypes) > 0,
    [[gpb_codegen:format_fn(
        fj_mapfield_fold,
        fun(MapMsgName, New, AddElem, Finalize, DecodeKey, DecodeValue,
            JMapfield, TrUserData) ->
                Finalize(
                  fj_mapfield_fold_aux(fj_iter(JMapfield),
                                       MapMsgName, DecodeKey, DecodeValue,
                                       AddElem,
                                       New([], TrUserData),
                                       TrUserData),
                  TrUserData)
        end),
      case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
          '2tuples' ->
              gpb_codegen:format_fn(
                fj_mapfield_fold_aux,
                fun(JIter, MapMsgName, DecodeKey, DecodeValue,
                    AddElem, Acc, TrUserData) ->
                        case fj_next(JIter) of
                            {JKey, JValue, JRest} ->
                                EKey = DecodeKey(JKey),
                                EValue = DecodeValue(JValue),
                                TmpMsg = {MapMsgName, EKey, EValue},
                                Acc1 = AddElem(TmpMsg, Acc, TrUserData),
                                call_self(JRest,
                                          MapMsgName, DecodeKey, DecodeValue,
                                          AddElem, Acc1, TrUserData);
                            none ->
                                Acc
                        end
                end);
          maps ->
              {EKey, EValue} = {?expr(EKey), ?expr(EValue)},
              MkMapExpr = gpb_lib:map_create([{key, EKey}, {value, EValue}],
                                             Opts),
              gpb_codegen:format_fn(
                fj_mapfield_fold_aux,
                fun(JIter, MapMsgName, DecodeKey, DecodeValue,
                    AddElem, Acc, TrUserData) ->
                        case fj_next(JIter) of
                            {JKey, JValue, JRest} ->
                                EKey = DecodeKey(JKey),
                                EValue = DecodeValue(JValue),
                                TmpMsg = '#{key => EKey, value => EValue}',
                                Acc1 = AddElem(TmpMsg, Acc, TrUserData),
                                call_self(JRest,
                                          MapMsgName, DecodeKey, DecodeValue,
                                          AddElem, Acc1, TrUserData);
                            none ->
                                Acc
                        end
                end,
                [replace_tree('#{key => EKey, value => EValue}', MkMapExpr)])
      end] || HaveMapfields].

format_json_type_helpers(Defs, #anres{used_types=UsedTypes}, Opts) ->
    StrBin = gpb_lib:get_strings_as_binaries_by_opts(Opts),
    NeedIntType = lists:any(fun(T) -> gpb_lib:smember(T, UsedTypes) end,
                            [sint32, int32, uint32,
                             sint64, int64, uint64,
                             fixed32, sfixed32,
                             fixed64, sfixed64]),
    NeedBoolType = gpb_lib:smember(bool, UsedTypes),
    NeedEnumType = lists:any(fun({enum,_}) -> true;
                                (_) -> false
                             end,
                             sets:to_list(UsedTypes)),
    NeedFloatType = lists:any(fun(T) -> gpb_lib:smember(T, UsedTypes) end,
                              [float, double]),
    NeedStringType = gpb_lib:smember(string, UsedTypes),
    NeedBytesType = gpb_lib:smember(bytes, UsedTypes),
    [[gpb_codegen:format_fn(
        fj_int,
        %% Leading zeros are not allowed according to RFC7159,
        %% so no octal representation decoding or anything such is needed.
        fun(N) when is_integer(N) ->
                N;
           (S) when is_binary(S) ->
                list_to_integer(binary_to_list(S));
           (S) when is_list(S) ->
                list_to_integer(S)
        end) || NeedIntType],
     [[%% The protobuf also accepts both boolean values as well as
       %% string representation of the same. It seems to also accepts
       %% string representations of 0 and 1, but oddly enough
       %% not the integers 0 and 1.  (protobuf 3.8.0-rc1)
       gpb_codegen:format_fn(
         fj_bool,
         fun(B) when is_boolean(B) -> B;
            (B) when is_binary(B) ->
                 case fj_bool_bin_casecanon(B, <<>>) of
                     (<<"true">>)           -> true;
                     (<<"false">>)          -> false;
                     (<<"1">>)              -> true;
                     (<<"0">>)              -> false
                 end;
            (S) when is_list(S) ->
                 call_self(list_to_binary(S))
         end),
       gpb_codegen:format_fn(
         fj_bool_bin_casecanon, % to lowercase
         fun(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
                 call_self(Rest, <<Acc/binary, (C + 32)>>); % $a - $A == 32
            (<<C, Rest/binary>>, Acc) ->
                 call_self(Rest, <<Acc/binary, C>>);
            (<<>>, Acc) ->
                 Acc
         end)]
      || NeedBoolType],
     [begin
          JSymStrsToSyms = canonify_enum_jstrs(unalias_enum_syms(Enums), Opts),
          IntStrsToSyms = enum_ints_to_syms(Enums),
          [{_, Sym1} | _] = JSymStrsToSyms,
          StrSyms = JSymStrsToSyms ++ IntStrsToSyms,
          gpb_codegen:format_fn(
            gpb_lib:mk_fn(fj_enum_, EnumName),
            fun(S) when is_binary(S) ->
                    case  fj_casecanon_enum(S, <<>>) of
                        '<<"Str">>' -> '<EnumSym>';
                        _ -> 'FirstSym'
                    end;
               (N) when is_integer(N) ->
                    'd_enum_<EName>'(N);
               (S) when is_list(S) ->
                    call_self(list_to_binary(S))
            end,
            [replace_term('d_enum_<EName>', gpb_lib:mk_fn(d_enum_, EnumName)),
             replace_term('FirstSym', Sym1),
             repeat_clauses(
               '<<"Str">>',
               [[replace_tree('<<"Str">>', bstr(Str)),
                 replace_term('<EnumSym>', Sym)]
                || {Str, Sym} <- StrSyms])])
      end
      || {{enum,EnumName}, Enums} <- Defs,
         NeedEnumType],
     [%% Extra enum helper(s)
      case proplists:get_bool(json_case_insensitive_enum_parsing, Opts) of
          true ->
              [gpb_codegen:format_fn(
                 fj_casecanon_enum,
                 fun(<<C, Tl/binary>>, Acc) ->
                         call_self(Tl, <<Acc/binary, (fj_casecanon_char(C))>>);
                    (<<>>, Acc) ->
                         Acc
                 end),
               gpb_codegen:format_fn(
                 fj_casecanon_char, % to uppercase
                 fun(C) when $a =< C, C =< $z -> C - 32; % $a - $A == 32
                    ($-) -> $_;
                    (C) -> C
                 end)];
          false ->
              [gpb_codegen:format_fn(
                 fj_casecanon_enum,
                 fun(B, _Acc) ->
                         B
                 end)]
      end
      || NeedEnumType],
     [[%% float with helper
       gpb_codegen:format_fn(
         fj_float,
         fun(N) when is_integer(N) -> float(N);
            (N) when is_float(N) -> N;
            (S) when is_binary(S) -> call_self(binary_to_list(S));
            ("NaN") -> nan;
            ("Infinity") -> infinity;
            ("-Infinity") -> '-infinity';
            (S) when is_list(S) ->
                 case fj_d_num2e(S, integer, "") of
                     {integer, S2} -> float(list_to_integer(S2));
                     {float, S2}   -> list_to_float(S2)
                 end
         end),
       gpb_codegen:format_fn(
         fj_d_num2e,
         fun("-"++Rest, St, Acc) ->
                 call_self(Rest, St, "-" ++ Acc);
            ("+"++Rest, St, ""=Acc) ->
                 %% Ignore leading plus
                 call_self(Rest, St, Acc);
            ([D | Rest], St, Acc) when $0 =< D, D =< $9 ->
                 call_self(Rest, St, [D | Acc]);
            ("."++_ = S, _, Acc) when Acc =:= ""; Acc =:= "-" ->
                 %% Initial leading decimal point: prepend a leading zero
                 %% and continue processing
                 call_self(S, float, "0" ++ Acc);
            ("."++[D|_] = S, _, Acc) when $0 =< D, D =< $9 ->
                 %% A decimal point: the rest (if wellformed as per rfc 7159)
                 %% will be parseable as an erlang float
                 {float, lists:reverse(Acc, S)};
            ("."++Rest, _, Acc) ->
                 %% A decimal point followed by non-digit (exponent or
                 %% or end  of string): Turn "." into ".0" to make
                 %% it parseable as an Erlang float
                 {float, lists:reverse(Acc, ".0" ++ Rest)};
            ([E | _]=Rest, integer, Acc) when E == $e; E == $E ->
                 %% Exponent: not preceded by a decimal point:
                 %% add ".0" before the exponent, and it will (if wellformed)
                 %% be parseable as an erlang float
                 {float, lists:reverse(Acc, ".0"++Rest)};
            ("", St, Acc) ->
                 {St, lists:reverse(Acc)}
         end)] || NeedFloatType],
     [[%% String
       case StrBin of
           true ->
               gpb_codegen:format_fn(
                 fj_string,
                 fun(S) when is_binary(S); is_list(S) ->
                         unicode:characters_to_binary(S)
                 end);
           false ->
               gpb_codegen:format_fn(
                 fj_string,
                 fun(S) when is_binary(S); is_list(S) ->
                         unicode:characters_to_list(S)
                 end)
       end || NeedStringType]],
     [gpb_codegen:format_fn(
        fj_bytes,
        fun(S) when is_binary(S); is_list(S) ->
                %% Convert any url-safe encoding to normal base64 encoding
                B64 = <<<<if C =:= $- -> $+;
                             C =:= $_ -> $/;
                             true -> C
                          end>>
                        || <<C>> <= iolist_to_binary(S)>>,
                base64:decode(B64)
        end) || NeedBytesType]].


occurrence_or_mapfield(repeated, {map, _, _}) -> mapfield;
occurrence_or_mapfield(Occurrence, _)         -> Occurrence.

have_nonempty_msg([{{msg,_MsgName}, Fields} | Rest]) ->
    if Fields /= [] -> true;
       Fields == [] -> have_nonempty_msg(Rest)
    end;
have_nonempty_msg([_ | Rest]) ->
    have_nonempty_msg(Rest);
have_nonempty_msg([]) ->
    false.

have_repeated_fields([{{msg,_Msg}, Fields} | Rest]) ->
    case have_repeated_aux(Fields) of
        true  -> true;
        false -> have_repeated_fields(Rest)
    end;
have_repeated_fields([_ | Rest]) ->
    have_repeated_fields(Rest);
have_repeated_fields([]) ->
    false.

have_repeated_aux([#?gpb_field{occurrence = repeated} | _]) ->
    true;
have_repeated_aux([_ | Rest]) ->
    have_repeated_aux(Rest);
have_repeated_aux([]) ->
    false.

bstr(S) when is_list(S) ->
    %% Want <<"str">> and not <<102,110,97,109,101>>
    %% so make a text node
    erl_syntax:text(?ff("<<~p>>", [S])).

canonify_enum_jstrs(JStrsToSyms, _Opts) ->
    [{gpb_lib:uppercase(JStr), Sym} || {JStr, Sym} <- JStrsToSyms].

unalias_enum_syms(Enums) ->
    %% Enum can also have {option, allow_alias, true} elements.
    Enums1 = [Enum || {_Sym,_Num}=Enum <- Enums],
    %% In case of aliases: make a mapping:
    %%   If .proto is:           Then resulting mapping is:
    %%   enum E { E_0 = 0;       [{"E_0", 'E_0'},
    %%            E_1a = 1;       {"E_1a", 'E_1a'},
    %%            E_1b = 1; }     {"E_1b", 'E_1a'}]
    [{atom_to_list(Sym), ensure_sym_unaliased(Sym, Enums1)}
     || {Sym, _Num} <- Enums1].

ensure_sym_unaliased(Sym, Enums) ->
    {Sym, Num} = lists:keyfind(Sym, 1, Enums),
    {Sym1, Num} = lists:keyfind(Num, 2, Enums),
    Sym1.

enum_ints_to_syms(Enums) ->
    [{integer_to_list(Num), Sym} || {Sym,Num} <- gpb_lib:unalias_enum(Enums)].
