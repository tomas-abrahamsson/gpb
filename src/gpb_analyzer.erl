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

%%% @doc Analyzes proto defs. Result is used by the generator modules
%%% and functions.
%%% @private

-module(gpb_analyzer).

-export([analyze_defs/2]).

-include("../include/gpb.hrl").
-include("gpb_compile.hrl").

%% -- analysis -----------------------------------------------------

analyze_defs(Defs, Opts) ->
    MapTypes = find_map_types(Defs),
    MapsAsMsgs = map_types_to_msgs(MapTypes),
    MapMsgEnums = enums_for_maps_as_msgs(MapTypes, Defs),
    Translations = compute_translations(Defs, Opts),
    KnownMsgSize = find_msgsizes_known_at_compile_time(MapsAsMsgs ++ Defs),
    #anres{used_types          = find_used_types(Defs),
           known_msg_size      = KnownMsgSize,
           fixlen_types        = find_fixlen_types(MapsAsMsgs ++ Defs),
           num_packed_fields   = find_num_packed_fields(MapsAsMsgs ++ Defs),
           num_fields          = find_num_fields(MapsAsMsgs ++ Defs),
           d_field_pass_method = compute_decode_field_pass_methods(
                                   MapsAsMsgs ++ Defs, Opts),
           maps_as_msgs        = MapsAsMsgs ++ MapMsgEnums,
           translations        = Translations,
           default_transls     = compute_used_default_translators(
                                   Defs, Translations, KnownMsgSize, Opts),
           map_types           = MapTypes,
           map_value_types     = compute_map_value_types(MapTypes),
           group_occurrences   = find_group_occurrences(Defs),
           has_p3_opt_strings  = has_p3_opt_strings(Defs)}.

find_map_types(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, _MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              sets:add_element({KeyType,ValueType}, Acc);
         (_, _MsgName, _Field, Acc) ->
              Acc
      end,
      sets:new(),
      Defs).

map_types_to_msgs(MapTypes) ->
    sets:fold(fun({KeyType, ValueType}, Acc) ->
                      [{{msg, gpb_lib:map_type_to_msg_name(KeyType,ValueType)},
                        gpb:map_item_pseudo_fields(KeyType, ValueType)} | Acc]
              end,
              [],
              MapTypes).

enums_for_maps_as_msgs(MapTypes, Defs) ->
    MapEnumNames = sets:fold(fun({_Key, {enum, EName}}, Acc) -> [EName | Acc];
                                ({_KeyType, _ValueType}, Acc) -> Acc
                             end,
                             [],
                             MapTypes),
    [Enum || {{enum, EnumName}, _}=Enum <- Defs,
             lists:member(EnumName, MapEnumNames)].

compute_map_value_types(MapTypes) ->
    sets:fold(
      fun({_KT, {msg,_}}, {_SubMsgs, NonSubMsgs})  -> {true, NonSubMsgs};
         ({_KT, _VT},     {SubMsgs,  _NonSubMsgs}) -> {SubMsgs, true}
      end,
      {false, false},
      MapTypes).

find_used_types(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_Type, _MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              Acc1 = sets:add_element(KeyType, Acc),
              sets:add_element(ValueType, Acc1);
         (_Type, _MsgName, #?gpb_field{type=Type}, Acc) ->
              sets:add_element(Type, Acc)
      end,
      sets:new(),
      Defs).

find_fixlen_types(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, _, #?gpb_field{type=Type, occurrence=Occ}=FieldDef, Acc) ->
              IsPacked = gpb_lib:is_packed(FieldDef),
              FixlenTypeInfo = #ft{type       = Type,
                                   occurrence = Occ,
                                   is_packed  = IsPacked},
              case Type of
                  fixed32  -> sets:add_element(FixlenTypeInfo, Acc);
                  sfixed32 -> sets:add_element(FixlenTypeInfo, Acc);
                  float    -> sets:add_element(FixlenTypeInfo, Acc);
                  fixed64  -> sets:add_element(FixlenTypeInfo, Acc);
                  sfixed64 -> sets:add_element(FixlenTypeInfo, Acc);
                  double   -> sets:add_element(FixlenTypeInfo, Acc);
                  _        -> Acc
              end
      end,
      sets:new(),
      Defs).

find_num_packed_fields(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, _MsgName, FieldDef, Acc) ->
              case gpb_lib:is_packed(FieldDef) of
                  true  -> Acc + 1;
                  false -> Acc
              end
      end,
      0,
      Defs).

find_num_fields(Defs) ->
    lists:foldl(fun({_msg_or_group, MsgName, MsgDef}, Acc) ->
                        dict:store(MsgName, length(MsgDef), Acc)
                end,
                dict:new(),
                gpb_lib:msgs_or_groups(Defs)).

find_msgsizes_known_at_compile_time(Defs) ->
    T = ets:new(gpb_msg_sizes, [set, public]),
    [find_msgsize(MsgName, Defs, T) || {{msg,MsgName},_Fields} <- Defs],
    Result = dict:from_list(ets:tab2list(T)),
    ets:delete(T),
    Result.

find_msgsize(MsgName, Defs, T) ->
    case ets:lookup(T, MsgName) of
        [] ->
            {{msg,MsgName}, Fields} = lists:keyfind({msg,MsgName}, 1, Defs),
            Result = find_msgsize_2(Fields, 0, Defs, T),
            ets:insert(T, {MsgName, Result}),
            Result;
        [{MsgName, Result}] ->
            Result
    end.

find_groupsize(GroupName, Defs, T) ->
    {{group,GroupName}, Fields} = lists:keyfind({group,GroupName}, 1, Defs),
    find_msgsize_2(Fields, 0, Defs, T).

find_msgsize_2([#gpb_oneof{} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=repeated} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=optional} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{type=Type, fnum=FNum} | Rest], AccSize, Defs, T) ->
    FKeySize =
        case Type of
            {group, _} ->
                not_applicable;
            _ ->
                FKey = (FNum bsl 3) bor gpb:encode_wiretype(Type),
                byte_size(gpb:encode_varint(FKey))
        end,
    case Type of
        sint32   -> undefined;
        sint64   -> undefined;
        int32    -> undefined;
        int64    -> undefined;
        uint32   -> undefined;
        uint64   -> undefined;
        bool     -> find_msgsize_2(Rest, AccSize+FKeySize+1, Defs, T);
        {enum,EnumName} ->
            case all_enum_values_encode_to_same_size(EnumName, Defs) of
                {yes, ESize} ->
                    find_msgsize_2(Rest, AccSize+FKeySize+ESize, Defs, T);
                no ->
                    undefined
            end;
        fixed64  -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        sfixed64 -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        double   -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        string   -> undefined;
        bytes    -> undefined;
        {msg,MsgName} ->
            case find_msgsize(MsgName, Defs, T) of
                MsgSize when is_integer(MsgSize) ->
                    SizeOfLength = byte_size(gpb:encode_varint(MsgSize)),
                    SubMsgFieldSize = FKeySize + SizeOfLength + MsgSize,
                    find_msgsize_2(Rest, AccSize + SubMsgFieldSize, Defs, T);
                undefined ->
                    undefined
            end;
        {group,GroupName} ->
            case find_groupsize(GroupName, Defs, T) of
                GroupSize when is_integer(GroupSize) ->
                    StartTag = (FNum bsl 3) + gpb:encode_wiretype(group_start),
                    EndTag   = (FNum bsl 3) + gpb:encode_wiretype(group_end),
                    SizeOfStartTag = byte_size(gpb:encode_varint(StartTag)),
                    SizeOfEndTag = byte_size(gpb:encode_varint(EndTag)),
                    GroupFieldSize = SizeOfStartTag + GroupSize + SizeOfEndTag,
                    find_msgsize_2(Rest, AccSize + GroupFieldSize, Defs, T);
                undefined ->
                    undefined
            end;
        fixed32  -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T);
        sfixed32 -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T);
        float    -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T)
    end;
find_msgsize_2([], AccSize, _Defs, _T) ->
    AccSize.


all_enum_values_encode_to_same_size(EnumName, Defs) ->
    {{enum,EnumName}, EnumDef} = lists:keyfind({enum,EnumName}, 1, Defs),
    EnumSizes = [begin
                     <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                     byte_size(gpb:encode_varint(N))
                 end
                 || {_EnumSym, Value} <- EnumDef],
    case lists:usort(EnumSizes) of
        [Size] -> {yes, Size};
        _      -> no
    end.

compute_decode_field_pass_methods(Defs, Opts) ->
    lists:foldl(fun({_Type, Name, Fields}, D) ->
                        PassHow = d_field_pass_method(Name, Fields, Opts),
                        %% FIXME:GROUP: are all group+msg names unique?
                        dict:store(Name, PassHow, D)
                end,
                dict:new(),
                gpb_lib:msgs_or_groups(Defs)).

d_field_pass_method(MsgName, MsgDef, Opts) ->
    %% Allow overriding options, mainly intended for testing
    case proplists:get_value({field_pass_method,MsgName}, Opts) of
        undefined ->
            case proplists:get_value(field_pass_method, Opts) of
                undefined ->
                    d_field_pass_method(MsgDef);
                Method when Method==pass_as_record; Method==pass_as_params ->
                    Method
            end;
        Method when Method==pass_as_record; Method==pass_as_params ->
            Method
    end.

d_field_pass_method(MsgDef) ->
    %% Compute estimated costs:
    %% Either passing a message record, or pass the fields as parameters
    %% to the functions, one parameter for each field, then as the last
    %% operation, stuff all parameters into a record.
    %%
    %% There are different advantages and disadvantages:
    %% - Updating fields in a record means the vm will have to verify
    %%   that the term is a record (for each time a field is parsed/added)
    %% - Passing the fields eliminates the cost above, but for each
    %%   (non-tail-recursive) function call, the field-parameters will
    %%   be saved to the stack, then restored after the call.
    %%   Such function calls, are: call to unicode:characters_to_list
    %%   for strings, calls to parse sub messages or packed fields and
    %%   final top-level calls to lists:reverse for repeated fields.
    NF = length(MsgDef), %% num fields (awk-istic terminology)
    if NF >= 250 ->
            pass_as_record; %% Functions can take at most 255 arguments
       NF == 0 ->
            pass_as_params;
       true ->
            NumSubMsgFields = count_submsg_fields(MsgDef),
            NumMapFields = count_map_fields(MsgDef),
            NumGroupFields = count_group_fields(MsgDef),
            IsMsgDominatedBySubMsgsOrMaps =
                (NumSubMsgFields + NumMapFields + NumGroupFields) / NF > 0.5,
            if IsMsgDominatedBySubMsgsOrMaps, NF >= 100 ->
                    pass_as_record;
               true ->
                    pass_as_params
            end
    end.

count_submsg_fields(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{type={msg,_}}, N) -> N+1;
         (#?gpb_field{}, N)             -> N
      end,
      0,
      MsgDef).

count_map_fields(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{type={map,_,_}}, N) -> N+1;
         (#?gpb_field{}, N)               -> N
      end,
      0,
      MsgDef).

count_group_fields(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{type={group,_}}, N) -> N+1;
         (#?gpb_field{}, N)               -> N
      end,
      0,
      MsgDef).

compute_translations(Defs, Opts) ->
    remove_empty_translations(
      remove_merge_translations_for_repeated_elements(
        lists:foldl(
          fun({Name, Dict}, D) ->
                  %% For now it is an (internal) error if translations overlap,
                  %% (don't expect that to happen with current translations)
                  %% but in the future (eg with user-specified translations)
                  %% they might stack instead: ie Ts1 ++ Ts2 instead of error.
                  dict:merge(
                    fun(Key, Ts1, Ts2) ->
                            error({error,{duplicate_translation,
                                          {when_adding_transls_for,Name},
                                          {key,Key},
                                          {translations,Ts1,Ts2}}})
                    end,
                    Dict, D)
          end,
          dict:new(),
          [{map_translations, compute_map_translations(Defs, Opts)},
           {any_translations, compute_any_translations(Defs, Opts)}]))).

remove_merge_translations_for_repeated_elements(D) ->
    dict:map(fun(Key, Ops) ->
                     case is_repeated_element_path(Key) of
                         true -> lists:keydelete(merge, 1, Ops);
                         false -> Ops
                     end
             end,
             D).

is_repeated_element_path([_, _, []]) -> true;
is_repeated_element_path(_) -> false.

remove_empty_translations(D) ->
    dict:filter(fun(_Key, Ops) -> Ops /= [] end, D).

compute_map_translations(Defs, Opts) ->
    MapInfos =
        gpb_lib:fold_msg_fields(
          fun(MsgName, #?gpb_field{name=FName, type={map,KType,VType}}, Acc) ->
                  [{{MsgName, FName}, {KType, VType}} | Acc];
             (_MsgName, _Field, Acc) ->
                  Acc
          end,
          [],
          Defs),
    MapFieldFmt = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts),
    dict:from_list(
      lists:append(
        [mk_map_transls(MsgName, FName, KeyType, ValueType, MapFieldFmt)
         || {{MsgName, FName}, {KeyType, ValueType}} <- MapInfos])).

mk_map_transls(MsgName, FName, KeyType, ValueType, '2tuples')->
    MapAsMsgName = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
    AddItemTrFn = case ValueType of
                    {msg,_} -> mt_add_item_r_verify_value;
                    _       -> mt_add_item_r
                  end,
    [{[MsgName,FName,[]],
      [{encode, {mt_maptuple_to_pseudomsg_r, ['$1', MapAsMsgName]}}]},
     {[MsgName,FName],
      [{decode_init_default,      {mt_empty_map_r,       []}},
       {decode_repeated_add_elem, {AddItemTrFn,          ['$1', '$2']}},
       {decode_repeated_finalize, {mt_finalize_items_r,  ['$1']}},
       {merge,                    {mt_merge_maptuples_r, ['$1', '$2']}}]}];
mk_map_transls(MsgName, FName, _KeyType, ValueType, maps)->
    AddItemTrFn = case ValueType of
                    {msg,_} -> mt_add_item_m_verify_value;
                    _       -> mt_add_item_m
                  end,
    [{[MsgName,FName,[]],
      [{encode,                   {mt_maptuple_to_pseudomsg_m, ['$1']}}]},
     {[MsgName,FName],
      [{encode,                   {mt_map_to_list_m, ['$1']}},
       {decode_init_default,      {mt_empty_map_m,   []}},
       {decode_repeated_add_elem, {AddItemTrFn,      ['$1', '$2']}},
       {decode_repeated_finalize, {id,               ['$1', '$user_data']}},
       {merge,                    {mt_merge_maps_m,  ['$1', '$2']}}]}].

compute_any_translations(Defs, Opts) ->
    case proplists:get_value(any_translate,Opts) of
        undefined ->
            dict:new();
        AnyTranslations ->
            compute_any_translations_2(Defs, AnyTranslations)
    end.

compute_any_translations_2(Defs, AnyTranslations) ->
    P3AnyInfos =
        gpb_lib:fold_msg_or_group_fields_o(
          fun(_Type,
              MsgName, #?gpb_field{name=FName, type={msg,Any}, occurrence=Occ},
              Oneof,
              Acc) when Any == 'google.protobuf.Any' ->
                  Path = case {Oneof, Occ} of
                             {false, repeated}  -> [MsgName,FName,[]];
                             {false, _}         -> [MsgName,FName];
                             {{true,CFName}, _} -> [MsgName,CFName,FName]
                         end,
                  [Path | Acc];
             (_Type,
              _MsgName, #?gpb_field{type={map,KeyType,{msg,Any}=ValueType}},
              _Oneof,
              Acc) when Any == 'google.protobuf.Any' ->
                  MsgAsMapName = gpb_lib:map_type_to_msg_name(
                                   KeyType, ValueType),
                  Path = [MsgAsMapName,value],
                  [Path | Acc];
             (_Type, _MsgName, _Field, _Oneof, Acc) ->
                  Acc
          end,
          [],
          Defs),
    Encode = {encode, fetch_any_translation(encode, AnyTranslations)},
    Decode = {decode, fetch_any_translation(decode, AnyTranslations)},
    Merge  = {merge,  fetch_any_translation(
                        merge,  AnyTranslations,
                        gpb_gen_translators:default_any_merge_translator())},
    Verify = {verify, fetch_any_translation(
                        verify, AnyTranslations,
                        gpb_gen_translators:default_any_verify_translator())},
    dict:from_list(
      [{Path, ([Encode,Decode,Verify]
               ++ [Merge || not is_repeated_elem_path(Path)])}
       || Path <- P3AnyInfos]).

fetch_any_translation(Op, Translations) ->
    fetch_any_translation(Op, Translations, undefined).
fetch_any_translation(Op, Translations, Default) ->
    case proplists:get_value(Op, Translations, Default) of
        undefined ->
            error({error, {missing_any_translation, {op,Op}, Translations}});
        {M,F,ArgTempl} ->
            {M,F,ArgTempl};
        {F,ArgTempl} ->
            {F,ArgTempl}
    end.

is_repeated_elem_path([_MsgName,_FName,[]]) -> true;
is_repeated_elem_path(_) -> false.

compute_used_default_translators(Defs, Translations, KnownMsgSize, Opts) ->
    fold_fields_and_paths(
      fun(Field, Path, _IsOneOf, Acc) ->
              Calls = get_translations(Field,Path, Translations,
                                       KnownMsgSize, Opts),
              lists:foldl(
                fun({FnName,ArgsTmpl}, A) when is_list(ArgsTmpl) ->
                        Arity = length(ArgsTmpl),
                        sets:add_element({FnName, Arity}, A);
                   ({FnName,Arity}, A) when is_integer(Arity) ->
                        sets:add_element({FnName, Arity}, A);
                   (_, A) -> % remote call (ie: to other module)
                        A
                end,
                Acc,
                Calls)
      end,
      sets:new(),
      Defs).

get_translations(#gpb_oneof{}, _Path, _Translations, _KnownMsgSize, _Opts) ->
    [];
get_translations(#?gpb_field{type=Type, occurrence=Occ},
                 Path, Translations, KnownMsgSize, Opts) ->
    {IsRepeated, IsKnownSizeElem} =
        if Occ == repeated ->
                {true, is_known_size_element(Type, KnownMsgSize)};
           true ->
                {false, false}
        end,
    IsElem = IsRepeated andalso lists:last(Path) == [],
    DoNif = proplists:get_bool(nif, Opts),
    Ops = if DoNif ->
                  [merge, verify];
             IsElem ->
                  [encode,decode,merge,verify];
             IsRepeated, IsKnownSizeElem ->
                  [encode,
                   decode_repeated_add_elem,
                   decode_repeated_finalize,
                   merge,
                   verify];
             IsRepeated, not IsKnownSizeElem ->
                  [encode,
                   decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize,
                   merge,
                   verify];
             true ->
                  [encode,decode,merge,verify]
          end,
    PathTransls = case dict:find(Path, Translations) of
                      {ok, Ts} -> Ts;
                      error    -> []
                  end,
    [case lists:keyfind(Op, 1, PathTransls) of
         {Op, Transl} ->
             Transl;
         false ->
             if Op == merge, IsRepeated, not IsElem ->
                     {'erlang_++',3};
                true ->
                     FnName = gpb_gen_translators:default_fn_by_op(
                                Op, undefined),
                     Arity = length(gpb_gen_translators:args_by_op2(Op)) + 1,
                     {FnName, Arity}
             end
     end
     || Op <- Ops].

is_known_size_element(fixed32, _) -> true;
is_known_size_element(fixed64, _) -> true;
is_known_size_element(sfixed32, _) -> true;
is_known_size_element(sfixed64, _) -> true;
is_known_size_element(float, _) -> true;
is_known_size_element(double, _) -> true;
is_known_size_element({msg,MsgName}, KnownMsgSize) ->
    dict:find(MsgName, KnownMsgSize) /= error;
is_known_size_element({group,Name}, KnownMsgSize) ->
    dict:find(Name, KnownMsgSize) /= error;
is_known_size_element({map,KeyType,ValueType}, KnownMsgSize) ->
    MapAsMsgName = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
    dict:find(MapAsMsgName, KnownMsgSize) /= error;
is_known_size_element(_Type, _) ->
    false.

fold_fields_and_paths(F, InitAcc, Defs) ->
    lists:foldl(
      fun({{msg, MsgName}, Fields}, Acc) ->
              fold_field_and_path(F, [MsgName], false, Acc, Fields);
         ({{group, GroupName}, Fields}, Acc) ->
              fold_field_and_path(F, [GroupName], false, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).

fold_field_and_path(F, Root, IsOneOf, InitAcc, Fields) ->
    lists:foldl(
      fun(#?gpb_field{name=FName, occurrence=repeated}=Field, Acc) ->
              Path = Root ++ [FName],
              EPath = Root ++ [FName, []],
              F(Field, EPath, IsOneOf, F(Field, Path, IsOneOf, Acc));
         (#?gpb_field{name=FName}=Field, Acc) ->
              Path = Root ++ [FName],
              F(Field, Path, IsOneOf, Acc);
         (#gpb_oneof{name=CFName, fields=OFields}=Field, Acc) ->
              Path = Root ++ [CFName],
              fold_field_and_path(F, Path, {true, CFName},
                                  F(Field, Path, IsOneOf, Acc),
                                  OFields)
      end,
      InitAcc,
      Fields).

find_group_occurrences(Defs) ->
    gpb_lib:fold_msg_or_group_fields_o(
      fun(_msg_or_group, _MsgName,
          #?gpb_field{type={group,GroupName}, occurrence=Occurrence},
          _IsOnoeof, D)->
              dict:store(GroupName, Occurrence, D);
         (_msg_or_group, _MsgName, _Field, _IsOnoeof, D) ->
              D
      end,
      dict:new(),
      Defs).

has_p3_opt_strings(Defs) ->
    P3Msgs = case lists:keyfind(proto3_msgs, 1, Defs) of
                 {proto3_msgs, Names} -> Names;
                 false                -> []
             end,
    try gpb_lib:fold_msg_or_group_fields_o(
          fun(_msg_or_group, MsgName, #?gpb_field{type=Type,occurrence=Occ},
              _IsOneOf, Acc) ->
                  if Type == string, Occ == optional ->
                          case lists:member(MsgName, P3Msgs) of
                              true -> throw(true);
                              false -> Acc
                          end;
                     true ->
                          Acc
                  end
          end,
          false,
          Defs)
    catch throw:true ->
            true
    end.
