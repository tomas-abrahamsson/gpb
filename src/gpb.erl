%%% Copyright (C) 2010-2013  Tomas Abrahamsson
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

-module(gpb).
%-compile(export_all).
-export([decode_msg/3]).
-export([encode_msg/2]).
-export([merge_msgs/3]).
-export([verify_msg/2, check_scalar/2]).
-export([map_item_pseudo_fields/2]).
-export([is_allowed_as_key_type/1]).
-export([is_msg_proto3/2, proto3_type_default/2]).
-export([encode_varint/1, decode_varint/1, decode_varint/2]).
-export([encode_wiretype/1, decode_wiretype/1]).
-export([version_as_string/0, version_as_list/0]).
-export([field_records_to_proplists/1, proplists_to_field_records/1]).
-export([field_record_to_proplist/1,   proplist_to_field_record/1]).
-export([defs_records_to_proplists/1,  proplists_to_defs_records/1]).
-export([rpc_records_to_proplists/1, rpc_record_to_proplist/1, proplists_to_rpc_records/1]).
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("../include/gpb_version.hrl").

%% TODO
%%
%% * Add a new_default_msg that sets default values according to
%%   type (and optionalness) as documented on the google web:
%%   strings="", booleans=false, integers=0, enums=<first value> and so on.
%%   Maybe only for required fields?
%%
%%   Message fields can also have default values specified in the .proto file.
%%
%%   Records with default values could fit nicely here.
%%
%% * Verify type-mismatches spec<-->actual-wire-contents? (optionally?)
%%
%% * Crash or silent truncation on values out of range when encoding?
%%   Example: (1 bsl 33) for an uint32? The bit-syntax silently truncates,
%%   but this has been under debate on the erlang mailing list as it
%%   was unexpected. Related: principle of least astonishment.


%% Valid version format is:
%%      <n>.<m>            % e.g. 2.1, 2.1.1, etc (any number of dots and ints)
%%      <n>.<m>-<o>-<text> % e.g. 2.1-53-gb996fbe means: a commit after 2.1
%%
%% The format is what `git describe --always --tags' produces,
%% given that all tags are always on the format <n>.<m>.
version_as_string() ->
    S = ?gpb_version,
    assert_version_format(S),
    S.

%% The version_as_list is better if you want to be able to compare
%% versions, for instance to see if one version is larger/later than
%% another.
%%
%% For the case of non-tagged versions, this scheme often works, but
%% is a bit of a kluge, since in erlang, numbers are the smallest of
%% types, and a string such as "gb996fbe" cannot (and generally should
%% not) be converted to a number. So for non-tagged versions, one
%% should only check whether they are or are not equal, not whether
%% one is larger/later or smaller/earlier than another.
%%
%% This will return for example:
%%    "2.1"             -> [2,1]
%%    "2.1-53-gb996fbe" -> [2,1,0,0,53,"gb996fbe"]
%%    "2.1.1"           -> [2,1,1]
%%    "2.2"             -> [2,2]
%%
%% (Lists are better than tuples when doing comparisons.  For tuples
%% this holds: {2,2} < {2,1,1}, since tuples are first compared by
%% size then element by element for tuples of the same size. For
%% lists, it holds instead that: [2,1,1] < [2,2].)
version_as_list() ->
    version_as_list(version_as_string()).

version_as_list(S) ->
    v2l(S, "").

assert_version_format(S) ->
    case analyse_vsn_format(S) of
        git  -> ok;
        text -> erlang:error({invalid_version_format,S})
    end.

-define(is_digit(C), $0 =< C, C =< $9).

analyse_vsn_format(S) ->
    case catch analyse_vsn_1(S) of
        git -> git;
        _X  -> text
    end.

analyse_vsn_1([C|T]) when ?is_digit(C) -> analyse_vsn_2(T). % must begin with 0-9

analyse_vsn_2([C|T]) when ?is_digit(C) -> analyse_vsn_2(T);
analyse_vsn_2("."++T)                  -> analyse_vsn_3(T);
analyse_vsn_2("-"++T)                  -> analyse_vsn_4(T);
analyse_vsn_2("")                      -> git.

analyse_vsn_3([C|T]) when ?is_digit(C) -> analyse_vsn_2(T). % 0-9 must follow .

analyse_vsn_4([C|T]) when ?is_digit(C) -> analyse_vsn_5(T). % 0-9 must follow -

analyse_vsn_5([C|T]) when ?is_digit(C) -> analyse_vsn_5(T);
analyse_vsn_5("-"++[_|_])              -> git. % at least one char after -

v2l([C|T], Acc) when ?is_digit(C) -> v2l(T, [C|Acc]);
v2l("."++T, Acc)                  -> [v_acc_to_int(Acc) | v2l(T, "")];
v2l("", Acc)                      -> [v_acc_to_int(Acc)];
v2l("-"++T, Acc)                  -> [v_acc_to_int(Acc), 0, 0 | v2l2(T, "")].

v2l2([C|Tl], Acc) when ?is_digit(C) -> v2l2(Tl, [C|Acc]);
v2l2("-"++T, Acc)                   -> [v_acc_to_int(Acc), T].

v_acc_to_int(Acc) ->
    list_to_integer(lists:reverse(Acc)).

decode_msg(Bin, MsgName, MsgDefs) ->
    MsgKey = {msg,MsgName},
    Msg    = new_initial_msg(MsgKey, MsgDefs),
    MsgDef = keyfetch(MsgKey, MsgDefs),
    decode_field(Bin, MsgDef, MsgDefs, Msg).

new_initial_msg({msg,MsgName}=MsgKey, MsgDefs) ->
    MsgDef = keyfetch(MsgKey, MsgDefs),
    IsProto3 = is_msg_proto3(MsgName, MsgDefs),
    lists:foldl(fun(#?gpb_field{rnum=RNum, occurrence=repeated}, Record) ->
                        setelement(RNum, Record, []);
                   (#?gpb_field{type={msg,_Name}, occurrence=optional}, Record)->
                        Record;
                   (#?gpb_field{rnum=RNum, type={msg,_Name}=FMsgKey}, Record) ->
                        if not IsProto3 ->
                                SubMsg = new_initial_msg(FMsgKey, MsgDefs),
                                setelement(RNum, Record, SubMsg);
                           IsProto3 ->
                                Record
                        end;
                   (#?gpb_field{type=Type, occurrence=required, rnum=RNum},
                    Record) when IsProto3 ->
                        Default = proto3_type_default(Type, MsgDefs),
                        setelement(RNum, Record, Default);
                   (#?gpb_field{}, Record) ->
                        Record;
                   (#gpb_oneof{}, Record) ->
                        Record
                end,
                erlang:make_tuple(length(MsgDef)+1, undefined, [{1,MsgName}]),
                MsgDef).

decode_field(Bin, MsgDef, MsgDefs, Msg) when byte_size(Bin) > 0 ->
    {Key, Rest} = decode_varint(Bin, 32),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    case find_field(FieldNum, MsgDef) of
        false ->
            Rest2 = skip_field(Rest, WireType),
            decode_field(Rest2, MsgDef, MsgDefs, Msg);
        {#?gpb_field{type=FieldType, rnum=RNum}=FieldDef, IsOneof} ->
            case fielddef_matches_wiretype_get_packed(WireType, FieldDef) of
                {yes,true} ->
                    AccSeq = element(RNum, Msg),
                    {NewSeq, Rest2} = decode_packed(FieldType, Rest, MsgDefs,
                                                   AccSeq),
                    NewMsg = setelement(RNum, Msg, NewSeq),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                {yes,false} ->
                    {NewValue, Rest2} = decode_type(FieldType, Rest, MsgDefs),
                    NewMsg = add_field(NewValue, FieldDef, IsOneof, MsgDefs,
                                       Msg),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                no ->
                    Rest2 = skip_field(Rest, WireType),
                    decode_field(Rest2, MsgDef, MsgDefs, Msg)
            end
    end;
decode_field(<<>>, MsgDef, _MsgDefs, Record0) ->
    %% Reverse any repeated fields, but only on the top-level, not recursively.
    RepeatedRNums = [N || #?gpb_field{rnum=N, occurrence=repeated} <- MsgDef],
    lists:foldl(fun(RNum, Record) ->
                        OldValue = element(RNum, Record),
                        ReversedField = lists:reverse(OldValue),
                        setelement(RNum, Record, ReversedField)
                end,
                Record0,
                RepeatedRNums).

find_field(N, [#?gpb_field{fnum=N}=F | _]) ->
    {F, false};
find_field(N, [#?gpb_field{} | Rest]) ->
    find_field(N, Rest);
find_field(N, [#gpb_oneof{fields=Fs} | Rest]) ->
    case lists:keyfind(N, #?gpb_field.fnum, Fs) of
        #?gpb_field{}=F -> {F, true};
        false           -> find_field(N, Rest)
    end;
find_field(_, []) ->
    false.

fielddef_matches_wiretype_get_packed(WireType, #?gpb_field{type=Type}=FieldDef)->
    IsPacked = is_packed(FieldDef),
    ExpectedWireType = if IsPacked     -> encode_wiretype(bytes);
                          not IsPacked -> encode_wiretype(Type)
                       end,
    if WireType == ExpectedWireType -> {yes, IsPacked};
       WireType /= ExpectedWireType -> no
    end.

decode_wiretype(0) -> varint;
decode_wiretype(1) -> bits64;
decode_wiretype(2) -> length_delimited;
decode_wiretype(5) -> bits32.

skip_field(Bin, WireType) ->
    case decode_wiretype(WireType) of
        varint ->
            {_N, Rest} = decode_varint(Bin, 64),
            Rest;
        bits64 ->
            <<_:64, Rest/binary>> = Bin,
            Rest;
        length_delimited ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<_:Len/binary, Rest2/binary>> = Rest,
            Rest2;
        bits32 ->
            <<_:32, Rest/binary>> = Bin,
            Rest
    end.

decode_packed(FieldType, Bin, MsgDefs, Seq0) ->
    {Len, Rest} = decode_varint(Bin, 64),
    <<Bytes:Len/binary, Rest2/binary>> = Rest,
    {decode_packed_aux(Bytes, FieldType, MsgDefs, Seq0), Rest2}.

decode_packed_aux(Bytes, FieldType, MsgDefs, Acc) when byte_size(Bytes) > 0 ->
    {NewValue, Rest} = decode_type(FieldType, Bytes, MsgDefs),
    decode_packed_aux(Rest, FieldType, MsgDefs, [NewValue | Acc]);
decode_packed_aux(<<>>, _FieldType, _MsgDefs, Acc) ->
    Acc.

decode_type(FieldType, Bin, MsgDefs) ->
    case FieldType of
        sint32 ->
            {NV, T} = decode_varint(Bin, 32),
            {decode_zigzag(NV), T};
        sint64 ->
            {NV, T} = decode_varint(Bin, 64),
            {decode_zigzag(NV), T};
        int32 ->
            {NV, T} = decode_varint(Bin, 32),
            %% Contrary to the 64 bit encoding done for int32 (and enum),
            %% decode the value as 32 bits, so we decode negatives
            %% given both as 32 bits and as 64 bits wire encodings
            %% to the same integer.
            <<N:32/signed>> = <<NV:32>>,
            {N, T};
        int64 ->
            {NV, T} = decode_varint(Bin, 64),
            <<N:64/signed>> = <<NV:64>>,
            {N, T};
        uint32 ->
            {_N, _Rest} = decode_varint(Bin, 32);
        uint64 ->
            {_N, _Rest} = decode_varint(Bin, 64);
        bool ->
            {N, Rest} = decode_varint(Bin, 64),
            {N =/= 0, Rest};
        {enum, _EnumName}=Key ->
            {N, Rest} = decode_type(int32, Bin, MsgDefs),
            {value, {Key, EnumValues}} = lists:keysearch(Key, 1, MsgDefs),
            {value, {EnumName, N}} = lists:keysearch(N, 2, EnumValues),
            {EnumName, Rest};
        fixed64 ->
            <<N:64/little, Rest/binary>> = Bin,
            {N, Rest};
        sfixed64 ->
            <<N:64/little-signed, Rest/binary>> = Bin,
            {N, Rest};
        double ->
            <<N:64/little-float, Rest/binary>> = Bin,
            {N, Rest};
        string ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<Utf8Str:Len/binary, Rest2/binary>> = Rest,
            {unicode:characters_to_list(Utf8Str, unicode), Rest2};
        bytes ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<Bytes:Len/binary, Rest2/binary>> = Rest,
            {Bytes, Rest2};
        {msg,MsgName} ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<MsgBytes:Len/binary, Rest2/binary>> = Rest,
            {decode_msg(MsgBytes, MsgName, MsgDefs), Rest2};
        fixed32 ->
            <<N:32/little, Rest/binary>> = Bin,
            {N, Rest};
        sfixed32 ->
            <<N:32/little-signed, Rest/binary>> = Bin,
            {N, Rest};
        float ->
            <<N:32/little-float, Rest/binary>> = Bin,
            {N, Rest};
        {map,KeyType,ValueType} ->
            MsgDefs1 = [map_item_tmp_def(KeyType, ValueType) | MsgDefs],
            MsgName = map_item_tmp_name(),
            {{MsgName,Key,Value}, Rest2} =
                decode_type({msg, MsgName}, Bin, MsgDefs1),
            {{Key,Value}, Rest2}
        end.

add_field(Value, FieldDef, false=_IsOneof, MsgDefs, Record) ->
    %% FIXME: what about bytes?? "For numeric types and strings, if
    %% the same value appears multiple times, the parser accepts the
    %% last value it sees." But what about bytes?
    %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
    %% For now, we assume it works like strings.
    case FieldDef of
        #?gpb_field{rnum = RNum, occurrence = required, type = {msg,_FMsgName}}->
            merge_field(RNum, Value, Record, MsgDefs);
        #?gpb_field{rnum = RNum, occurrence = optional, type = {msg,_FMsgName}}->
            merge_field(RNum, Value, Record, MsgDefs);
        #?gpb_field{rnum = RNum, occurrence = required}->
            setelement(RNum, Record, Value);
        #?gpb_field{rnum = RNum, occurrence = optional}->
            setelement(RNum, Record, Value);
        #?gpb_field{rnum = RNum, occurrence = repeated, type={map,_,_}} ->
            append_to_map(RNum, Value, Record);
        #?gpb_field{rnum = RNum, occurrence = repeated} ->
            append_to_element(RNum, Value, Record)
    end;
add_field(Value, FieldDef, true=_IsOneof, MsgDefs, Record) ->
    #?gpb_field{rnum=RNum, name=Name} = FieldDef,
    case FieldDef of
        #?gpb_field{type={msg,_SubMsgType}} ->
            case element(RNum, Record) of
                {Name, PrevMsg} ->
                    MergedMsg = {Name, merge_msgs(PrevMsg, Value, MsgDefs)},
                    setelement(RNum, Record, MergedMsg);
                _ ->
                    setelement(RNum, Record, {Name, Value})
            end;
        _ ->
            setelement(RNum, Record, {Name, Value})
    end.

merge_field(RNum, NewMsg, Record, MsgDefs) ->
    case element(RNum, Record) of
        undefined ->
            setelement(RNum, Record, NewMsg);
        PrevMsg ->
            MergedMsg = merge_msgs(PrevMsg, NewMsg, MsgDefs),
            setelement(RNum, Record, MergedMsg)
    end.

append_to_element(RNum, NewElem, Record) ->
    PrevElems = element(RNum, Record),
    setelement(RNum, Record, [NewElem | PrevElems]).

append_to_map(RNum, {Key, _Value}=NewItem, Record) ->
    PrevElems = element(RNum, Record),
    NewElems = lists:keystore(Key, 1, PrevElems, NewItem),
    setelement(RNum, Record, NewElems).

merge_msgs(PrevMsg, NewMsg, MsgDefs)
  when element(1,PrevMsg) == element(1,NewMsg) ->
    MsgName = element(1, NewMsg),
    MsgDef = keyfetch({msg,MsgName}, MsgDefs),
    lists:foldl(
      fun(#?gpb_field{rnum=RNum, occurrence=repeated, type=Type}, AccRecord) ->
              case Type of
                  {map,_,_} ->
                      NewMap  = element(RNum, NewMsg),
                      lists:foldl(
                        fun(NewItem, R) -> append_to_map(RNum, NewItem, R) end,
                        AccRecord,
                        NewMap);
                  _ ->
                      PrevSeq = element(RNum, AccRecord),
                      NewSeq  = element(RNum, NewMsg),
                      setelement(RNum, AccRecord, PrevSeq ++ NewSeq)
              end;
         (#?gpb_field{rnum=RNum, type={msg,_FieldMsgName}}, AccRecord) ->
              case {element(RNum, AccRecord), element(RNum, NewMsg)} of
                  {undefined, undefined} ->
                      AccRecord;
                  {undefined, NewSubMsg} ->
                      setelement(RNum, AccRecord, NewSubMsg);
                  {_PrevSubMsg, undefined} ->
                      AccRecord;
                  {PrevSubMsg, NewSubMsg} ->
                      MergedSubMsg = merge_msgs(PrevSubMsg, NewSubMsg, MsgDefs),
                      setelement(RNum, AccRecord, MergedSubMsg)
              end;
         (#?gpb_field{rnum=RNum}, AccRecord) ->
              case element(RNum, NewMsg) of
                  undefined -> AccRecord;
                  NewValue  -> setelement(RNum, AccRecord, NewValue)
              end;
         (#gpb_oneof{rnum=RNum, fields=OFields}, AccRecord) ->
              %% The language guide for oneof says that
              %%
              %%   "If the parser encounters multiple members of the
              %%   same oneof on the wire, only the last member seen
              %%   is used in the parsed message."
              %%
              %% In practice, this seems to mean they are merged,
              %% at least according to experiments with generated c++ code.
              %%
              case {element(RNum, AccRecord), element(RNum, NewMsg)} of
                  {undefined, undefined} ->
                      AccRecord;
                  {undefined, NewElem} ->
                      setelement(RNum, AccRecord, NewElem);
                  {_PrevElem, undefined} ->
                      AccRecord;
                  {{OFName, PrevValue}, {OFName, NewValue}=NewElem} ->
                      case lists:keyfind(OFName, #?gpb_field.name, OFields) of
                          #?gpb_field{type={msg,_}} ->
                              NewSub = merge_msgs(PrevValue, NewValue, MsgDefs),
                              setelement(RNum, AccRecord, {OFName,NewSub});
                          #?gpb_field{} ->
                              setelement(RNum, AccRecord, NewElem)
                      end;
                  {_PrevElem, NewElem} ->
                      %% oneof fields
                      setelement(RNum, AccRecord, NewElem)
              end
      end,
      PrevMsg,
      MsgDef).


encode_msg(Msg, MsgDefs) ->
    MsgName = element(1, Msg),
    MsgDef = keyfetch({msg, MsgName}, MsgDefs),
    encode_2(MsgDef, Msg, MsgDefs, <<>>).

encode_2([#?gpb_field{occurrence=Occurrence}=Field | Rest], Msg, MsgDefs, Acc) ->
    EncodedField =
        case {Occurrence, is_packed(Field)} of
            {repeated, true} ->
                encode_packed(Field, Msg, MsgDefs);
            _ ->
                encode_field(Field, Msg, MsgDefs)
        end,
    encode_2(Rest, Msg, MsgDefs, <<Acc/binary, EncodedField/binary>>);
encode_2([#gpb_oneof{fields=Fields, rnum=RNum} | Rest], Msg, MsgDefs, Acc) ->
    case element(RNum, Msg) of
        {Name, Value} ->
            Field = lists:keyfind(Name, #?gpb_field.name, Fields),
            NewAcc = encode_2([Field], setelement(RNum, Msg, Value), MsgDefs,
                              Acc),
            encode_2(Rest, Msg, MsgDefs, NewAcc);
        undefined ->
            encode_2(Rest, Msg, MsgDefs, Acc)
    end;
encode_2([], _Msg, _MsgDefs, Acc) ->
    Acc.

encode_packed(#?gpb_field{rnum=RNum, fnum=FNum, type=Type}, Msg, MsgDefs) ->
    case element(RNum, Msg) of
        []    ->
            <<>>;
        Elems ->
            PackedFields = encode_packed_2(Elems, Type, MsgDefs, <<>>),
            <<(encode_fnum_type(FNum, bytes))/binary,
              (encode_varint(byte_size(PackedFields)))/binary,
              PackedFields/binary>>
    end.

encode_packed_2([Elem | Rest], Type, MsgDefs, Acc) ->
    NewAcc = <<Acc/binary, (encode_value(Elem, Type, MsgDefs))/binary>>,
    encode_packed_2(Rest, Type, MsgDefs, NewAcc);
encode_packed_2([], _Type, _MsgDefs, Acc) ->
    Acc.

encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=required},
             Msg, MsgDefs) ->
    Value = element(RNum, Msg),
    case is_msg_proto3(element(1, Msg), MsgDefs)
        andalso proto3_type_default(Type, MsgDefs) =:= Value of
        true ->
            <<>>;
        false ->
            encode_field_value(Value, FNum, Type, MsgDefs)
    end;
encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=optional},
             Msg, MsgDefs) ->
    case element(RNum, Msg) of
        undefined -> <<>>;
        Value     -> encode_field_value(Value, FNum, Type, MsgDefs)
    end;
encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=repeated},
             Msg, MsgDefs) ->
    encode_repeated(element(RNum, Msg), FNum, Type, MsgDefs, <<>>).

encode_repeated([Elem | Rest], FNum, Type, MsgDefs, Acc) ->
    EncodedValue = encode_field_value(Elem, FNum, Type, MsgDefs),
    NewAcc = <<Acc/binary, EncodedValue/binary>>,
    encode_repeated(Rest, FNum, Type, MsgDefs, NewAcc);
encode_repeated([], _FNum, _Type, _MsgDefs, Acc) ->
    Acc.

encode_field_value(Value, FNum, Type, MsgDefs) ->
    <<(encode_fnum_type(FNum, Type))/binary,
      (encode_value(Value, Type, MsgDefs))/binary>>.

encode_fnum_type(FNum, Type) ->
    encode_varint((FNum bsl 3) bor encode_wiretype(Type)).

encode_value(Value, Type, MsgDefs) ->
    case Type of
        sint32 ->
            encode_varint(encode_zigzag(Value));
        sint64 ->
            encode_varint(encode_zigzag(Value));
        int32 ->
            if Value >= 0 ->
                    encode_varint(Value);
               true ->
                    %% Encode as a 64 bit value, for interop compatibility.
                    %% Some implementations don't decode 32 bits properly,
                    %% and Google's protobuf (C++) encodes as 64 bits
                    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                    encode_varint(N)
            end;
        int64 ->
            if Value >= 0 ->
                    encode_varint(Value);
               true ->
                    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                    encode_varint(N)
            end;
        uint32 ->
            encode_varint(Value);
        uint64 ->
            encode_varint(Value);
        bool ->
            if Value     -> encode_varint(1);
               not Value -> encode_varint(0)
            end;
        {enum, _EnumName}=Key ->
            {value, {Key, EnumValues}} = lists:keysearch(Key, 1, MsgDefs),
            {value, {Value, N}} = lists:keysearch(Value, 1, EnumValues),
            encode_value(N, int32, MsgDefs);
        fixed64 ->
            <<Value:64/little>>;
        sfixed64 ->
            <<Value:64/signed-little>>;
        double ->
            <<Value:64/float-little>>;
        string ->
            Utf8 = unicode:characters_to_binary(Value),
            <<(encode_varint(byte_size(Utf8)))/binary, Utf8/binary>>;
        bytes ->
            <<(encode_varint(byte_size(Value)))/binary, Value/binary>>;
        {msg,_MsgName} ->
            SubMsg = encode_msg(Value, MsgDefs),
            <<(encode_varint(byte_size(SubMsg)))/binary, SubMsg/binary>>;
        fixed32 ->
            <<Value:32/little>>;
        sfixed32 ->
            <<Value:32/signed-little>>;
        float ->
            <<Value:32/float-little>>;
        {map,KeyType,ValueType} ->
            {Key,Value1} = Value,
            MsgName = map_item_tmp_name(),
            MsgDefs1 = [map_item_tmp_def(KeyType, ValueType) | MsgDefs],
            encode_value({MsgName,Key,Value1}, {msg,MsgName}, MsgDefs1)
    end.


encode_wiretype(sint32)            -> 0;
encode_wiretype(sint64)            -> 0;
encode_wiretype(int32)             -> 0;
encode_wiretype(int64)             -> 0;
encode_wiretype(uint32)            -> 0;
encode_wiretype(uint64)            -> 0;
encode_wiretype(bool)              -> 0;
encode_wiretype({enum, _EnumName}) -> 0;
encode_wiretype(fixed64)           -> 1;
encode_wiretype(sfixed64)          -> 1;
encode_wiretype(double)            -> 1;
encode_wiretype(string)            -> 2;
encode_wiretype(bytes)             -> 2;
encode_wiretype({msg,_MsgName})    -> 2;
encode_wiretype(fixed32)           -> 5;
encode_wiretype(sfixed32)          -> 5;
encode_wiretype(float)             -> 5;
encode_wiretype({map,_KT,_VT}) -> encode_wiretype({msg,map_item_tmp_name()}).


decode_varint(Bin) -> decode_varint(Bin, 64).
decode_varint(Bin, MaxNumBits) -> de_vi(Bin, 0, 0, MaxNumBits).

de_vi(<<1:1, X:7, Rest/binary>>, N, Acc, MaxNumBits) when N < (64-7) ->
    de_vi(Rest, N+7, X bsl N + Acc, MaxNumBits);
de_vi(<<0:1, X:7, Rest/binary>>, N, Acc, MaxNumBits) ->
    Mask = (1 bsl MaxNumBits) - 1,
    {(X bsl N + Acc) band Mask, Rest}.

encode_varint(N) -> en_vi(N).

en_vi(N) when N =< 127 -> <<N>>;
en_vi(N) when N >= 128 -> <<1:1, (N band 127):7, (en_vi(N bsr 7))/binary>>.


decode_zigzag(N) when N band 1 =:= 0 -> N bsr 1;        %% N is even
decode_zigzag(N) when N band 1 =:= 1 -> -((N+1) bsr 1). %% N is odd

encode_zigzag(N) when N >= 0 -> N * 2;
encode_zigzag(N) when N <  0 -> N * -2 - 1.


verify_msg(Msg, MsgDefs) when is_tuple(Msg), tuple_size(Msg) >= 1 ->
    MsgName = element(1, Msg),
    case lists:keysearch({msg,MsgName}, 1, MsgDefs) of
        {value, _} ->
            verify_msg2(Msg, MsgName, MsgDefs, [top_level]);
        false ->
            mk_type_error(not_a_known_message, MsgName, [top_level])
    end;
verify_msg(Msg, _MsgDefs) ->
    mk_type_error(expected_a_message, Msg, []).

%% Verify that Msg is actually a message named MsgName as defined in MsgDefs
verify_msg2(Msg, MsgName, MsgDefs, Path) when is_tuple(Msg),
                                              element(1, Msg) == MsgName ->
    MsgKey = {msg, MsgName},
    {value, {MsgKey, Fields}} = lists:keysearch(MsgKey, 1, MsgDefs),
    if tuple_size(Msg) == length(Fields) + 1 ->
            Path2 = if Path == [top_level] -> [MsgName];
                       true                -> Path
                    end,
            verify_fields(Msg, Fields, Path2, MsgDefs);
       true ->
            mk_type_error({bad_record,MsgName}, Msg, Path)
    end;
verify_msg2(V, MsgName, _MsgDefs, Path) ->
    mk_type_error({bad_msg, MsgName}, V, Path).

verify_fields(Msg, Fields, Path, MsgDefs) when tuple_size(Msg)
                                               == length(Fields) + 1 ->
    lists:foreach(
      fun(#?gpb_field{name=Name, type=Type, rnum=RNum, occurrence=Occurrence}) ->
              Value = element(RNum, Msg),
              verify_value(Value, Type, Occurrence, Path++[Name], MsgDefs);
         (#gpb_oneof{name=Name, rnum=RNum, fields=OFields}) ->
              case element(RNum, Msg) of
                  {FName, Value} ->
                      case lists:keyfind(FName, #?gpb_field.name, OFields) of
                          #?gpb_field{type=Type} ->
                              verify_value(Value, Type, optional, Path++[Name],
                                           MsgDefs);
                          false ->
                              mk_type_error(bad_oneof_indicator, FName, Path)
                      end;
                  undefined ->
                      ok;
                  Other ->
                      mk_type_error(bad_oneof_value, Other, Path)
              end
      end,
      Fields);
verify_fields(Msg, _Fields, Path, _MsgDefs) ->
    mk_type_error(bad_record, Msg, Path).

verify_value(Value, Type, Occurrence, Path, MsgDefs) ->
    case Occurrence of
        required -> verify_value_2(Value, Type, Path, MsgDefs);
        repeated -> verify_list(Value, Type, Path, MsgDefs);
        optional -> verify_optional(Value, Type, Path, MsgDefs)
    end.

check_scalar(Value, Type) when is_atom(Type) ->
    try
        verify_value_2(Value, Type, [], [])
    catch
        error:{gpb_type_error, {Reason, _Info}} ->
            {error, {Reason, Value}}
    end.

verify_value_2(V, int32, Path, _MsgDefs)    -> verify_int(V, {i,32}, Path);
verify_value_2(V, int64, Path, _MsgDefs)    -> verify_int(V, {i,64}, Path);
verify_value_2(V, uint32, Path, _MsgDefs)   -> verify_int(V, {u,32}, Path);
verify_value_2(V, uint64, Path, _MsgDefs)   -> verify_int(V, {u,64}, Path);
verify_value_2(V, sint32, Path, _MsgDefs)   -> verify_int(V, {i,32}, Path);
verify_value_2(V, sint64, Path, _MsgDefs)   -> verify_int(V, {i,64}, Path);
verify_value_2(V, fixed32, Path, _MsgDefs)  -> verify_int(V, {u,32}, Path);
verify_value_2(V, fixed64, Path, _MsgDefs)  -> verify_int(V, {u,64}, Path);
verify_value_2(V, sfixed32, Path, _MsgDefs) -> verify_int(V, {i,32}, Path);
verify_value_2(V, sfixed64, Path, _MsgDefs) -> verify_int(V, {i,64}, Path);
verify_value_2(V, bool, Path, _MsgDefs)     -> verify_bool(V, Path);
verify_value_2(V, float, Path, _MsgDefs)    -> verify_float(V, Path);
verify_value_2(V, double, Path, _MsgDefs)   -> verify_float(V, Path);
verify_value_2(V, string, Path, _MsgDefs)   -> verify_string(V, Path);
verify_value_2(V, bytes, Path, _MsgDefs)    -> verify_bytes(V, Path);
verify_value_2(V, {enum,E}, Path, MsgDefs)  -> verify_enum(V, E, MsgDefs, Path);
verify_value_2(V, {msg,M}, Path, MsgDefs)   -> verify_msg2(V, M, MsgDefs, Path);
verify_value_2(V, {map,_,_}=M, Path, MsgDefs) -> verify_map(V, M, MsgDefs,Path).

verify_int(V, {i,32}, _) when -(1 bsl 31) =< V, V =< (1 bsl 31 - 1) -> ok;
verify_int(V, {i,64}, _) when -(1 bsl 63) =< V, V =< (1 bsl 63 - 1) -> ok;
verify_int(V, {u,32}, _) when 0 =< V, V =< (1 bsl 32 - 1)           -> ok;
verify_int(V, {u,64}, _) when 0 =< V, V =< (1 bsl 64 - 1)           -> ok;
verify_int(V, {S,Bits}, Path) ->
    Signedness = case S of
                     i -> signed;
                     u -> unsigned
                 end,
    if is_integer(V) ->
            mk_type_error({value_out_of_range, Signedness, Bits}, V, Path);
       true ->
            mk_type_error({bad_integer_value, Signedness, Bits}, V, Path)
    end.

verify_bool(true,  _) -> ok;
verify_bool(false, _) -> ok;
verify_bool(V, Path) ->
    mk_type_error(bad_boolean_value, V, Path).

verify_float(V, _) when is_float(V) -> ok;
verify_float(V, _) when is_integer(V) -> ok;
verify_float(V, Path) ->
    mk_type_error(bad_floating_point_value, V, Path).

verify_string(V, Path) when is_list(V); is_binary(V) ->
    try
        unicode:characters_to_binary(V),
        ok
    catch error:badarg ->
            mk_type_error(bad_unicode_string, V, Path)
    end;
verify_string(V, Path) ->
    mk_type_error(bad_unicode_string, V, Path).

verify_bytes(V, _) when is_binary(V) ->
    ok;
verify_bytes(V, Path) ->
    mk_type_error(bad_binary_value, V, Path).

verify_enum(V, EnumName, MsgDefs, Path) ->
    EnumKey = {enum, EnumName},
    {value, {EnumKey, Enumerations}} = lists:keysearch(EnumKey, 1, MsgDefs),
    case lists:keymember(V, 1, Enumerations) of
        true  -> ok;
        false -> mk_type_error(bad_enum_value, V, Path)
    end.

verify_map({Key,Value}, {map, KeyType, ValueType}, MsgDefs, Path) ->
    MsgName = map_item_tmp_name(),
    MsgDefs1 = [map_item_tmp_def(KeyType, ValueType) | MsgDefs],
    MapAsMsg = {MsgName, Key, Value},
    verify_msg2(MapAsMsg, MsgName, MsgDefs1, [mapitem | Path]);
verify_map(V, _, _, Path) ->
    mk_type_error(bad_map_item_value, V, Path).

verify_list(Elems, Type, Path, MsgDefs) when is_list(Elems) ->
    lists:foreach(fun(Elem) -> verify_value_2(Elem, Type, Path, MsgDefs) end,
                  Elems);
verify_list(Elems, Type, Path, _MsgDefs) ->
    mk_type_error({bad_repeated,Type}, Elems, Path).

verify_optional(undefined, _Type, _Path, _MsgDefs) ->
    ok;
verify_optional(Value, Type, Path, MsgDefs) ->
    verify_value_2(Value, Type, Path, MsgDefs).

mk_type_error(Error, ValueSeen, Path) ->
    Path2 = if Path == [] -> top_level;
               true -> list_to_atom(string:join([atom_to_list(E) || E <- Path],
                                                "."))
            end,
    erlang:error({gpb_type_error, {Error, [{value, ValueSeen},{path, Path2}]}}).

%% --

%% Conversion functions between various forms of #?gpb_field{} and a proplist
%% with keys being the #?gpb_field{} record's field names.

defs_records_to_proplists(Defs) ->
    [case Def of
         {{msg,Msg}, Fields} ->
             {{msg,Msg}, field_records_to_proplists(Fields)};
         Other ->
             Other
     end
     || Def <- Defs].

proplists_to_defs_records(Defs) ->
    [case Def of
         {{msg,Msg}, PropList} ->
             {{msg,Msg}, proplists_to_field_records(PropList)};
         Other ->
             Other
     end
     || Def <- Defs].

field_records_to_proplists(Fields) when is_list(Fields) ->
    [case F of
         #?gpb_field{} -> field_record_to_proplist(F);
         #gpb_oneof{}  -> oneof_record_to_proplist(F)
     end
     || F <- Fields].

field_record_to_proplist(#?gpb_field{}=F) ->
    Names = record_info(fields, ?gpb_field),
    lists:zip(Names, tl(tuple_to_list(F))).

oneof_record_to_proplist(#gpb_oneof{}=F) ->
    Names = record_info(fields, gpb_oneof),
    [if FName == fields -> {FName, field_records_to_proplists(FValue)};
        FName /= fields -> {FName, FValue}
     end
     || {FName, FValue} <- lists:zip(Names, tl(tuple_to_list(F)))].

proplists_to_field_records(PLs) ->
    [case {is_field_pl(PL), is_oneof_pl(PL)} of
         {true, false} -> proplist_to_field_record(PL);
         {false, true} -> proplist_to_oneof_record(PL)
     end
     || PL <- PLs].

is_field_pl(PL) -> are_all_fields_present(record_info(fields, ?gpb_field), PL).

is_oneof_pl(PL) -> are_all_fields_present(record_info(fields, gpb_oneof), PL).

are_all_fields_present(FNames, PL) ->
    lists:all(fun(FName) -> lists:keymember(FName, 1, PL) end,
              FNames).

proplist_to_field_record(PL) when is_list(PL) ->
    Names = record_info(fields, ?gpb_field),
    RFields = [proplists:get_value(Name, PL) || Name <- Names],
    list_to_tuple([?gpb_field | RFields]).

proplist_to_oneof_record(PL) when is_list(PL) ->
    Names = record_info(fields, gpb_oneof),
    RFields = [proplists:get_value(Name, PL) || Name <- Names],
    list_to_tuple(
      [gpb_oneof | [if N == fields -> proplists_to_field_records(V);
                       N /= fields -> V
                    end
                    || {N, V} <- lists:zip(Names, RFields)]]).

rpc_records_to_proplists(Rpcs) when is_list(Rpcs) ->
    [rpc_record_to_proplist(R) || R <- Rpcs].

rpc_record_to_proplist(#?gpb_rpc{}=R) ->
    Names = record_info(fields, ?gpb_rpc),
    lists:zip(Names, tl(tuple_to_list(R))).

proplists_to_rpc_records(PLs) ->
    [proplist_to_rpc_record(PL) || PL <- PLs].

proplist_to_rpc_record(PL) when is_list(PL) ->
    Names = record_info(fields, ?gpb_rpc),
    RFields = [proplists:get_value(Name, PL) || Name <- Names],
    list_to_tuple([?gpb_rpc | RFields]).

map_item_tmp_def(KeyType, ValueType) ->
    {{msg, map_item_tmp_name()}, map_item_pseudo_fields(KeyType, ValueType)}.

map_item_tmp_name() ->
    '$mapitem'.

map_item_pseudo_fields(KeyType, ValueType) ->
    [#?gpb_field{name=key, fnum=1, rnum=2,
                 occurrence=required, type=KeyType},
     #?gpb_field{name=value, fnum=2, rnum=3,
                 occurrence=required, type=ValueType}].

is_allowed_as_key_type({enum,_}) -> false;
is_allowed_as_key_type({msg,_}) -> false;
is_allowed_as_key_type(double) -> false;
is_allowed_as_key_type(float) -> false;
is_allowed_as_key_type(bytes) -> false;
is_allowed_as_key_type(_) -> true.

%% --

is_packed(#?gpb_field{opts=Opts}) ->
    lists:member(packed, Opts).

is_msg_proto3(Name, MsgDefs) ->
    case lists:keyfind(proto3_msgs, 1, MsgDefs) of
        {proto3_msgs, Names} ->
            lists:member(Name, Names);
        false ->
            false
    end.

proto3_type_default(Type, MsgDefs) ->
    case Type of
        sint32   -> 0;
        sint64   -> 0;
        int32    -> 0;
        int64    -> 0;
        uint32   -> 0;
        uint64   -> 0;
        bool     -> false;
        fixed64  -> 0;
        sfixed64 -> 0;
        double   -> 0.0;
        string   -> "";
        bytes    -> <<>>;
        {msg,_}  -> undefined;
        fixed32  -> 0;
        sfixed32 -> 0;
        float    -> 0.0;
        {map,_KT,_VT} -> [];
        {enum, _EnumName}=Key ->
            {Key,[{Sym0,_V0} | _]} = lists:keyfind(Key, 1, MsgDefs),
            Sym0
    end.

keyfetch(Key, KVPairs) ->
    case lists:keysearch(Key, 1, KVPairs) of
        {value, {Key, Value}} ->
            Value;
        false ->
            erlang:error({error, {no_such_key, Key, KVPairs}})
    end.

version_format_test() ->
    ok = assert_version_format("2"),
    ok = assert_version_format("2.1"),
    ok = assert_version_format("2.1.1"),
    ok = assert_version_format("2.1.1.1"),
    %% a development version after 2.1, but before any 2.2
    ok = assert_version_format("2.1-53-gb996fbe"),
    %% non-digit version components
    ?assertError(_, assert_version_format("2.2x")),
    ?assertError(_, assert_version_format("2.x")),
    ?assertError(_, assert_version_format("3y")),
    ?assertError(_, assert_version_format("y")),
    ?assertError(_, assert_version_format("2.1-4z-gb996fbe")),
    ?assertError(_, assert_version_format("2.1-z-gb996fbe")),
    %% malplaced dots
    ?assertError(_, assert_version_format(".")),
    ?assertError(_, assert_version_format(".2")),
    ?assertError(_, assert_version_format("..2")),
    ?assertError(_, assert_version_format("2.")),
    ?assertError(_, assert_version_format("2.1..")),
    ?assertError(_, assert_version_format("2..1")),
    %% missing bits and pieces
    ?assertError(_, assert_version_format("2.1-53-")),
    ?assertError(_, assert_version_format("2.1-53-")),
    ?assertError(_, assert_version_format("2.1-")),
    ?assertError(_, assert_version_format("2-")),
    ?assertError(_, assert_version_format("-")),
    %% misc other
    ?assertError(_, assert_version_format("2.1--53-gb996fbe")).

version_as_list_test() ->
    [2,1] = version_as_list("2.1"),
    [2,1,1] = version_as_list("2.1.1"),
    [2,1,0,0,53,"gb996fbe"] = version_as_list("2.1-53-gb996fbe"),
    [2,2] = version_as_list("2.2").

encode_zigzag_test() ->
    0 = encode_zigzag(0),
    1 = encode_zigzag(-1),
    2 = encode_zigzag(1),
    3 = encode_zigzag(-2),
    4294967294 = encode_zigzag(2147483647),
    4294967295 = encode_zigzag(-2147483648).

decode_zigzag_test() ->
    0  = decode_zigzag(0),
    -1 = decode_zigzag(1),
    1  = decode_zigzag(2),
    -2 = decode_zigzag(3),
    2147483647  = decode_zigzag(4294967294),
    -2147483648 = decode_zigzag(4294967295).

decode_invalid_varint_fails_test() ->
    %% This varint is invalid because it is too long.
    %% approx 2.3e105, which is much longer than 32 or 64 bits
    %% The limit is not set too narrowly above 64 bits; the purpose
    %% is more to catch malicious input causing the decoder to
    %% eat memory until the vm dies (denial of service).
    InvalidVarint = iolist_to_binary([lists:duplicate(50, 255), 0]),
    ?assertError(_, decode_varint(InvalidVarint)).
