%%% Copyright (C) 2010  Tomas Abrahamsson
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

-module(gpb).
%-compile(export_all).
-export([decode_msg/3]).
-export([encode_msg/2]).
-export([merge_msgs/3]).
-export([verify_msg/2]).
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

%% TODO:
%%
%% * Add a new_default_msg that sets default values according to
%%   type (and optionalness) as docoumented on the google web:
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

decode_msg(Bin, MsgName, MsgDefs) ->
    MsgKey = {msg,MsgName},
    Msg    = new_initial_msg(MsgKey, MsgDefs),
    MsgDef = keyfetch(MsgKey, MsgDefs),
    %decode_field(Bin, MsgDef, MsgDefs, Msg).
    d_read_field_def(Bin, 0, 0, MsgDef, MsgDefs, Msg).

new_initial_msg({msg,MsgName}=MsgKey, MsgDefs) ->
    MsgDef = keyfetch(MsgKey, MsgDefs),
    lists:foldl(fun(#field{rnum=RNum, occurrence=repeated}, Record) ->
                        setelement(RNum, Record, []);
                   (#field{type={msg,_Name}, occurrence=optional}, Record) ->
                        Record;
                   (#field{rnum=RNum, type={msg,_Name}=FMsgKey}, Record) ->
                        SubMsg = new_initial_msg(FMsgKey, MsgDefs),
                        setelement(RNum, Record, SubMsg);
                   (#field{}, Record) ->
                        Record
                end,
                erlang:make_tuple(length(MsgDef)+1, undefined, [{1,MsgName}]),
                MsgDef).


d_read_field_def(<<1:1, X:7, Rest/binary>>, N, Acc, MsgDef, Defs, Msg) ->
    d_read_field_def(Rest, N+1, X bsl (N*7) + Acc, MsgDef, Defs, Msg);
d_read_field_def(<<0:1, X:7, Rest/binary>>, N, Acc, MsgDef, Defs, Msg) ->
    Key = X bsl (N*7) + Acc,
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    case lists:keyfind(FieldNum, #field.fnum, MsgDef) of
        false ->
            case decode_wiretype(WireType) of
                varint ->
                    d_skip_varint(Rest, MsgDef, Defs, Msg);
                bits64 ->
                    d_skip64(Rest, MsgDef, Defs, Msg);
                length_delimited ->
                    d_skip_length_delimited(Rest, 0, 0, MsgDef, Defs, Msg);
                bits32 ->
                    d_skip32(Rest, MsgDef, Defs, Msg)
                end;
        #field{is_packed=true} = FieldDef ->
            d_packed(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
        #field{type=Type} = FieldDef ->
            case Type of
                sint32   -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                sint64   -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                int32    -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                int64    -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                uint32   -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                uint64   -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                bool     -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                {enum,_} -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                fixed32  -> d_uf32(Rest, FieldDef, MsgDef, Defs, Msg);
                sfixed32 -> d_sf32(Rest, FieldDef, MsgDef, Defs, Msg);
                float    -> d_float(Rest, FieldDef, MsgDef, Defs, Msg);
                fixed64  -> d_uf64(Rest, FieldDef, MsgDef, Defs, Msg);
                sfixed64 -> d_sf64(Rest, FieldDef, MsgDef, Defs, Msg);
                double   -> d_double(Rest, FieldDef, MsgDef, Defs, Msg);
                string   -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                bytes    -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg);
                {msg,_}  -> d_vi_based(Rest, 0, 0, FieldDef, MsgDef, Defs, Msg)
            end
    end;
d_read_field_def(<<>>, 0, 0, MsgDef, _Defs, Msg) ->
    %% Reverse any repeated fields, but only on the top-level, not recursively.
    RepeatedRNums = [N || #field{rnum=N, occurrence=repeated} <- MsgDef],
    lists:foldl(fun(RNum, Record) ->
                        OldValue = element(RNum, Record),
                        ReversedField = lists:reverse(OldValue),
                        setelement(RNum, Record, ReversedField)
                end,
                Msg,
                RepeatedRNums).

d_skip64(<<_:64,Rest/binary>>, MsgDef, Defs, Msg) ->
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, Msg).

d_skip32(<<_:32,Rest/binary>>, MsgDef, Defs, Msg) ->
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, Msg).

d_skip_varint(<<1:1, _:7, Rest/binary>>, MsgDef, Defs, Msg) ->
    d_skip_varint(Rest, MsgDef, Defs, Msg);
d_skip_varint(<<0:1, _:7, Rest/binary>>, MsgDef, Defs, Msg) ->
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, Msg).

d_skip_length_delimited(<<1:1, X:7, Rest/binary>>, N, Acc, MsgDef, Defs, Msg) ->
    d_skip_length_delimited(Rest, N+1, X bsl (N*7) + Acc, MsgDef, Defs, Msg);
d_skip_length_delimited(<<0:1, X:7, Rest/binary>>, N, Acc, MsgDef, Defs, Msg) ->
    Length = X bsl (N*7) + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    d_read_field_def(Rest2, 0, 0, MsgDef, Defs, Msg).

d_vi_based(<<1:1, X:7, Rest/binary>>, N, Acc, FieldDef, MsgDef, Defs, Msg) ->
    d_vi_based(Rest, N+1, X bsl (N*7) + Acc, FieldDef, MsgDef, Defs, Msg);
d_vi_based(<<0:1, X:7, Rest/binary>>, N, Acc, FieldDef, MsgDef, Defs, Msg) ->
    BValue = X bsl (N*7) + Acc,
    {Value, Rest3} =
        case FieldDef#field.type of
            sint32 -> {decode_zigzag(BValue), Rest};
            sint64 -> {decode_zigzag(BValue), Rest};
            int32  -> {uint32_to_int32(BValue), Rest};
            int64  -> {uint64_to_int64(BValue), Rest};
            uint32 -> {BValue, Rest};
            uint64 -> {BValue, Rest};
            bool   -> {BValue =/= 0, Rest};
            {enum, _EnumName}=Key ->
                EnumValue = uint32_to_int32(BValue),
                {Key, EnumValues} = lists:keyfind(Key, 1, Defs),
                {EnumName, EnumValue} = lists:keyfind(EnumValue, 2, EnumValues),
                {EnumName, Rest};
            string ->
                Len = BValue,
                <<Utf8Str:Len/binary, Rest2/binary>> = Rest,
                {unicode:characters_to_list(Utf8Str, unicode), Rest2};
            bytes ->
                Len = BValue,
                <<Bytes:Len/binary, Rest2/binary>> = Rest,
                %% Decrease ref count of the binary we are decoding (R14+)
                %% binary:copy(Bytes)
                {Bytes, Rest2};
            {msg,MsgName} ->
                Len = BValue,
                <<MsgBytes:Len/binary, Rest2/binary>> = Rest,
                {decode_msg(MsgBytes, MsgName, Defs), Rest2}
        end,
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest3, 0, 0, MsgDef, Defs, NewMsg).

d_uf32(<<Value:32/little, Rest/binary>>, FieldDef, MsgDef, Defs, Msg) ->
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, NewMsg).

d_sf32(<<Value:32/little-signed, Rest/binary>>, FieldDef, MsgDef, Defs, Msg) ->
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, NewMsg).

d_float(<<Value:32/little-float, Rest/binary>>, FieldDef, MsgDef, Defs, Msg) ->
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, NewMsg).

d_double(<<Value:64/little-float, Rest/binary>>, FieldDef, MsgDef, Defs, Msg) ->
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, NewMsg).

d_uf64(<<Value:64/little, Rest/binary>>, FieldDef, MsgDef, Defs, Msg) ->
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, NewMsg).

d_sf64(<<Value:64/little-signed, Rest/binary>>, FieldDef, MsgDef, Defs, Msg) ->
    NewMsg = add_field(Value, FieldDef, Defs, Msg),
    d_read_field_def(Rest, 0, 0, MsgDef, Defs, NewMsg).

d_packed(<<1:1, X:7, Rest/binary>>, N, Acc, FieldDef, MsgDef, Defs, Msg) ->
    d_packed(Rest, N+1, X bsl (N*7) + Acc, FieldDef, MsgDef, Defs, Msg);
d_packed(<<0:1, X:7, Rest/binary>>, N, Acc, FieldDef, MsgDef, Defs, Msg) ->
    Len = (X bsl (N*7) + Acc),
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    #field{type=Type, rnum=RNum} = FieldDef,
    AccSeq = element(RNum, Msg),
    NewSeq = case Type of
                 fixed32  -> dpfixed32(PackedBytes, AccSeq);
                 sfixed32 -> dpsfixed32(PackedBytes, AccSeq);
                 float    -> dpfloat(PackedBytes, AccSeq);
                 fixed64  -> dpfixed64(PackedBytes, AccSeq);
                 sfixed64 -> dpsfixed64(PackedBytes, AccSeq);
                 double   -> dpdouble(PackedBytes, AccSeq);
                 _        -> dpvi(PackedBytes, 0, 0, Type, AccSeq, Defs)
             end,
    NewMsg = setelement(RNum, Msg, NewSeq),
    d_read_field_def(Rest2, 0, 0, MsgDef, Defs, NewMsg).

uint32_to_int32(N) ->
    <<Result:32/signed-native>> = <<N:32/unsigned-native>>,
    Result.

uint64_to_int64(N) ->
    <<Result:64/signed-native>> = <<N:64/unsigned-native>>,
    Result.

dpvi(<<1:1, X:7, Rest/binary>>, N, Acc, Type, AccSeq, Defs) ->
    dpvi(Rest, N+1, X bsl (N*7) + Acc, Type, AccSeq, Defs);
dpvi(<<0:1, X:7, Rest/binary>>, N, Acc, Type, AccSeq, Defs) ->
    BValue = X bsl (N*7) + Acc,
    Value =
        case Type of
            sint32 -> decode_zigzag(BValue);
            sint64 -> decode_zigzag(BValue);
            int32  -> uint32_to_int32(BValue);
            int64  -> uint64_to_int64(BValue);
            uint32 -> BValue;
            uint64 -> BValue;
            bool   -> BValue =/= 0;
            {enum,_}=Key ->
                EnumValue = uint32_to_int32(BValue),
                {Key, EnumValues} = lists:keyfind(Key, 1, Defs),
                {EnumName, EnumValue} = lists:keyfind(EnumValue, 2, EnumValues),
                EnumName
        end,
    dpvi(Rest, 0, 0, Type, [Value | AccSeq], Defs);
dpvi(<<>>, 0, 0, _Type, AccSeq, _Defs) ->
    AccSeq.

dpfixed32(<<Value:32/little, Rest/binary>>, AccSeq) ->
    dpfixed32(Rest, [Value | AccSeq]);
dpfixed32(<<>>, AccSeq) ->
    AccSeq.

dpsfixed32(<<Value:32/little-signed, Rest/binary>>, AccSeq) ->
    dpsfixed32(Rest, [Value | AccSeq]);
dpsfixed32(<<>>, AccSeq) ->
    AccSeq.

dpfixed64(<<Value:64/little, Rest/binary>>, AccSeq) ->
    dpfixed64(Rest, [Value | AccSeq]);
dpfixed64(<<>>, AccSeq) ->
    AccSeq.

dpsfixed64(<<Value:64/little-signed, Rest/binary>>, AccSeq) ->
    dpsfixed64(Rest, [Value | AccSeq]);
dpsfixed64(<<>>, AccSeq) ->
    AccSeq.

dpfloat(<<Value:32/little-float, Rest/binary>>, AccSeq) ->
    dpfloat(Rest, [Value | AccSeq]);
dpfloat(<<>>, AccSeq) ->
    AccSeq.

dpdouble(<<Value:64/little-float, Rest/binary>>, AccSeq) ->
    dpdouble(Rest, [Value | AccSeq]);
dpdouble(<<>>, AccSeq) ->
    AccSeq.

decode_wiretype(0) -> varint;
decode_wiretype(1) -> bits64;
decode_wiretype(2) -> length_delimited;
decode_wiretype(5) -> bits32.

add_field(Value, FieldDef, MsgDefs, Record) ->
    %% FIXME: what about bytes?? "For numeric types and strings, if
    %% the same value appears multiple times, the parser accepts the
    %% last value it sees." But what about bytes?
    %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
    %% For now, we assume it works like strings.
    case FieldDef of
        #field{rnum = RNum, occurrence = required, type = {msg,_FMsgName}} ->
            merge_field(RNum, Value, Record, MsgDefs);
        #field{rnum = RNum, occurrence = optional, type = {msg,_FMsgName}} ->
            merge_field(RNum, Value, Record, MsgDefs);
        #field{rnum = RNum, occurrence = required}->
            setelement(RNum, Record, Value);
        #field{rnum = RNum, occurrence = optional}->
            setelement(RNum, Record, Value);
        #field{rnum = RNum, occurrence = repeated} ->
            append_to_element(RNum, Value, Record)
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

merge_msgs(PrevMsg, NewMsg, MsgDefs)
  when element(1,PrevMsg) == element(1,NewMsg) ->
    MsgName = element(1, NewMsg),
    MsgDef = keyfetch({msg,MsgName}, MsgDefs),
    lists:foldl(
      fun(#field{rnum=RNum, occurrence=repeated}, AccRecord) ->
              PrevSeq = element(RNum, AccRecord),
              NewSeq  = element(RNum, NewMsg),
              setelement(RNum, AccRecord, PrevSeq ++ NewSeq);
         (#field{rnum=RNum, type={msg,_FieldMsgName}}, AccRecord) ->
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
         (#field{rnum=RNum}, AccRecord) ->
              case element(RNum, NewMsg) of
                  undefined -> AccRecord;
                  NewValue  -> setelement(RNum, AccRecord, NewValue)
              end
      end,
      PrevMsg,
      MsgDef).


encode_msg(Msg, MsgDefs) ->
    MsgName = element(1, Msg),
    MsgDef = keyfetch({msg, MsgName}, MsgDefs),
    encode_2(MsgDef, Msg, MsgDefs, <<>>).

encode_2([Field | Rest], Msg, MsgDefs, Acc) ->
    EncodedField =
        case {Field#field.occurrence, Field#field.is_packed} of
            {repeated, true} ->
                encode_packed(Field, Msg, MsgDefs);
            _ ->
                encode_field(Field, Msg, MsgDefs)
        end,
    encode_2(Rest, Msg, MsgDefs, <<Acc/binary, EncodedField/binary>>);
encode_2([], _Msg, _MsgDefs, Acc) ->
    Acc.

encode_packed(#field{rnum=RNum, fnum=FNum, type=Type}, Msg, MsgDefs) ->
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

encode_field(#field{rnum=RNum, fnum=FNum, type=Type, occurrence=required},
             Msg, MsgDefs) ->
    encode_field_value(element(RNum, Msg), FNum, Type, MsgDefs);
encode_field(#field{rnum=RNum, fnum=FNum, type=Type, occurrence=optional},
             Msg, MsgDefs) ->
    case element(RNum, Msg) of
        undefined -> <<>>;
        Value     -> encode_field_value(Value, FNum, Type, MsgDefs)
    end;
encode_field(#field{rnum=RNum, fnum=FNum, type=Type, occurrence=repeated},
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
    encode_varint((FNum bsl 3) bor encode_wire_type(Type)).

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
                    <<N:32/unsigned-native>> = <<Value:32/signed-native>>,
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
            <<Value:32/float-little>>
    end.


encode_wire_type(sint32)            -> 0;
encode_wire_type(sint64)            -> 0;
encode_wire_type(int32)             -> 0;
encode_wire_type(int64)             -> 0;
encode_wire_type(uint32)            -> 0;
encode_wire_type(uint64)            -> 0;
encode_wire_type(bool)              -> 0;
encode_wire_type({enum, _EnumName}) -> 0;
encode_wire_type(fixed64)           -> 1;
encode_wire_type(sfixed64)          -> 1;
encode_wire_type(double)            -> 1;
encode_wire_type(string)            -> 2;
encode_wire_type(bytes)             -> 2;
encode_wire_type({msg,_MsgName})    -> 2;
encode_wire_type(fixed32)           -> 5;
encode_wire_type(sfixed32)          -> 5;
encode_wire_type(float)             -> 5.


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
            verify_msg2(Msg, MsgName, MsgDefs, [MsgName]);
        false ->
            mk_type_error(undefined_msg, MsgName, [])
    end;
verify_msg(Msg, _MsgDefs) ->
    mk_type_error(expected_a_message, Msg, []).

%% Verify that Msg is actually a message named MsgName as defined in MsgDefs
verify_msg2(Msg, MsgName, MsgDefs, Path) when is_tuple(Msg),
                                              element(1, Msg) == MsgName ->
    MsgKey = {msg, MsgName},
    {value, {MsgKey, Fields}} = lists:keysearch(MsgKey, 1, MsgDefs),
    if tuple_size(Msg) == length(Fields) + 1 ->
            verify_fields(Msg, Fields, Path, MsgDefs);
       true ->
            mk_type_error({bad_record,MsgName}, Msg, Path)
    end;
verify_msg2(V, MsgName, _MsgDefs, Path) ->
    mk_type_error({bad_msg, MsgName}, V, Path).

verify_fields(Msg, Fields, Path, MsgDefs) when tuple_size(Msg)
                                               == length(Fields) + 1 ->
    lists:foreach(
      fun(#field{name=Name, type=Type, rnum=RNum, occurrence=Occurrence}) ->
              Value = element(RNum, Msg),
              verify_value(Value, Type, Occurrence, Path++[Name], MsgDefs)
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
verify_value_2(V, {msg,M}, Path, MsgDefs)   -> verify_msg2(V, M, MsgDefs, Path).

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

verify_string(V, Path) when is_list(V) ->
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

keyfetch(Key, KVPairs) ->
    case lists:keysearch(Key, 1, KVPairs) of
        {value, {Key, Value}} ->
            Value;
        false ->
            erlang:error({error, {no_such_key, Key, KVPairs}})
    end.


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

encode_varint_test() ->
    <<0>>      = encode_varint(0),
    <<127>>    = encode_varint(127),
    <<128, 1>> = encode_varint(128),
    <<150, 1>> = encode_varint(150).
