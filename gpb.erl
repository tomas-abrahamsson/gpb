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
-include_lib("eunit/include/eunit.hrl").
-include("gpb.hrl").

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
    decode_field(Bin, MsgDef, MsgDefs, Msg).

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

decode_field(Bin, MsgDef, MsgDefs, Msg) when byte_size(Bin) > 0 ->
    {Key, Rest} = decode_varint(Bin),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    case lists:keyfind(FieldNum, #field.fnum, MsgDef) of
        false ->
            Rest2 = skip_field(Rest, WireType),
            decode_field(Rest2, MsgDef, MsgDefs, Msg);
        #field{type = FieldType, rnum=RNum, opts=Opts} = FieldDef ->
            case proplists:get_bool(packed, Opts) of
                true ->
                    AccSeq = element(RNum, Msg),
                    {NewSeq, Rest2} = decode_packed(FieldType, Rest, MsgDefs,
                                                   AccSeq),
                    NewMsg = setelement(RNum, Msg, NewSeq),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                false ->
                    {NewValue, Rest2} = decode_type(FieldType, Rest, MsgDefs),
                    NewMsg = add_field(NewValue, FieldDef, MsgDefs, Msg),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg)
            end
    end;
decode_field(<<>>, MsgDef, _MsgDefs, Record0) ->
    %% Reverse any repeated fields, but only on the top-level, not recursively.
    RepeatedRNums = [N || #field{rnum=N, occurrence=repeated} <- MsgDef],
    lists:foldl(fun(RNum, Record) ->
                        OldValue = element(RNum, Record),
                        ReversedField = lists:reverse(OldValue),
                        setelement(RNum, Record, ReversedField)
                end,
                Record0,
                RepeatedRNums).

decode_wiretype(0) -> varint;
decode_wiretype(1) -> bits64;
decode_wiretype(2) -> length_delimited;
decode_wiretype(5) -> bits32.

skip_field(Bin, WireType) ->
    case decode_wiretype(WireType) of
        varint ->
            {_N, Rest} = decode_varint(Bin),
            Rest;
        bits64 ->
            <<_:64, Rest/binary>> = Bin,
            Rest;
        length_delimited ->
            {Len, Rest} = decode_varint(Bin),
            <<_:Len/binary, Rest2/binary>> = Rest,
            Rest2;
        bits32 ->
            <<_:32, Rest/binary>> = Bin,
            Rest
    end.

decode_packed(FieldType, Bin, MsgDefs, Seq0) ->
    {Len, Rest} = decode_varint(Bin),
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
            {NV, T} = decode_varint(Bin),
            {decode_zigzag(NV), T};
        sint64 ->
            {NV, T} = decode_varint(Bin),
            {decode_zigzag(NV), T};
        int32 ->
            {NV, T} = decode_varint(Bin),
            <<N:32/signed>> = <<NV:32>>,
            {N, T};
        int64 ->
            {NV, T} = decode_varint(Bin),
            <<N:64/signed>> = <<NV:64>>,
            {N, T};
        uint32 ->
            {_N, _Rest} = decode_varint(Bin);
        uint64 ->
            {_N, _Rest} = decode_varint(Bin);
        bool ->
            {N, Rest} = decode_varint(Bin),
            {N =/= 0, Rest};
        {enum, _EnumName}=Key ->
            {N, Rest} = decode_type(int32, Bin, MsgDefs),
            {Key, EnumValues} = lists:keyfind(Key, 1, MsgDefs),
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
            {Len, Rest} = decode_varint(Bin),
            <<Utf8Str:Len/binary, Rest2/binary>> = Rest,
            {unicode:characters_to_list(Utf8Str, unicode), Rest2};
        bytes ->
            {Len, Rest} = decode_varint(Bin),
            <<Bytes:Len/binary, Rest2/binary>> = Rest,
            {Bytes, Rest2};
        {msg,MsgName} ->
            {Len, Rest} = decode_varint(Bin),
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
            {N, Rest}
    end.

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
        case {Field#field.occurrence, lists:member(packed,Field#field.opts)} of
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
            {Key, EnumValues} = lists:keyfind(Key, 1, MsgDefs),
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




decode_varint(Bin) -> de_vi(Bin, 0, 0).

de_vi(<<1:1, X:7, Rest/binary>>, N, Acc) -> de_vi(Rest, N+1, X bsl (N*7) + Acc);
de_vi(<<0:1, X:7, Rest/binary>>, N, Acc) -> {X bsl (N*7) + Acc, Rest}.


encode_varint(N) -> en_vi(N).

en_vi(N) when N =< 127 -> <<N>>;
en_vi(N) when N >= 128 -> <<1:1, (N band 127):7, (en_vi(N bsr 7))/binary>>.


decode_zigzag(N) when N band 1 =:= 0 -> N bsr 1;        %% N is even
decode_zigzag(N) when N band 1 =:= 1 -> -((N+1) bsr 1). %% N is odd

encode_zigzag(N) when N >= 0 -> N * 2;
encode_zigzag(N) when N <  0 -> N * -2 - 1.



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

decode_varint_test() ->
    {0, <<255>>}   = decode_varint(<<0,255>>),
    {127, <<255>>} = decode_varint(<<127,255>>),
    {128, <<255>>} = decode_varint(<<128, 1, 255>>),
    {150, <<255>>} = decode_varint(<<150, 1, 255>>).


-record(m1,{a}).

skipping_unknown_varint_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<32,150,1>>, %% field number 4 (not known), wire type = 0
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).

skipping_unknown_length_delimited_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<34,1,1>>, %% field number 4 (not known), wire type = 2
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).

skipping_unknown_64bit_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<33,0,0,0,0,0,0,0,0>>, %% field number 4, wire type = 1
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).
skipping_unknown_32bit_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<37,0,0,0,0>>, %% field number 4, wire type = 5
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).

decode_msg_simple_occurrence_test() ->
    #m1{a = undefined} =
        decode_msg(<<>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]),
    #m1{a = 150} =
        decode_msg(<<8,150,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=required, opts=[]}]}]),
    #m1{a = [150, 151]} =
        decode_msg(<<8,150,1, 8,151,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[]}]}]),
    ok.

decode_msg_with_enum_field_test() ->
    #m1{a = v2} =
        decode_msg(<<8,150,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, 150}]}]).

decode_msg_with_negative_enum_value_test() ->
    #m1{a = v2} =
        decode_msg(<<8, 254,255,255,255,15>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, -2}]}]).

decode_msg_with_bool_field_test() ->
    #m1{a = true} =
        decode_msg(<<8,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]),
    #m1{a = false} =
        decode_msg(<<8,0>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]).

decoding_float_test() ->
    %% Stole idea from the python test in google-protobuf:
    %% 1.125 is perfectly representable as a float (no rounding error).
    #m1{a = 1.125} =
        decode_msg(<<13,0,0,144,63>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=float,
                                       occurrence=required, opts=[]}]}]).

decoding_double_test() ->
    #m1{a = 1.125} =
        decode_msg(<<9,0,0,0,0,0,0,242,63>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=double,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_string_field_test() ->
    #m1{a = "abc\345\344\366"++[1022]} =
        decode_msg(<<10,11,
                    $a,$b,$c,$\303,$\245,$\303,$\244,$\303,$\266,$\317,$\276>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=string,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_bytes_field_test() ->
    #m1{a = <<0,0,0,0>>} =
        decode_msg(<<10,4,0,0,0,0>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bytes,
                                       occurrence=required, opts=[]}]}]).

-record(m2, {b}).

decode_msg_with_sub_msg_field_test() ->
    #m1{a = #m2{b = 150}} =
        decode_msg(<<10,3, 8,150,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=required, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_optional_nonpresent_sub_msg_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=optional, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=required, opts=[]}]}]).

decoding_zero_instances_of_packed_varints_test() ->
    %%    "A packed repeated field containing zero elements does not
    %%     appear in the encoded message."
    %%    -- http://code.google.com/apis/protocolbuffers/docs/encoding.html
    #m1{a = []} =
        decode_msg(<<>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[packed]}]}]).

decoding_one_packed_chunk_of_varints_test() ->
    #m1{a = [3, 270, 86942]} =
        decode_msg(<<16#22,                 % tag (field number 4, wire type 2)
                     16#06,                 % payload size (6 bytes)
                     16#03,                 % first element (varint 3)
                     16#8E, 16#02,          % second element (varint 270)
                     16#9E, 16#a7, 16#05>>, % third element (varint 86942)
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=4, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[packed]}]}]).

decoding_two_packed_chunks_of_varints_test() ->
    %%    "Note that although there's usually no reason to encode more
    %%     than one key-value pair for a packed repeated field, encoders
    %%     must be prepared to accept multiple key-value pairs. In this
    %%     case, the payloads should be concatenated. Each pair must
    %%     contain a whole number of elements."
    %%    -- http://code.google.com/apis/protocolbuffers/docs/encoding.html
    #m1{a = [3, 270, 86942, 4, 271, 86943]} =
        decode_msg(<<16#22, 16#06, 16#03, 16#8E, 16#02, 16#9E, 16#a7, 16#05,
                     16#22, 16#06, 16#04, 16#8F, 16#02, 16#9F, 16#a7, 16#05>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=4, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[packed]}]}]),
    ok.

encode_required_varint_field_test() ->
    <<8,150,1>> =
        encode_msg(#m1{a=150},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=required, opts=[]}]}]).

encode_optional_varint_field_test() ->
    <<>> =
        encode_msg(#m1{a=undefined},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=optional, opts=[]}]}]).

encode_repeated_empty_field_test() ->
    <<>> =
        encode_msg(#m1{a=[]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[packed]}]}]),
    <<>> =
        encode_msg(#m1{a=[]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[]}]}]).

encode_repeated_nonempty_field_test() ->
    <<10,4, 150,1, 151,1>> =
        encode_msg(#m1{a=[150,151]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[packed]}]}]),
    <<8,150,1, 8,151,1>> =
        encode_msg(#m1{a=[150,151]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[]}]}]).

encode_msg_with_sub_msg_field_test() ->
    <<10,3, 8,150,1>> =
        encode_msg(#m1{a = #m2{b = 150}},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=required, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=required, opts=[]}]}]).

encode_msg_with_enum_field_test() ->
    <<8,150,1>> =
        encode_msg(#m1{a = v2},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, 150}]}]).

encode_msg_with_negative_enum_value_test() ->
    <<8, 254,255,255,255,15>> =
        encode_msg(#m1{a = v2},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, -2}]}]).

encode_msg_with_bool_field_test() ->
    <<8,1>> =
        encode_msg(#m1{a = true},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]),
    <<8,0>> =
        encode_msg(#m1{a = false},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]).

encode_float_test() ->
    %% Stole idea from the python test in google-protobuf:
    %% 1.125 is perfectly representable as a float (no rounding error).
    <<13,0,0,144,63>> =
        encode_msg(#m1{a = 1.125},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=float,
                                       occurrence=required, opts=[]}]}]).

encode_packed_repeated_bools_test() ->
    <<16#22,1,1>> =
        encode_msg(#m1{a=[true]},
                   [{{msg,m1},[#field{name=a,fnum=4,rnum=#m1.a, type=bool,
                                      occurrence=repeated, opts=[packed]}]}]).
decode_packed_repeated_bools_test() ->
    #m1{a=[true]} =
        decode_msg(<<16#22,1,1>>,
                   m1,
                   [{{msg,m1},[#field{name=a,fnum=4,rnum=#m1.a, type=bool,
                                      occurrence=repeated, opts=[packed]}]}]).



encode_double_test() ->
    <<9,0,0,0,0,0,0,242,63>> =
        encode_msg(#m1{a = 1.125},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=double,
                                       occurrence=required, opts=[]}]}]).


merging_second_required_integer_overrides_first_test() ->
    #m1{a=20} = merge_msgs(#m1{a=10}, #m1{a=20},
                           [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                              occurrence=required,opts=[]}]}]).

merging_second_optional_integer_overrides_undefined_test() ->
    #m1{a=22} = merge_msgs(#m1{a=undefined}, #m1{a=22},
                           [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                              occurrence=optional,opts=[]}]}]).

merging_undefined_does_not_overrides_defined_integer_test() ->
    #m1{a=25} = merge_msgs(#m1{a=25}, #m1{a=undefined},
                           [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                              occurrence=optional,opts=[]}]}]).

merging_sequences_test() ->
    #m1{a=[11,12, 21,22]} =
        merge_msgs(#m1{a=[11,12]}, #m1{a=[21,22]},
                   [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                      occurrence=repeated,opts=[]}]}]).

-record(m4, {x,y}).

merging_messages_recursively_test() ->
    #m1{a=#m4{x = 210,
              y = [111, 112, 211, 212]}} =
        merge_msgs(#m1{a = #m4{x = 110,
                               y = [111, 112]}},
                   #m1{a = #m4{x = 210,
                               y = [211, 212]}},
                   [{{msg,m1}, [#field{name=a,fnum=1, rnum=#m1.a,
                                       type={msg,m4},
                                       occurrence=required, opts=[]}]},
                    {{msg,m4}, [#field{name=x, fnum=1, rnum=#m4.x, type=uint32,
                                       occurrence=optional, opts=[]},
                                #field{name=y, fnum=2, rnum=#m4.y, type=uint32,
                                       occurrence=repeated, opts=[]}]}]).

merging_optional_messages_recursively1_test() ->
    #m1{a=#m2{b = 110}} =
        merge_msgs(#m1{a = #m2{b = 110}},
                   #m1{a = undefined},
                   [{{msg,m1}, [#field{name=a,fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=optional, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=optional, opts=[]}]}]).

merging_optional_messages_recursively2_test() ->
    #m1{a=#m2{b = 210}} =
        merge_msgs(#m1{a = undefined},
                   #m1{a = #m2{b = 210}},
                   [{{msg,m1}, [#field{name=a,fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=optional, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=optional, opts=[]}]}]).
