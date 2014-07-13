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
-export([encode_varint/1, decode_varint/1, decode_varint/2]).
-export([encode_wiretype/1, decode_wiretype/1]).
-export([version_as_string/0, version_as_list/0]).
-export([field_records_to_proplists/1, proplists_to_field_records/1]).
-export([field_record_to_proplist/1,   proplist_to_field_record/1]).
-export([defs_records_to_proplists/1,  proplists_to_defs_records/1]).
-include_lib("eunit/include/eunit.hrl").
-include("gpb.hrl").

%% TODO
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


%% Valid version format is:
%%      <n>.<m>            % e.g. 2.1, 2.1.1, etc (any number of dots and ints)
%%
%% The format is what `git describe --always --tags' produces,
%% given that all tags are always on the format <n>.<m>.
version_as_string() ->
    application:load(gpb),
    {ok, Vsn} = application:get_key(gpb, vsn),
    assert_version_format(Vsn),
    Vsn.

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
    {Key, Rest} = decode_varint(Bin, 32),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    case lists:keysearch(FieldNum, #field.fnum, MsgDef) of
        false ->
            Rest2 = skip_field(Rest, WireType),
            decode_field(Rest2, MsgDef, MsgDefs, Msg);
        {value, #field{type = FieldType, rnum=RNum} = FieldDef} ->
            case fielddef_matches_wiretype_get_packed(WireType, FieldDef) of
                {yes,true} ->
                    AccSeq = element(RNum, Msg),
                    {NewSeq, Rest2} = decode_packed(FieldType, Rest, MsgDefs,
                                                   AccSeq),
                    NewMsg = setelement(RNum, Msg, NewSeq),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                {yes,false} ->
                    {NewValue, Rest2} = decode_type(FieldType, Rest, MsgDefs),
                    NewMsg = add_field(NewValue, FieldDef, MsgDefs, Msg),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                no ->
                    Rest2 = skip_field(Rest, WireType),
                    decode_field(Rest2, MsgDef, MsgDefs, Msg)
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

fielddef_matches_wiretype_get_packed(WireType, #field{type=Type}=FieldDef) ->
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
        case {Field#field.occurrence, is_packed(Field)} of
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
encode_wiretype(float)             -> 5.


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

%% --

%% Conversion functions between various forms of #field{} and a proplist
%% with keys being the #field{} record's field names.

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
    [field_record_to_proplist(F) || F <- Fields].

field_record_to_proplist(#field{}=F) ->
    Names = record_info(fields, field),
    lists:zip(Names, tl(tuple_to_list(F))).

proplists_to_field_records(PLs) ->
    [proplist_to_field_record(PL) || PL <- PLs].

proplist_to_field_record(PL) when is_list(PL) ->
    Names = record_info(fields, field),
    list_to_tuple([field | [proplists:get_value(Name, PL) || Name <- Names]]).

%% --

is_packed(#field{opts=Opts}) ->
    lists:member(packed, Opts).

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
