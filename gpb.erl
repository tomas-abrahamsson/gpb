-module(gpb).
-compile(export_all).
-export([decode_msg/3]).
-include_lib("eunit/include/eunit.hrl").

-record(field,
        {name, fnum, type, multiplicity, opts}).

decode_msg(MsgName, MsgDefs, Bin) ->
    Msg    = new_default_msg(MsgName, MsgDefs),
    MsgDef = keyfetch(MsgName, MsgDefs),
    decode_field(MsgDef, MsgDefs, Bin, Msg).

new_default_msg(MsgName, MsgDefs) ->
    MsgDef = keyfetch(MsgName, MsgDefs),
    {MsgName, lists:map(fun(#field{name=FName, multiplicity=repeated}) ->
                                {FName, []};
                           (#field{name=FName, type={msg,FieldMsgName}}) ->
                                {FName, new_default_msg(FieldMsgName, MsgDefs)};
                           (#field{name=FName}) ->
                                {FName, undefined}
                        end,
                        MsgDef)}.

decode_field(MsgDef, MsgDefs, Bin, Msg) when size(Bin) > 0 ->
    {Key, Rest} = decode_varint(Bin),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    case {WireType, lists:keyfind(FieldNum, #field.fnum, MsgDef)} of
        {0, #field{type=sint32} = FieldDef} ->
            add_zigzag32_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type=sint64} = FieldDef} ->
            add_zigzag64_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type=int32} = FieldDef} ->
            add_2compl32_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type=int64} = FieldDef} ->
            add_2compl64_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type=uint32} = FieldDef} ->
            add_unsigned32_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type=uint64} = FieldDef} ->
            add_unsigned64_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type=bool} = FieldDef} ->
            add_bool_varint(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, #field{type={enum,EnumName}} = FieldDef} ->
            add_enum_varint(Rest, EnumName, FieldDef, MsgDef, MsgDefs, Msg);
        {1, #field{type=fixed64} = FieldDef} ->
            add_unsigned64(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {1, #field{type=sfixed64} = FieldDef} ->
            add_signed64(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {1, #field{type=double} = FieldDef} ->
            add_double(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {2, #field{type=string} = FieldDef} ->
            add_string(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {2, #field{type=bytes} = FieldDef} ->
            add_bytes(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {2, #field{type={msg,SubMsgName}} = FieldDef} ->
            add_embedded_msg(Rest, SubMsgName, FieldDef, MsgDef, MsgDefs, Msg);
        {2, #field{type=packed_repeated} = FieldDef} ->
            add_packed_repeated(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {5, #field{type=fixed32} = FieldDef} ->
            add_unsigned32(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {5, #field{type=sfixed32} = FieldDef} ->
            add_signed32(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {5, #field{type=float} = FieldDef} ->
            add_float(Rest, FieldDef, MsgDef, MsgDefs, Msg);
        {0, false} ->
            skip_varint(Rest, MsgDef, MsgDefs, Msg);
        {1, false} ->
            skip_64bits(Rest, MsgDef, MsgDefs, Msg);
        {2, false} ->
            skip_length_delimited(Rest, MsgDef, MsgDefs, Msg);
        {5, false} ->
            skip_32bits(Rest, MsgDef, MsgDefs, Msg)
    end;
decode_field(MsgDef, _MsgDefs, <<>>, {MsgName, Fields}) ->
    %% Reverse any repeated fields.
    RepeatedFNames = [N || #field{name=N, multiplicity=repeated} <- MsgDef],
    {MsgName, lists:foldl(fun(FName, Acc) ->
                                  OldValue = keyfetch(FName, Acc),
                                  NewValue = lists:reverse(OldValue),
                                  replace_field(FName, NewValue, Acc)
                          end,
                          Fields,
                          RepeatedFNames)}.

add_zigzag32_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    {NV, Rest} = decode_varint(Bin),
    N = decode_zigzag(NV),
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_zigzag64_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    {NV, Rest} = decode_varint(Bin),
    N = decode_zigzag(NV),
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_2compl32_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    %% This doc says: "If you use int32 or int64 as the type for a
    %% negative number, the resulting varint is always ten bytes long --
    %% it is, effectively, treated like a very large unsigned integer."
    %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
    add_2compl64_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg).

add_2compl64_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    {NV, Rest} = decode_varint(Bin),
    <<N:64/signed>> = <<NV:64>>,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_unsigned32_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    {N, Rest} = decode_varint(Bin),
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_unsigned64_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    {N, Rest} = decode_varint(Bin),
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_bool_varint(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    {N, Rest} = decode_varint(Bin),
    add_field(N =/= 0, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_enum_varint(_Bin, _EnumName, _FieldDef, _MsgDef, _MsgDefs, _Msg) -> fixme.

add_unsigned64(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    <<N:64/little, Rest/binary>> = Bin,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_signed64(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    <<N:64/little-signed, Rest/binary>> = Bin,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_double(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    <<N:64/little-float, Rest/binary>> = Bin,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_string(_Bin, _FieldDef, _MsgDef, _MsgDefs, _Msg) -> fixme. % utf8?
add_bytes(_Bin, _FieldDef, _MsgDef, _MsgDefs, _Msg) -> fixme.
add_embedded_msg(_Bin, _SubMsgName, _FieldDef, _MsgDef, _MsgDefs, _Msg) -> fixme.
add_packed_repeated(_Bin, _FieldDef, _MsgDef, _MsgDefs, _Msg) -> fixme.

add_unsigned32(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    <<N:32/little, Rest/binary>> = Bin,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_signed32(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    <<N:32/little-signed, Rest/binary>> = Bin,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

add_float(Bin, FieldDef, MsgDef, MsgDefs, Msg) ->
    <<N:32/little-float, Rest/binary>> = Bin,
    add_field(N, FieldDef, MsgDef, MsgDefs, Msg, Rest).

skip_varint(Bin, MsgDef, MsgDefs, Msg) ->
    {_N, Rest} = decode_varint(Bin),
    decode_field(MsgDef, MsgDefs, Rest, Msg).

skip_64bits(Bin, MsgDef, MsgDefs, Msg) ->
    <<_:64, Rest/binary>> = Bin,
    decode_field(MsgDef, MsgDefs, Rest, Msg).

skip_length_delimited(Bin, MsgDef, MsgDefs, Msg) ->
    {Len, Rest} = decode_varint(Bin),
    <<_:Len/binary, Rest2>> = Rest,
    decode_field(MsgDef, MsgDefs, Rest2, Msg).

skip_32bits(Bin, MsgDef, MsgDefs, Msg) ->
    <<_:32, Rest/binary>> = Bin,
    decode_field(MsgDef, MsgDefs, Rest, Msg).

add_field(Value, FieldDef, MsgDef, MsgDefs, {MsgName, Fields}, Rest) ->
    %% FIXME: what about bytes?? "For numeric types and strings, if
    %% the same value appears multiple times, the parser accepts the
    %% last value it sees." But what about bytes?
    %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
    NewFields =
        case FieldDef of
            #field{name = FName, multiplicity = required, type={msg,FMsgName}}->
                merge_field(FName, Value, Fields, FMsgName, MsgDefs);
            #field{name = FName, multiplicity = optional, type={msg,FMsgName}}->
                merge_field(FName, Value, Fields, FMsgName, MsgDefs);
            #field{name = FName, multiplicity = required}->
                replace_field(FName, Value, Fields);
            #field{name = FName, multiplicity = optional}->
                replace_field(FName, Value, Fields);
            #field{name = FName, multiplicity = repeated} ->
                append_to_field(FName, Value, Fields)
        end,
    decode_field(MsgDef, MsgDefs, Rest, {MsgName, NewFields}).

merge_field(FName, NewMsg, Fields, FMsgName, MsgDefs) ->
    case keyfetch(FName, Fields) of
        undefined ->
            replace_field(FName, NewMsg, Fields);
        {FMsgName, _PrevFields} = PrevMsg ->
            MergedMsg = merge_msg(PrevMsg, NewMsg, MsgDefs),
            replace_field(FName, MergedMsg, Fields)
    end.

merge_msg({MsgName, PrevFields}, {MsgName, NewFields}, MsgDefs) ->
    MsgDef = keyfetch(MsgName, MsgDefs),
    {MsgName,
     lists:foldl(
       fun(#field{name=FName, multiplicity=repeated}, AccFields) ->
               PrevSeq = keyfetch(FName, AccFields),
               NewSeq = keyfetch(FName, NewFields),
               replace_field(FName, append_seqs(PrevSeq, NewSeq), AccFields);
          (#field{name=FName, type={msg,_FieldMsgName}}, AccFields) ->
               PrevMsg = keyfetch(FName,AccFields),
               NewMsg  = keyfetch(FName,NewFields),
               MergedMsg = merge_msg(PrevMsg, NewMsg, MsgDefs),
               replace_field(FName, MergedMsg, AccFields);
          (#field{name=FName}, AccFields) ->
               NewValue = keyfetch(FName, NewFields),
               replace_field(FName, NewValue, AccFields)
       end,
       PrevFields,
       MsgDef)}.

replace_field(FName, Value, Fields) ->
    lists:keystore(FName, 1, Fields, {FName, Value}).

append_to_field(FName, NewElem, Fields) ->
    PrevElems = keyfetch(FName, Fields),
    replace_field(FName, [NewElem | PrevElems], Fields).

append_seqs(Seq1, Seq2) ->
    lists:reverse(Seq2, Seq1).


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

decode_msg_simple_multiplicity_test() ->
    {t1,[{a,undefined}]} =
        gpb:decode_msg(t1,
                       [{t1, [#field{name=a, fnum=1, type=int32,
                                     multiplicity=optional, opts=[]}]}],
                       <<>>),
    {t1,[{a,150}]} =
        gpb:decode_msg(t1,
                       [{t1, [#field{name=a, fnum=1, type=int32,
                                     multiplicity=required, opts=[]}]}],
                       <<8,150,1>>),
    {t1,[{a,[150, 151]}]} =
        gpb:decode_msg(t1,
                       [{t1, [#field{name=a, fnum=1, type=int32,
                                     multiplicity=repeated, opts=[]}]}],
                       <<8,150,1, 8,151,1>>).
