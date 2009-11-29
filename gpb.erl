-module(gpb).
%-compile(export_all).
-export([decode_msg/3]).
-include_lib("eunit/include/eunit.hrl").

%% TODO:
%%
%% * Support records on the erlang side:
%%   let #fields have a record field index number: #record.field
%%   and do setelement instead of keyfetch and replace_field
%%
%% * Add a new_default_msg that sets default values according to
%%   type (and optionalness) as docoumented on the google web:
%%   strings="", booleans=false, integers=0, enums=<first value> and so on.
%%
%%   Message fields can also have default values specified in the .proto file.
%%
%% * Optionally non-crash on type-mismatches spec<-->actual-wire-contents
%%   Ignore/skip instead of crash?
%%
%% * Crash or silent truncation on values out of range?
%%   Example: (1 bsl 33) for an uint32? The bitsyntax silently truncats,
%%   but this has been under debate on the erlang mailing list since it
%%   was unexpected. Related: principle of least astonishment.
-record(field,
        {name, fnum, type, occurrence, opts}).

decode_msg(Bin, MsgName, MsgDefs) ->
    MsgKey = {msg,MsgName},
    Msg    = new_initial_msg(MsgKey, MsgDefs),
    MsgDef = keyfetch(MsgKey, MsgDefs),
    decode_field(Bin, MsgDef, MsgDefs, Msg).

new_initial_msg({msg,MsgName}=MsgKey, MsgDefs) ->
    MsgDef = keyfetch(MsgKey, MsgDefs),
    {MsgName, lists:map(fun(#field{name=FName, occurrence=repeated}) ->
                                {FName, []};
                           (#field{name=FName, type={msg,_Name}=FMsgKey}) ->
                                {FName, new_initial_msg(FMsgKey, MsgDefs)};
                           (#field{name=FName}) ->
                                {FName, undefined}
                        end,
                        MsgDef)}.

decode_field(Bin, MsgDef, MsgDefs, Msg) when size(Bin) > 0 ->
    {Key, Rest} = decode_varint(Bin),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    %% Cleanup: do {NewValue, ...} = case {WireType, ...} of ...
    %% then do the updating here, keeping recursion to be only in this function
    %% thus sending down fewer args to auxiliary subfunctions
    %% Inline subfunctions??
    case lists:keyfind(FieldNum, #field.fnum, MsgDef) of
        false ->
            Rest2 = skip_field(Rest, WireType),
            decode_field(MsgDef, MsgDefs, Rest2, Msg);
        #field{type = FieldType} = FieldDef ->
            {NewValue, Rest2} = decode_type(FieldType, WireType, Rest, MsgDefs),
            NewMsg = add_field(NewValue, FieldDef, MsgDefs, Msg),
            decode_field(Rest2, MsgDef, MsgDefs, NewMsg)
    end;
decode_field(<<>>, MsgDef, _MsgDefs, {MsgName, Fields}) ->
    %% Reverse any repeated fields.
    RepeatedFNames = [N || #field{name=N, occurrence=repeated} <- MsgDef],
    {MsgName, lists:foldl(fun(FName, Acc) ->
                                  OldValue = keyfetch(FName, Acc),
                                  NewValue = lists:reverse(OldValue),
                                  replace_field(FName, NewValue, Acc)
                          end,
                          Fields,
                          RepeatedFNames)}.

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
            <<_:Len/binary, Rest2>> = Rest,
            Rest2;
        bits32 ->
            <<_:32, Rest/binary>> = Bin,
            Rest
    end.

decode_type(FieldType, WireType, Bin, MsgDefs) ->
    case {FieldType, decode_wiretype(WireType)} of
        {sint32, varint} ->
            {NV, T} = decode_varint(Bin),
            {decode_zigzag(NV), T};
        {sint64, varint} ->
            {NV, T} = decode_varint(Bin),
            {decode_zigzag(NV), T};
        {int32, varint} ->
            %% This doc says: "If you use int32 or int64 as the type
            %% for a negative number, the resulting varint is always
            %% ten bytes long -- it is, effectively, treated like a
            %% very large unsigned integer."
            %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
            %%
            %% 10 bytes is what it takes to varint-encode a -1 as a 64
            %% bit signed int. -1 as a 32 bit signed int is only 5 bytes.
            decode_type(int64, WireType, Bin, MsgDefs);
        {int64, varint} ->
            {NV, T} = decode_varint(Bin),
            <<N:64/signed>> = <<NV:64>>,
            {N, T};
        {uint32, varint} ->
            {_N, _Rest} = decode_varint(Bin);
        {uint64, varint} ->
            {_N, _Rest} = decode_varint(Bin);
        {bool, varint} ->
            {N, Rest} = decode_varint(Bin),
            {N =/= 0, Rest};
        {{enum, _EnumName}=Key, varint} ->
            {N, Rest} = decode_varint(Bin),
            {Key, EnumValues} = lists:keyfind(Key, 1, MsgDefs),
            {value, {EnumName, N}} = lists:keysearch(N, 2, EnumValues),
            {EnumName, Rest};
        {fixed64, bits64} ->
            <<N:64/little, Rest/binary>> = Bin,
            {N, Rest};
        {sfixed64, bits64} ->
            <<N:64/little-signed, Rest/binary>> = Bin,
            {N, Rest};
        {double, bits64} ->
            <<N:64/little-float, Rest/binary>> = Bin,
            {N, Rest};
        {string, length_delimited} ->
            {Len, Rest} = decode_varint(Bin),
            <<Utf8Str:Len/binary, Rest2/binary>> = Rest,
            {unicode:characters_to_list(Utf8Str, unicode), Rest2};
        {bytes, length_delimited} ->
            {Len, Rest} = decode_varint(Bin),
            <<Bytes:Len/binary, Rest2/binary>> = Rest,
            {Bytes, Rest2};
        {{msg,MsgName}, length_delimited} ->
            {Len, Rest} = decode_varint(Bin),
            <<MsgBytes:Len/binary, Rest2/binary>> = Rest,
            {decode_msg(MsgBytes, MsgName, MsgDefs), Rest2};
        {packed, length_delimited} ->
            fixme_handle_packed;
        {fixed32, bits32} ->
            <<N:32/little, Rest/binary>> = Bin,
            {N, Rest};
        {sfixed32, bits32} ->
            <<N:32/little-signed, Rest/binary>> = Bin,
            {N, Rest};
        {float, bits32} ->
            <<N:32/little-float, Rest/binary>> = Bin,
            {N, Rest}
    end.

add_field(Value, FieldDef, MsgDefs, {MsgName, Fields}) ->
    %% FIXME: what about bytes?? "For numeric types and strings, if
    %% the same value appears multiple times, the parser accepts the
    %% last value it sees." But what about bytes?
    %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
    %% For now, we assume it works like strings.
    {MsgName,
     case FieldDef of
         #field{name = FName, occurrence = required, type = {msg, FMsgName}} ->
             merge_field(FName, Value, Fields, FMsgName, MsgDefs);
         #field{name = FName, occurrence = optional, type = {msg, FMsgName}} ->
             merge_field(FName, Value, Fields, FMsgName, MsgDefs);
         #field{name = FName, occurrence = required}->
             replace_field(FName, Value, Fields);
         #field{name = FName, occurrence = optional}->
             replace_field(FName, Value, Fields);
         #field{name = FName, occurrence = repeated} ->
             append_to_field(FName, Value, Fields)
     end}.

merge_field(FName, NewMsg, Fields, FMsgName, MsgDefs) ->
    case keyfetch(FName, Fields) of
        undefined ->
            replace_field(FName, NewMsg, Fields);
        {FMsgName, _PrevFields} = PrevMsg ->
            MergedMsg = merge_msg(PrevMsg, NewMsg, MsgDefs),
            replace_field(FName, MergedMsg, Fields)
    end.

merge_msg({MsgName, PrevFields}, {MsgName, NewFields}, MsgDefs) ->
    MsgDef = keyfetch({msg,MsgName}, MsgDefs),
    {MsgName,
     lists:foldl(
       fun(#field{name=FName, occurrence=repeated}, AccFields) ->
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

decode_msg_simple_occurrence_test() ->
    {t1,[{a,undefined}]} =
        decode_msg(<<>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=int32,
                                       occurrence=optional, opts=[]}]}]),
    {t1,[{a,150}]} =
        decode_msg(<<8,150,1>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=int32,
                                       occurrence=required, opts=[]}]}]),
    {t1,[{a,[150, 151]}]} =
        decode_msg(<<8,150,1, 8,151,1>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=int32,
                                       occurrence=repeated, opts=[]}]}]),
    ok.

decode_msg_with_enum_field_test() ->
    {t1, [{a,v2}]} =
        decode_msg(<<8,150,1>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, 150}]}]).

decode_msg_with_bool_field_test() ->
    {t1, [{a,true}]} =
        decode_msg(<<8,1>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=bool,
                                       occurrence=required, opts=[]}]}]),
    {t1, [{a,false}]} =
        decode_msg(<<8,0>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=bool,
                                       occurrence=required, opts=[]}]}]).
decode_msg_with_string_field_test() ->
    {t1, [{a,"abc\345\344\366"++[1022]}]} =
        decode_msg(<<10,11,
                    $a,$b,$c,$\303,$\245,$\303,$\244,$\303,$\266,$\317,$\276>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=string,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_bytes_field_test() ->
    {t1, [{a,<<0,0,0,0>>}]} =
        decode_msg(<<10,4,0,0,0,0>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type=bytes,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_sub_msg_field_test() ->
    {t1, [{a, {t2, [{b,150}]}}]} =
        decode_msg(<<10,3, 8,150,1>>,
                   t1,
                   [{{msg,t1}, [#field{name=a, fnum=1, type={msg,t2},
                                       occurrence=required, opts=[]}]},
                    {{msg,t2}, [#field{name=b, fnum=1, type=uint32,
                                       occurrence=required, opts=[]}]}]).
