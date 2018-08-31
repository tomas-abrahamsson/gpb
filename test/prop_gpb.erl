%%% Original author: Thomas Arts <thomas.arts@quviq.com>
%%% Further developed by: Tomas Abrahamsson <tab@lysator.liu.se>
%%% Translated to PropEr from EQC by: Pierre Fenoll <pierrefenoll@gmail.com>
%%% Description : Testing protocol buffer implemented by Tomas Abrahamsson
%%% Created : 12 May 2010 by Thomas Arts
-module(prop_gpb).

-ifndef('NO_HAVE_PROPER').

%% -define(PROPER_NO_TRANS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-define(MOD1, gpb_prop_mod1).
-define(MOD2, gpb_prop_mod2).
-export([any_tr_merge/2
        ,any_tr_pack_m/1
        ,any_tr_pack_r/1
        ,any_tr_unpack_m/1
        ,any_tr_unpack_r/1
        ,any_tr_verify/2
        ,install_msg_defs/2 % for debugging purposes
        ]).

-define(f(Fmt, Args), io_lib:format(Fmt, Args)).

message_defs() ->
    message_defs([]).

message_defs(Opts) ->
    %% Can we have messages that refer to themselves?
    %% Actually not if field is required, since then we cannot generate
    %% a message of that kind.
    %% left_of/1 guarantees that the messages only refer to earlier definitions
    %% Enums are globally unique. Hence, we generate them globally
    DoAny = not lists:member(no_any, Opts),
    ?LET({MsgNames,Any}, {non_empty(ulist("m")),
                          elements([[], ['google.protobuf.Any' || DoAny]])},
         ?LET(EnumDefs,enums(),
              begin
                  AnyDef = [mk_anymsg_def() || Any /= []],
                  EnumNames = [EName || {{enum,EName},_}<-EnumDefs],
                  Enum0Names = [EName || {{enum,EName},[{_,0}|_]}<-EnumDefs],
                 shuffle(EnumDefs
                          ++ AnyDef
                          ++ [{{msg,Msg},message_fields(
                                           left_of(Msg,Any++MsgNames),
                                           EnumNames, Enum0Names,
                                           Opts)}
                              || Msg<-MsgNames])
              end)).

mk_anymsg_def() ->
    {{msg,'google.protobuf.Any'},
     [#?gpb_field{name=type_url, type=string,
                  fnum=1, rnum=2, occurrence=required, opts=[]},
      #?gpb_field{name=value, type=bytes,
                  fnum=2, rnum=3, occurrence=required, opts=[]}]}.

%% Take all values left of a certain value
left_of(X, Xs) ->
    lists:takewhile(fun(Y) -> Y /= X end, Xs).

message_fields(MsgNames, EnumNames, Enum0Names, Opts) ->
    %% can we have definitions without any field?
    WithMapFields = not lists:member(no_maps, Opts),
    WithOneofFields = not lists:member(no_oneof, Opts),
    ?LET({FieldDefs, FNumBase0},
         {non_empty(
            list({elements([{required, msg_field_type(MsgNames, EnumNames)}
                           ,{optional, msg_field_type(MsgNames, EnumNames)}
                           ,{repeated, msg_field_type(MsgNames, EnumNames)}
                           ] ++ [{oneof, oneof_fields(MsgNames, Enum0Names)}
                                 || WithOneofFields]
                           ++ [{repeated, map_field(MsgNames, Enum0Names)}
                               || WithMapFields])
                 ,field_name()
                 }))
         ,uint(10)
         },
         mk_fields(FieldDefs, FNumBase0+1)).

field_name() ->
    elements([a,b,c,field1,f]).

oneof_fields(MsgNames, EnumNames) ->
    ?LET({FieldDefs, FNumBase0},
         {non_empty(
            list({elements([{optional, msg_field_type(MsgNames, EnumNames)}]),
                  field_name()})),
          uint(10)},
         mk_fields(FieldDefs, FNumBase0+1)).

map_field(MsgNames, EnumNames) ->
    ?LET({KeyType,ValueType},
         {elements(map_key_types()), msg_field_type(MsgNames,EnumNames)},
         {map, KeyType, ValueType}
        ).

msg_field_type([], []) ->
    elements(basic_msg_field_types());
msg_field_type([], EnumNames) ->
    ?LET(EnumName,elements(EnumNames),
         elements(basic_msg_field_types() ++ [{enum, EnumName}]));
msg_field_type(MsgNames, []) ->
    ?LET(MsgName,elements(MsgNames),
         elements(basic_msg_field_types() ++ [{'msg',MsgName}]));
msg_field_type(MsgNames, EnumNames) ->
    ?LET({MsgName, EnumName}, {elements(MsgNames), elements(EnumNames)},
         elements(basic_msg_field_types() ++
                  [{enum, EnumName}, {'msg',MsgName}])).

basic_msg_field_types() ->
    [bool,sint32,sint64,int32,int64,uint32,
     uint64,
     fixed64,sfixed64,double,
     fixed32,
     sfixed32,
     float,
     bytes,
     string
    ].

map_key_types() ->
    [bool,sint32,sint64,int32,int64,uint32,
     uint64,
     fixed64,sfixed64,
     fixed32,
     sfixed32,
     string
    ].

mk_fields(FieldDefs, FNumBase) ->
    UFieldDefs = keyunique(2, FieldDefs),
    {Fields, _NextFNum} =
        lists:mapfoldl(
          fun({{{required, Type}, FieldName}, RNum}, FNum) ->
                  {#?gpb_field{name=FieldName, fnum=FNum, rnum=RNum,
                               type=Type, occurrence=required, opts=[]},
                   FNum+1};
             ({{{optional, Type}, FieldName}, RNum}, FNum) ->
                  {#?gpb_field{name=FieldName, fnum=FNum, rnum=RNum,
                               type=Type, occurrence=optional, opts=[]},
                   FNum+1};
             ({{{repeated, Type}, FieldName}, RNum}, FNum) ->
                  Opts = case Type of
                             {map,_,_} -> [];
                             {msg,_} -> [];
                             string  -> [];
                             bytes   -> [];
                             _       -> elements([[], [packed]])
                         end,
                  {#?gpb_field{name=FieldName, fnum=FNum, rnum=RNum,
                               type=Type, occurrence=repeated, opts=Opts},
                   FNum+1};
             ({{{oneof, OFields}, FieldName}, RNum}, FNum) ->
                  %% Oneof fields, must have unique names and field numbers
                  %% (within the message)
                  {OFields2, NewFNum} =
                      lists:mapfoldl(
                        fun(#?gpb_field{name=ONm}=F, OFNum) ->
                                OFieldName = combine_name(FieldName, ONm),
                                {F#?gpb_field{name=OFieldName,
                                              rnum=RNum, fnum=OFNum},
                                 OFNum+1}
                        end,
                        FNum,
                        OFields),
                  {#gpb_oneof{name=FieldName, rnum=RNum, fields=OFields2},
                   NewFNum}
          end,
          FNumBase,
          seq_index(UFieldDefs, 2)),
    Fields.

keyunique(_N, []) ->
    [];
keyunique(N, [Tuple|Rest]) ->
    [Tuple| keyunique(N, [ T2 || T2 <- Rest,
                                 element(N,T2) /= element(N,Tuple)])].

seq_index(L, Start) ->
    {Begin,End} = {1+(Start-1), length(L)+(Start-1)},
    lists:zip(L, lists:seq(Begin, End)).

combine_name(NameA, NameB) ->
    list_to_atom(lists:concat([NameA, "_", NameB])).

%% In fact, we should improve this to have different enums containing same value
%% e.g. [ {{enum,e1},[{x1,10}]}, {{enum,x2},[{x2,10}]} ]
enums() ->
    ?LET({N,Values,Names}, {int(),ulist("x"),ulist("e")},
         ?LET(Constants, unique_values(Values,N),
              enums(Names,Constants))).

ulist(String) ->
    ?LET(N, nat(), [list_to_atom(String ++ integer_to_list(K))
                    || K <- lists:seq(1, N)]).

%% EQC's shuffle/1: Shuffles a list and shrinks to the unshuffled list.
%% Our: Just shuffle the list.
shuffle(Xs) ->
    [X || {_,X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].

%% Unique names and unqiue values
%% Example
%% enum file_open_return_values { enoent=1, eacess=2 }
unique_values([],_N) ->
    [];
unique_values([Cons|Conss],N) ->
    ?LET(Next, nat(), [{Cons,N}|unique_values(Conss,Next+N+1)]).

enums([],_Conss) ->
    [];
enums(_Enames,[]) ->
    [];
enums([Ename|Enames],Conss) ->
    ?LET(Element, elements(Conss),
         begin
             Prefix = left_of(Element,Conss) ++ [Element],
             [{{enum,Ename},Prefix} | enums(Enames,Conss--Prefix)]
         end).


%% generator for messages that respect message definitions

msg_name(MsgDefs, ChoiceOpts) ->
    IsOk = case proplists:get_bool(no_any, ChoiceOpts) of
               true  -> fun(MsgName) -> MsgName /= 'google.protobuf.Any' end;
               false -> fun(_) -> true end

           end,
    oneof([M || {{msg,M},_} <- MsgDefs,
                IsOk(M)]).

message(MessageDefs,Opts) ->
    MsgDefs = [MD || {{msg,_MsgName},_}=MD <- MessageDefs], % filter out enums
    CandidateMsgDefs = case proplists:get_value(any_translate, Opts) of
                           undefined ->
                               MsgDefs;
                           _ ->
                               [MD || {{msg,Name},_}=MD <- MsgDefs,
                                      Name /= 'google.protobuf.Any']
                       end,
    ?LET({{msg,Msg},_Fields}, oneof(CandidateMsgDefs),
         message(Msg,MessageDefs,Opts)).

message(Msg,MessageDefs,Opts) ->
    Fields = proplists:get_value({msg,Msg},MessageDefs),
    FieldValues = [case Field of
                       #?gpb_field{} -> field_value(Field, MessageDefs, Opts);
                       #gpb_oneof{}  -> oneof_value(Field, MessageDefs, Opts)
                   end
                   || Field <- Fields],
    list_to_tuple([Msg|FieldValues]).

oneof_value(#gpb_oneof{fields=OFields}, MessageDefs, Opts) ->
    ?LET(OField, oneof(OFields),
         begin
             #?gpb_field{name=Name} = OField,
             oneof([undefined,
                    {Name, field_value(OField#?gpb_field{occurrence=required},
                                       MessageDefs,
                                       Opts)}])
         end).

field_value(#?gpb_field{type=Type, occurrence=Occurrence}, MsgDefs, Opts) ->
    field_val2(Type, Occurrence, MsgDefs, Opts).

field_val2(Type, optional, MsgDefs, Opts) ->
    default(undefined, value(Type,MsgDefs,Opts));
field_val2({map,KeyType,ValueType}, repeated, MsgDefs, Opts) ->
    ?LET(Keys, list(value(KeyType, MsgDefs, Opts)),
         [{Key, value(ValueType, MsgDefs, Opts)} || Key <- lists:usort(Keys)]);
field_val2(Type, repeated, MsgDefs, Opts) ->
    list(value(Type, MsgDefs, Opts));
field_val2(Type, required, MsgDefs, Opts) ->
    value(Type,MsgDefs, Opts).

value({msg,M},MessageDefs,Opts) ->
    if M == 'google.protobuf.Any' ->
            case proplists:get_value(any_translate,Opts) of
                undefined ->
                    message(M,MessageDefs,Opts);
                _ ->
                    uint(32)
            end;
       M /= 'google.protobuf.Any' ->
            message(M,MessageDefs,Opts)
    end;
value({enum,E},MessageDefs,_Opts) ->
    {value, {{enum,E},EnumValues}} = lists:keysearch({enum,E}, 1, MessageDefs),
    ?LET({Symbolic,_ActualValue}, elements(EnumValues),
         Symbolic);
value({map,KeyType,ValueType}, MessageDefs, Opts) ->
    {value(KeyType, MessageDefs, Opts), value(ValueType, MessageDefs, Opts)};
value(bool,_,_) ->
    bool();
value(sint32,_,_) ->
    sint(32);
value(sint64,_,_) ->
    sint(64);
value(int32,_,_) ->
    int(32);
value(int64,_,_) ->
    int(64);
value(uint32,_,_) ->
    uint(32);
value(uint64,_,_) ->
    uint(64);
value(fixed64,_,_) ->
    uint(64);
value(sfixed64,_,_) ->
    sint(64);
value(fixed32,_,_) ->
    uint(32);
value(sfixed32,_,_) ->
    sint(32);
value(double, _,_) ->
    frequency([{70,real()}, {10,'infinity'}, {10,'-infinity'}, {10,nan}]);
value(float, _,_) ->
    frequency([{70,real()}, {10,'infinity'}, {10,'-infinity'}, {10,nan}]);
value(bytes, _,_) ->
    binary();
value(string, _,_) ->
    list(encodable_unicode_code_point()).

encodable_unicode_code_point() ->
    %% http://www.unicode.org/versions/Unicode6.0.0/ch03.pdf
    %% * Section 3.3, D9: Code points: integers in the range 0..10ffff
    %% * Section 3.9: The Unicode Standard supports three character
    %%   encoding forms: UTF-32, UTF-16, and UTF-8. Each encoding form
    %%   maps the Unicode code points U+0000..U+D7FF and
    %%   U+E000..U+10FFFF to unique code unit sequences
    ?SUCHTHAT(CP, oneof([uint(16), choose(16#10000, 16#10FFFF)]),
              (0 =< CP andalso CP =< 16#d7ff)
              orelse
                (16#e000 =< CP andalso CP =< 16#10ffff)).

sint(Base) ->
    int(Base).

int(Base) ->
    ?LET(I,uint(Base),
         begin
             << N:Base/signed >> = <<I:Base>>,
             N
         end).

uint(Base) ->
    oneof([ choose(0,pow2(B)-1) || B<-lists:seq(1,Base)]).

pow2(0)            -> 1;
pow2(N) when N > 0 -> 2*pow2(N-1);
pow2(N) when N < 0 -> 1/pow2(-N).

%%% properties

prop_encode_decode() ->
    ?FORALL(
       MsgDefs,message_defs(),
       ?FORALL(
          {Encoder, Decoder, COpts}, encoder_decoder(?MOD1),
          ?FORALL(
             Msg, message(MsgDefs, COpts),
             begin
                 MsgName = element(1, Msg),
                 install_msg_defs(?MOD1, MsgDefs, Encoder, Decoder, COpts),
                 Bin = encode_msg(Msg, MsgDefs, Encoder, COpts),
                 DecodedMsg = decode_msg(Bin, MsgName, MsgDefs, Decoder, COpts),
                 ?WHENFAIL(io:format("~p /= ~p\n",[Msg, DecodedMsg]),
                           msg_approximately_equals(Msg, DecodedMsg,
                                                    MsgDefs, COpts))
             end))).

%% add a round-trip via the `protoc' program in the protobuf package.
%% The `protoc' is the compiler generates code from a .proto file, but
%% it can also decode and encode messages on the fly, given a .proto
%% file, so we can use it as an sort of interop test.
prop_encode_decode_via_protoc() ->
    case _HaveProtoc = (find_protoc() /= false) of
        true ->
            encode_decode_via_protoc();
        false ->
            io:format(
              "Note: 'protoc' not in $PATH and no PROTOC env var defined,~n"
              "so not doing this property~n."),
            proper:numtests(1, true)
    end.

encode_decode_via_protoc() ->
    MDOpts = base_opts() ++ [no_any],
    ?FORALL(
       MsgDefs,message_defs(MDOpts),
       ?FORALL(
          {Encoder, Decoder, COpts}, encoder_decoder(?MOD1, [no_any]),
          ?FORALL(
             Msg, message(MsgDefs, COpts),
             begin
                 MsgName = element(1, Msg),
                 install_msg_defs(?MOD1, MsgDefs, Encoder, Decoder, COpts),
                 TmpDir = get_create_tmpdir(),
                 try
                     install_msg_defs_as_proto(MsgDefs, TmpDir),
                     GpbBin = encode_msg(Msg, MsgDefs, Encoder, COpts),
                     ProtoBin = decode_then_reencode_via_protoc(
                                  GpbBin, Msg, TmpDir),
                     DecodedMsg = decode_msg(ProtoBin, MsgName, MsgDefs,
                                             Decoder, COpts),
                     ?WHENFAIL(begin
                                   maybe_copy_tmpdir(TmpDir),
                                   io:format("~p /= ~p\n",[Msg,DecodedMsg])
                               end,
                               msg_approximately_equals(Msg, DecodedMsg,
                                                        MsgDefs, COpts))
                 after
                     delete_tmpdir(TmpDir)
                 end
             end))).

maybe_copy_tmpdir(TmpDir) ->
    case os:getenv("GPB_QC_COPY_TMPDIR_ON_FAIL") of
        false -> ok;
        _ ->
            SaveDir = TmpDir ++ "-save",
            io:format("Copying to save-dir ~p...", [SaveDir]),
            cp_dir(TmpDir, SaveDir)
    end.

%% test that we can ignore unknown fields
prop_encode_decode_with_skip() ->
    ?FORALL(
       MsgDefs, message_defs([no_any]),
       ?FORALL(
          InitialMsg, message(MsgDefs, []),
          ?FORALL(
             {{SubMsg, SubDefs},
              {Encoder1, Decoder1, COpts1},
              {Encoder2, Decoder2, COpts2}},
             {message_subset_defs(InitialMsg, MsgDefs),
              encoder_decoder(?MOD1, [no_any]),
              encoder_decoder(?MOD2, [no_any])},
             begin
                 MsgName = element(1, InitialMsg),
                 install_msg_defs(?MOD1, MsgDefs, Encoder1, Decoder1, COpts1),
                 install_msg_defs(?MOD2, SubDefs, Encoder2, Decoder2, COpts2),
                 Encoded = encode_msg(InitialMsg, MsgDefs, Encoder1, COpts1),
                 %% now decode the byte sequence with a decoder that knows
                 %% only a subset of the fields for each message.
                 Decoded = decode_msg(Encoded, MsgName,  SubDefs, Decoder2,
                                      COpts2),
                 ?WHENFAIL(io:format("~p /= ~p\n",[SubMsg, Decoded]),
                           msg_approximately_equals(SubMsg, Decoded,
                                                    SubDefs, []))
             end))).

%% test merging of messages
prop_merge() ->
    ?FORALL(
       MsgDefs, message_defs(),
       ?FORALL(
          MsgName, msg_name(MsgDefs, [no_any]),
          ?FORALL(
             {Encoder, Decoder, COpts}, encoder_decoder(?MOD1),
             ?FORALL(
                {Msg1, Msg2}, {message(MsgName, MsgDefs, COpts),
                               message(MsgName, MsgDefs, COpts)},
                begin
                    install_msg_defs(?MOD1, MsgDefs, Encoder, Decoder, COpts),
                    MergedMsg = merge_msgs(Msg1, Msg2, MsgDefs,
                                           Encoder, Decoder, COpts),
                    Bin1 = encode_msg(Msg1, MsgDefs, Encoder, COpts),
                    Bin2 = encode_msg(Msg2, MsgDefs, Encoder, COpts),
                    MergedBin = <<Bin1/binary,Bin2/binary>>,
                    DecodedMerge = decode_msg(MergedBin, MsgName, MsgDefs,
                                              Decoder, COpts),
                    msg_equals(MergedMsg, DecodedMerge, MsgDefs, COpts)
                end)))).

%% compute a subset of the fields, and also a subset of the msg,
%% corresponding to the subset of the fields.
%% Return {SubsetMsg, SubsetDefs}
message_subset_defs(Msg, MsgDefs) ->
    ?LET(DefsWithSkips,
         [case Elem of
              {{enum,_}, _}=Enum ->
                  Enum;
              {{msg,'google.protobuf.Any'}, _Fields}=MsgDef -> % keep intact
                  MsgDef;
              {{msg,MsgName}, MsgFields} ->
                  {{msg, MsgName}, msg_fields_subset_skips(MsgFields)}
          end
          || Elem <- MsgDefs],
         begin
             SubsetMsg  = remove_fields_by_skips(Msg, DefsWithSkips),
             SubsetDefs = remove_skips_from_defs(DefsWithSkips),
             {SubsetMsg, SubsetDefs}
         end).

msg_fields_subset_skips(Fields) when length(Fields) == 1 ->
    %% can't remove anything if there's only one field
    ?LET(_, int(),
         Fields);
msg_fields_subset_skips(Fields) ->
    ?LET(Fields2, [elements([Field, skip]) || Field <- Fields],
         %% compensate for removed fields
         Fields2).

remove_fields_by_skips(Msg, DefsWithSkips) ->
    MsgName = element(1, Msg),
    {{msg,MsgName}, MsgDef} = lists:keyfind({msg,MsgName}, 1, DefsWithSkips),
    Fields = [case Field of
                  #?gpb_field{type={msg, _SubMsgName}, occurrence=repeated} ->
                      [remove_fields_by_skips(Elem, DefsWithSkips)
                       || Elem <- Value];
                  #?gpb_field{type={map,_,{msg, _SubMsgName}}} ->
                      [{K,remove_fields_by_skips(V, DefsWithSkips)}
                       || {K, V} <- Value];
                  #?gpb_field{type={msg, _SubMsgName}, occurrence=Occurrence} ->
                      if Occurrence == optional, Value == undefined ->
                              Value;
                         true ->
                              remove_fields_by_skips(Value, DefsWithSkips)
                      end;
                  #gpb_oneof{fields=OFields} ->
                      case Value of
                          undefined ->
                              Value;
                          {OFName, Value2} ->
                              Pos = #?gpb_field.name,
                              case lists:keyfind(OFName, Pos, OFields) of
                                  #?gpb_field{type={msg, _SubMsgName}} ->
                                      {OFName, remove_fields_by_skips(
                                                 Value2, DefsWithSkips)};
                                  _ ->
                                      Value
                              end
                      end;
                  _ ->
                      Value
              end
              || {Value, Field} <- lists:zip(tl(tuple_to_list(Msg)), MsgDef),
                 Field /= skip],
    list_to_tuple([MsgName | Fields]).

remove_skips_from_defs(DefsWithSkips) ->
    [case Elem of
         {{enum,_}, _}=Enum ->
             Enum;
         {{msg,MsgName}, FieldsAndSkips} ->
             {{msg, MsgName}, remove_skips_recalculate_rnums(FieldsAndSkips)}
     end
     || Elem <- DefsWithSkips].

remove_skips_recalculate_rnums(FieldsAndSkips) ->
    {RecalculatedFieldsReversed, _TotalNumSkipped} =
        lists:foldl(
          fun(skip, {Fs, NumSkipped}) ->
                  {Fs, NumSkipped+1};
             (#?gpb_field{rnum=RNum}=F, {Fs, NumSkipped}) ->
                  {[F#?gpb_field{rnum=RNum-NumSkipped} | Fs], NumSkipped};
             (#gpb_oneof{rnum=RNum, fields=OFs}=F, {Fs, NumSkipped}) ->
                  OFs2 = [O#?gpb_field{rnum=RNum-NumSkipped} || O <- OFs],
                  F2 = F#gpb_oneof{rnum=RNum-NumSkipped,
                                   fields=OFs2},
                  {[F2 | Fs], NumSkipped}
          end,
          {[], 0},
          FieldsAndSkips),
    lists:reverse(RecalculatedFieldsReversed).

encoder_decoder(Mod) -> encoder_decoder(Mod, []).
encoder_decoder(Mod, Opts) ->
    DoAny = not lists:member(no_any, Opts),
    ?LET(
       {Encoder, Decoder}, {oneof([gpb, Mod]), oneof([gpb, Mod])},
       ?LET(
          COpts1,
          [{copy_bytes,        oneof([false, true, auto, choose(2,4)])},
           {field_pass_method, oneof([pass_as_record, pass_as_params])}
           | map_opts()],
          ?LET(
             COpts2, any_translation_opts(COpts1),
             if DoAny, Encoder /= gpb, Decoder /= gpb ->
                     {Encoder, Decoder, COpts1 ++ COpts2};
                true ->
                     {Encoder, Decoder, COpts1}
             end))).

map_opts() ->
    HaveMaps = case get(cache_have_maps) of
                   undefined ->
                       V = have_maps(),
                       put(cache_have_maps, V),
                       V;
                   V ->
                       V
               end,
    if HaveMaps ->
            [{maps, oneof([false, true])},
             {maps_unset_optional, oneof([present_undefined, omitted])},
             {maps_oneof, oneof([tuples, flat])}];
       not HaveMaps ->
            []
    end.

have_maps() ->
    try maps:size(maps:new()) of
        0 ->
            true
    catch error:undef ->
            false
    end.

any_translation_opts(Opts0) ->
    DoMaps = proplists:get_bool(maps, Opts0),
    ?LET(DoTranslate, oneof([false, {true, bool(), bool()}]),
         case DoTranslate of
             {true, TranslateMerge, TranslateVerify} ->
                 Encode = if DoMaps ->
                                  [{encode,{?MODULE,any_tr_pack_m,['$1']}}];
                             not DoMaps ->
                                  [{encode,{?MODULE,any_tr_pack_r,['$1']}}]
                          end,
                 Decode = if DoMaps ->
                                  [{decode,{?MODULE,any_tr_unpack_m,['$1']}}];
                             not DoMaps ->
                                  [{decode,{?MODULE,any_tr_unpack_r,['$1']}}]
                          end,
                 Merge  = [{merge,{?MODULE,any_tr_merge,['$1','$2']}}
                           || TranslateMerge],
                 Verify = [{verify,{?MODULE,any_tr_verify,['$1','$errorf']}}
                           || TranslateVerify],
                 [{any_translate, Encode ++ Decode ++ Merge ++ Verify}];
             false ->
                 []
         end).


any_tr_pack_m(N) ->
    NStr = integer_to_list(N),
    maps:from_list([{type_url,NStr},
                    {value,list_to_binary(NStr)}]).

any_tr_unpack_m(M) ->
    ML = maps:to_list(M),
    NStr = proplists:get_value(type_url,ML),
    NBin = proplists:get_value(value,ML),
    N = list_to_integer(NStr),
    N = list_to_integer(binary_to_list(NBin)),
    N.


any_tr_pack_r(N) ->
    NStr = integer_to_list(N),
    {'google.protobuf.Any',NStr,list_to_binary(NStr)}.

any_tr_unpack_r({'google.protobuf.Any',NStr,NBin}) ->
    N = list_to_integer(NStr),
    N = list_to_integer(binary_to_list(NBin)),
    N.

any_tr_merge(_,N2) ->
    N2.

any_tr_verify(V, _ErrorF) when is_integer(V) -> ok;
any_tr_verify(_, ErrorF) -> ErrorF(not_an_integer).


encode_msg(Msg, MsgDefs, Encoder, COpts) ->
    case Encoder of
        gpb ->
            gpb:encode_msg(Msg, MsgDefs);
        _ ->
            case proplists:get_value(maps, COpts) of
                false ->
                    Encoder:encode_msg(Msg);
                true ->
                    map_encode_msg(Msg, MsgDefs, Encoder, COpts)
            end
    end.

map_encode_msg(Msg, MsgDefs, Encoder, COpts) ->
    MsgAsMap = msg_to_map(Msg, MsgDefs, COpts),
    MsgName = element(1, Msg),
    Encoder:encode_msg(MsgAsMap, MsgName).

decode_msg(Bin, MsgName, MsgDefs, Decoder, COpts) ->
    case Decoder of
        gpb ->
            gpb:decode_msg(Bin, MsgName, MsgDefs);
        _ ->
            case proplists:get_value(maps, COpts) of
                false ->
                    Decoder:decode_msg(Bin, MsgName);
                true ->
                    map_decode_msg(Bin, MsgName, MsgDefs, Decoder, COpts)
            end
    end.

map_decode_msg(Bin, MsgName, MsgDefs, Decoder, COpts) ->
    MsgAsMap = Decoder:decode_msg(Bin, MsgName),
    map_to_msg(MsgAsMap, MsgName, MsgDefs, COpts).

merge_msgs(Msg1, Msg2, MsgDefs, Encoder, Decoder, COpts) ->
    if Encoder == gpb, Decoder == gpb ->
            gpb:merge_msgs(Msg1, Msg2, MsgDefs);
       Encoder /= gpb ->
            case proplists:get_value(maps, COpts) of
                false ->
                    Encoder:merge_msgs(Msg1, Msg2);
                true ->
                    map_merge_msgs(Msg1, Msg2, MsgDefs, Encoder, COpts)
            end;
       Decoder /= gpb ->
            case proplists:get_value(maps, COpts) of
                false ->
                    Decoder:merge_msgs(Msg1, Msg2);
                true ->
                    map_merge_msgs(Msg1, Msg2, MsgDefs, Decoder, COpts)
            end
    end.

map_merge_msgs(Msg1, Msg2, MsgDefs, Mod, COpts) ->
    Msg1AsMap = msg_to_map(Msg1, MsgDefs, COpts),
    Msg2AsMap = msg_to_map(Msg2, MsgDefs, COpts),
    MsgName = element(1, Msg1),
    ResultAsMap = Mod:merge_msgs(Msg1AsMap, Msg2AsMap, MsgName),
    map_to_msg(ResultAsMap, MsgName, MsgDefs, COpts).

install_msg_defs(Mod, MsgDefs, Encoder, Decoder, COpts) ->
    if Encoder == gpb, Decoder == gpb ->
            ok; %% nothing needs to be done
       true ->
            install_msg_defs_aux(Mod, MsgDefs, COpts)
    end.

install_msg_defs(Mod, MsgDefs) ->
    install_msg_defs_aux(Mod, MsgDefs, [{copy_bytes, auto}]).

install_msg_defs_aux(Mod, MsgDefs, Opts) when is_list(Opts) ->
    Opts2 = [binary, {verify, always}, return_warnings | Opts],
    {{ok, Mod, Code, _},_} = {gpb_compile:msg_defs(Mod, MsgDefs, Opts2),
                              compile},
    ok = delete_old_versions_of_code(Mod),
    {{module, Mod},_} = {code:load_binary(Mod, "<nofile>", Code), load_code},
    ok.

delete_old_versions_of_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

msg_equals(Msg1, Msg2, MsgDefs, Opts) ->
    case msg_approximately_equals(Msg1, Msg2, MsgDefs, Opts) of
        true  ->
            true;
        false ->
            %% Run equals, even though we know it'll return
            %% false, because it'll show the messages
            %% appropritately -- e.g. not when shrinking.
            equals(Msg1,Msg2)
    end.

msg_approximately_equals(M1, M2, MsgDefs, Opts)
  when is_tuple(M1), is_tuple(M2),
       element(1,M1) == element(1,M2),
       tuple_size(M1) == tuple_size(M2) ->
    MsgName = element(1,M1),
    {{msg,MsgName},Fields} = lists:keyfind({msg,MsgName},1,MsgDefs),
    lists:all(fun({F1, F2, Field}) ->
                      field_approximately_equals(F1, F2, Field, MsgDefs, Opts)
              end,
              lists:zip3(tl(tuple_to_list(M1)),
                         tl(tuple_to_list(M2)),
                         Fields));
msg_approximately_equals(_X, _Y, _MsgDefs, _Opts) ->
    false.

field_approximately_equals(F1, F2, #?gpb_field{type={map,_,VT}},
                           MsgDefs, Opts) ->
    DoTranslateAny = proplists:get_value(any_translate,Opts) /= undefined,
    lists:all(fun({{K1,V1}, {K2,V2}}) ->
                      case VT of
                          {msg,'google.protobuf.Any'} when DoTranslateAny ->
                              is_value_approx_eq(K1,K2)
                                  andalso
                                  is_value_approx_eq(V1,V2);
                          {msg,_} ->
                              is_value_approx_eq(K1,K2)
                                  andalso
                                  msg_approximately_equals(V1,V2,MsgDefs,Opts);
                          _ ->
                              is_value_approx_eq(K1,K2)
                                  andalso
                                  is_value_approx_eq(V1,V2)
                      end
              end,
              lists:zip(lists:sort(F1),lists:sort(F2)));
field_approximately_equals(F1, F2, #?gpb_field{type={msg,MsgName},
                                               occurrence=Occ},
                           MsgDefs, Opts) ->
    DoTranslateAny = proplists:get_value(any_translate,Opts) /= undefined,
    case {Occ, MsgName} of
        {_, 'google.protobuf.Any'} when DoTranslateAny ->
            is_value_approx_eq(F1, F2);
        {repeated,_} ->
            lists:all(fun({E1,E2}) ->
                              msg_approximately_equals(E1, E2, MsgDefs, Opts)
                      end,
                      lists:zip(F1,F2));
        {optional,_} ->
            if F1 == undefined, F2 == undefined ->
                    true;
               true ->
                    msg_approximately_equals(F1, F2, MsgDefs, Opts)
            end;
        {required,_} ->
            msg_approximately_equals(F1, F2, MsgDefs, Opts)
    end;
field_approximately_equals({T,F1}, {T,F2}, #gpb_oneof{fields=OFs},
                           MsgDefs, Opts) ->
    DoTranslateAny = proplists:get_value(any_translate,Opts) /= undefined,
    case lists:keyfind(T,#?gpb_field.name,OFs) of
        #?gpb_field{type={msg,'google.protobuf.Any'}} when DoTranslateAny ->
            is_value_approx_eq(F1, F2);
        #?gpb_field{type={msg,_}} ->
            msg_approximately_equals(F1, F2, MsgDefs, Opts);
        _ ->
            is_value_approx_eq(F1, F2)
    end;
field_approximately_equals(F1, F2, _Field, _MsgDefs, _Opts) ->
    is_value_approx_eq(F1, F2).

is_value_approx_eq(F1, F2) when is_float(F1), is_float(F2) ->
    is_float_equivalent(F1, F2);
is_value_approx_eq(L1, L2) when is_list(L1), is_list(L2) ->
    lists:all(fun({E1,E2}) -> is_value_approx_eq(E1,E2) end, lists:zip(L1,L2));
is_value_approx_eq(X, X) ->
    true;
is_value_approx_eq(_X, _Y) ->
    io:format("Not equal: ~p <--> ~p~n", [_X, _Y]),
    false.

-define(ABS_ERROR, 1.0e-10). %% was: 1.0e-16
-define(REL_ERROR, 1.0e-6).  %% was: 1.0e-10

is_float_equivalent(F, F) -> true;
is_float_equivalent(F1,F2) ->
 if (abs(F1-F2) < ?ABS_ERROR) -> true;
    (abs(F1) > abs(F2)) -> abs( (F1-F2)/F1 ) < ?REL_ERROR;
    (abs(F1) < abs(F2)) -> abs( (F1-F2)/F2 ) < ?REL_ERROR
end.

% Recursively translate a record to a map
msg_to_map(Msg, MsgDefs, COpts) ->
    MsgName = element(1, Msg),
    {{msg,MsgName},Fields} = lists:keyfind({msg,MsgName}, 1, MsgDefs),
    FVTs = [case F of
                #?gpb_field{name=FName}=Field ->
                    V2 = field_to_map(V, Field, MsgDefs, COpts),
                    {FName, V2, Field};
                #gpb_oneof{name=FName, fields=OFields}=OField ->
                    V2 = case V of
                             undefined ->
                                 undefined;
                             {Tag, TV} ->
                                 Field = lists:keyfind(Tag, #?gpb_field.name,
                                                       OFields),
                                 {Tag, field_to_map(TV, Field, MsgDefs, COpts)}
                         end,
                    {FName, V2, OField}
            end
            || {F,V} <- lists:zip(Fields, tl(tuple_to_list(Msg)))],
    case proplists:get_value(maps_unset_optional, COpts) of
        present_undefined ->
            maps:from_list([{Nm,Value} || {Nm,Value,_T} <- FVTs]);
        omitted ->
            case proplists:get_value(maps_oneof, COpts) of
                tuples ->
                    maps:from_list([{Nm,Value} || {Nm,Value,_T} <- FVTs,
                                                  Value /= undefined]);
                flat ->
                    maps:from_list(
                      [case {T, Value} of
                           {#gpb_oneof{}, {Tag, TagValue}} ->
                               {Tag, TagValue};
                           _ ->
                               {Nm, Value}
                       end
                       || {Nm,Value,T} <- FVTs,
                          Value /= undefined])
            end
    end.

field_to_map(V, #?gpb_field{type={msg,MsgName},occurrence=Occurrence},
             MsgDefs,COpts) ->
    submsg_to_map1(Occurrence, MsgName, V, MsgDefs, COpts);
field_to_map(V, #?gpb_field{type={map,_,ValueType}}, MsgDefs, COpts) ->
    maps:from_list(
      [case ValueType of
           {msg, MsgName} ->
               {K, submsg_to_map1(required, MsgName, V2, MsgDefs, COpts)};
           _        -> {K, V2}
       end
       || {K,V2} <- V]);
field_to_map(V, _FieldDef, _MsgDefs, _COpts) ->
    V.

submsg_to_map1(Occurrence, MsgName, V, MsgDefs, COpts) ->
    if MsgName == 'google.protobuf.Any' ->
            case proplists:get_value(any_translate, COpts) of
                undefined ->
                    submsg_to_map2(Occurrence, V, MsgDefs, COpts);
                _ ->
                    %% it is really an integer since we've added translations
                    %% for it
                    V
            end;
       true ->
            submsg_to_map2(Occurrence, V, MsgDefs, COpts)
    end.

submsg_to_map2(required, V, MsgDefs, COpts) ->
    msg_to_map(V, MsgDefs, COpts);
submsg_to_map2(repeated, Seq, MsgDefs, COpts) ->
    [msg_to_map(Elem, MsgDefs, COpts) || Elem <- Seq];
submsg_to_map2(optional, V, MsgDefs, COpts) ->
    if V == undefined -> undefined;
       V /= undefined -> msg_to_map(V, MsgDefs, COpts)
    end.

map_to_msg(Map, MsgName, MsgDefs, COpts) ->
    {{msg,MsgName},Fields} = lists:keyfind({msg,MsgName}, 1, MsgDefs),
    FlatOneof = case proplists:get_value(maps_unset_optional, COpts) of
                    omitted -> proplists:get_value(maps_oneof, COpts) =:= flat;
                    _       -> false
                end,
    list_to_tuple(
      [MsgName |
       [case F of
            #?gpb_field{name=FName} ->
                case maps:find(FName, Map) of
                    error ->
                        undefined;
                    {ok, V} ->
                        field_from_map(V, F, MsgDefs, COpts)
                end;
            #gpb_oneof{} ->
                if FlatOneof ->
                        flat_oneof_from_map(F, Map, MsgDefs, COpts);
                   not FlatOneof ->
                        tuple_oneof_from_map(F, Map, MsgDefs, COpts)
                end
        end
        || F <- Fields]]).

flat_oneof_from_map(#gpb_oneof{fields=OFields}, Map, MsgDefs, COpts) ->
    lists:foldl(
      fun(#?gpb_field{name=Tag}=Field, undefined) ->
              case maps:find(Tag, Map) of
                  error ->
                      undefined;
                  {ok, Value} ->
                      {Tag, field_from_map(Value, Field, MsgDefs, COpts)}
              end;
         (_, Acc) ->
              Acc
      end,
      undefined,
      OFields).

tuple_oneof_from_map(#gpb_oneof{name=FName, fields=OFields},
                     Map, MsgDefs, COpts) ->
    case maps:find(FName, Map) of
        error ->
            undefined;
        {ok, undefined} ->
            undefined;
        {ok, {Tag, TV}} ->
            Field = lists:keyfind(Tag, #?gpb_field.name, OFields),
            {Tag, field_from_map(TV, Field, MsgDefs, COpts)}
    end.


field_from_map(V, #?gpb_field{type={msg,SubMsgName}, occurrence=Occurrence},
               MsgDefs, COpts) ->
    submsg_from_map1(Occurrence, V, SubMsgName, MsgDefs, COpts);
field_from_map(V, #?gpb_field{type={map,_,ValueType}}, MsgDefs, COpts) ->
    [case ValueType of
         {msg, Name2} -> {K, submsg_from_map1(required, V2, Name2, MsgDefs,
                                              COpts)};
         _ -> {K,V2}
     end
     || {K,V2} <- maps:to_list(V)];
field_from_map(V, _FieldDef, _MsgDefs, _COpts) ->
    V.

submsg_from_map1(Occurrence, Map, MsgName, MsgDefs, COpts) ->
    if MsgName == 'google.protobuf.Any' ->
            case proplists:get_value(any_translate, COpts) of
                undefined ->
                    submsg_from_map2(Occurrence, Map, MsgName, MsgDefs, COpts);
                _ ->
                    %% it is really an integer since we've added translations
                    %% for it
                    Map
            end;
       true ->
            submsg_from_map2(Occurrence, Map, MsgName, MsgDefs, COpts)
    end.

submsg_from_map2(required, Map, MsgName, MsgDefs, COpts) ->
    map_to_msg(Map, MsgName, MsgDefs, COpts);
submsg_from_map2(repeated, Seq, MsgName, MsgDefs, COpts) ->
    [map_to_msg(Elem, MsgName, MsgDefs, COpts) || Elem <- Seq];
submsg_from_map2(optional, V, MsgName, MsgDefs, COpts) ->
    if V == undefined -> undefined;
       V /= undefined -> map_to_msg(V, MsgName, MsgDefs, COpts)
    end.

get_create_tmpdir() ->
    D = filename:join("/tmp", ?f("~s-~s", [?MODULE, os:getpid()])),
    filelib:ensure_dir(filename:join(D, "dummy-file-name")),
    [file:delete(X) || X <- filelib:wildcard(filename:join(D,"*"))],
    D.

cp_dir(SrcDir, DestDir) -> % expects SrcDir contains only files, ie no cp -r
    filelib:ensure_dir(filename:join(DestDir, "dummy-file-name")),
    [file:copy(F, filename:join(DestDir,filename:basename(F)))
     || F <- filelib:wildcard(filename:join(SrcDir,"*"))].

delete_tmpdir(TmpDir) ->
    [file:delete(X) || X <- filelib:wildcard(filename:join(TmpDir,"*"))],
    file:del_dir(TmpDir).

install_msg_defs_as_proto(MsgDefs, TmpDir) ->
    ProtoFile = filename:join(TmpDir, "x.proto"),
    ok = file:write_file(ProtoFile, msg_defs_to_proto(MsgDefs)).

msg_defs_to_proto(MsgDefs) ->
    iolist_to_binary(
      ["syntax=\"proto2\";\n",
       lists:map(fun msg_def_to_proto/1, MsgDefs)]).

msg_def_to_proto({{enum, Name}, EnumValues}) ->
    Values = lists:map(fun({N,V}) -> ?f("  ~s = ~w;~n", [N, V]) end,
                       EnumValues),
    ?f("enum ~s {~n"
       "~s"
       "}~n~n",
       [Name, Values]);
msg_def_to_proto({{msg, Name}, Fields}) ->
    Values = lists:map(fun(#?gpb_field{}=F) -> field_to_proto(F, unless_map);
                          (#gpb_oneof{}=F) ->  oneof_to_proto(F)
                       end,
                       Fields),
    ?f("message ~s {~n"
       "~s"
       "}~n~n",
       [Name, Values]).

field_to_proto(#?gpb_field{name=FName, fnum=FNum, type=Type, opts=Opts,
                           occurrence=Occurrence}, ShowOccurrence) ->
    Packed = lists:member(packed, Opts),
    ?f("  ~s ~s ~s = ~w~s;~n",
       [case {ShowOccurrence,Type} of
            {unless_map,{map,_,_}} -> "  ";
            {unless_map,_}         -> Occurrence;
            {false,_}              -> "  "
        end,
        fmt_type(Type),
        FName,
        FNum,
        case Packed of
            true -> " [packed=true]";
            false -> ""
        end]).

fmt_type({msg,Name2})  -> Name2;
fmt_type({enum,Name2}) -> Name2;
fmt_type({map,KT,VT})  -> ?f("map<~s,~s>", [fmt_type(KT),fmt_type(VT)]);
fmt_type(Type)         -> Type.

oneof_to_proto(#gpb_oneof{name=FName, fields=OFields}) ->
    Values = [field_to_proto(OField, false) || OField <- OFields],
    ?f("  oneof ~s {~n"
       "~s"
       "  };~n",
       [FName, Values]).

decode_then_reencode_via_protoc(GpbBin, Msg, TmpDir) ->
    ProtoFile = filename:join(TmpDir, "x.proto"),
    ETxtFile = filename:join(TmpDir, "x.etxt"),
    EMsgFile = filename:join(TmpDir, "x.emsg"),
    PMsgFile = filename:join(TmpDir, "x.pmsg"),
    TxtFile = filename:join(TmpDir, "x.txt"),
    MsgName = element(1, Msg),
    ok = file:write_file(ETxtFile, iolist_to_binary(?f("~p~n", [Msg]))),
    ok = file:write_file(EMsgFile, GpbBin),
    Protoc = find_protoc(),
    DRStr = os:cmd(?f("'~s' --proto_path '~s'"
                      " --decode=~s '~s'"
                      " < '~s' > '~s'; echo $?~n",
                      [Protoc, TmpDir, MsgName, ProtoFile, EMsgFile,
                       TxtFile])),
    try 0 = list_to_integer(nonl(DRStr))
    catch error:T when T =:= badarg; T =:= badmatch ->
            {ok,Proto0} = file:read_file(ProtoFile),
            Proto = string:trim(Proto0, trailing),
            io:format(user, ">>> Decoding ~p\n >  ~s\n>>  ~s\n",
                      [ProtoFile,Proto,DRStr]),
            error({Proto,DRStr})
    end,
    ERStr = os:cmd(?f("'~s' --proto_path '~s'"
                      " --encode=~s '~s'"
                      " < '~s' > '~s'; echo $?~n",
                      [Protoc, TmpDir, MsgName, ProtoFile, TxtFile,
                       PMsgFile])),
    0 = list_to_integer(nonl(ERStr)),
    {ok, ProtoBin} = file:read_file(PMsgFile),
    ProtoBin.

nonl(Str) -> [C || C <- Str, C =/= $\n].

base_opts() ->
    case find_protoc_version() of
        {ok, Vsn} when Vsn >= [3,0] -> [];
        {ok, _} ->
            %% Skip testing maps & oneof on older protoc versions
            [no_maps, no_oneof]
    end.

find_protoc_version() ->
    Output = os:cmd(find_protoc() ++ " --version"),
    case find_protoc_version_aux(string:tokens(Output, " \t\r\n"), Output) of
        {ok, _}=Res -> Res;
        {error, X}=Res ->
            io:format(user,"Trouble finding protoc version in ~s~n", [X]),
            Res
    end.

find_protoc() ->
    case os:getenv("PROTOC") of
        false  -> os:find_executable("protoc");
        Protoc -> Protoc
    end.

find_protoc_version_aux(["libprotoc", VersionStr | _], All) ->
    try {ok, [list_to_integer(X) || X <- string:tokens(VersionStr, ".")]}
    catch error:badarg -> {error, {failed_to_interpret, VersionStr, All}}
    end;
find_protoc_version_aux([_ | Rest], All) ->
    find_protoc_version_aux(Rest, All);
find_protoc_version_aux([], All) ->
    {error, {no_version_string_found, All}}.


-endif. %% 'NO_HAVE_PROPER'
