%%% Copyright (C) 2022  Tomas Abrahamsson
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

-module(gpb_parse_descr_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gpb_descriptor.hrl").

%% ------------------------------------------------------------------

simple_msg_and_back_test() ->
    Opts = [{proto_defs_version, gpb_defs:latest_defs_version()}],
    {ok, Defs, []} =
        gpb_compile_descr_tests:compile_files_as_iolists(
          [{"main.proto",
            ["syntax='proto2';
              import 'aux.proto';
              message M { };"]},
           {"aux.proto",
            ["syntax='proto2';
              message A { optional uint32 g = 1; }"]}],
          Opts),
    {Bin, _PBins} = gpb_compile_descr:encode_defs_to_descriptors(Defs, Opts),
    Fds = gpb_descriptor:decode_msg(Bin, 'FileDescriptorSet'),
    #'FileDescriptorSet'{} = Fds,
    ?assertEqual(
       proto_sort(Defs),
       proto_sort(ok_value(gpb_parse_descr:defs_from_descriptors(Fds, Opts)))),
    ?assertEqual(
       proto_sort(Defs),
       proto_sort(ok_value(gpb_parse_descr:defs_from_descriptors(Bin, Opts)))).

msg_with_submsg_test() ->
    verify_defs_roundtrip_via_descr(
      [{"main.proto",
        ["syntax='proto2';
          message Msg { optional Sub f = 1;}
          message Sub { optional uint32 g = 2;}"]}]),
    ok.

msg_with_oneof_test() ->
    verify_defs_roundtrip_via_descr(
      "syntax='proto2';
       message Msg {
         oneof c {
           uint32 a1 = 1;
           uint32 a2 = 2;
         }
       }"),
    ok.

msg_with_maptype_fields_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M { map<int32,string> mm = 1};">>),
    ok.

msg_with_maptype_with_enum_msg_refs_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M { map<uint32,Mm> mm = 1};
         message Mm { }">>),
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M { map<uint32,Ee> mm = 1};
         enum Ee { A = 0; B = 1; }">>),
    ok.

packages_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         package a.b.c;
         message M {
           optional .a.b.c.Sub f = 1;
           optional Sub        g = 2;
         }
         message Sub { }
        ">>,
      [use_packages]),
    ok.

nested_fields_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
           optional Sub mm = 1;
           message Sub {
           }
         }">>),
    ok.

referring_relatively_to_nested_down_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
           optional Sub.Subsub mm = 1;
           message Sub {
             message Subsub {
             }
           }
         }">>),
    ok.

referring_with_root_anchored_path_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
           optional .M.Sub.Subsub mm = 1;
           message Sub {
             message Subsub {
             }
           }
         }">>),
    ok.

enums_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M { optional Ee f = 1; }
         enum Ee { A = 0; B = 1; }">>),
    ok.

enum_options_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M { optional Ee f = 1; }
         enum Ee { option deprecated = true;
                   A = 0; B = 1 [deprecated=true];
                 }">>),
    ok.

nested_enums_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
            optional Ee    f = 1;
            optional .M.Ee g = 2;
            enum Ee { A = 0; B = 1; }
         }">>),
    ok.

extensions_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
            required uint32 f1=1;
            extensions 200 to 299;
         }
         extend M {
            optional uint32 f2 = 200;
         }
        ">>),

    %% Same again, but a package has been declared (no opt to use it, though)
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         package p;
         message M {
            required uint32 f1=1;
            extensions 200 to 299;
         }
         extend M {
            optional uint32 f2 = 200;
         }
        ">>),

    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         package p;
         message M {
            required uint32 f1=1;
            extensions 200 to 299;
         }
         extend M {
            optional uint32 f2 = 200;
         }
        ">>,
      [use_packages]),
    ok.

reserveds_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
           reserved 2, 11 to 13, 9, 15 to max;
           reserved 'foo', 'bar';
         }">>),
    ok.

simple_proto3_test() ->
    %% {proto3_msgs, MsgNames} element from one .proto
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto3';
         message M {
            uint32           f = 1;
            repeated fixed32 g = 2;
            Sub              h = 3;
         }
         message Sub { uint32 a = 1; }
        ">>),
    ok.

proto3_msgs_over_several_protos_test() ->
    %% {proto3_msgs, MsgNames} element in/from several .proto
    verify_defs_roundtrip_via_descr(
      [{"main.proto", <<"syntax='proto3';
                         import 'aux3.proto';
                         message M { uint32 f = 1; };
                        ">>},
       %% Proto3, should get included in {proto3_msgs, MsgNames}
       {"aux3.proto", <<"syntax='proto3';
                         import 'aux2.proto';
                         message A1 { uint32 g = 2; };
                        ">>},
       %% Proto2, should not get included
       {"aux2.proto", <<"syntax='proto2';
                         message A2 { optional uint32 h = 3; };
                        ">>}]),
    ok.

proto3_optional_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto3';
         message M {
           optional uint32 f = 1; // wrapped in a oneof on descr level
           oneof c1 { uint32 a1 = 10; }
         }
        ">>),
    ok.

services_rpcs_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto3';
         message M1 { message S { }; }
         message M2 { uint32 f = 1; }
         service S {
           rpc R1(M1.S) returns (M2);
           rpc R2(M2)   returns (M1);
         }
        ">>, []), % no use_packages

    %% packages
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto3';
         package a.b.c;
         message M { message Sub {}; }
         service S {
           rpc R1(M.Sub) returns (M);
           rpc R2(M.Sub) returns (M);
         }
        ">>, [use_packages]),

    %% Check stream as well
    [begin
         io:format("Checking ~s~n", [Rpc]),
         verify_defs_roundtrip_via_descr(
           <<"syntax='proto3';
              message M { uint32 f = 1; }
              service S { ", Rpc/binary, " }">>)
     end
     || Rpc <- [<<"rpc Req(stream M) returns (M);">>,
                <<"rpc Req(M)        returns (stream M);">>,
                <<"rpc Req(stream M) returns (stream M);">>]],
    ok.

services_and_rpc_options_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M { }
         service S {
           option deprecated = true;
           rpc Rr(M) returns(M) {
             option deprecated=true;
             option idempotency_level=NO_SIDE_EFFECTS;
           }
         }">>),
    ok.

msg_and_field_options_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message M {
            option deprecated=true;
            optional uint32 f_x = 1 [deprecated=true, json_name='f_x'];
         }">>),
    ok.

msg_field_default_values_test() ->
    [begin
         io:format("Testing field ~s~n", [Field]),
         verify_defs_roundtrip_via_descr(
           <<"syntax='proto2';
              message M {", Field/binary, "};

             ">>)
     end
     || Field <- [<<"optional bool   f =  1 [default=true];">>,
                  <<"optional uint32 g =  2 [default=1];">>,
                  <<"optional uint32 h =  3 [default=+1];">>,
                  <<"optional sint32 i =  4 [default=-1];">>,
                  <<"optional float  j =  5 [default=1.125];">>,
                  <<"optional float  k =  6 [default=inf];">>,
                  <<"optional float  l =  7 [default=-inf];">>,
                  <<"optional float  m =  8 [default=nan];">>,
                  <<"optional Ee     n =  9 [default=E_AA];
                     enum Ee { E_AA = 0; E_AB = 1; };">>,
                  <<"optional string o = 10 [default='abc'];">>,
                  <<"optional bytes  p = 11 [default='abc\x10'];">>,
                  <<"oneof c {
                       uint32 a1 = 100 [default=1];
                    }">>]],
    ok.

groups_test() ->
    verify_defs_roundtrip_via_descr(
      <<"syntax='proto2';
         message Mm {
           optional uint32 f1 = 1;
           optional group g = 2 {
             required uint32 gf = 3;
           }
         }
        ">>),
    ok.

only_type_name_no_type_in_field_test() ->
    %% The descriptor.proto says: "If type_name is set, [field 'type']
    %% need not be set.  If both [field 'type'] and type_name are set,
    %% [field 'type'] must be one of TYPE_ENUM, TYPE_MESSAGE or
    %% TYPE_GROUP.

    %% So, first a reference to a message, let the 'type_name'
    %% but not 'type' be set.
    Fds1 = to_file_descriptor_set(
             <<"syntax='proto3';
                message M { Other f = 1; }
                message Other { }
               ">>),
    #'FileDescriptorSet'{} = Fds1,
    Fds11 = set(Fds1, 'file[0].message_type[<name="M">].field[0].type',
                undefined),
    {ok, Defs1} = from_file_descriptor_set(Fds1, []),
    {ok, Defs11} = from_file_descriptor_set(Fds11, []),
    ?assertEqual(Defs1, Defs11).

return_on_proto_defs_version_1_test() ->
    Fds = to_file_descriptor_set(
            <<"syntax='proto2';
               message M { optional uint32 f = 1; }
              ">>),
    {ok, [{proto_defs_version, 1} | _More]} =
        from_file_descriptor_set(Fds, [{proto_defs_version, 1}]).

return_on_proto_defs_version_2_test() ->
    Fds = to_file_descriptor_set(
            <<"syntax='proto2';
               message M { optional uint32 f = 1; }
              ">>),
    {ok, [{proto_defs_version, 2} | _More]} =
        from_file_descriptor_set(Fds, [{proto_defs_version, 2}]).

return_on_proto_defs_version_3_test() ->
    Fds = to_file_descriptor_set(
            <<"syntax='proto2';
               message M { optional uint32 f = 1; }
              ">>),
    {ok, [{proto_defs_version, 3} | _More]} =
        from_file_descriptor_set(Fds, [{proto_defs_version, 3}]).

return_on_proto_defs_version_4_test() ->
    Fds = to_file_descriptor_set(
            <<"syntax='proto2';
               message M { optional uint32 f = 1; }
              ">>),
    {ok, [{proto_defs_version, 4} | _More]} =
        from_file_descriptor_set(Fds, [{proto_defs_version, 4}]).

format_error_test() ->
    %% This is not exhaustive, just make sure we have at least _some_
    %% coverage for format_error.
    Fds = to_file_descriptor_set(
            <<"syntax='proto3';
               message M { Other f = 1; }
               message Other { }
              ">>),
    #'FileDescriptorSet'{file=[F]} = Fds,
    #'FileDescriptorProto'{
       message_type=[#'DescriptorProto'{name="M"}=M,
                     #'DescriptorProto'{name="Other"}]} = F,
    %% Create an invalid one (missing the 'Other' message)
    %% to get something that renders as an error.
    F1 = #'FileDescriptorProto'{message_type=[M]},
    Fds1 = #'FileDescriptorSet'{file=[F1]},

    {ok, _} = from_file_descriptor_set(Fds, []),
    {error, Reason} = Error = from_file_descriptor_set(Fds1, []),
    io:format("Error=~n  ~p~n", [Error]),
    assert_is_iodata(gpb_parse_descr:format_error(Error)),
    assert_is_iodata(gpb_parse_descr:format_error(Reason)),
    ok.

%% -- helpers --

ok_value({ok, Value}) -> Value.

verify_defs_roundtrip_via_descr(ProtosOrIolist) ->
    verify_defs_roundtrip_via_descr(ProtosOrIolist, []).

-spec verify_defs_roundtrip_via_descr(ProtosOrIolist, Opts) -> ok when
      ProtosOrIolist :: [{Filename::string(), ProtoText::iolist()}]
                      | iolist(),
      Opts :: gpb_compile:opts().
verify_defs_roundtrip_via_descr(ProtosOrIolist, Opts0) ->
    %% Parse definitions, then round-trip via the descriptor
    %% and check that we got back what we had initially.
    %%
    %% Allow for order of elements to vary within each file, for example
    %% order of messages, or order of enums versus messages etc, by
    %% sorting within each chunk/file.

    Opts1 = default_add_latest_option_unless_set(Opts0),
    Opts2 = [return_also_str_defs | Opts1],
    {{ok, Parsed}, Defs} = roundtrip_via_descr(ProtosOrIolist, Opts2),
    io:format("~nStrDefs=~p~n"
              "Parsed =~p~n", [proto_sort(Defs), proto_sort(Parsed)]),
    ?assertEqual(proto_sort(Defs),
                 proto_sort(Parsed)),
    ok.

roundtrip_via_descr(ProtosOrIolist, Opts) ->
    {ReturnStrDefs, Opts1} = opt_take_bool(return_also_str_defs, Opts),
    Opts2 = [return_also_str_defs | Opts1],
    {Defs, Fds} = to_file_descriptor_set(ProtosOrIolist, Opts2),
    Res = from_file_descriptor_set(Fds, Opts2),
    if not ReturnStrDefs -> Res;
       ReturnStrDefs     -> {Res, Defs}
    end.

to_file_descriptor_set(ProtosOrIolist) ->
    to_file_descriptor_set(ProtosOrIolist, []).

to_file_descriptor_set(ProtosOrIolist, Opts) ->
    Opts1 = default_add_latest_option_unless_set(Opts),
    {ReturnStrDefs, Opts2} = opt_take_bool(return_also_str_defs, Opts1),
    Protos = case is_list_of_two_tuples(ProtosOrIolist) of
                 true -> ProtosOrIolist;
                 false -> [{"main.proto", ProtosOrIolist}]
             end,
    {ok, Defs, []} =
        gpb_compile_descr_tests:compile_files_as_iolists(
          Protos,
          Opts2),
    {Bin, _PBins} = gpb_compile_descr:encode_defs_to_descriptors(Defs, Opts2),
    Res = gpb_descriptor:decode_msg(Bin, 'FileDescriptorSet'),
    if not ReturnStrDefs -> Res;
       ReturnStrDefs     -> {Defs, Res}
    end.

from_file_descriptor_set(Fds, Opts) ->
    gpb_parse_descr:defs_from_descriptors(Fds, Opts).

default_add_latest_option_unless_set(Opts) ->
    case proplists:is_defined(proto_defs_version, Opts) of
        true  -> Opts;
        false -> [{proto_defs_version, gpb_defs:latest_defs_version()} | Opts]
    end.

is_list_of_two_tuples(X) when is_list(X) ->
    case is_proper_list(X) of
        true ->
            lists:all(fun({_, _}) -> true;
                         (_) -> false
                      end,
                      X);
        false ->
            false
    end;
is_list_of_two_tuples(_) ->
    false.

is_proper_list(L) ->
    try length(L) of
        _ -> true
    catch error:badarg ->
            false
    end.

opt_take_bool(Key, Opts) ->
    case proplists:get_bool(Key, Opts) of
        false -> {false, Opts};
        true -> {true, lists:keydelete(Key, 1, Opts) -- [Key]}
    end.

%% Split off chunks starting at {file, ...} elements and sort each chunk
proto_sort(Defs) ->
    Chunks = split_to_proto_chunks(Defs),
    lists:append([lists:sort(Chunk) || Chunk <- Chunks]).

split_to_proto_chunks(Defs) ->
    spl(Defs,
        [], % Current chunk (reversed)
        []  % All chunks (reversed)
       ).

spl([{proto_defs_version,_}=Elem | Rest], Curr, Acc) ->
    Acc1 = add_to_acc([Elem | Curr], Acc),
    spl(Rest, [], Acc1);
spl([{file, _}=Elem | Rest], Curr, Acc) ->
    Acc1 = add_to_acc(Curr, Acc),
    spl(Rest, [Elem], Acc1);
spl([Elem | Rest], Curr, Acc) ->
    spl(Rest, [Elem | Curr], Acc);
spl([], Curr, Acc) ->
    lists:reverse(add_to_acc(Curr, Acc)).

add_to_acc([], Acc) -> Acc; % don't add empty chunks
add_to_acc(Curr, Acc) -> [lists:reverse(Curr) | Acc].

assert_is_iodata([Elem | Rest]) ->
    assert_is_iodata(Elem),
    assert_is_iodata(Rest);
assert_is_iodata(X) ->
    if is_integer(X), 0 =< X, X =< 16#10ffff -> true;
       is_binary(X) -> true;
       X == [] -> true;
       true -> error({non_iodata_elem, X})
    end.

%% -- set value by dotted path

set(Record, FieldRef, NewValue) ->
    Path = split_path_components(atom_to_list(FieldRef)),
    set_aux(Record, Path, NewValue).

split_path_components(S) ->
    case collect_until(S, "[.") of
        {F, "[" ++ Rest} -> [{field, l2a(F)} | split_array_ref(Rest)];
        {F, "." ++ Rest} -> [{field, l2a(F)} | split_path_components(Rest)];
        {F, ""}          -> [{field, l2a(F)}]
    end.

split_array_ref(S) ->
    case collect_until(S, "]") of
        {IndexExpr, "]" ++ Rest} ->
            ARef = {array_ref, analysze_array_ref(IndexExpr)},
            case Rest of
                ""           -> [ARef];
                "." ++ Rest2 -> [ARef | split_path_components(Rest2)]
            end
    end.

-define(is_digit(D), ($0 =< (D) andalso (D) =< $9)).
analysze_array_ref("<"++Rest) ->
    analyze_array_field_pred(Rest);
analysze_array_ref([D | _]=NumStr) when ?is_digit(D) ->
    list_to_integer(NumStr).

analyze_array_field_pred(S) ->
    {Field, "=" ++ Rest} = collect_until(S, "="),
    {Str, ">"} = collect_until(Rest, ">"),
    {l2a(Field), '==', parse_term(Str)}.

set_aux(_Valye, [], NewValue) ->
    NewValue;
set_aux(Record, [{field, F} | Rest], NewValue) ->
    FV = get_record_field(Record, F),
    FV1 = set_aux(FV, Rest, NewValue),
    set_record_field(Record, F, FV1);
set_aux(Array, [{array_ref, IndexExpr} | Rest], NewValue) ->
    {Index, Elem} = get_array_elem(Array, IndexExpr),
    Elem2 = set_aux(Elem, Rest, NewValue),
    set_array_elem(Array, Index, Elem2).

collect_until(S, Terminators) ->
    lists:splitwith(fun(C) -> not lists:member(C, Terminators) end, S).

parse_term(S) ->
    {ok, Tokens, End} = erl_scan:string(S),
    {ok, Value} = erl_parse:parse_term(Tokens ++ [{dot, End}]),
    Value.

l2a(Str) ->
    list_to_atom(Str).

-define(rmatch(RName, R, Op),
        RName -> Op(RName, lists:zip(record_info(fields, RName),
                                     tl(tuple_to_list(R))))).

get_record_field(Record, FName) ->
    Op = fun(_RName, FNamesValues) ->
                 {FName, FValue} = lists:keyfind(FName, 1, FNamesValues),
                 FValue
         end,
    %% Those that can happen in this file:
    case element(1, Record) of
        ?rmatch('FileDescriptorSet', Record, Op);
        ?rmatch('FileDescriptorProto', Record, Op);
        ?rmatch('DescriptorProto', Record, Op);
        ?rmatch('FieldDescriptorProto', Record, Op)
    end.

set_record_field(Record, FName, NewValue) ->
    Op = fun(RName, FNamesValues) ->
                 FNVs1 = lists:keyreplace(FName, 1, FNamesValues,
                                          {FName, NewValue}),
                 list_to_tuple([RName | [V || {_N, V} <- FNVs1]])
         end,
    %% Those that can happen in this file:
    case element(1, Record) of
        ?rmatch('FileDescriptorSet', Record, Op);
        ?rmatch('FileDescriptorProto', Record, Op);
        ?rmatch('DescriptorProto', Record, Op);
        ?rmatch('FieldDescriptorProto', Record, Op)
    end.

get_array_elem(Array, I) when is_integer(I) ->
    {I, lists:nth(I+1, Array)};
get_array_elem(Array, ElemPred) ->
    get_array_elem_aux(Array, ElemPred, 0).

get_array_elem_aux([Elem | Rest], Pred, I) ->
    case test_pred(Pred, Elem) of
        true -> {I, Elem};
        false -> get_array_elem_aux(Rest, Pred, I+1)
    end.

test_pred({Field, '==', Value}, Elem) ->
    get_record_field(Elem, Field) == Value.

set_array_elem([_Elem | Rest], 0, New) -> [New | Rest];
set_array_elem([Elem | Rest], N, New) ->
    [Elem | set_array_elem(Rest, N+1, New)].
