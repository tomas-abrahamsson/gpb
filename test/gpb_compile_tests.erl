%%% Copyright (C) 2010-2011  Tomas Abrahamsson
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

-module(gpb_compile_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("gpb_nif_test_helpers.hrl"). % the `?nif_if_supported(FnName)' macro

-export([test_nifs/1]). %% set whether to test nifs or not

-export([compile_iolist/2]).
-export([compile_protos/2]).
-export([compile_to_string_get_hrl/2]).
-export([compile_erl_iolist/1]).
-export([unload_code/1]).

%% NIF related
-export([nif_tests_check_prerequisites/1]).
-export([check_nif_features_supported/2]).
-export([guess_features/1]).
-export([with_tmpdir/1, with_tmpdir/2]).
-export([in_separate_vm/4]).
-export([compile_nif_msg_defs/3, compile_nif_msg_defs/4]).
-export([compile_nif_several_msg_defs/4]).
-export([check_protoc_can_do_oneof/0]).
-export([check_protoc_can_do_mapfields/0]).
-export([check_protoc_can_do_proto3/0]).
-export([check_protoc_can_do_json/0]).

%% internally used
-export([main_in_separate_vm/1]).

%% Translators for without user-data
-export([any_e_atom/1, any_d_atom/1, any_m_atom/2, any_v_atom/2]).
-export([any_v_atom/1]).
%% Translators for user-data
-export([any_e_atom/2, any_d_atom/2, any_m_atom/3, any_v_atom/3]).
%% Translators for user-data and op
-export([any_e_atom/3, any_d_atom/3, any_m_atom/4, any_v_atom/4]).

%% Translators for {translate_type, {{msg,uuid},...}} option tests:
-export([uuid_e/1, uuid_d/1, uuid_m/2, uuid_v/1]).

%% Translators for {translate_type, {bytes,...}} tests
-export([e_ipv4_addr/2, d_ipv4_addr/2, v_ipv4_addr/2]).

%% Translators for {translate_type, {<Scalar>,...}} tests
-export([e_astt/1, d_astt/1, v_astt/1]).

%% Translators for {translate_type, {{map,_,_},...}} test
-export([is_dict/1]).

%% Translators for {translate_field, {<Oneof>,...}} tests
-export([e_ipv4or6/1, d_ipv4or6/1, v_ipv4or6/1]).

%% Translators for {translate_field, {<Repeated>,...} tests
-export([id/1, v_is_set/1]).

%% Translators for top-level message tests
-export([e_value_to_msg/2, d_msg_to_value/2, v_value/2]).


-ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R:St ->).
-else. % -ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R -> St = erlang:get_stacktrace(),).
-endif. % -ifdef(OTP_RELEASE).

%% Include a bunch of tests from gpb_tests.
%% The shared tests are for stuff that must work both
%% for gpb and for the code that gpb_compile generates.
%% (I know it is a bit unorthodox to include .erl files,
%% but actually seems to work. Better than duplicating tests)
-define(gpb_compile_common_tests, true).
-ifdef(gpb_compile_common_tests).
-include("gpb_tests.erl").
-else.  %% gpb_compile_common_tests
-record(m1,{a}).
-endif. %% gpb_compile_common_tests

-define(is_string(X), is_list(X)).

parses_non_importing_file_test() ->
    Contents = iolist_to_binary(
                 ["message Msg { required uint32 field1 = 1; }\n"]),
    ok = gpb_compile:file(
           "X.proto",
           [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
            mk_defs_probe_sender_opt(self()),
            {i,"."}]),
    [{{msg,'Msg'},_}] = receive_filter_sort_msgs_defs().


parses_importing_file_test() ->
    ContentsX = iolist_to_binary(
                  ["import \"Y.proto\";\n"
                   "message X { required Y f1 = 1; }\n"]),
    ContentsY = iolist_to_binary(
                  ["message Y { required uint32 f1 = 1; }\n"]),
    ok = gpb_compile:file(
           "X.proto",
           [mk_fileop_opt([{read_file, fun("X.proto") -> {ok, ContentsX};
                                          ("Y.proto") -> {ok, ContentsY}
                                       end}]),
            mk_defs_probe_sender_opt(self()),
            {i,"."}]),
    [{{msg,'X'},_}, {{msg,'Y'},_}] = receive_filter_sort_msgs_defs().


import_fetcher_test() ->
    Master = self(),
    ContentsX = iolist_to_binary(
                  ["import \"Y.proto\";\n"
                   "message X { required Y f1 = 1; }\n"]),
    ContentsY = iolist_to_binary(
                  ["message Y { required uint32 f1 = 1; }\n"]),
    FileReadOpt = mk_fileop_opt([{read_file,
                                  fun("X.proto") ->
                                          Master ! {read,"X.proto"},
                                          {ok, ContentsX};
                                     ("Y.proto") ->
                                          Master ! {read,"Y.proto"},
                                          {ok, ContentsY}
                                  end}]),
    %% Importer returns `from_file'
    ok = gpb_compile:file(
           "X.proto",
           [{import_fetcher, fun(F) -> Master ! {redir_to_file,F},
                                       from_file
                             end},
            FileReadOpt, {i,"."}]),
    [{redir_to_file,"X.proto"},
     {read,"X.proto"},
     {redir_to_file,"Y.proto"},
     {read,"Y.proto"}] = flush_msgs(),

    %% Importer returns contents
    ok = gpb_compile:file(
           "X.proto",
           [{import_fetcher, fun(F) ->
                                     Master ! {fetched,F},
                                     {ok, binary_to_list(
                                            case F of
                                                "X.proto" -> ContentsX;
                                                "Y.proto" -> ContentsY
                                            end)}
                             end},
            FileReadOpt, {i,"."}]),
    [{fetched,"X.proto"},
     {fetched,"Y.proto"}] = flush_msgs(),

    %% Importer returns error
    {error,{fetcher_issue,"Y.proto",reason_for_y}} =
        gpb_compile:file(
          "X.proto",
          [{import_fetcher, fun(F) ->
                                    case F of
                                        "X.proto" -> from_file;
                                        "Y.proto" -> {error,reason_for_y}
                                    end
                            end},
           FileReadOpt, {i,"."}]),

    %% Make sure we've flushed every message, synchronously
    ok = gpb_compile:string(x, binary_to_list(ContentsY),
                            [mk_defs_probe_sender_opt(self()),
                             mk_fileop_opt([])]),
    [_|_] = receive_filter_sort_msgs_defs(),
    flush_msgs(),
    ok.



flush_msgs() ->
    receive
        M ->
             [M | flush_msgs()]
    after 0 ->
            []
    end.

parses_file_to_binary_test() ->
    Contents = <<"message Msg { required uint32 field1 = 1; }\n">>,
    {ok, 'X', Code, []} =
        gpb_compile:file(
          "X.proto",
          [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
           mk_defs_probe_sender_opt(self()),
           {i,"."},
           binary, return_warnings]),
    true = is_binary(Code),
    [{{msg,'Msg'},_}] = receive_filter_sort_msgs_defs().

parses_file_to_msg_defs_test() ->
    Contents = <<"message Msg { required uint32 field1 = 1; }\n">>,
    {ok, [{file, _},
          {{msg_containment,"X"},['Msg']},
          {{enum_containment, _}, _},
          {{msg,'Msg'},[#?gpb_field{}]}]=MsgDefs} =
        gpb_compile:file(
          "X.proto",
          [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
           {i,"."},
           to_proto_defs, report_warnings]),
    %% Check that the returned msgdefs are usable
    M = compile_defs(MsgDefs),
    ?assertMatch(<<_/binary>>, M:encode_msg({'Msg',33})),
    unload_code(M).

parses_msgdefs_to_binary_test() ->
    Defs = [{{msg,'Msg'},
             [#?gpb_field{name=field1, rnum=2, fnum=1, type=uint32,
                          occurrence=required, opts=[]}]}],
    M = find_unused_module(),
    {ok, M, Code} = gpb_compile:proto_defs(M, Defs, [binary]),
    true = is_binary(Code).

parses_and_generates_good_code_also_for_reserved_keywords_test() ->
    %% use erlang reserved words on as many .proto locations as possible
    %% to verify that the generated code compiles and works.
    M = compile_iolist(["enum if { begin=1; end=2; }"
                        "message catch { required if case = 1; }\n"]),
    ?assertMatch(true, is_binary(M:encode_msg({'catch', 'begin'}))),
    unload_code(M).

parses_and_generates_good_code_also_for_empty_msgs_test() ->
    M = compile_iolist(["message m1 { }\n"]),
    ?assertMatch(true, is_binary(M:encode_msg({m1}))),
    ?assertMatch({m1}, M:decode_msg(M:encode_msg({m1}), m1)),
    unload_code(M).

encoding_decoding_functions_for_epb_compatibility_test() ->
    epb_encoding_decoding_functions_aux(epb_compatibility).

encoding_decoding_functions_for_epb_functions_test() ->
    epb_encoding_decoding_functions_aux(epb_functions).

epb_encoding_decoding_functions_aux(Opt) ->
    DefsM1 = "message m1 { required uint32 a = 1; }\n",
    DefsNoMsgs = "enum ee { a = 0; }\n",
    {error, Reason1, []} = compile_iolist_get_errors_or_warnings(
                             DefsM1,
                             [Opt, maps]),
    true = is_list(gpb_compile:format_error(Reason1)),

    %% Verify we get an error for epb_compatibility with a message named 'msg'
    %% due to collision with standard gpb encode_msg/decode_msg functions
    {error, Reason2, []} = compile_iolist_get_errors_or_warnings(
                             "message msg { }\n",
                             [Opt, maps]),
    true = is_list(gpb_compile:format_error(Reason2)),

    Mod1 = compile_iolist(DefsM1, [Opt]),
    M1 = #m1{a=1234},
    B1 = Mod1:encode(M1),
    ?assertMatch(true, is_binary(B1)),
    B1 = Mod1:encode_m1(M1),
    M1 = Mod1:decode(m1, B1),
    M1 = Mod1:decode_m1(B1),
    unload_code(Mod1),

    %% verify no compatibility functions generated with no compat options
    Mod2 = compile_iolist(DefsM1, []),
    ?assertError(undef, Mod2:encode(M1)),
    ?assertError(undef, Mod2:encode_m1(M1)),
    ?assertError(undef, Mod2:decode(m1, B1)),
    ?assertError(undef, Mod2:decode_m1(B1)),
    unload_code(Mod2),

    %% verify functions generated ok when no msgs specified
    Mod3 = compile_iolist(DefsNoMsgs, [Opt]),
    _ = Mod3:module_info(),
    unload_code(Mod3).

epb_compatibility_opt_implies_pb_modsuffix_test() ->
    Contents = <<"message m { required uint32 f = 1; }\n">>,
    T = self(),
    ok = gpb_compile:file(
           "X.proto",
           [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end},
                           {write_file, fun(Nm, _) -> T ! {file_name, Nm},
                                                      ok
                                        end}]),
            {i,"."},
            epb_compatibility]),
    ["X_pb.erl", "X_pb.hrl"] =
        lists:sort([receive {file_name, Nm1} -> Nm1 end,
                    receive {file_name, Nm2} -> Nm2 end]).

epb_compatibility_opt_implies_msg_name_to_lower_test() ->
    Contents = <<"message SomeMsg { required uint32 f = 1; }\n">>,
    ok = gpb_compile:file(
           "X.proto",
           [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
            mk_defs_probe_sender_opt(self()),
            {i,"."},
            epb_compatibility]),
    [{{msg,somemsg},_}] = receive_filter_sort_msgs_defs().

epb_compatibility_opt_implies_defaults_for_omitted_optionals_test() ->
    Proto = ["message m {",
             "  optional uint32 f = 1 [default=3];\n",
             %% for verifying that type-defaults is not implied:
             "  optional uint32 g = 2;\n",
             "}\n"],
    M = compile_iolist(Proto, [epb_compatibility]),
    {m,3,undefined} = M:decode_msg(<<>>, m),
    unload_code(M).

bypassed_wrappers_records_test() ->
    DefsM1 = "message m1 { required uint32 a = 1; }\n",
    DefsNoMsgs = "enum ee { a = 0; }\n",

    Mod1 = compile_iolist(DefsM1, [bypass_wrappers]),
    M1 = #m1{a=1234},
    B1 = Mod1:encode_msg_m1(M1),
    B1 = Mod1:encode_msg_m1(M1, undefined),
    ?assertMatch(true, is_binary(B1)),
    M1 = Mod1:decode_msg_m1(B1),
    M1 = Mod1:decode_msg_m1(B1, undefined),
    unload_code(Mod1),

    %% verify no compatibility functions generated with no compat options
    Mod2 = compile_iolist(DefsM1, []),
    ?assertError(undef, Mod2:encode_msg_m1(M1)),
    ?assertError(undef, Mod2:decode_msg_m1(B1)),
    unload_code(Mod2),

    %% verify functions generated ok when no msgs specified
    Mod3 = compile_iolist(DefsNoMsgs, []),
    _ = Mod3:module_info(),
    unload_code(Mod3).

no_gen_introspect_test() ->
    DefsM1 = "message m1 { required uint32 a = 1; }\n",

    Mod1 = compile_iolist(DefsM1, [{gen_introspect,false}]),
    M1 = #m1{a=1234},
    B1 = Mod1:encode_msg(M1),
    M1 = Mod1:decode_msg(B1, m1),
    Fn = get_msg_names,
    ?assertError(undef, Mod1:Fn()),
    unload_code(Mod1),
    %% verify function gets generated with no option (to verify test works)
    Mod2 = compile_iolist(DefsM1, []),
    [_] = Mod2:Fn(),
    unload_code(Mod2).

field_pass_as_params_test() ->
    {timeout,10,fun field_pass_as_params_test_aux/0}.

field_pass_as_params_test_aux() ->
    MsgDef = ["message m2 { required uint32 f22 = 1; }"
              "message m1 { required uint32  f1 = 1;",
              "             optional fixed32 f2 = 2;",
              "             repeated fixed32 f3 = 3;",
              "             repeated fixed32 f4 = 4 [packed];",
              "             repeated uint32  f5 = 5;",
              "             repeated uint32  f6 = 6 [packed];",
              "             optional string  f7 = 7;",
              "             optional m2      f8 = 8;",
              "             oneof o1 { m2     x1 = 15;",
              "                        uint32 y1 = 16; };",
              "             oneof o2 { m2     x2 = 25;",
              "                        uint32 y2 = 26; }",
              "             oneof o3 { m2     x3 = 35;",
              "                        uint32 y3 = 36; }",
              "}"],
    Msg = {m1, 4711, undefined,      %% f1,f2
           [4713,4714], [4715,4716], %% f3,f4
           [4717,4718], [4719,4720], %% f5,f6
           "abc", {m2,33}, {x1,{m2,45}}, {y2,226}, undefined},
    lists:foreach(
      fun(Opts) ->
              ?assertMatch({Msg,_},
                           {encode_decode_round_trip(MsgDef, Msg, Opts), Opts})
      end,
      [[{field_pass_method,pass_as_params}],
       [{field_pass_method,pass_as_record}],
       [{{field_pass_method,m1},pass_as_record},
        {{field_pass_method,m2},pass_as_params}],
       [{{field_pass_method,m1},pass_as_params},
        {{field_pass_method,m2},pass_as_record}]]).

encode_decode_round_trip(MsgDefAsIoList, Msg, Opts) ->
    M = compile_iolist(MsgDefAsIoList, Opts),
    MsgName = element(1, Msg),
    Result = M:decode_msg(M:encode_msg(Msg), MsgName),
    unload_code(M),
    Result.

mk_fileop_opt(NonDefaults) ->
    NonDefaults1 = [case Op of
                        read_file_info -> {Op, mk_with_basename_1(Fn)};
                        read_file      -> {Op, mk_with_basename_1(Fn)};
                        write_file     -> {Op, mk_with_basename_2(Fn)}
                    end
                    || {Op, Fn} <- NonDefaults],
    {file_op, NonDefaults1 ++ mk_default_file_ops()}.

mk_with_basename_1(Fn) -> fun(Path) -> Fn(filename:basename(Path)) end.
mk_with_basename_2(Fn) -> fun(Path, A) -> Fn(filename:basename(Path), A) end.

mk_default_file_ops() ->
    [{read_file_info, fun(_FileName) -> {ok, #file_info{access=read}} end},
     {read_file,      fun(_FileName) -> {error, enoent} end},
     {write_file,     fun(_FileName, _Bin) -> ok end}].

mk_defs_probe_sender_opt(SendTo) ->
    {probe_defs, fun(Defs) -> SendTo ! {defs, Defs} end}.

receive_filter_sort_msgs_defs() ->
    lists:sort([Msg || {{msg,_},_} = Msg <- receive {defs, Defs} -> Defs end]).

-record(m9,{aa, bb, cc, dd}).
-record(m10,{aaa}).

code_generation_when_submsg_size_is_known_at_compile_time_test() ->
    KnownSizeM9 =
        [{{msg,m9}, [#?gpb_field{name=aa, type={enum,e}, occurrence=required,
                                 fnum=1, rnum=#m9.aa, opts=[]},
                     #?gpb_field{name=bb, type=fixed32, occurrence=required,
                                 fnum=2, rnum=#m9.bb, opts=[]},
                     #?gpb_field{name=cc, type=fixed64, occurrence=required,
                                 fnum=3, rnum=#m9.cc, opts=[]},
                     #?gpb_field{name=dd, type={msg,m10}, occurrence=required,
                                 fnum=4, rnum=#m9.dd, opts=[]}]}],
    UnknownSizeM9 =
        [{{msg,m9}, [#?gpb_field{name=aa, type={enum,e}, occurrence=optional,
                                 fnum=1, rnum=#m9.aa, opts=[]},
                     #?gpb_field{name=bb, type=fixed32, occurrence=optional,
                                 fnum=2, rnum=#m9.bb, opts=[]},
                     #?gpb_field{name=cc, type=fixed64, occurrence=optional,
                                 fnum=3, rnum=#m9.cc, opts=[]},
                     #?gpb_field{name=dd, type={msg,m10}, occurrence=required,
                                 fnum=4, rnum=#m9.dd, opts=[]}]}],

    CommonDefs =
        [{{msg,m1}, [#?gpb_field{name=a, type={msg,m9}, occurrence=required,
                                 fnum=1, rnum=2, opts=[]}]},
         {{msg,m10},[#?gpb_field{name=aaa, type=bool, occurrence=required,
                                 fnum=1, rnum=#m10.aaa, opts=[]}]},
         {{enum,e}, [{x1, 1}, {x2, 2}]} %% all enum values same encode size
        ],

    M1 = compile_defs(CommonDefs++KnownSizeM9),
    M2 = compile_defs(CommonDefs++UnknownSizeM9),
    Msg = #m1{a=#m9{aa=x1, bb=33, cc=44, dd=#m10{aaa=true}}},
    Encoded1 = M1:encode_msg(Msg),
    Encoded2 = M2:encode_msg(Msg),
    Encoded1 = Encoded2,
    unload_code(M1),
    unload_code(M2).

code_generation_when_map_enum_size_is_unknown_at_compile_time_test() ->
    %% calculation of whether e_varint is needed or not when only
    %% an enum needs it, when that enum is in a map
    Defs = [{{msg,m1}, [#?gpb_field{name=a, type={map,bool,{enum,e1}},
                                    fnum=1, rnum=2, occurrence=repeated,
                                    opts=[]}]},
            {{enum,e1}, [{x1,0},{x2,128}]}], %% encodes to different sizes
    M = compile_defs(Defs),
    true = is_binary(M:encode_msg({m1,[{true,x1}]})),
    unload_code(M).

no_dialyzer_attributes_for_erlang_version_pre_18_test() ->
    %% -dialyzer({nowarn_function,f/1}). attrs first appeared in Erlang/OTP 18
    %% such attributes are emitted for verifiers and map translators
    Proto = "message m { map<uint32,string> m = 1; }",
    S1 = compile_to_string(Proto, [{target_erlang_version,17}]),
    false = gpb_lib:is_substr("-dialyzer(", S1),
    S2 = compile_to_string(Proto, [{target_erlang_version,18}]),
    true = gpb_lib:is_substr("-dialyzer(", S2).

empty_group_test() ->
    M = compile_iolist("syntax = \"proto2\";
                        message Quz {optional group E = 11 {}}",
                       [{verify,always},
                        to_proto_defs]),
    EStart = <<91>>, % (11 bsl 3) bor 3
    EEnd = <<92>>,   % (11 bsl 3) bor 4
    EGroup = <<EStart/binary, EEnd/binary>>,
    EGroup = M:encode_msg({'Quz',{'Quz.E'}}),
    unload_code(M).

%% --- default values --------------

default_value_handling_test() ->
    Proto = ["message m {",
             "  optional uint32 f1 = 1;",
             "  optional uint32 f2 = 2 [default=2];",
             "}"],
    [begin
         M = compile_iolist(Proto, Opts ++ OptVariation),
         ?assertMatch({Expected,_}, {M:decode_msg(<<>>, m), Opts}),
         unload_code(M)
     end
     || {Expected, Opts} <-
            [{{m,undefined,undefined}, []},
             {{m,0,2},         [defaults_for_omitted_optionals,
                                type_defaults_for_omitted_optionals]},
             {{m,undefined,2}, [defaults_for_omitted_optionals]},
             {{m,0,0},         [type_defaults_for_omitted_optionals]}],
        OptVariation <- [[pass_as_params],
                         [pass_as_record]]].

type_default_option_should_be_ignored_for_proto3_test() ->
    Proto = ["syntax=\"proto3\";\n",
             "message m { uint32 f1 = 1; };"],
    M = compile_iolist(Proto, [{type_defaults_for_omitted_optionals, false}]),
    {m,0} = M:decode_msg(<<>>, m),
    unload_code(M).

%% --- introspection ---------------

introspection_package_name_test() ->
    M = compile_iolist(["package foo.bar;",
                        "message M { required uint32 f1=1; }"]),
    'foo.bar' = M:get_package_name(),
    unload_code(M),
    M = compile_iolist(["message M { required uint32 f1=1; }"]),
    undefined = M:get_package_name(),
    unload_code(M).

introspection_uses_packages_test() ->
    M = compile_iolist(["package foo.bar;",
                        "message M { required uint32 f1=1; }"],
                       [use_packages]),
    true = M:uses_packages(),
    unload_code(M),
    M = compile_iolist(["message M { required uint32 f1=1; }"]),
    false = M:uses_packages(),
    unload_code(M).

introspection_msgs_test() ->
    M = compile_iolist(["message msg1 { required uint32 f1=1; }"]),
    [msg1] = M:get_msg_names(),
    [#?gpb_field{name=f1, type=uint32, fnum=1}] = M:find_msg_def(msg1),
    [#?gpb_field{name=f1, type=uint32, fnum=1}] = M:fetch_msg_def(msg1),
    error = M:find_msg_def(msg_ee),
    ?assertError(_, M:fetch_msg_def(msg_ee)),
    unload_code(M).

introspection_enums_test() ->
    %% Names
    M = compile_iolist(["enum e1 { n1=1; n2=2; }",
                        "message msg1 { required uint32 f1=1; }"]),
    [e1] = M:get_enum_names(),
    %% find and fetch
    [{n1,1},{n2,2}] = M:find_enum_def(e1),
    error = M:find_enum_def(ee),
    [{n1,1},{n2,2}] = M:fetch_enum_def(e1),
    ?assertError(_, M:fetch_enum_def(ee)),
    %% symbol <--> value mapping
    n1 = M:enum_symbol_by_value(e1, 1),
    n2 = M:enum_symbol_by_value(e1, 2),
    n1 = M:enum_symbol_by_value_e1(1),
    n2 = M:enum_symbol_by_value_e1(2),
    1  = M:enum_value_by_symbol(e1, n1),
    2  = M:enum_value_by_symbol(e1, n2),
    1  = M:enum_value_by_symbol_e1(n1),
    2  = M:enum_value_by_symbol_e1(n2),
    unload_code(M).

introspection_groups_test() ->
    M0 = compile_iolist(["enum e1 { n1=1; n2=2; }"]), % no message or groups
    [] = M0:get_msg_names(),
    [] = M0:get_group_names(),
    [] = M0:get_msg_or_group_names(),
    error = M0:find_msg_def(e1), % e1 for the lack of better...
    ?assertError(_, M0:fetch_msg_def(e1)),
    M1 = compile_iolist(["message m1 {",
                         "  required group g = 1 { required uint32 f = 2; };",
                         "}"]),
    [m1] = M1:get_msg_names(),
    [G] = M1:get_group_names(),
    [m1, G] = lists:sort(M1:get_msg_or_group_names()),
    [#?gpb_field{name=g, type={group,G}}] = M1:find_msg_def(m1),
    [#?gpb_field{name=f, type=uint32}]    = M1:find_msg_def(G),
    [#?gpb_field{name=g, type={group,G}}] = M1:fetch_msg_def(m1),
    [#?gpb_field{name=f, type=uint32}]    = M1:fetch_msg_def(G),
    unload_code(M0),
    unload_code(M1).

introspection_defs_as_proplists_test() ->
    Proto = ["message msg1 { required uint32 f1=1; }",
             "service s1 {",
             "  rpc req1(msg1) returns (msg1);",
             "  rpc req2(msg1) returns (msg1);",
             "}",
             "service s2 {",
             "  rpc req2(msg1) returns (msg1);",
             "}"],
    %% With the defs_as_proplists option
    M = compile_iolist(Proto, [defs_as_proplists]),
    NoIStr = {input_stream, false},
    NoOStr = {output_stream, false},
    NoOpts = {opts, []},
    [[{name,       f1},
      {fnum,       1},
      {rnum,       2},
      {type,       uint32},
      {occurrence, required},
      {opts,       []}]] = PL = M:find_msg_def(msg1),
    [{{msg, msg1}, PL}] = M:get_msg_defs(),
    [s1, s2] = M:get_service_names(),
    {{service, s1},
     [[{name, req1}, {input, msg1}, {output, msg1}, NoIStr, NoOStr, NoOpts],
      [{name, req2}, {input, msg1}, {output, msg1}, NoIStr, NoOStr, NoOpts]]} =
        M:get_service_def(s1),
    {{service, s2},
     [[{name, req2}, {input, msg1}, {output, msg1}, NoIStr, NoOStr, NoOpts]]} =
        M:get_service_def(s2),
    [{name, req1}, {input, msg1}, {output, msg1}, NoIStr, NoOStr, NoOpts] =
        M:find_rpc_def(s1, req1),
    [{name, req2}, {input, msg1}, {output, msg1}, NoIStr, NoOStr, NoOpts] =
        M:find_rpc_def(s2, req2),
    unload_code(M),

    %% No defs_as_proplists option
    M = compile_iolist(Proto, [{defs_as_proplists, false}]),
    [#?gpb_field{name       = f1,
                 fnum       = 1,
                 rnum       = 2,
                 type       = uint32,
                 occurrence = required,
                 opts       = []}] = Fs = M:find_msg_def(msg1),
    [{{msg, msg1}, Fs}] = Defs = M:get_msg_defs(),
    {{service, s1},
     [#?gpb_rpc{name=req1, input=msg1, output=msg1},
      #?gpb_rpc{name=req2, input=msg1, output=msg1}]} =
        M:get_service_def(s1),
    {{service, s2},
     [#?gpb_rpc{name=req2, input=msg1, output=msg1}]} =
        M:get_service_def(s2),
    #?gpb_rpc{name=req1, input=msg1, output=msg1} = M:find_rpc_def(s1, req1),
    #?gpb_rpc{name=req2, input=msg1, output=msg1} = M:find_rpc_def(s2, req2),
    unload_code(M),

    %% make sure the generated erl file does not -include[_lib] "gpb.hrl"
    Master = self(),
    ReportOutput = fun(FName, Contents) ->
                           Master ! {filename:extension(FName), Contents},
                           ok
                   end,
    FileOpOpt = mk_fileop_opt([{write_file, ReportOutput}]),
    ok = gpb_compile:proto_defs(M, Defs, [FileOpOpt, defs_as_proplists]),
    receive {".hrl", Hrl1} -> nomatch = re:run(Hrl1, "\"gpb.hrl\"") end,
    receive {".erl", Erl1} -> nomatch = re:run(Erl1, "\"gpb.hrl\"") end,
    ok = gpb_compile:proto_defs(M, Defs, [FileOpOpt, defs_as_proplists,
                                          include_as_lib]),
    receive {".hrl", Hrl2} -> nomatch = re:run(Hrl2, "\"gpb.hrl\"") end,
    receive {".erl", Erl2} -> nomatch = re:run(Erl2, "\"gpb.hrl\"") end.

introspection_rpcs_test() ->
    Proto = ["message m1 { required uint32 f1=1; }",
             "message m2 { required uint32 f2=1; }",
             "service s1 {",
             "  rpc req1(m1) returns (m2);",
             "  rpc req2(m2) returns (m1);",
             "}"],
    M = compile_iolist(Proto),
    {{service, s1},
     [#?gpb_rpc{name=req1, input='m1', output='m2'},
      #?gpb_rpc{name=req2, input='m2', output='m1'}]} = M:get_service_def(s1),
    [req1, req2] = M:get_rpc_names(s1),
    #?gpb_rpc{name=req1, input='m1', output='m2'} = M:find_rpc_def(s1, req1),
    #?gpb_rpc{name=req1, input='m1', output='m2'} = M:fetch_rpc_def(s1, req1),
    #?gpb_rpc{name=req2, input='m2', output='m1'} = M:fetch_rpc_def(s1, req2),
    #?gpb_rpc{name=req2, input='m2', output='m1'} = M:find_rpc_def(s1, req2),
    error = M:find_rpc_def(s1, req_ee),
    ?assertError(_, M:fetch_rpc_def(s2, req_ee)),
    unload_code(M).

introspection_multiple_rpcs_test() ->
    Proto = ["message m1 { required uint32 f1=1; }",
             "message m2 { required uint32 f2=1; }",
             "service s1 {",
             "  rpc req1(m1) returns (m2);",
             "  rpc req2(m2) returns (m1);",
             "}",
             "service s2 {",
             "  rpc req21(m2) returns (m1);",
             "  rpc req22(m1) returns (m2);",
             "}"],
    M = compile_iolist(Proto),
    [s1, s2] = M:get_service_names(),
    {{service, s1},
     [#?gpb_rpc{name=req1, input='m1', output='m2'},
      #?gpb_rpc{name=req2, input='m2', output='m1'}]} = M:get_service_def(s1),
    {{service, s2},
     [#?gpb_rpc{name=req21, input='m2', output='m1'},
      #?gpb_rpc{name=req22, input='m1', output='m2'}]} = M:get_service_def(s2),
    #?gpb_rpc{name=req21,  input='m2', output='m1'} = M:find_rpc_def(s2, req21),
    #?gpb_rpc{name=req1, input='m1', output='m2'} = M:fetch_rpc_def(s1, req1),
    #?gpb_rpc{name=req2,  input='m2', output='m1'} = M:fetch_rpc_def(s1, req2),
    #?gpb_rpc{name=req22, input='m1', output='m2'} = M:find_rpc_def(s2, req22),
    error = M:find_rpc_def(s1, req_ee),
    error = M:find_rpc_def(s2, req_aa),
    ?assertError(_, M:fetch_rpc_def(s2, req_ee)),
    ?assertError(_, M:fetch_rpc_def(s1, req_aa)),
    unload_code(M).

service_name_to_from_binary_binary_test() ->
    Proto = ["package foo.bar;",
             "message M { required uint32 f1=1; }",
             "service S {",
             "  rpc R1(M) returns (M);",
             "}"],
    %% Without the use_package option
    M1 = compile_iolist(Proto, []),
    'S' = M1:fqbin_to_service_name(<<"foo.bar.S">>),
    <<"foo.bar.S">> = M1:service_name_to_fqbin('S'),
    {'S', 'R1'} =
        M1:fqbins_to_service_and_rpc_name(<<"foo.bar.S">>, <<"R1">>),
    {<<"foo.bar.S">>, <<"R1">>} =
        M1:service_and_rpc_name_to_fqbins('S', 'R1'),
    unload_code(M1),
    %% _With_ the use_package option
    M2 = compile_iolist(Proto, [use_packages]),
    'foo.bar.S' = M2:fqbin_to_service_name(<<"foo.bar.S">>),
    <<"foo.bar.S">> = M2:service_name_to_fqbin('foo.bar.S'),
    {'foo.bar.S', 'R1'} =
        M2:fqbins_to_service_and_rpc_name(<<"foo.bar.S">>, <<"R1">>),
    {<<"foo.bar.S">>, <<"R1">>} =
        M2:service_and_rpc_name_to_fqbins('foo.bar.S', 'R1'),
    %% Unknowns
    ?assertError({gpb_error, {badservice,<<"something-else">>}},
                 M2:fqbin_to_service_name(<<"something-else">>)),
    ?assertError({gpb_error, {badservice,'something-else'}},
                 M2:service_name_to_fqbin('something-else')),
    ?assertError({gpb_error,
                  {badservice_or_rpc, {<<"something-else">>, <<"x">>}}},
                 M2:fqbins_to_service_and_rpc_name(<<"something-else">>,
                                                   <<"x">>)),
    ?assertError({gpb_error,
                  {badservice_or_rpc, {'something-else', x}}},
                 M2:service_and_rpc_name_to_fqbins('something-else',
                                                   x)),
    unload_code(M2).

service_name_to_from_binary_with_renamings_test() ->
    Proto = ["package foo.bar;",
             "message MsgXyz { required uint32 f1=1; }",
             "service MyService {",
             "  rpc GetAbc(MsgXyz) returns (MsgXyz);",
             "  rpc SetAbc(MsgXyz) returns (MsgXyz);",
             "}"],
    Renamings = [{rename,{pkg_name, dots_to_underscores}},
                 {rename,{pkg_name, lowercase}},
                 {rename,{msg_name, snake_case}},
                 {rename,{service_name, snake_case}},
                 {rename,{rpc_name, lowercase}}],
    %% Without the use_package option
    M1 = compile_iolist(Proto, Renamings),
    'my_service' = M1:fqbin_to_service_name(<<"foo.bar.MyService">>),
    <<"foo.bar.MyService">> = M1:service_name_to_fqbin('my_service'),
    {'my_service', getabc} =
        M1:fqbins_to_service_and_rpc_name(<<"foo.bar.MyService">>,
                                          <<"GetAbc">>),
    {<<"foo.bar.MyService">>, <<"GetAbc">>} =
        M1:service_and_rpc_name_to_fqbins('my_service', getabc),
    unload_code(M1),
    %% _With_ the use_package option
    M2 = compile_iolist(Proto, [use_packages | Renamings]),
    'foo_bar.my_service' = M2:fqbin_to_service_name(<<"foo.bar.MyService">>),
    <<"foo.bar.MyService">> = M2:service_name_to_fqbin('foo_bar.my_service'),
    {'foo_bar.my_service', getabc} =
        M2:fqbins_to_service_and_rpc_name(<<"foo.bar.MyService">>,
                                          <<"GetAbc">>),
    {<<"foo.bar.MyService">>, <<"GetAbc">>} =
         M2:service_and_rpc_name_to_fqbins('foo_bar.my_service', getabc),
    unload_code(M2).

msg_from_binary_test() ->
    %% No msgs are ok
    Proto0 = ["enum E { a=1; b=2; }"],
    M0 = compile_iolist(Proto0, []),
    ?assertError({gpb_error, _}, M0:fqbin_to_msg_name(<<"x">>)),
    ?assertError({gpb_error, _}, M0:msg_name_to_fqbin(x)),
    unload_code(M0),

    Proto1 = ["package foo.bar;",
              "message SomeMsg { required uint32 f = 1; }"],
    Renamings = [{rename,{pkg_name, dots_to_underscores}},
                 {rename,{pkg_name, lowercase}},
                 {rename,{msg_name, snake_case}}],

    %% Without the use_package option
    M1 = compile_iolist(Proto1, []),
    'SomeMsg' = M1:fqbin_to_msg_name(<<"foo.bar.SomeMsg">>),
    <<"foo.bar.SomeMsg">> = M1:msg_name_to_fqbin('SomeMsg'),
    unload_code(M1),

    %% _With_ the use_package option
    M2 = compile_iolist(Proto1, [use_packages]),
    'foo.bar.SomeMsg' = M2:fqbin_to_msg_name(<<"foo.bar.SomeMsg">>),
    <<"foo.bar.SomeMsg">> = M2:msg_name_to_fqbin('foo.bar.SomeMsg'),
    unload_code(M2),

    %% With the use_package option _and_ renamings
    M3 = compile_iolist(Proto1, [use_packages | Renamings]),
    'foo_bar.some_msg' = M3:fqbin_to_msg_name(<<"foo.bar.SomeMsg">>),
    <<"foo.bar.SomeMsg">> = M3:msg_name_to_fqbin('foo_bar.some_msg'),
    unload_code(M3).

enum_from_binary_test() ->
    %% No enums are ok
    Proto0 = ["message M { required uint32 f = 1; }"],
    M0 = compile_iolist(Proto0, []),
    ?assertError({gpb_error, _}, M0:fqbin_to_enum_name(<<"x">>)),
    ?assertError({gpb_error, _}, M0:enum_name_to_fqbin(x)),
    unload_code(M0),

    Proto1 = ["package foo.bar;",
              "enum E1 { a=1; b=2; }",
              "message Msg { required E2 f = 1;",
              "  enum E2 { aa=0; bb=1; }", % a nested enum
              "}"],
    %% Without the use_package option
    M1 = compile_iolist(Proto1, []),
    'E1' = M1:fqbin_to_enum_name(<<"foo.bar.E1">>),
    <<"foo.bar.E1">> = M1:enum_name_to_fqbin('E1'),
    'Msg.E2' = M1:fqbin_to_enum_name(<<"foo.bar.Msg.E2">>),
    <<"foo.bar.Msg.E2">> = M1:enum_name_to_fqbin('Msg.E2'),
    unload_code(M1),

    %% _With_ the use_package option
    M2 = compile_iolist(Proto1, [use_packages]),
    'foo.bar.E1' = M2:fqbin_to_enum_name(<<"foo.bar.E1">>),
    <<"foo.bar.E1">> = M2:enum_name_to_fqbin('foo.bar.E1'),
    'foo.bar.Msg.E2' = M2:fqbin_to_enum_name(<<"foo.bar.Msg.E2">>),
    <<"foo.bar.Msg.E2">> = M2:enum_name_to_fqbin('foo.bar.Msg.E2'),
    unload_code(M2),

    %% With the use_package _and_ renaming option
    M3 = compile_iolist(Proto1, [use_packages, {rename,{msg_name,lowercase}}]),
    'foo.bar.E1' = M3:fqbin_to_enum_name(<<"foo.bar.E1">>),
    <<"foo.bar.E1">> = M3:enum_name_to_fqbin('foo.bar.E1'),
    'foo.bar.msg.E2' = M3:fqbin_to_enum_name(<<"foo.bar.Msg.E2">>),
    <<"foo.bar.Msg.E2">> = M3:enum_name_to_fqbin('foo.bar.msg.E2'),
    unload_code(M3).

sources_and_basenames_test() ->
    M = compile_protos(
          [{"<gen>.proto", ["import \"a.proto\";",
                            "message M { required uint32 f = 1; }\n"]},
           {"a.proto",     ["message A { required uint32 g = 2; }"]}],
          []),
    Expected = atom_to_list(M) ++ ".proto",
    ExpectedSansExt = atom_to_list(M),
    ?assert(Expected /= "a.proto"),
    Expected = M:source_basename(),
    [Expected, "a.proto"] = M:get_all_source_basenames(),
    [ExpectedSansExt, "a"] = M:get_all_proto_names(),
    unload_code(M).

get_containments_test() ->
    Protos = [{"<generated>.proto",
               ["import \"a.proto\";",
                "import \"b.proto\";",
                "import \"c.proto\";",
                "package top;",
                "message M { required uint32 f = 1; }\n"]},
              {"a.proto",
               ["enum EA { ea1 = 1; ea2 = 2; };",
                "package a;",
                "message MA { uint32 f = 1; };",
                "service SA { ",
                "  rpc req_RA_1(MA) returns (MA);",
                "  rpc req_RA_2(MA) returns (MA);",
                "}"]},
              {"b.proto",
               ["enum EB { eb1 = 1; eb2 = 2; };",
                "package b;",
                "message MB { uint32 g = 1; };",
                "service SB { ",
                "  rpc req_RB_1(MB) returns (MB);",
                "  rpc req_RB_2(MB) returns (MB);",
                "}"]},
              {"c.proto",
               %% empty to test absence of things
               ""}],
    M1 = compile_protos(Protos, [use_packages]),
    P1 = atom_to_list(M1),
    M2 = compile_protos(Protos, []),
    P2 = atom_to_list(M2),
    M1S = atom_to_list(M1),
    M2S = atom_to_list(M2),

    %% With packages
    ['top.M'] = M1:get_msg_containment(P1),
    ['a.MA'] = M1:get_msg_containment("a"),
    ['b.MB'] = M1:get_msg_containment("b"),
    []       = M1:get_msg_containment("c"),
    M1S      = M1:get_proto_by_msg_name_as_fqbin(<<"top.M">>),
    ?assertError({gpb_error, _}, M1:get_msg_containment("x")),
    ?assertError({gpb_error, _}, M1:get_msg_containment(wrong_type)),
    "a"      = M1:get_proto_by_msg_name_as_fqbin(<<"a.MA">>),
    "b"      = M1:get_proto_by_msg_name_as_fqbin(<<"b.MB">>),
    %% Without packages
    ['M']    = M2:get_msg_containment(P2),
    ['MA']   = M2:get_msg_containment("a"),
    ['MB']   = M2:get_msg_containment("b"),
    []       = M2:get_msg_containment("c"),
    M2S      = M2:get_proto_by_msg_name_as_fqbin(<<"top.M">>), % top=first pkg
    "a"      = M2:get_proto_by_msg_name_as_fqbin(<<"top.MA">>),
    "b"      = M2:get_proto_by_msg_name_as_fqbin(<<"top.MB">>),

    %% With packages
    []       = M1:get_enum_containment(P1),
    ['a.EA'] = M1:get_enum_containment("a"),
    ['b.EB'] = M1:get_enum_containment("b"),
    []       = M1:get_enum_containment("c"),
    ?assertError({gpb_error, _}, M1:get_enum_containment("x")),
    ?assertError({gpb_error, _}, M1:get_enum_containment(wrong_type)),
    "a"      = M1:get_proto_by_enum_name_as_fqbin(<<"a.EA">>),
    "b"      = M1:get_proto_by_enum_name_as_fqbin(<<"b.EB">>),
    %% Without packages
    []       = M2:get_enum_containment(P2),
    ['EA']   = M2:get_enum_containment("a"),
    ['EB']   = M2:get_enum_containment("b"),
    []       = M2:get_enum_containment("c"),
    "a"      = M2:get_proto_by_enum_name_as_fqbin(<<"top.EA">>),
    "b"      = M2:get_proto_by_enum_name_as_fqbin(<<"top.EB">>),

    top       = M1:get_pkg_containment(P1),
    %% With packages
    a         = M1:get_pkg_containment("a"),
    b         = M1:get_pkg_containment("b"),
    undefined = M1:get_pkg_containment("c"),
    ?assertError({gpb_error, _}, M1:get_pkg_containment("x")),
    ?assertError({gpb_error, _}, M1:get_pkg_containment(wrong_type)),
    [M1S]     = M1:get_protos_by_pkg_name_as_fqbin(<<"top">>),
    ["a"]     = M1:get_protos_by_pkg_name_as_fqbin(<<"a">>),
    ["b"]     = M1:get_protos_by_pkg_name_as_fqbin(<<"b">>),
    %% Without packages
    undefined = M2:get_pkg_containment(P2),
    undefined = M2:get_pkg_containment("a"),
    undefined = M2:get_pkg_containment("b"),
    undefined = M2:get_pkg_containment("c"),
    ?assertError({gpb_error, _}, M2:get_protos_by_pkg_name_as_fqbin(<<"top">>)),

    []       = M1:get_service_containment(P1),
    ['a.SA'] = M1:get_service_containment("a"),
    ['b.SB'] = M1:get_service_containment("b"),
    []       = M1:get_service_containment("c"),
    ?assertError({gpb_error, _}, M1:get_enum_containment("x")),
    ?assertError({gpb_error, _}, M1:get_enum_containment(wrong_type)),
    "a"      = M1:get_proto_by_service_name_as_fqbin(<<"a.SA">>),
    "b"      = M1:get_proto_by_service_name_as_fqbin(<<"b.SB">>),
    "a"      = M2:get_proto_by_service_name_as_fqbin(<<"top.SA">>),
    "b"      = M2:get_proto_by_service_name_as_fqbin(<<"top.SB">>),

    %% With packages
    []                  = M1:get_rpc_containment(P1),
    [{'a.SA',req_RA_1},
     {'a.SA',req_RA_2}] = M1:get_rpc_containment("a"),
    [{'b.SB',req_RB_1},
     {'b.SB',req_RB_2}] = M1:get_rpc_containment("b"),
    []                  = M1:get_rpc_containment("c"),
    ?assertError({gpb_error, _}, M1:get_enum_containment("x")),
    ?assertError({gpb_error, _}, M1:get_enum_containment(wrong_type)),
    %% Without packages
    []                                = M2:get_rpc_containment(P2),
    [{'SA',req_RA_1},{'SA',req_RA_2}] = M2:get_rpc_containment("a"),
    [{'SB',req_RB_1},{'SB',req_RB_2}] = M2:get_rpc_containment("b"),
    []                                = M2:get_rpc_containment("c"),

    unload_code(M1),
    unload_code(M2).

%% --- decoder tests ---------------

decodes_overly_long_varints_test() ->
    M = compile_defs([{{msg,m1}, [#?gpb_field{name=a, type=int32,
                                              fnum=1, rnum=#m1.a,
                                              occurrence=required, opts=[]}]}]),
    #m1{a=54} = M:decode_msg(<<8, 54>>, m1), %% canonically encoded
    #m1{a=54} = M:decode_msg(<<8, (128+54), 128, 128, 0>>, m1),
    unload_code(M).

decode_failure_error_for_invalid_binary_test() ->
    M = compile_defs([{{msg,m1}, [#?gpb_field{name=a, type=int32,
                                              fnum=1, rnum=#m1.a,
                                              occurrence=required,
                                              opts=[]}]}]),
    Bad1 = <<8>>,
    ?assertError({gpb_error,
                  {decoding_failure,
                   {Bad1, m1, {_Class,_Reason,_Stack}}}},
                 M:decode_msg(Bad1, m1)),
    unload_code(M).

%% --- scoped messages ---------------

dotted_names_gives_no_compilation_error_test() ->
    %% make sure dotted names does not give compilation errors,
    %% for instance if some generated code would rely on names
    %% having the same syntax as erlang atoms, or, when prepended
    %% with an upper case character, having the same syntax as an
    %% erlang variable
    M = compile_iolist(["message m1 {"
                        "  message m2 { required uint32 x = 1; }",
                        "  enum    e1 { ea = 17; eb = 18; }",
                        "  required m2     y = 1;",
                        "  required .m1.m2 z = 2;",
                        "  required e1     w = 3;",
                        "}",
                        "message m3 { required m1.m2 b = 1; }"]),
    M1Msg = {m1, {'m1.m2', 1}, {'m1.m2', 2}, ea},
    Data = M:encode_msg(M1Msg),
    M1Msg = M:decode_msg(Data, m1),
    unload_code(M).

%% --- module/msg name prefix/suffix ---------------
module_msg_name_prefix_test() ->
    Proto = <<"message msg1 { required uint32 f1=1; }\n">>,
    Master = self(),
    ReadInput = fun(FName) -> Master ! {read, FName}, {ok, Proto} end,
    ReportOutput = fun(FName, Contents) ->
                           Ext = list_to_atom(tl(filename:extension(FName))),
                           Master ! {write, {Ext, FName, Contents}},
                           ok
                   end,
    FileOpOpt = mk_fileop_opt([{read_file, ReadInput},
                               {write_file, ReportOutput}]),
    ModPrefix = "mp_",
    MsgPrefix = "mm_",
    ModSuffix = "_xp",
    MsgSuffix = "_xm",
    ok = gpb_compile:file("m.proto",
                          [FileOpOpt, {i,"."},
                           {module_name_prefix, ModPrefix},
                           {msg_name_prefix, MsgPrefix},
                           {module_name_suffix, ModSuffix},
                           {msg_name_suffix, MsgSuffix}]),
    receive
        {read, "m.proto"} -> ok;
        {read, X} -> erlang:error({"reading from odd file", X})
    end,
    receive
        {write, {hrl, "mp_m_xp.hrl", Hrl}} ->
            assert_contains_regexp(Hrl, "mm_msg1_xm"),
            ok;
        {write, {hrl, "m.hrl", _}} ->
            erlang:error("hrl file not prefixed or suffixed!");
        {write, {hrl, X2, C2}} ->
            erlang:error({"writing odd hrl file!", X2, C2})
    end,
    receive
        {write, {erl, "mp_m_xp.erl", Erl}} ->
            assert_contains_regexp(Erl, "-include.*\"mp_m_xp.hrl\""),
            assert_contains_regexp(Erl, "-module.*mp_m_xp"),
            assert_contains_regexp(Erl, "mm_msg1_xm"),
            ok;
        {write, {erl, "m.erl", _}} ->
            erlang:error("erl file not prefixed or suffixed!");
        {write, {erl, X3, C3}} ->
            erlang:error({"writing odd erl file!", X3, C3})
    end,
    ok.

module_name_test() ->
    Proto = <<"message msg1 { required uint32 f1=1; }\n">>,
    Master = self(),
    ReadInput = fun(FName) -> Master ! {read, FName}, {ok, Proto} end,
    ReportOutput = fun(FName, Contents) ->
                           Ext = list_to_atom(tl(filename:extension(FName))),
                           Master ! {write, {Ext, FName, Contents}},
                           ok
                   end,
    FileOpOpt = mk_fileop_opt([{read_file, ReadInput},
                               {write_file, ReportOutput}]),
    ok = gpb_compile:file("m.proto",
                          [FileOpOpt, {i,"."},
                           {module_name, "new"}]),
    receive
        {read, "m.proto"} -> ok;
        {read, X} -> erlang:error({"reading from odd file", X})
    end,
    receive
        {write, {hrl, "new.hrl", _Hrl}} ->
            ok;
        {write, {hrl, "m.hrl", _}} ->
            erlang:error("expected new.hrl, not m.hrl!");
        {write, {hrl, X2, C2}} ->
            erlang:error({"writing odd hrl file!", X2, C2})
    end,
    receive
        {write, {erl, "new.erl", Erl}} ->
            assert_contains_regexp(Erl, "-include.*\"new.hrl\""),
            assert_contains_regexp(Erl, "-module.*new"),
            ok;
        {write, {erl, "m.erl", _}} ->
            erlang:error("expected new.erl, not m.erl!");
        {write, {erl, X3, C3}} ->
            erlang:error({"writing odd erl file!", X3, C3})
    end,
    ok.

module_name_with_suffix_prefix_test() ->
    %% interaction between options module_name and module_name_prefix/suffix
    Proto = <<"message msg1 { required uint32 f1=1; }\n">>,
    Master = self(),
    ReadInput = fun(FName) -> Master ! {read, FName}, {ok, Proto} end,
    ReportOutput = fun(FName, Contents) ->
                           Ext = list_to_atom(tl(filename:extension(FName))),
                           Master ! {write, {Ext, FName, Contents}},
                           ok
                   end,
    FileOpOpt = mk_fileop_opt([{read_file, ReadInput},
                               {write_file, ReportOutput}]),
    ModPrefix = "mp_",
    ModSuffix = "_xp",
    ok = gpb_compile:file("m.proto",
                          [FileOpOpt, {i,"."},
                           {module_name, "new"},
                           {module_name_prefix, ModPrefix},
                           {module_name_suffix, ModSuffix}]),
    receive
        {read, "m.proto"} -> ok;
        {read, X} -> erlang:error({"reading from odd file", X})
    end,
    receive
        {write, {hrl, "mp_new_xp.hrl", _Hrl}} ->
            ok;
        {write, {hrl, "m.hrl", _}} ->
            erlang:error("hrl file not changed + prefixed or suffixed!");
        {write, {hrl, X2, C2}} ->
            erlang:error({"writing odd hrl file!", X2, C2})
    end,
    receive
        {write, {erl, "mp_new_xp.erl", Erl}} ->
            assert_contains_regexp(Erl, "-include.*\"mp_new_xp.hrl\""),
            assert_contains_regexp(Erl, "-module.*mp_new_xp"),
            ok;
        {write, {erl, "m.erl", _}} ->
            erlang:error("erl file not changed + prefixed or suffixed!");
        {write, {erl, X3, C3}} ->
            erlang:error({"writing odd erl file!", X3, C3})
    end,
    ok.

type_name_prefix_test() ->
    Proto = ["enum EE { a=0; b=1; };\n"
             "message M {\n"
             "  required EE f1=1;\n"
             "  optional M  f2=2;\n"
             "};\n"],
    RenameOpts = [{rename, {msg_typename, {prefix, "mp_"}}},
                  {rename, {msg_typename, {suffix, "_ms"}}},
                  {rename, {enum_typename, {prefix, "ep_"}}},
                  {rename, {enum_typename, {suffix, "_es"}}}],
    S = compile_to_string(Proto, RenameOpts),
    H = compile_to_string_get_hrl(Proto, RenameOpts),
    Spaces = "\\s+",
    assert_contains_regexp(S, "-type" ++ Spaces ++ "mp_M_ms()"),
    assert_contains_regexp(S, "-type" ++ Spaces ++ "ep_EE_es()"),
    assert_contains_regexp(H, ":mp_M_ms()"),
    ok.

assert_contains_regexp(IoData, Re) ->
    case re:run(IoData, Re) of
        {match, _} -> ok;
        nomatch    ->
            ?debugFmt("~nERROR: Regexp ~s not found in:~n~s~n", [Re, IoData]),
            erlang:error({"Re ", Re, "not found in", IoData})
    end.

%% --- bytes ----------

list_as_bytes_indata_test() ->
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            M = compile_iolist(["message m1 { required bytes f1 = 1; }"]),
            Data = M:encode_msg({m1, [1,2,3,4]}),
            {m1, <<1,2,3,4>>} = M:decode_msg(Data, m1),
            unload_code(M);
       true ->
            %% nothing to test
            ok
    end.

-define(btest(Fn, N), {atom_to_list(Fn), fun() -> Fn(N) end}).
copy_bytes_test_() ->
    %% From Erlang 22.1, matched sub-binaries =< 64 bytes gets copied
    %% to heap-local binaries even though there's no call to
    %% binary:copy(). Previously they didn't.
    %%
    %% Since we used binary:referenced_byte_size() as an indication to
    %% check whether bytes were copied or not, we must stay over that
    %% limit. But it is a bit fragile, limits can be upped etc, so check
    %% if we can find such a limit. If not, skip the tests (don't fail).
    case calc_lower_size_bound_when_sub_binaries_dont_get_copied() of
        {found, {at_least, N}} ->
            {"copy_bytes tests",
             [?btest(copy_bytes_unconditionally_aux, N),
              ?btest(copy_bytes_false_aux, N),
              ?btest(copy_bytes_auto_aux, N),
              ?btest(copy_bytes_fraction_aux, N)]};
        not_found ->
            {"skipping", _NoTests=[]}
    end.

calc_lower_size_bound_when_sub_binaries_dont_get_copied() ->
    calc_byte_copy_lim_aux(32).

calc_byte_copy_lim_aux(N) when N =< 8192 ->
    N1 = N + 1,
    C = binary:copy(<<"a">>, N + 11),
    <<A:10/binary, B:N1/binary>> = C,
    case {binary:referenced_byte_size(A), % better do something with A
          binary:referenced_byte_size(B),
          byte_size(C)} of
        {_, Sz, Sz} ->
            ?assertEqual(10, binary:referenced_byte_size(binary:copy(A))),
            {found, {at_least, N1}};
        _X ->
            %% Maybe sub-binaries of larger size don't get copied?
            io:format("_X=~p~n", [_X]),
            calc_byte_copy_lim_aux(N * 2)
    end;
calc_byte_copy_lim_aux(_) ->
    not_found.


copy_bytes_unconditionally_aux(MinSize) ->
    Bytes = binary:copy(<<"d">>, MinSize),
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, true}]),
    Data = M:encode_msg({m1, Bytes}),
    {m1, Bs} = M:decode_msg(Data, m1),
    %% If the Bs has not been copied, then it is a sub-binary
    %% of a larger binary: of the message, ie of Data.
    %% So verify copying by verifying size of referenced data.
    ?assertEqual(byte_size(Bs), binary:referenced_byte_size(Bs)),
    unload_code(M).


copy_bytes_false_aux(MinSize) ->
    Bytes = binary:copy(<<"d">>, MinSize),
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, false}]),
    Data = M:encode_msg({m1, Bytes}),
    {m1, Bs} = M:decode_msg(Data, m1),
    %% If the StrBin has not been copied, then it is a sub-binary
    %% of a larger binary: of the message, ie of Data.
    %% So verify copying by verifying size of referenced data.
    ?assertEqual(byte_size(Data), binary:referenced_byte_size(Bs)),
    unload_code(M).

copy_bytes_auto_aux(MinSize) ->
    Bytes = binary:copy(<<"d">>, MinSize),
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, auto}]),
    Data = M:encode_msg({m1, Bytes}),
    {m1, Bs} = M:decode_msg(Data, m1),
    ?assertEqual(byte_size(Bs), binary:referenced_byte_size(Bs)),
    unload_code(M).

copy_bytes_fraction_aux(MinSize) ->
    Proto = ["message m1 {",
             "  required bytes f1 = 1;",
             "  required bytes f2 = 2;",
             "}"],
    M1 = compile_iolist(Proto, [{copy_bytes, 2}]),   % fraction as int
    M2 = compile_iolist(Proto, [{copy_bytes, 2.0}]), % fraction as float
    D1 = binary:copy(<<"d">>, MinSize), %% small
    D2 = binary:copy(<<"d">>, MinSize * 5), %% large
    Data = M1:encode_msg({m1, D1, D2}),
    ?assert(byte_size(Data) > (2 * byte_size(D1))),
    ?assert(byte_size(Data) < (2 * byte_size(D2))),
    {m1, D1Bs, D2Bs} = M1:decode_msg(Data, m1),
    ?assertEqual(D1, D1Bs),
    ?assertEqual(D2, D2Bs),
    %% The small data should have been copied, but not the larger
    ?assertEqual(byte_size(D1Bs), binary:referenced_byte_size(D1Bs)),
    ?assertEqual(byte_size(Data), binary:referenced_byte_size(D2Bs)),

    {m1, D3Bs, D4Bs} = M2:decode_msg(Data, m1),
    ?assertEqual(D1, D3Bs),
    ?assertEqual(D2, D4Bs),
    ?assertEqual(byte_size(D3Bs), binary:referenced_byte_size(D3Bs)),
    ?assertEqual(byte_size(Data), binary:referenced_byte_size(D4Bs)),

    unload_code(M1),
    unload_code(M2).

%% --- strings ----------

strings_as_binaries_option_produces_bins_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "}"],
                       [strings_as_binaries]),
    Data = M:encode_msg({m1, "some string"}),
    {m1, <<"some string">>} = M:decode_msg(Data, m1),
    unload_code(M).

strings_as_lists_is_the_default_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "}"],
                       []),
    Data = M:encode_msg({m1, "some string"}),
    {m1, "some string"} = M:decode_msg(Data, m1),
    unload_code(M).

strings_as_binaries_opt_together_with_copy_bytes_opt_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "}"],
                       [strings_as_binaries, {copy_bytes, auto}]),
    Data = M:encode_msg({m1, "some string"}),
    {m1, <<"some string">>=StrBin} = M:decode_msg(Data, m1),
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            ?assertEqual(byte_size(StrBin),
                         binary:referenced_byte_size(StrBin));
       true ->
            ok
    end,
    unload_code(M).

accepts_both_strings_and_binaries_as_input_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "  required string f2 = 2;",
                        "}"]),
    Data = M:encode_msg({m1, "some string", <<"some other string">>}),
    {m1, "some string", "some other string"} = M:decode_msg(Data, m1),
    unload_code(M).

verifies_both_strings_and_binaries_as_input_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "  required string f2 = 2;",
                        "}"],
                        [strings_as_binaries]),
    R = {m1, "some string", <<"some other string">>},
    ok = M:verify_msg(R),
    ?assertError(_, M:verify_msg({m1, "a", <<97,98,99,255,191>>})),
    unload_code(M).

iodata_as_input_for_proto3_string_test() ->
    M = compile_iolist(["syntax=\"proto3\";\n"
                        "message m1 {"
                        "  string a = 1;",
                        "}"],
                        [strings_as_binaries]),
    B1 = M:encode_msg({m1, "xyz"}),
    ?assert(is_binary(B1)),
    ?assert(byte_size(B1) > 0),
    B1 = M:encode_msg({m1, <<"xyz">>}),
    B1 = M:encode_msg({m1, ["x", <<"y">> | "z"]}),
    <<>> = M:encode_msg({m1, [[], [[[<<>> | <<>>] | []], ""]]}),
    <<>> = M:encode_msg({m1, ""}),
    <<>> = M:encode_msg({m1, <<>>}),
    unload_code(M).

iodata_as_input_for_proto3_string_only_in_oneof_test() ->
    M = compile_iolist(["syntax=\"proto3\";\n"
                        "message m1 {"
                        "  oneof c { string a1 = 2; }",
                        "}"],
                        [strings_as_binaries]),
    B1 = M:encode_msg({m1, {a1, "xyz"}}),
    ?assert(is_binary(B1)),
    ?assert(byte_size(B1) > 0),
    B1 = M:encode_msg({m1, {a1, <<"xyz">>}}),
    B1 = M:encode_msg({m1, {a1, ["x", <<"y">> | "z"]}}),
    %% should be present even if empty, I think
    EmptyA1 = {m1, {a1, <<>>}},
    <<18,0>> = B2 = M:encode_msg(EmptyA1),
    EmptyA1 = M:decode_msg(B2, m1),
    unload_code(M).

utf8_bom_test() ->
    Utf8ByteOrderMark = <<239,187,191>>, % EF BB BF
    M = compile_iolist([Utf8ByteOrderMark,
                        "message m1 {"
                        "  required string f1 = 1;",
                        "}"]),
    Data = M:encode_msg({m1, "x"}),
    {m1, "x"} = M:decode_msg(Data, m1),
    unload_code(M).

nonascii_default_values_for_strings_test() ->
    Utf8 = unicode:characters_to_binary([1000,2000,3000]),
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1 [default=\"",Utf8,"\"];",
                        "}"]),
    Data = M:encode_msg({m1, "x"}),
    {m1, "x"} = M:decode_msg(Data, m1),
    unload_code(M).

reading_file_falls_back_to_latin1_test() ->
    Latin1 = [255,255,255], % Not decodable as utf8
    M = compile_iolist(["// "++Latin1++"\n",
                        "message m1 {"
                        "  required string f1 = 1;",
                        "}"]),
    Data = M:encode_msg({m1, "x"}),
    {m1, "x"} = M:decode_msg(Data, m1),
    unload_code(M).

error_for_invalid_boms_test() ->
    [{_,{error,{utf8_decode_failed,{invalid_proto_byte_order_mark,_},_},[]}} =
         {Bom, compile_iolist_get_errors_or_warnings(
                 [Bom, "message m1 {"
                  "  required string f1 = 1;",
                  "}"])}
     || Bom <- [<<0,0,16#FE,16#FF>>, % utf32-big endian
                <<16#FF,16#FE,0,0>>, % utf32-little
                <<16#FE,16#FF>>,     % utf16-big
                <<16#FF,16#FE>>]].   % utf16-little


generates_escaped_utf8_for_old_erlang_versions_test() ->
    Unicode = [255],
    Utf8 = unicode:characters_to_binary(Unicode),
    Proto = ["message m1 {"
             "  required string f1 = 1 [default=\"",Unicode,"\"];",
             "}"],
    S1 = compile_to_string_get_hrl(Proto, [{target_erlang_version,15}]),
    true = gpb_lib:is_substr("x{ff}", S1), %% 255 = 16#ff
    S2 = compile_to_string_get_hrl(Proto, [{target_erlang_version,16}]),
    true = gpb_lib:is_substr(binary_to_list(Utf8), S2),
    [Line1 | _] = gpb_lib:string_lexemes(S2, "\n"),
    true = gpb_lib:is_substr("coding: ", Line1).

%% -- translation of google.protobuf.Any ----------

-define(x_com_atom_1(C), 10,10,"x.com/atom",18,1,C).

'translation_of_google.protobuf.Any_test_'() ->
    {timeout,10,fun 'translation_of_google.protobuf.Any_aux'/0}.

'translation_of_google.protobuf.Any_aux'() ->
    %% The any.proto contains:
    %%
    %%     syntax = "proto3";
    %%     ...
    %%     message Any {
    %%       string type_url = 1;
    %%       bytes value = 2;
    %%     }
    %%
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  repeated google.protobuf.Any f1=1;",
           "  required google.protobuf.Any f2=3;",
           "  optional google.protobuf.Any f3=4;",
           "  oneof f4 {",
           "    google.protobuf.Any f5=6;",
           "  }",
           "}"],
          [use_packages,
           %% The translations assume value is an atom.
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1']}},
                           {decode,{?MODULE,any_d_atom,['$1']}},
                           {merge,{?MODULE,any_m_atom,['$1','$2']}},
                           {verify,{?MODULE,any_v_atom,['$1','$errorf']}}]}]),
    R = {m, [a,b,c], d, e, {f5,f}},
    <<10,15,?x_com_atom_1("a"),
      10,15,?x_com_atom_1("b"),
      10,15,?x_com_atom_1("c"),
      26,15,?x_com_atom_1("d"),
      34,15,?x_com_atom_1("e"),
      50,15,?x_com_atom_1("f")>> = B = M:encode_msg(R),
    R = M:decode_msg(B, m),

    ok = M:verify_msg(R),
    ?verify_gpb_err(M:verify_msg({m, ["a",b,c], d, e, {f5,f}})),
    ?verify_gpb_err(M:verify_msg({m, [a,b,c], "d", e, {f5,f}})),
    ?verify_gpb_err(M:verify_msg({m, [a,b,c], d, "e", {f5,f}})),
    ?verify_gpb_err(M:verify_msg({m, [a,b,c], d, e, {f5,"f"}})),

    RR = {m, [a,b,c,a,b,c], dd, ee, {f5,ff}},
    RR = M:merge_msgs(R, R),
    RR = M:decode_msg(<<B/binary, B/binary>>, m),
    unload_code(M).

translation_of_Any_as_a_map_value_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  map<string,google.protobuf.Any> f1=1;",
           "}"],
          [use_packages,
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1']}},
                           {decode,{?MODULE,any_d_atom,['$1']}},
                           {merge,{?MODULE,any_m_atom,['$1','$2']}}, % unused
                           {verify,{?MODULE,any_v_atom,['$1','$errorf']}}]}]),
    R = {m, MapI=[{"x",a},{"y",b}]},
    <<10,20, % "pseudo" msg for map item
      10,1,"x", % key=x
      18,15,?x_com_atom_1("a"), % value=a
      10,20,
      10,1,"y",
      18,15,?x_com_atom_1("b")>> = B = M:encode_msg(R),
    {m,MapO} = M:decode_msg(B, m),
    true = lists:sort(MapI) == lists:sort(MapO),

    ok = M:verify_msg(R),
    ?verify_gpb_err(M:verify_msg({m, [{"a","not an atom"}]})),
    unload_code(M).

merge_callback_for_Any_is_optional_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  required google.protobuf.Any f1=1;",
           "}"],
          [use_packages,
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1']}},
                           {decode,{?MODULE,any_d_atom,['$1']}},
                           {verify,{?MODULE,any_v_atom,['$1','$errorf']}}]}]),
    %% Expected behaviour in case of a "default" merge op is overwrite
    {m,a} = M:decode_msg(<<10,15,?x_com_atom_1("a")>>, m),
    {m,b} = M:decode_msg(<<10,15,?x_com_atom_1("a"),
                           10,15,?x_com_atom_1("b")>>, % overwrite
                         m),
    unload_code(M).

-define(recv(Pattern),
        (fun() -> receive Pattern=__V -> __V
                  after 4000 ->
                          error({receive_timed_out,
                                 {pattern,??Pattern},
                                 {message_queue,
                                  element(2,process_info(self(),messages))}})
                  end
         end)()).

userdata_to_Any_callback_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  required google.protobuf.Any f1=1;",
           "}"],
          [use_packages,
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1','$user_data']}},
                           {decode,{?MODULE,any_d_atom,['$1','$user_data']}},
                           {merge,{?MODULE,any_m_atom,['$1','$2',
                                                       '$user_data']}},
                           {verify,{?MODULE,any_v_atom,['$1','$errorf',
                                                        '$user_data']}}]}]),
    R1 = {m,a},
    Self = self(),
    SendToSelf = fun(Result) -> Self ! {res,Result} end,
    B1 = M:encode_msg(R1,[{user_data,SendToSelf}]),
    ?recv({res,{'google.protobuf.Any',"x.com/atom",<<"a">>}}),
    R1 = M:decode_msg(B1, m, [{user_data,SendToSelf}]),
    ?recv({res,a}),
    {m,aa} = M:merge_msgs(R1, R1, [{user_data,SendToSelf}]),
    ?recv({res,aa}),
    ok = M:verify_msg(R1, [{user_data,SendToSelf}]),
    ?recv({res,ok}),
    unload_code(M).

userdata_and_op_to_Any_callback_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  required google.protobuf.Any f1=1;",
           "}"],
          [use_packages,
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1',
                                                        '$user_data','$op']}},
                           {decode,{?MODULE,any_d_atom,['$1',
                                                        '$user_data','$op']}},
                           {merge,{?MODULE,any_m_atom,['$1','$2',
                                                       '$user_data','$op']}},
                           {verify,{?MODULE,any_v_atom,['$1','$errorf',
                                                        '$user_data','$op']}}]}
          ]),
    R1 = {m,a},
    Self = self(),
    SendToSelf = fun(Result,Op) -> Self ! {{res,Result},{op,Op}} end,
    B1 = M:encode_msg(R1,[{user_data,SendToSelf}]),
    ?recv({{res,{'google.protobuf.Any',"x.com/atom",<<"a">>}},{op,encode}}),
    %% When encode is called with verify, the same option list and
    %% hence the same user data is sent to verify too, so for that
    %% case, expect two messages back.
    B1 = M:encode_msg(R1,[{user_data,SendToSelf},
                          {verify,true}]),
    ?recv({{res,{'google.protobuf.Any',"x.com/atom",<<"a">>}},{op,encode}}),
    ?recv({{res,ok},{op,verify}}),
    %% now for decode etc...
    R1 = M:decode_msg(B1, m, [{user_data,SendToSelf}]),
    ?recv({{res,a},{op,decode}}),
    {m,aa} = M:merge_msgs(R1, R1, [{user_data,SendToSelf}]),
    ?recv({{res,aa},{op,merge}}),
    ok = M:verify_msg(R1, [{user_data,SendToSelf}]),
    ?recv({{res,ok},{op,verify}}),
    unload_code(M).

default_merge_callback_for_repeated_Any_test() ->
    %% A merge callback for a google.protobuf.Any that is repeated,
    %% is not needed
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  repeated google.protobuf.Any f1=1;",
           "}"],
          [use_packages,
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1']}},
                           {decode,{?MODULE,any_d_atom,['$1']}},
                           {verify,{?MODULE,any_v_atom,['$1','$errorf']}}]}]),
    {m,[a,b]} = M:decode_msg(<<10,15,?x_com_atom_1("a"),
                               10,15,?x_com_atom_1("b")>>,
                             m),
    {m,[a,b]} = M:merge_msgs({m,[a]}, {m,[b]}),
    unload_code(M).

verify_callback_for_Any_is_optional_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  required google.protobuf.Any f1=1;",
           "}"],
          [use_packages,
           {any_translate,[{encode,{?MODULE,any_e_atom,['$1']}},
                           {decode,{?MODULE,any_d_atom,['$1']}},
                           {merge,{?MODULE,any_m_atom,['$1','$2']}}]}]),
    %% Expected behaviour in case of a "default" verify op to accept anything
    ok = M:verify_msg({m,a}),
    ok = M:verify_msg({m,"not an atom"}),
    unload_code(M).

verify_callback_with_and_without_errorf_test() ->
    DefsM1 = ["syntax=\"proto3\";",
              "import \"google/protobuf/any.proto\";",
              "message m1 {",
              "  required google.protobuf.Any a=1;",
              "}"],

    Mod1 = compile_iolist(
             DefsM1,
             [use_packages,
              {any_translate,
               [{encode,{?MODULE,any_e_atom,['$1']}},
                {decode,{?MODULE,any_d_atom,['$1']}},
                {verify,{?MODULE,any_v_atom,['$1','$errorf']}}]}]),
    ok = Mod1:verify_msg(#m1{a=abc}),
    ?assertError({gpb_type_error,{not_an_atom,[{value,123},{path,'m1.a'}]}},
                 Mod1:verify_msg(#m1{a=123})),
    unload_code(Mod1),

    Mod2 = compile_iolist(
             DefsM1,
             [use_packages,
              {any_translate,
               [{encode,{?MODULE,any_e_atom,['$1']}},
                {decode,{?MODULE,any_d_atom,['$1']}},
                {verify,{?MODULE,any_v_atom,['$1']}}]}]), % no '$errorf'
    ok = Mod2:verify_msg(#m1{a=abc}),
    ?assertError({gpb_type_error,{oops_no_atom,[{value,123},{path,'m1.a'}]}},
                 Mod2:verify_msg(#m1{a=123})),
    unload_code(Mod2).

%% Translators/callbacks:
any_e_atom(A) ->
    {'google.protobuf.Any', "x.com/atom", list_to_binary(atom_to_list(A))}.

any_d_atom({'google.protobuf.Any', "x.com/atom", B}) ->
    list_to_atom(binary_to_list(B)).

any_m_atom(A1, A2) ->
    list_to_atom(atom_to_list(A1) ++ atom_to_list(A2)).

any_v_atom(A, ErrorF) ->
    if is_atom(A) -> ok;
       true -> ErrorF(not_an_atom)
    end.

any_v_atom(A) when is_atom(A) -> ok;
any_v_atom(_) -> erlang:error(oops_no_atom).

%% Translators/callbacks for user-data
any_e_atom(A, Fn) -> call_tr_userdata_fn(Fn, any_e_atom(A)).
any_d_atom(Any, Fn) -> call_tr_userdata_fn(Fn, any_d_atom(Any)).
any_m_atom(A1, A2, Fn) -> call_tr_userdata_fn(Fn, any_m_atom(A1, A2)).
any_v_atom(A, ErrorF, Fn) -> call_tr_userdata_fn(Fn, any_v_atom(A, ErrorF)).

call_tr_userdata_fn(Fn, Result) ->
    Fn(Result),
    Result.

%% Translators/callbacks for user-data and op
any_e_atom(A, Fn, Op) -> call_tr_userdata_fn(Fn, any_e_atom(A), Op).
any_d_atom(Any, Fn, Op) -> call_tr_userdata_fn(Fn, any_d_atom(Any), Op).
any_m_atom(A1, A2, Fn, Op) -> call_tr_userdata_fn(Fn, any_m_atom(A1, A2), Op).
any_v_atom(A, ErrorF, Fn, Op) -> call_tr_userdata_fn(Fn, any_v_atom(A, ErrorF),
                                                     Op).
call_tr_userdata_fn(Fn, Result, Op) ->
    Fn(Result, Op),
    Result.

%% -- translation of other messages ----------

translate_msg_type_test() ->
    %% in this test, the internal representation of the uuid message
    %% is an integer.
    M = compile_iolist(
          ["message m {",
           "  repeated uuid f1=1;",
           "  required uuid f2=2;",
           "  optional uuid f3=3;",
           "  oneof f4 { uuid f5=5; }",
           "}",
           "message uuid { required string id = 1; }",
           "",
           %% For comparison: similar message x, with similar sub message u2,
           %% which should encode to the same, so we can verify encoding.
           "message x {",
           "  repeated u f1=1;",
           "  required u f2=2;",
           "  optional u f3=3;",
           "  oneof f4 { u f5=5; }",
           "}",
           "message u { required string id = 1; }"],
          [%% The translation changes #uuid{id=string()} <-> integer()
           {translate_type,
            {{msg,uuid},
             [{encode, {?MODULE, uuid_e, ['$1']}},
              {decode, {?MODULE, uuid_d, ['$1']}},
              {merge,  {?MODULE, uuid_m, ['$1','$2']}},
              {verify, {?MODULE, uuid_v, ['$1']}}]}}]),
    M1 = {m, [11,12], 22, 33, {f5,55}},
    X1 = {x, [{u,"11"}, {u,"12"}], {u,"22"}, {u,"33"}, {f5,{u,"55"}}},
    ok = M:verify_msg(M1),
    B1 = M:encode_msg(M1),
    B1 = M:encode_msg(X1),
    M2 = {m, [13,14], 23, 34, {f5,56}},
    X2 = {x, [{u,"13"}, {u,"14"}], {u,"23"}, {u,"34"}, {f5,{u,"56"}}},
    B2 = M:encode_msg(M2),
    B2 = M:encode_msg(X2),
    Expected = {m,
                [11,12, 13,14],
                22 bxor 23, % the "odd" merge operation is bitwise xor
                33 bxor 34,
                {f5, 55 bxor 56}},
    Expected = M:decode_msg(<<B1/binary, B2/binary>>, m),
    Expected = M:merge_msgs(M1, M2),
    unload_code(M).

uuid_e(Uuid) when is_integer(Uuid) ->
    {uuid, integer_to_list(Uuid)}.

uuid_d({uuid,UuidStr}) when ?is_string(UuidStr) ->
    list_to_integer(UuidStr).

uuid_v(Uuid) when is_integer(Uuid) -> ok;
uuid_v(X) -> error({non_int_uuid,X}).

uuid_m(Uuid1, Uuid2) when is_integer(Uuid1), is_integer(Uuid2) ->
    Uuid1 bxor Uuid2.

%% -- translation of other types ----------

basic_translate_with_userdata_test() ->
    %% For this test, we'll pretend `bytes' values are ipv4 addresses
    %% and the userdata denotes a network (for instance: "192.168.0.0/16")
    M = compile_iolist(
          ["message m {",
           "  required bytes f = 1;",
           "}"],
          [{translate_type,
            {bytes, % no merge function since bytes is a scalar type
             [{encode, {?MODULE, e_ipv4_addr, ['$1', '$user_data']}},
              {decode, {?MODULE, d_ipv4_addr, ['$1', '$user_data']}},
              {verify, {?MODULE, v_ipv4_addr, ['$1', '$user_data']}}]}}]),
    Nw = fun(V) -> [{user_data, V}] end,
    <<10,4, 127,0,0,1>>   = M:encode_msg({m,{127,0,0,1}}, Nw("127.0.0.0/8")),
    <<10,4, 127,1,2,3>>   = M:encode_msg({m,{255,1,2,3}}, Nw("127.0.0.0/8")),
    <<10,4, 192,168,2,3>> = M:encode_msg({m,{127,1,2,3}}, Nw("192.168.0.0/16")),
    {m,{127,0,0,1}}  = M:decode_msg(<<10,4, 127,0,0,1>>,m,Nw("127.0.0.0/8")),
    {m,{127,1,2,3}}  = M:decode_msg(<<10,4, 255,1,2,3>>,m,Nw("127.0.0.0/8")),
    {m,{192,168,2,3}}= M:decode_msg(<<10,4, 127,1,2,3>>,m,Nw("192.168.0.0/16")),
    ok = M:verify_msg({m,{127,0,0,1}}, Nw("127.0.0.0/8")),
    ?assertError(_, M:verify_msg({m,{10,1,2,3}}, Nw("127.0.0.0/8"))),
    unload_code(M).

e_ipv4_addr({A,B,C,D}, Net) ->
    list_to_binary(tuple_to_list(apply_ipv4_netmask({A,B,C,D}, Net))).

d_ipv4_addr(<<A,B,C,D>>, Net) ->
    apply_ipv4_netmask({A,B,C,D}, Net).

v_ipv4_addr({A,B,C,D}, Net) ->
    %% verify IP {A,B,C,D} is within Net (on format "10.0.0.0/8")
    {ok, [N1,N2,N3,N4, Netmask], []} = io_lib:fread("~d.~d.~d.~d/~d", Net),
    <<Ip:32>> = <<A,B,C,D>>,
    <<N:32>> = <<N1,N2,N3,N4>>,
    M = ((1 bsl Netmask) - 1) bsl (32 - Netmask),
    if ((Ip band M) bxor (N band M)) =:= 0 -> ok;
       true -> error({address_outside_of_network,{A,B,C,D},Net})
    end.

apply_ipv4_netmask({A,B,C,D}, Net) ->
    %% set/change the network bits of ip {A,B,C,D}, to those in Net
    {ok, [N1,N2,N3,N4, Netmask], []} = io_lib:fread("~d.~d.~d.~d/~d", Net),
    <<Ip:32>> = <<A,B,C,D>>,
    <<N:32>> = <<N1,N2,N3,N4>>,
    Subnetmask = (1 bsl (32 - Netmask)) - 1,
    Mask = ((1 bsl Netmask) - 1) bsl (32 - Netmask),
    list_to_tuple(
      binary_to_list(<<((N band Mask) + (Ip band Subnetmask)):32>>)).

translate_all_scalar_types_test() ->
    {timeout,10,fun translate_all_scalar_types_test_aux/0}.

translate_all_scalar_types_test_aux() ->
    M = compile_iolist(
          ["message o_i32     { optional int32      f = 1; }",
           "message o_s32     { optional sint32     f = 1; }",
           "message o_uf32    { optional fixed32    f = 1; }",
           "message o_ee      { optional ee         f = 1; }",
           "message o_bool    { optional bool       f = 1; }",
           "message o_str     { optional string     f = 1; }",
           "message o_bytes   { optional bytes      f = 1; }",
           "message o_float   { optional float      f = 1; }",
           "message o_double  { optional double     f = 1; }",
           "",
           "message u_i32     { oneof u { int32      f = 1; } }",
           "message u_s32     { oneof u { sint32     f = 1; } }",
           "message u_uf32    { oneof u { fixed32    f = 1; } }",
           "message u_ee      { oneof u { ee         f = 1; } }",
           "message u_bool    { oneof u { bool       f = 1; } }",
           "message u_str     { oneof u { string     f = 1; } }",
           "message u_bytes   { oneof u { bytes      f = 1; } }",
           "message u_float   { oneof u { float      f = 1; } }",
           "message u_double  { oneof u { double     f = 1; } }",
           "",
           "message rq_i32     { required int32     f = 1; }",
           "message rq_s32     { required sint32    f = 1; }",
           "message rq_uf32    { required fixed32   f = 1; }",
           "message rq_ee      { required ee        f = 1; }",
           "message rq_bool    { required bool      f = 1; }",
           "message rq_str     { required string    f = 1; }",
           "message rq_bytes   { required bytes     f = 1; }",
           "message rq_float   { required float     f = 1; }",
           "message rq_double  { required double    f = 1; }",
           "",
           "message rp_i32     { repeated int32     f = 1; }",
           "message rp_s32     { repeated sint32    f = 1; }",
           "message rp_uf32    { repeated fixed32   f = 1; }",
           "message rp_ee      { repeated ee        f = 1; }",
           "message rp_bool    { repeated bool      f = 1; }",
           "message rp_str     { repeated string    f = 1; }",
           "message rp_bytes   { repeated bytes     f = 1; }",
           "message rp_float   { repeated float     f = 1; }",
           "message rp_double  { repeated double    f = 1; }",
           "",
           "message rpp_i32     { repeated int32    f = 1 [packed]; }",
           "message rpp_s32     { repeated sint32   f = 1 [packed]; }",
           "message rpp_uf32    { repeated fixed32  f = 1 [packed]; }",
           "message rpp_ee      { repeated ee       f = 1 [packed]; }",
           "message rpp_bool    { repeated bool     f = 1 [packed]; }",
           "message rpp_str     { repeated string   f = 1; } // unpackable;\n",
           "message rpp_bytes   { repeated bytes    f = 1; } // unpackable;\n",
           "message rpp_float   { repeated float    f = 1 [packed]; }",
           "message rpp_double  { repeated double   f = 1 [packed]; }",
           "",
           "enum ee { zero = 0; one = 1; }"
          ],
          [{translate_type,
            {Scalar,
             [{encode, {?MODULE, e_astt, ['$1']}},
              {decode, {?MODULE, d_astt, ['$1']}},
              {verify, {?MODULE, v_astt, ['$1']}}]}}
           || Scalar <- [int32, sint32, fixed32, {enum,ee}, bool,
                         string, bytes, float, double]]),

    [ok = round_trip_translate_test(
            [{Prefix, i32,    "value:4711"},
             {Prefix, s32,    "value:4711"},
             {Prefix, s32,    "value:-4711"},
             {Prefix, uf32,   "value:4711"},
             {Prefix, ee,     "value:one"},
             {Prefix, bool,   "value:true"},
             {Prefix, str,    "value:\"some-string\""},
             {Prefix, bytes,  "value:<<13,14,215,216>>"},
             {Prefix, float,  "value:1.125"},
             {Prefix, double, "value:1.25"}],
            M)
     || Prefix <- [o, u, rq, rp, rpp]],
        unload_code(M).

round_trip_translate_test([{Prefix, Suffix, IntValue0} | Rest], M) ->
    IntValues = case Prefix of
                   o   -> [IntValue0, undefined];
                   u   -> [{f,IntValue0}, undefined];
                   rq  -> [IntValue0];
                   rp  -> [[IntValue0]];
                   rpp -> [[IntValue0]]
                end,
    MsgName = list_to_atom(lists:concat([Prefix, "_", Suffix])),
    [begin
         ok = M:verify_msg({MsgName, IntValue}),
         ?assertError(_, M:verify_msg({MsgName, "bad"++IntValue})),
         Msg = {MsgName, IntValue},
         Encoded = M:encode_msg(Msg),
         Msg = M:decode_msg(Encoded, MsgName)
     end
     || IntValue <- IntValues],
    round_trip_translate_test(Rest, M);
round_trip_translate_test([], _M) ->
    ok.

e_astt("value:"++Rest) -> string_to_value(Rest).

d_astt(Value) -> "value:"++value_to_string(Value).

v_astt("value:"++_Rest)   -> ok;
v_astt("badvalue:"++Rest) -> error({badvalue,Rest}).

string_to_value(S) ->
    {ok,Tokens,_EndL} = erl_scan:string(S++"."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

value_to_string(V) ->
    lists:flatten(io_lib:format("~p", [V])).

%%-
translate_maptype_test() ->
    %% For this tests, the internal format of the map<_,_> is a dict()
    M = compile_iolist(
          ["message m {",
           "  map<int32,string> m = 1;"
           "}"],
          [{translate_type,
            {{map,int32,string},
             [{encode,{dict,to_list, ['$1']}},
              {decode,{dict,from_list, ['$1']}},
              {verify,{?MODULE,is_dict, ['$1']}}]}}]),
    D0 = dict:from_list([{1,"one"},{2,"two"}]),
    M1 = {m,D0},
    ok = M:verify_msg(M1),
    B1 = M:encode_msg(M1),
    {m,D1} = M:decode_msg(B1, m),
    ?assertEqual(lists:sort(dict:to_list(D0)),
                 lists:sort(dict:to_list(D1))),
    ?assertError({gpb_type_error, _}, M:verify_msg({m, not_a_dict})),
    unload_code(M).

is_dict(D) ->
    try dict:to_list(D), ok
    catch _:_ -> error({not_a_dict,D})
    end.

%% -
translate_oneof_test() ->
    %% For this test, we'll have an oneof which is either an ipv4 or ipv6
    %% (with some non-obvious types, just to test different)
    %% and translations of the ip field itself (the oneo) is what we want
    %% to test.
    %% The internal format is either a 4-tuple or an 8-tuple.
    M = compile_iolist(
          ["message m {",
           "  oneof ip {",
           "    fixed32 ipv4 = 1;",
           "    bytes ipv6 = 2;",
           "  }",
           "}"],
          [{translate_field,
            {[m,ip], [{encode, {?MODULE, e_ipv4or6, ['$1']}},
                      {decode, {?MODULE, d_ipv4or6, ['$1']}},
                      {verify, {?MODULE, v_ipv4or6, ['$1']}}]}}]),
    M1 = {m, {127,0,0,1}},
    M2 = {m, {0,0,0,0, 0,0,0,1}},
    ok = M:verify_msg(M1),
    ok = M:verify_msg(M2),
    <<13, _/bits>>    = B1 = M:encode_msg(M1), % check field tag+wiretype
    <<18,16, _/bits>> = B2 = M:encode_msg(M2), % check field tag+wiretype, len
    M1 = M:decode_msg(B1, m),
    M2 = M:decode_msg(B2, m),
    ?assertError({gpb_type_error, _},
                 M:verify_msg({m,{1,2,3,4,5,6}})), % wrong tuple size
    unload_code(M).

e_ipv4or6({A,B,C,D}) ->
    <<Ipv4AsInt:32>> = <<A,B,C,D>>,
    {ipv4, Ipv4AsInt};
e_ipv4or6({A,B,C,D, E,F,G,H}) ->
    Bytes = << <<N:16>> || N <- [A,B,C,D, E,F,G,H] >>,
    {ipv6, Bytes}.

d_ipv4or6({ipv4, Ipv4AsInt}) when is_integer(Ipv4AsInt) ->
    <<A,B,C,D>> = <<Ipv4AsInt:32>>,
    {A,B,C,D};
d_ipv4or6({ipv6, Bytes}) when bit_size(Bytes) =:= 128 ->
    [A,B,C,D, E,F,G,H] = [N || <<N:16>> <= Bytes],
    {A,B,C,D, E,F,G,H}.

v_ipv4or6({_,_,_,_}) -> ok;
v_ipv4or6({_,_,_,_, _,_,_,_}) -> ok;
v_ipv4or6(X) -> error({invalid_ipv4_or_ipv6, X}).

%%-
translate_repeated_test() ->
    %% For this test, the internal format of a repeated field is a set
    M = compile_iolist(
          ["message m {",
           "  repeated uint32 f = 1;",
           "}"],
          [{translate_field,
            {[m,f], [{encode, {sets,to_list, ['$1']}},
                     {decode_init_default, {sets, new, []}},
                     {decode_repeated_add_elem, {sets, add_element,
                                                 ['$1', '$2']}},
                     {decode_repeated_finalize, {?MODULE, id, ['$1']}},
                     {merge, {sets, union, ['$1', '$2']}},
                     {verify, {?MODULE, v_is_set, ['$1']}}]}}]),
    S0 = sets:from_list([1,2,3,4,5]),
    S2 = sets:from_list([4,5,6,7,8]),
    M1 = {m,S0},
    ok = M:verify_msg(M1),
    B1 = M:encode_msg(M1),
    {m,S1} = M:decode_msg(B1, m),
    ?assertEqual(lists:sort(sets:to_list(S0)),
                 lists:sort(sets:to_list(S1))),
    ?assertError({gpb_type_error, _}, M:verify_msg({m, not_a_set})),
    M2 = {m,S2},
    B2 = M:encode_msg(M2),
    {m,S22a} = M:decode_msg(<<B1/binary, B2/binary>>, m),
    ?assertEqual(lists:sort(sets:to_list(sets:union(S0,S2))),
                 lists:sort(sets:to_list(S22a))),
    {m,S22b} = M:merge_msgs(M1, M2),
    ?assertEqual(lists:sort(sets:to_list(sets:union(S0,S2))),
                 lists:sort(sets:to_list(S22b))),
    unload_code(M).

v_is_set(X) ->
    case sets:is_set(X) of
        true  -> ok;
        false -> error({not_a_set, X})
    end.

%%-
translate_messages_on_toplevel_test() ->
    %% For this test, the internal format of a message, m1, is an integer.
    %% and a string for f2.
    M = compile_iolist(
          ["message m1 {",
           "  required uint32 f = 1;",
           "}",
           "message m2 {",
           "  required string f = 1;",
           "}"],
          [{translate_type,
            {{msg,m1}, [{encode, {?MODULE, e_value_to_msg, ['$1', m1]}},
                        {decode, {?MODULE, d_msg_to_value, ['$1', m1]}},
                        {verify, {?MODULE, v_value, ['$1', integer]}},
                        {merge,  {erlang, '+', ['$1', '$2']}}]}},
           {translate_field,
            {[m2], [{encode, {?MODULE, e_value_to_msg, ['$1', m2]}},
                    {decode, {?MODULE, d_msg_to_value, ['$1', m2]}},
                    {verify, {?MODULE, v_value, ['$1', string]}},
                    {merge,  {erlang, '++', ['$1', '$2']}}]}}]),
    I1 = 28746,
    ok = M:verify_msg(I1, m1),
    B1 = M:encode_msg(I1, m1),
    I1 = M:decode_msg(B1, m1),
    ?assertEqual(I1 * 2, M:merge_msgs(I1, I1, m1)),
    ?assertError({gpb_type_error, _}, M:verify_msg(xyz, m1)),
    S2 = "abc",
    B2 = M:encode_msg(S2, m2),
    S2 = M:decode_msg(B2, m2),
    ?assertEqual(S2 ++ "def", M:merge_msgs(S2, "def", m2)),
    ?assertError({gpb_type_error, _}, M:verify_msg(xyzw, m2)),
    unload_code(M).

e_value_to_msg(Value, MsgName) -> {MsgName, Value}.

d_msg_to_value({MsgName, Value}, MsgName) -> Value.

v_value(Value, integer) when is_integer(Value) -> ok;
v_value(Value, string) when is_list(Value) -> ok;
v_value(X, Expected) -> error({bad_value, Expected, X}).

verify_is_optional_for_translate_toplevel_messages_test() ->
    M = compile_iolist(
          ["message m1 {",
           "  required uint32 f = 1;",
           "}"],
          [{translate_field,
            {[m1], [{encode, {?MODULE, e_value_to_msg, ['$1', m2]}},
                    {decode, {?MODULE, d_msg_to_value, ['$1', m2]}},
                    {merge,  {erlang, '++', ['$1', '$2']}}]}}]),
    ok = M:verify_msg(9348, m1),
    ok = M:verify_msg(bad_int_ok_since_no_verify_specified, m1),
    unload_code(M).

%% --- misc ----------

wellknows_found_also_for_syntax_proto2_test() ->
    M = compile_iolist(
          ["syntax=\"proto2\";",
           "import \"google/protobuf/any.proto\";",
           "message m {",
           "  required google.protobuf.Any f1=1;",
           "}"],
          [use_packages]),
    unload_code(M).

typespecs_and_uppercase_oneof_fields_test() ->
    M = compile_iolist(["message M {",
                        "  oneof x {",
                        "    uint32 Abc = 1;",
                        "  }",
                        "}"],
                       [type_specs]),
    E = M:encode_msg({'M', {'Abc', 17}}),
    ?assert(is_binary(E)),
    unload_code(M).

only_enums_no_msgs_test() ->
    M = compile_iolist(["enum e {"
                        "  a = 1;",
                        "}"]),
    ?assertError({gpb_error, no_messages}, M:encode_msg({x})),
    ?assertError({gpb_error, no_messages}, M:encode_msg({x}, [])),
    ?assertError({gpb_error, no_messages}, M:decode_msg(<<>>, x)),
    ?assertError({gpb_error, no_messages}, M:merge_msgs({x}, {x})),
    ?assertError({gpb_type_error, {not_a_known_message, _}}, M:verify_msg({x})),
    [] = M:get_msg_names(),
    [e] = M:get_enum_names(),
    unload_code(M).

ignores_packed_for_nonpackable_repeated_on_encoding_test() ->
    {ok, M, [_WarningAboutIgnoredPackedOption]} =
        compile_iolist_get_errors_or_warnings(
          ["message m1 { repeated string s1 = 1 [packed]; }"]),
    %% expect no length-delimited wrapping around the field
    %% just the elements one after the other.
    <<10,3,"abc",10,3,"def">> = M:encode_msg({m1, ["abc", "def"]}).

%% --- Returning/reporting warnings/errors (and warnings_as_errors) tests -----
%% ... when compiling to file/binary/defs
%% ... when compiling from file/defs
%% ... when there are/aren't warnings/errors

report_or_return_warnings_or_errors_test_() ->
    %% On my slow machine (1.6 GHz Atom N270), it currently takes ~61 seconds
    {timeout,120,fun report_or_return_warnings_or_errors_test_aux/0}.

report_or_return_warnings_or_errors_test_aux() ->
    [begin
         Options = WarningOptions ++ ErrorOptions ++ WarnsAsErrsOpts,
         try
             rwre_go(Options, CompileTo, SrcType, SrcQuality)
         catch ?STACKTRACE(Class,Reason,Stack)
                 %% Need some trouble shooting info for the failing combination
                 %% This could have been made into a test generator,
                 %% with each combination its won test,
                 %% but in total 544 tests are executed, and if running
                 %% with verbose mode, it'll always be half a thousand lines
                 %% of (almost) non-interesting info.
                 ?debugFmt("~nFailed for~n"
                           "   Options=~p~n"
                           "   CompileTo=~p~n"
                           "   SrcType=~p~n"
                           "   SrcQuality=~p~n",
                           [Options, CompileTo, SrcType, SrcQuality]),
                 erlang:raise(Class, Reason, Stack)
         end
     end
     || WarningOptions     <- [[], [report_warnings], [return_warnings],
                               [report_warnings, return_warnings]],
        ErrorOptions       <- [[], [report_errors], [return_errors],
                               [report_errors, return_errors]],
        WarnsAsErrsOpts    <- [[], [warnings_as_errors]],
        CompileTo          <- [to_binary, to_file, to_proto_defs],
        SrcType            <- [from_file, from_defs, from_string],
        SrcQuality         <- [clean_code, warningful_code, erroneous_code,
                               write_fails],
        %% Exclude a few combos
        not (SrcQuality == erroneous_code andalso SrcType == from_defs),
        not (SrcQuality == write_fails andalso CompileTo == to_binary),
        not (SrcQuality == write_fails andalso CompileTo == to_proto_defs)].

rwre_go(Options, CompileTo, SrcType, SrcQuality) ->
    ExpectedReturn = compute_expected_return(Options, CompileTo, SrcQuality),
    ExpectedOutput = compute_expected_output(Options, SrcQuality),
    {{return,Returned},
     {output,Output}} = compile_the_code(Options, CompileTo,
                                         SrcType, SrcQuality),
    eval_return(ExpectedReturn, Returned, Output,
                Options, CompileTo, SrcType, SrcQuality),
    eval_output(ExpectedOutput, Output, Returned,
                Options, CompileTo, SrcType, SrcQuality),
    ok.


compute_expected_return(Options, CompileTo, SrcQuality) ->
    WarnsAsErrs = proplists:get_bool(warnings_as_errors, Options),
    WarnOpt = get_warning_opt_from_perspective_of_return(Options),
    case {WarnsAsErrs, SrcQuality, WarnOpt} of
        {true, warningful_code, return} -> {error, '_', non_empty_list};
        {true, warningful_code, report} -> error;
        _ -> compute_expected_return_normal_warns(Options, CompileTo, SrcQuality)
    end.

compute_expected_return_normal_warns(Options, CompileTo, write_fails) ->
    compute_expected_return_normal_warns(Options, CompileTo, erroneous_code);
compute_expected_return_normal_warns(Options, to_file, SrcQuality) ->
    WarnOpt = get_warning_opt_from_perspective_of_return(Options),
    ErrOpt = get_error_opt_from_perspective_of_return(Options),
    case {WarnOpt, ErrOpt, SrcQuality} of
        {report, report, clean_code}      -> ok;
        {report, report, warningful_code} -> ok;
        {report, report, erroneous_code}  -> {error, '_'};
        {return, return, clean_code}      -> {ok, []};
        {return, return, warningful_code} -> {ok, non_empty_list};
        {return, return, erroneous_code}  -> {error, '_', []};
        {report, return, clean_code}      -> ok;
        {report, return, warningful_code} -> ok;
        {report, return, erroneous_code}  -> {error, '_'};
        {return, report, clean_code}      -> {ok, []};
        {return, report, warningful_code} -> {ok, non_empty_list};
        {return, report, erroneous_code}  -> {error, '_', []}
    end;
compute_expected_return_normal_warns(Options, to_binary, SrcQuality) ->
    WarnOpt = get_warning_opt_from_perspective_of_return(Options),
    ErrOpt = get_error_opt_from_perspective_of_return(Options),
    case {WarnOpt, ErrOpt, SrcQuality} of
        {report, report, clean_code}      -> {ok, mod, binary};
        {report, report, warningful_code} -> {ok, mod, binary};
        {report, report, erroneous_code}  -> {error, '_'};
        {return, return, clean_code}      -> {ok, mod, binary, []};
        {return, return, warningful_code} -> {ok, mod, binary, non_empty_list};
        {return, return, erroneous_code}  -> {error, '_', []};
        {report, return, clean_code}      -> {ok, mod, binary};
        {report, return, warningful_code} -> {ok, mod, binary};
        {report, return, erroneous_code}  -> {error, '_'};
        {return, report, clean_code}      -> {ok, mod, binary, []};
        {return, report, warningful_code} -> {ok, mod, binary, non_empty_list};
        {return, report, erroneous_code}  -> {error, '_', []}
    end;
compute_expected_return_normal_warns(Options, to_proto_defs, SrcQuality) ->
    WarnOpt = get_warning_opt_from_perspective_of_return(Options),
    ErrOpt = get_error_opt_from_perspective_of_return(Options),
    case {WarnOpt, ErrOpt, SrcQuality} of
        {report, report, clean_code}      -> {ok, non_empty_list};
        {report, report, warningful_code} -> {ok, non_empty_list};
        {report, report, erroneous_code}  -> {error, '_'};
        {return, return, clean_code}      -> {ok, non_empty_list, []};
        {return, return, warningful_code} -> {ok, non_empty_list,non_empty_list};
        {return, return, erroneous_code}  -> {error, '_', []};
        {report, return, clean_code}      -> {ok, non_empty_list};
        {report, return, warningful_code} -> {ok, non_empty_list};
        {report, return, erroneous_code}  -> {error, '_'};
        {return, report, clean_code}      -> {ok, non_empty_list, []};
        {return, report, warningful_code} -> {ok, non_empty_list,non_empty_list};
        {return, report, erroneous_code}  -> {error, '_', []}
    end.


compute_expected_output(_Options, clean_code) ->
    "";
compute_expected_output(Options, warningful_code) ->
    WarnOpt = get_warning_opt_from_perspective_of_output(Options),
    ErrOpt = get_error_opt_from_perspective_of_output(Options),
    case {WarnOpt, ErrOpt} of
        {report, report} -> non_empty_list;
        {report, return} -> non_empty_list;
        {return, report} -> "";
        {return, return} -> ""
    end;
compute_expected_output(Options, write_fails) ->
    compute_expected_output(Options, erroneous_code);
compute_expected_output(Options, erroneous_code) ->
    WarnOpt = get_warning_opt_from_perspective_of_output(Options),
    ErrOpt = get_error_opt_from_perspective_of_output(Options),
    case {WarnOpt, ErrOpt} of
        {report, report} -> non_empty_list;
        {report, return} -> "";
        {return, report} -> non_empty_list;
        {return, return} -> ""
    end.

get_warning_opt_from_perspective_of_return(Opts) -> get_warn_opt(Opts, return).
get_warning_opt_from_perspective_of_output(Opts) -> get_warn_opt(Opts, report).

get_error_opt_from_perspective_of_return(Opts) -> get_err_opt(Opts, return).
get_error_opt_from_perspective_of_output(Opts) -> get_err_opt(Opts, report).

get_warn_opt(Opts, WhatToReturnIfBothAreSet) ->
    case {member(return_warnings, Opts), member(report_warnings, Opts)} of
        {false, false} -> report; %% default
        {true,  false} -> return;
        {false,  true} -> report;
        {true,   true} -> WhatToReturnIfBothAreSet
    end.

get_err_opt(Opts, WhatToReturnIfBothAreSet) ->
    case {member(return_errors, Opts), member(report_errors, Opts)} of
        {false, false} -> report; %% default
        {true,  false} -> return;
        {false,  true} -> report;
        {true,   true} -> WhatToReturnIfBothAreSet
    end.

member(Elem, List) ->
    lists:member(Elem, List).

compile_the_code(Options, CompileTo, from_defs, SrcQuality) ->
    compile_msg_defs_get_output(get_proto_defs(SrcQuality),
                                compute_compile_opts(Options, CompileTo,
                                                     SrcQuality));
compile_the_code(Options, CompileTo, from_file, SrcQuality) ->
    compile_file_get_output(get_proto_file(SrcQuality),
                            compute_compile_opts(Options, CompileTo,
                                                 SrcQuality));
compile_the_code(Options, CompileTo, from_string, SrcQuality) ->
    compile_string_get_output(get_proto_file(SrcQuality),
                              compute_compile_opts(Options, CompileTo,
                                                   SrcQuality)).

get_proto_defs(clean_code) ->
    [{{msg,m1}, [#?gpb_field{name=field11, type=uint32, occurrence=optional,
                             fnum=1, rnum=2, opts=[]}]}];
get_proto_defs(warningful_code) ->
    %% warning: packed field of type bytes
    [{{msg,m1}, [#?gpb_field{name=field11, type=bytes, occurrence=optional,
                             fnum=1, rnum=2, opts=[packed]}]}];
get_proto_defs(write_fails) ->
    get_proto_defs(clean_code).

get_proto_file(clean_code) ->
    "message m1 { optional uint32 field11 = 1; }\n" ++
    "message MessageInfo1 { optional uint32 field11 = 1; }\n";
get_proto_file(warningful_code) ->
    %% warning: packed field of type bytes
    ["message m1 { optional bytes field11 = 1 [packed]; }\n"];
get_proto_file(erroneous_code) ->
    "g&~#";
get_proto_file(write_fails) ->
    get_proto_file(clean_code).

compute_compile_opts(Options, CompileTo, write_fails) ->
    compute_compile_opts_2(Options, CompileTo) ++ mk_failing_write_option();
compute_compile_opts(Options, CompileTo, _SrcQuality) ->
    compute_compile_opts_2(Options, CompileTo).

compute_compile_opts_2(Opts, to_binary)   -> [binary, type_specs | Opts];
compute_compile_opts_2(Opts, to_proto_defs) -> [to_proto_defs, type_specs | Opts];
compute_compile_opts_2(Opts, to_file)     -> [type_specs | Opts].

mk_failing_write_option() ->
    [fail_write].

compile_msg_defs_get_output(MsgDefs, Opts) ->
    Opts2 = case lists:member(fail_write, Opts) of
                false ->
                    Opts;
                true ->
                    RestOpts = Opts -- [fail_write],
                    FOpt = mk_fileop_opt([{write_file,fun(_,_) -> {error,eacces}
                                                      end}]),
                    [FOpt | RestOpts]
            end,
    Opts3 = ensure_file_writing_stubbed_opt(Opts2),
    capture_stdout(fun() -> gpb_compile:proto_defs('x', MsgDefs, Opts3) end).

compile_file_get_output(Txt, Opts) ->
    Contents = iolist_to_binary(Txt),
    FailWrite = lists:member(fail_write, Opts),
    RestOpts = Opts -- [fail_write],
    FileOpOpts = if FailWrite -> mk_fileop_opt(
                                   [{read_file, fun(_) -> {ok, Contents} end},
                                    {write_file,fun(_, _) -> {error,eacces} end}
                                   ]);
                    true -> mk_fileop_opt(
                              [{read_file, fun(_) -> {ok, Contents} end}])
                 end,
    Opts2 = [FileOpOpts, {i,"."} | RestOpts],
    Opts3 = ensure_file_writing_stubbed_opt(Opts2),
    capture_stdout(fun() -> gpb_compile:file("X.proto", Opts3) end).

compile_string_get_output(Txt, Opts) ->
    Opts2 = case lists:member(fail_write, Opts) of
                false ->
                    Opts;
                true ->
                    RestOpts = Opts -- [fail_write],
                    FOpt = mk_fileop_opt([{write_file,fun(_,_) -> {error,eacces}
                                                      end}]),
                    [FOpt | RestOpts]
            end,
    Opts3 = ensure_file_writing_stubbed_opt(Opts2),
    Txt2 = binary_to_list(iolist_to_binary(Txt)),
    capture_stdout(fun() -> gpb_compile:string('x', Txt2, Opts3) end).

ensure_file_writing_stubbed_opt(Opts) ->
    case proplists:get_value(file_op, Opts) of
        undefined ->
            [mk_fileop_opt([]) | Opts]; % the default will stub writing
        _ ->
            Opts % already stubbed or changed
    end.

eval_return(Expected, Actual, Output,
            Options, CompileTo, SrcType, SrcQuality) ->
    case match_values(Expected, Actual) of
        true ->
            ok;
        false ->
            erlang:error({bad_return,Expected,Actual,
                          [{output,Output},
                           {setup,{Options, CompileTo,
                                   SrcType, SrcQuality}}]})
    end.

eval_output(Expected, Actual, Returned,
            Options, CompileTo, SrcType, SrcQuality) ->
    case match_value(Expected, Actual) of
        true ->
            ok;
        false ->
            erlang:error({bad_output,Expected,Actual,
                          [{returned,Returned},
                           {setup,{Options, CompileTo,
                                   SrcType, SrcQuality}}]})
    end.


match_values(X, X) ->
    true;
match_values([E | ERest], [A | ARest]) ->
    case match_value(E, A) of
        true  -> match_values(ERest, ARest);
        false -> false
    end;
match_values(ET, AT) when is_tuple(ET), is_tuple(AT),
                          tuple_size(ET) == tuple_size(AT) ->
    match_values(tuple_to_list(ET), tuple_to_list(AT));
match_values(_, _) ->
    false.

match_value('_', _) ->
    true;
match_value(non_empty_list, X) when is_list(X), X /= [] ->
    true;
match_value(binary, X) when is_binary(X) ->
    true;
match_value(mod, X) when is_atom(X) ->
    true;
match_value(atom, X) when is_atom(X) ->
    true;
match_value(X, X)  ->
    true;
match_value(ET, AT) when is_tuple(ET), is_tuple(AT),
                          tuple_size(ET)==tuple_size(AT) ->
    match_values(ET, AT);
match_value(_, _) ->
    false.

capture_stdout_actually_works_test() ->
    Ret = x,
    {{return, Ret},
     {output, "z"}} = capture_stdout(fun() -> io:format("~s", [z]), Ret end).

capture_stdout(Fun) ->
    {_Pid,MRef} = spawn_monitor(
                   fun() ->
                           EvalExitWithResult = fun() -> exit(Fun()) end,
                           group_leader(self(), self()),
                           {_Pid, MRef} = spawn_monitor(EvalExitWithResult),
                           handle_io_requests(MRef, [])
                   end),
    receive
        {'DOWN', MRef, _, _, {{return, _Ret}, {output, _Output}}=Res} ->
            Res
    end.

handle_io_requests(MRef, Acc) ->
    receive
        {'DOWN', MRef, _, _, FunRes} ->
            exit({{return, FunRes},
                  {output, lists:flatten(lists:reverse(Acc))}});
        {io_request, From, ReplyAs, Req} ->
            {IoRes, Output} = handle_io_req(Req),
            From ! {io_reply, ReplyAs, IoRes},
            handle_io_requests(MRef, [Output | Acc])
    end.

handle_io_req({put_chars, Mod, Fun, Args}) ->
    {ok, apply(Mod, Fun, Args)};
handle_io_req({put_chars, _Enc, Mod, Fun, Args}) ->
    {ok, apply(Mod, Fun, Args)};
handle_io_req({put_chars, Txt}) ->
    {ok, Txt};
handle_io_req({put_chars, _Enc, Txt}) ->
    {ok, Txt};
handle_io_req({setopts, _}) ->
    {ok, ""};
handle_io_req({requests, IoRequests}) ->
    handle_io_reqs(IoRequests, []);
handle_io_req(_) ->
    %% {get_geometry, _??}
    %% {get_password, Prompt}
    %% {get_password, Enc, Prompt}
    %% {get_until, Prompt, Mod, Fun, Args}
    %% {get_until, Prompt, Enc, Mod, Fun, Args}
    %% {get_line, Prompt}
    %% {get_line, Enc, Prompt}
    %% {get_chars, Prompt, N}
    %% {get_chars, Enc, Prompt, N}
    {{error, enotsup}, ""}.

handle_io_reqs([Req | Rest], Acc) ->
    {_Res, Output} = handle_io_req(Req),
    handle_io_reqs(Rest, [Output | Acc]);
handle_io_reqs([], Acc) ->
    {ok, lists:flatten(lists:reverse(Acc))}.

failure_to_write_output_files_not_ignored_test() ->
    Contents = <<"message m1 { optional uint32 field11 = 1; }\n">>,
    CommonFileOpOpts = [{read_file, fun(_) -> {ok, Contents} end}],
    CommonOpts = [{i,"."}, return],
    WriteErlFailsOpts =
        [mk_fileop_opt([{write_file, fun("X.erl", _) -> ok;
                                        ("X.hrl", _) -> {error, eacces}
                                     end} | CommonFileOpOpts]) | CommonOpts],
    WriteHrlFailsOpts =
        [mk_fileop_opt([{write_file, fun("X.erl", _) -> ok;
                                        ("X.hrl", _) -> {error, eacces}
                                     end} | CommonFileOpOpts]) | CommonOpts],
    {error, _Reason, []}=Err1 = gpb_compile:file("X.proto", WriteErlFailsOpts),
    {error, _Reason, []}=Err2 = gpb_compile:file("X.proto", WriteHrlFailsOpts),
    gpb_compile:format_error(Err1),
    gpb_compile:format_error(Err2).

%% --- format_error and format_warning tests ----------

format_error_works_for_scan_errors_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["message Msg ~~ required uint32 field1 = & }\n"],
      [".proto:1: "]).

format_error_works_for_parse_errors_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["message Msg { required uint32 field1 = }\n"],
      [".proto:1: ", "syntax error"]).

format_error_works_when_failed_to_read_import_file_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["import \"ZZ.proto\";\n",
       "message Msg { required uint32 field1 = 2;}\n"],
      [{read_file, [{"ZZ.proto", {error, eacces}}]}],
      ["read", "permission denied"]).

format_error_works_when_import_file_not_found_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["import \"ZZ.proto\";\n",
       "message Msg { required uint32 field1 = 2;}\n"],
      [{read_file_info, [{"ZZ.proto", {error, enoent}}]}],
      ["import", "not"]).

format_error_works_for_verification_errors_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["message Msg1 { required Msg2 field1 = 2;}\n"],
      ["Msg2", "Msg1", "field1"]).

format_warning_works_with_packed_for_unpackable_test() ->
    compile_and_assert_that_format_warning_produces_iolist(
      ["message Msg1 { repeated string field1 = 2 [packed]; }\n"],
      ["Msg1", "field1", "ignor", "packed"]).

compile_and_assert_that_format_error_produces_iolist(Contents, ExpectedWords) ->
    compile_and_assert_that_format_error_produces_iolist(
      Contents, [], ExpectedWords).

compile_and_assert_that_format_error_produces_iolist(Contents,
                                                     ExtraFileOpReturnValues,
                                                     ExpectedWords) ->
    compile_and_assert_that_format_x_produces_iolist(
      Contents, ExtraFileOpReturnValues, ExpectedWords, format_error).

compile_and_assert_that_format_warning_produces_iolist(Contents,
                                                       ExpectedWords) ->
    compile_and_assert_that_format_x_produces_iolist(
      Contents, [], ExpectedWords, format_warning).


compile_and_assert_that_format_x_produces_iolist(Contents,
                                                 ExtraFileOpReturnValues,
                                                 ExpectedPhrases,
                                                 FormatWhat) ->
    FileContents = iolist_to_binary(Contents),
    FileRetriever = mk_file_retriever(FileContents, ExtraFileOpReturnValues),
    FileInfoReader = mk_read_file_info("X.proto", ExtraFileOpReturnValues),
    Opts = [mk_fileop_opt([{read_file, FileRetriever},
                           {read_file_info, FileInfoReader}]),
            mk_defs_probe_sender_opt(self()),
            {i,"."},
            return_errors, return_warnings],
    Txt = case gpb_compile:file("X.proto", Opts) of
              {error, _Reason, _Warns}=Res when FormatWhat == format_error ->
                  gpb_compile:format_error(Res);
              {ok, Warns} when FormatWhat == format_warning ->
                  [gpb_compile:format_warning(Warn) || Warn <- Warns]
          end,
    IsIoList = io_lib:deep_char_list(Txt),
    ?assertMatch({true, _}, {IsIoList, Txt}),
    FlatTxt = lists:flatten(Txt),
    PhrasesFound = [gpb_lib:is_substr(Word, FlatTxt)
                    || Word <- ExpectedPhrases],
    AllPhrasesFound = lists:all(fun id/1, PhrasesFound),
    ?assertMatch({true,_,_}, {AllPhrasesFound, FlatTxt, PhrasesFound}).

mk_file_retriever(MainProtoFileContents, ExtraFileOpReturnValues) ->
    ExtraFileReturnValues =
        proplists:get_value(read_file, ExtraFileOpReturnValues, []),
    fun(FileName) ->
            case lists:keysearch(FileName, 1, ExtraFileReturnValues) of
                {value, {FileName, ReturnValue}} ->
                    ReturnValue;
                false ->
                    {ok, MainProtoFileContents}
            end
    end.

mk_read_file_info(_MainProtoFileName, ExtraFileOpReturnValues) ->
    ExtraFileReturnValues =
        proplists:get_value(read_file_info, ExtraFileOpReturnValues, []),
    fun(FileName) ->
            case lists:keysearch(FileName, 1, ExtraFileReturnValues) of
                {value, {FileName, ReturnValue}} ->
                    ReturnValue;
                false ->
                    {ok, #file_info{access=read}}
            end
    end.


%% --- hrl file tests -----------------

defaults_for_proto3_fields_test() ->
    Proto = fun(ProtoVersion) ->
                    ["message m {",
                     case ProtoVersion of
                         proto2 ->
                             ["  optional int32    o_i32   = 11;",
                              "  optional string   o_str   = 12;",
                              "  optional m        o_subm  = 13;"];
                         proto3 ->
                             ["  int32    o_i32   = 11;",
                              "  string   o_str   = 12;",
                              "  m        o_subm  = 13;"]
                     end,
                     "  repeated int32 r_i = 21;",
                     "  oneof u { int32 u_i = 31; };",
                     "  map<int32, int32> mii = 41;",
                     "",
                     "}"]
            end,
    MkMod = fun(ProtoVersion, GOpts) ->
                    compile_erl_iolist(
                      ["-export([new_m_msg/0]).\n",
                       compile_to_string_get_hrl(
                         ["syntax=\"",atom_to_list(ProtoVersion),"\";\n",
                          Proto(ProtoVersion)],
                         [strip_preprocessor_lines | GOpts]),
                       "new_m_msg() -> #m{}.\n"])
            end,
    P3Ma = MkMod(proto3, [type_specs]),
    {m, 0, "", undefined, [], undefined, []} = P3Ma:new_m_msg(),
    unload_code(P3Ma),

    P3Mb = MkMod(proto3, [type_specs, strings_as_binaries]),
    {m, 0, <<>>, undefined, [], undefined, []} = P3Mb:new_m_msg(),
    unload_code(P3Mb),

    P3Mc = MkMod(proto3, []),
    {m, 0, "", undefined, [], undefined, []} = P3Mc:new_m_msg(),
    unload_code(P3Mc),

    P2M = MkMod(proto2, []),
    {m, undefined, undefined, undefined, [], undefined, []} = P2M:new_m_msg(),
    unload_code(P2M).

%% --- nif generation tests -----------------

generates_nif_as_binary_and_file_test() ->
    Defs = mk_one_msg_field_of_each_type(),
    M = gpb_nif_test,
    LoadNif = "load_nif() -> erlang:load_nif({{nifbase}}, {{loadinfo}}).\n",
    LoadNifOpt = {load_nif, LoadNif},
    {ok, M, Codes} = gpb_compile:proto_defs(M, Defs, [binary, nif, LoadNifOpt]),
    Nif1 = proplists:get_value(nif, Codes),
    Master = self(),
    ReportWriteCc = fun(FName, Contents) ->
                            case filename:extension(FName) of
                                ".cc" -> Master ! {cc, Contents}, ok;
                                _     -> ok
                            end
                    end,
    FileOpOpt = mk_fileop_opt([{write_file, ReportWriteCc}]),
    ok = gpb_compile:proto_defs(M, Defs, [nif, FileOpOpt, LoadNifOpt]),
    Nif2 = receive {cc, Cc} -> Cc end,
    ?assertMatch(Nif1, Nif2).

nif_code_test_() ->
    nif_tests_check_prerequisites(
      [{"Verify errors in sepatarate vm are caught",
        fun verify_errors_in_separate_vm_are_caught/0},
       ?nif_if_supported(nif_compiles),
       ?nif_if_supported(nif_encode_decode),
       ?nif_if_supported(nif_encode_decode_oneof),
       ?nif_if_supported(nif_encode_decode_mapfields),
       ?nif_if_supported(nif_encode_decode_mapfields),
       ?nif_if_supported(nif_encode_decode_proto3),
       ?nif_if_supported(nif_with_cxx_keywords),
       ?nif_if_supported(nif_enum_in_msg),
       ?nif_if_supported(nif_enum_with_pkgs),
       ?nif_if_supported(nif_enum_from_integers),
       ?nif_if_supported(nif_aliased_enums),
       ?nif_if_supported(nif_with_groups),
       ?nif_if_supported(nif_with_strbin),
       ?nif_if_supported(nif_with_booleans),
       ?nif_if_supported(nif_with_list_indata_for_bytes),
       ?nif_if_supported(nif_with_non_normal_floats),
       ?nif_if_supported(error_if_both_translations_and_nif),
       ?nif_if_supported(bypass_wrappers_records_nif),
       ?nif_if_supported(nif_with_packages_and_enums),
       ?nif_if_supported(nif_with_renamings),
       ?nif_if_supported(nif_with_opt_but_no_package),
       ?nif_if_supported(nif_without_mergers)]).

increase_timeouts({Descr, Tests}) ->
    %% On my slow 1.6 GHz Atom N270 machine, the map field test takes
    %% ~77 seconds to run, allow for a bit more
    PerTestTimeout = 140,
    {Descr,
     {timeout, PerTestTimeout * length(Tests),  %% timeout for all tests
      [{timeout, PerTestTimeout,
        [Test]} % Test can be for instance fun/0 or {"descr", fun/0
       || Test <- Tests]}}.

nif_tests_check_prerequisites(Tests) ->
    case nif_verify_prerequisites() of
        ok ->
            increase_timeouts({"nif tests", lists:flatten(Tests)});
        {error, Text} ->
            {"Nif tests skipped: " ++ Text, []}
    end.

nif_verify_prerequisites() ->
    case {want_nif_tests(), find_protoc(), find_cplusplus_compiler()} of
        {false,_,_} -> {error, "Nif tests not wanted"};
        {_,false,_} -> {error, "Protoc not found, not trying to compile"};
        {_,_,false} -> {error, "No C++ compiler found, not trying to compile"};
        {_,_,_}     -> can_compile_simple_libproto_depending_program()
    end.

can_compile_simple_libproto_depending_program() ->
    %% The 'protoc' might be available, but not necessarily
    %% any google/protobuf/*.h files, ie not the development
    %% packages. Attempt to detect this, to avoid compilation
    %% failure (and thus unit test failure) on such hosts.
    with_tmpdir(
      fun(TmpDir) ->
              CxxLines = ["#include <stdio.h>",
                          "#include <google/protobuf/message.h>",
                          "",
                          "::google::protobuf::Message *m;",
                          "",
                          "int main(int argc, char **argv)",
                          "{",
                          "    m = 0;",
                          "    return 0;",
                          "}"],
              ProgramBase = "test-libproto",
              Program = filename:join(TmpDir, ProgramBase),
              CxxFile = filename:join(TmpDir, ProgramBase ++ ".cc"),
              ok = file:write_file(CxxFile, [[L, "\n"] || L <- CxxLines]),
              CC = find_cplusplus_compiler(),
              CFlags = get_cflags(),
              LdFlags = get_ldflags(),
              Cmd = f("'~s' -g -O0 ~s ~s"
                      "    -o '~s' '~s' -lprotobuf",
                      [CC, LdFlags, CFlags,
                       Program, CxxFile]),
              case ccompile("~s", [Cmd], silent_on_compilation_error) of
                  ok ->
                      ok;
                  {error, _} ->
                      {error, "No libproto development files"}
              end
      end).

check_nif_features_supported(NeededFeatures, ExtraChecks) ->
    case nif_verify_prerequisites() of
        ok ->
            case protoc_meets_needs(NeededFeatures) of
                ok ->
                    case check_extras(ExtraChecks, NeededFeatures) of
                        ok ->
                            ok;
                        {error, Text} ->
                            {error, Text}
                    end;
                {error, Text} ->
                    {error, Text}
            end;
        {error, Text} ->
            {error, Text}
    end.

protoc_meets_needs(NeededFeatures) ->
    NeedMax = lists:max([protoc_feature_version_appearance(F)
                         || F <- NeededFeatures] ++ [[1,0]]),
    case cachingly_find_protoc_version() of
        {ok, Vsn} when Vsn >= NeedMax ->
            ok;
        {ok, _} ->
            {error, "needs protoc >= " ++ needed_vsn_to_text(NeedMax)};
        {error, _Text} ->
            {error, "needs protoc >= " ++ needed_vsn_to_text(NeedMax)}
    end.

needed_vsn_to_text(Vsn) ->
    Vsn3 = case Vsn of
               [Major, Minor]        -> [Major, Minor, 0];
               [Major, Minor, P | _] -> [Major, Minor, P]
           end,
    gpb_lib:dot_join([integer_to_list(N) || N <- Vsn3]).

protoc_feature_version_appearance(oneof)        -> [2,6];
protoc_feature_version_appearance(allow_alias)  -> [2,6];
protoc_feature_version_appearance(cxx_keywords) -> [3,0];
protoc_feature_version_appearance(mapfields)    -> [3,0];
protoc_feature_version_appearance(proto3)       -> [3,0];
protoc_feature_version_appearance(json)         -> [3,0];
protoc_feature_version_appearance(json_preserve_proto_field_names) -> [3,3].

check_extras([Check | Rest], NeededFeatures) ->
    case Check(NeededFeatures) of
        ok ->
            check_extras(Rest, NeededFeatures);
        {error, Text} ->
            {error, Text}
    end;
check_extras([], _) ->
    ok.

guess_features(S) ->
    S2 = binary_to_list(iolist_to_binary(S)),
    [Feat || {Substr, Feat} <- [{"syntax=\"proto3\"", proto3},
                                {"syntax='proto3'", proto3},
                                {"allow_alias", allow_alias}, % in enums
                                {"oneof", oneof},
                                {"map<", mapfields}],
             gpb_lib:is_substr(Substr, S2)].

verify_errors_in_separate_vm_are_caught() ->
    %% Sanity check of the machinery for running tests in a separate vm
    %% Verify that any errors emanating from "the other side" are caught
    ?assertError({in_separate_vm, bad_badness},
                 with_tmpdir(
                   fun(TmpDir) ->
                           M = gpb_in_separate_vm_test_env_check,
                           Code = create_dummy_module(M),
                           in_separate_vm(
                             TmpDir, M, Code,
                             fun() ->
                                     erlang:error(bad_badness)
                             end)
                   end)).

create_dummy_module(MName) ->
    {ok,Toks,_} = erl_scan:string(f("-module(~p).~n", [MName])),
    {ok,Form} = erl_parse:parse_form(Toks),
    {ok,MName,Code} = compile:forms([Form]),
    Code.

nif_compiles(features) -> [];
nif_compiles(title) -> "Nif compiles".
nif_compiles() ->
    with_tmpdir(
      fun(TmpDir) ->
              NCM = gpb_nif_test_c1,
              Defs = mk_one_msg_field_of_each_type(),
              {ok, _Code} = compile_nif_msg_defs(NCM, Defs, TmpDir)
      end).

nif_encode_decode(features) -> [];
nif_encode_decode(title) -> "Encode decode basic types".
nif_encode_decode() ->
    with_tmpdir(
      fun(TmpDir) ->
              NEDM = gpb_nif_test_ed1,
              Defs = mk_one_msg_field_of_each_type(),
              {ok, Code} = compile_nif_msg_defs(NEDM, Defs, TmpDir),
              in_separate_vm(
                TmpDir, NEDM, Code,
                fun() ->
                        nif_encode_decode_test_it(NEDM, Defs),
                        nif_encode_decode_strings(NEDM, Defs),
                        ok
                end)
      end).

nif_encode_decode_test_it(NEDM, Defs) ->
    MsgNames = [MsgName || {{msg, MsgName}, _Fields} <- Defs],
    Variants = [small, big, short, long],
    lists:foreach(fun({MsgName, Variant}) ->
                          OrigMsg = mk_msg(MsgName, Defs, Variant),
                          %% to avoid errors in nif encode/decode
                          %% cancelling out each other and nif bugs go
                          %% undetected, cross-check with gpb:encode/decode_msg
                          MEncoded  = NEDM:encode_msg(OrigMsg),
                          GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                          MMDecoded = NEDM:decode_msg(MEncoded, MsgName),
                          GMDecoded = gpb:decode_msg(MEncoded, MsgName, Defs),
                          MGDecoded = NEDM:decode_msg(GEncoded, MsgName),
                          ?assertEqual(OrigMsg, MMDecoded),
                          ?assertEqual(OrigMsg, GMDecoded),
                          ?assertMatch({OrigMsg,_,_,_,_},
                                       {MGDecoded,OrigMsg,GEncoded,MEncoded,
                                        Variant})
                  end,
                  [{MsgName,Variant} || MsgName <- MsgNames,
                                        Variant <- Variants]).

nif_encode_decode_strings(NEDM, Defs) ->
    %% Check UTF-8 encoding/decoding
    CodePoints = [0,                16#7f,  %% this range reqiures 1 octet
                  16#80,          16#7fff,  %% this range reqiures 2 octets
                  16#800,         16#FFff,  %% this range reqiures 3 octets
                  16#10000,     16#10FFff], %% this range reqiures 4 octets
    %%            16#200000,   16#3ffFFff,  %% would require 5 octets
    %%            16#4000000, 16#7fffFFff   %% would require 6 octets
    %% These are outside of unicode, but encodable integers using UTF-8:
    %% Maybe ought to run these through the nif encoder/decoder just
    %% to test its UTF-8 handling, but (a) would be able to cross check with
    %% the gpb encoder/decoder, and (b) might not get it through the protoc
    %% lib.
    lists:foreach(fun(CodePoint) ->
                          OrigMsg = {strmsg, [CodePoint]},
                          %% to avoid errors in nif encode/decode
                          %% cancelling out each other and nif bugs go
                          %% undetected, cross-check with gpb:encode/decode_msg
                          MEncoded  = NEDM:encode_msg(OrigMsg),
                          GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                          MMDecoded = NEDM:decode_msg(MEncoded, strmsg),
                          GMDecoded = gpb:decode_msg(MEncoded, strmsg, Defs),
                          MGDecoded = NEDM:decode_msg(GEncoded, strmsg),
                          ?assertEqual(OrigMsg, MMDecoded),
                          ?assertEqual(OrigMsg, GMDecoded),
                          ?assertEqual(OrigMsg, MGDecoded)
                  end,
                  CodePoints).

nif_encode_decode_oneof(features) -> [oneof];
nif_encode_decode_oneof(title) -> "Encode decode oneof".
nif_encode_decode_oneof() ->
    with_tmpdir(
      fun(TmpDir) ->
              NEDM = gpb_nif_test_ed_oneof1,
              Defs = mk_one_oneof_field_of_each_type(),
              {ok, Code} = compile_nif_msg_defs(NEDM, Defs, TmpDir),
              in_separate_vm(
                TmpDir, NEDM, Code,
                fun() ->
                        nif_encode_decode_oneof(NEDM, Defs),
                        ok
                end)
      end).


nif_encode_decode_oneof(NEDM, Defs) ->
    [#gpb_oneof{fields=OFields}] = [O || {{msg, oneof1}, [O]} <- Defs],
    Alts = [{Name, mk_field_value(OF, Defs, small)}
            || #?gpb_field{name=Name}=OF <- OFields] ++ [undefined],
    lists:foreach(fun(Alt) ->
                          OrigMsg = {oneof1, Alt},
                          %% to avoid errors in nif encode/decode
                          %% cancelling out each other and nif bugs go
                          %% undetected, cross-check with gpb:encode/decode_msg
                          MEncoded  = NEDM:encode_msg(OrigMsg),
                          GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                          MMDecoded = NEDM:decode_msg(MEncoded, oneof1),
                          GMDecoded = gpb:decode_msg(MEncoded, oneof1, Defs),
                          MGDecoded = NEDM:decode_msg(GEncoded, oneof1),
                          ?assertEqual(OrigMsg, MMDecoded),
                          ?assertEqual(OrigMsg, GMDecoded),
                          ?assertEqual(OrigMsg, MGDecoded)
                  end,
                  Alts).

nif_encode_decode_mapfields(features) -> [mapfields];
nif_encode_decode_mapfields(title) -> "Encode decode map<_,_> fields".
nif_encode_decode_mapfields() ->
    with_tmpdir(
      fun(TmpDir) ->
              NEDM = gpb_nif_test_ed_mapfields1,
              Defs = mk_one_map_field_of_each_type(),
              {ok, Code} = compile_nif_msg_defs(NEDM, Defs, TmpDir),
              in_separate_vm(
                TmpDir, NEDM, Code,
                fun() ->
                        nif_encode_decode_mapfields(NEDM, Defs),
                        ok
                end)
      end).

nif_encode_decode_proto3(features) -> [proto3, oneof];
nif_encode_decode_proto3(title) -> "Encode decode proto3".
nif_encode_decode_proto3() ->
    with_tmpdir(
      fun(TmpDir) ->
              NEDM = gpb_nif_test_ed_mapfields1,
              Defs = mk_proto3_fields(),
              {ok, Code} = compile_nif_msg_defs(NEDM, Defs, TmpDir),
              in_separate_vm(
                TmpDir, NEDM, Code,
                fun() ->
                        nif_encode_decode_test_it(NEDM, Defs),
                        ok
                end)
      end).

nif_encode_decode_mapfields(NEDM, Defs) ->
    OrigMsg = usort_all_fields(mk_msg(map1, Defs, small_random)),
    %% cross-check with gpb:encode/decode_msg to avoid errors cancelling out
    MEncoded  = NEDM:encode_msg(OrigMsg),
    GEncoded  = gpb:encode_msg(OrigMsg, Defs),
    MMDecoded = NEDM:decode_msg(MEncoded, map1),
    GMDecoded = gpb:decode_msg(MEncoded, map1, Defs),
    MGDecoded = NEDM:decode_msg(GEncoded, map1),
    ?assertEqual(OrigMsg, sort_all_fields(MMDecoded)),
    ?assertEqual(OrigMsg, sort_all_fields(GMDecoded)),
    ?assertEqual(OrigMsg, sort_all_fields(MGDecoded)).

usort_all_fields(R) -> map_all_fields(R, fun usort_by_mapkey/1).

sort_all_fields(R) -> map_all_fields(R, fun lists:sort/1).

usort_by_mapkey(L) ->
    lists:sort(key_unique(L)).

key_unique([{K,V} | Rest]) ->
    [{K,V} | key_unique([X2 || {K2,_}=X2 <- Rest, K2 =/= K])];
key_unique([]) ->
    [].

map_all_fields(R, Fn) ->
    [RName | Fields] = tuple_to_list(R),
    list_to_tuple([RName | [Fn(Field) || Field <- Fields]]).

nif_enum_in_msg(features) -> [];
nif_enum_in_msg(title) -> "Enums in messages".
nif_enum_in_msg() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_enum_in_msgs,
              DefsTxt = lf_lines(["message ntest1 {",
                                  "    enum bo {",
                                  "        x = 1;",
                                  "        y = 2;",
                                  "    };",
                                  "    optional bo f1 = 1;",
                                  "    repeated bo f2 = 2;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {ntest1,x,[x,y]},
                        MEncoded  = M:encode_msg(OrigMsg),
                        GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                        MMDecoded = M:decode_msg(MEncoded, ntest1),
                        GMDecoded = gpb:decode_msg(MEncoded, ntest1, Defs),
                        MGDecoded = M:decode_msg(GEncoded, ntest1),
                        ?assertEqual(OrigMsg, MMDecoded),
                        ?assertEqual(OrigMsg, GMDecoded),
                        ?assertEqual(OrigMsg, MGDecoded)
                end)
      end).

nif_enum_with_pkgs(features) -> [];
nif_enum_with_pkgs(title) -> "Enums with pkgs".
nif_enum_with_pkgs() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_enum_with_pkgs,
              DefsTxt = lf_lines(["package p1.p2;",
                                  "    enum ee {",
                                  "        ee1 = 1;",
                                  "        ee2 = 2;",
                                  "    };",
                                  "message ntest2 {",
                                  "    optional ee f1 = 1;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {ntest2,ee1},
                        MEncoded  = M:encode_msg(OrigMsg),
                        GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                        MMDecoded = M:decode_msg(MEncoded, ntest2),
                        GMDecoded = gpb:decode_msg(MEncoded, ntest2, Defs),
                        MGDecoded = M:decode_msg(GEncoded, ntest2),
                        ?assertEqual(OrigMsg, MMDecoded),
                        ?assertEqual(OrigMsg, GMDecoded),
                        ?assertEqual(OrigMsg, MGDecoded)
                end)
      end).

nif_enum_from_integers(features) -> [];
nif_enum_from_integers(title) -> "Enums from integers".
nif_enum_from_integers() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_enum_from_integers,
              DefsTxt = lf_lines(["enum e {",
                                  "    e0 = 0;",
                                  "    e1 = 1;",
                                  "};",
                                  "message ntest3 {",
                                  "    optional e f1 = 1;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {ntest3,1},
                        ExpectedEncoded = <<8,1>>,
                        MEncoded  = M:encode_msg(OrigMsg),
                        GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                        ?assertEqual(ExpectedEncoded, MEncoded),
                        ?assertEqual(ExpectedEncoded, GEncoded)
                end)
      end).

nif_aliased_enums(features) -> [allow_alias];
nif_aliased_enums(title) -> "Nif with aliased enums".
nif_aliased_enums() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_bypass_wrappers_records,
              DefsTxt = "message m1 {
                             required ee f = 1;
                         }
                         enum ee {
                           option allow_alias = true;
                           E0 = 0;
                           E1_A = 1;
                           E1_B = 1;
                         }
                        ",
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir, []),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg1 = {m1,'E0'},
                        Encoded1 = M:encode_msg(OrigMsg1),
                        OrigMsg1 = M:decode_msg(Encoded1, m1),

                        OrigMsg2 = {m1,'E1_A'},
                        Encoded2 = M:encode_msg(OrigMsg2),
                        OrigMsg2 = M:decode_msg(Encoded2, m1),

                        OrigMsg3 = {m1,'E1_B'},
                        Encoded3 = M:encode_msg(OrigMsg3),
                        {m1,'E1_A'} = M:decode_msg(Encoded3, m1)
                end)
      end).

nif_with_groups(features) -> [];
nif_with_groups(title) -> "Nif with groups".
nif_with_groups() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_with_groups,
              DefsTxt = lf_lines(["message m1 {",
                                  "    repeated group Rp = 10 {",
                                  "      required uint32 f = 11;",
                                  "    }",
                                  "    required group Rq = 20 {",
                                  "      required uint32 g = 21;",
                                  "    }",
                                  "    optional group O  = 30 {",
                                  "      required uint32 h = 31;",
                                  "    }",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {m1,
                                   [{'m1.Rp',111},{'m1.Rp',112}],
                                   {'m1.Rq',211},
                                   {'m1.O',311}},
                        MEncoded  = M:encode_msg(OrigMsg),
                        GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                        MMDecoded = M:decode_msg(MEncoded, m1),
                        GMDecoded = gpb:decode_msg(MEncoded, m1, Defs),
                        MGDecoded = M:decode_msg(GEncoded, m1),
                        ?assertEqual(OrigMsg, MMDecoded),
                        ?assertEqual(OrigMsg, GMDecoded),
                        ?assertEqual(OrigMsg, MGDecoded)
                end)
      end).

nif_with_strbin(features) -> [];
nif_with_strbin(title) -> "Nif with strbin".
nif_with_strbin() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_with_strbin,
              DefsTxt = lf_lines(["message ntest2 {",
                                  "    required string s = 1;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir,
                                                [strings_as_binaries]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsgB = {ntest2,<<"abc">>},
                        OrigMsgS = {ntest2,"abc"}, %% gpb can't do strbin
                        MEncoded  = M:encode_msg(OrigMsgB),
                        GEncoded  = gpb:encode_msg(OrigMsgB, Defs),
                        MMDecoded = M:decode_msg(MEncoded, ntest2),
                        GMDecoded = gpb:decode_msg(MEncoded, ntest2, Defs),
                        MGDecoded = M:decode_msg(GEncoded, ntest2),
                        ?assertEqual(OrigMsgB, MMDecoded),
                        ?assertEqual(OrigMsgS, GMDecoded),
                        ?assertEqual(OrigMsgB, MGDecoded)
                end)
      end).

nif_with_booleans(features) -> [];
nif_with_booleans(title) -> "Nif with booleans".
nif_with_booleans() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_with_booleans,
              DefsTxt = lf_lines(["message ntest3 {",
                                  "    required bool b = 1;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir, []),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsgInt = {ntest3,1},
                        OrigMsgAtom = {ntest3,true},
                        MEncoded  = M:encode_msg(OrigMsgInt),
                        GEncoded  = gpb:encode_msg(OrigMsgInt, Defs),
                        MMDecoded = M:decode_msg(MEncoded, ntest3),
                        GMDecoded = gpb:decode_msg(MEncoded, ntest3, Defs),
                        MGDecoded = M:decode_msg(GEncoded, ntest3),
                        ?assertEqual(OrigMsgAtom, MMDecoded),
                        ?assertEqual(OrigMsgAtom, GMDecoded),
                        ?assertEqual(OrigMsgAtom, MGDecoded)
                end)
      end).


nif_with_cxx_keywords(features) -> [oneof, cxx_keywords];
nif_with_cxx_keywords(title) -> "Handling of C++ keywords".
nif_with_cxx_keywords() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_with_cxx_keywords,
              DefsTxt = lf_lines(["enum E {",
                                  "  new = 0;",
                                  "  Delete = 1;",
                                  "}",
                                  "message ntestc {",
                                  "  optional E f1 = 1;"
                                  "  optional uint32 Private = 2;",
                                  "  oneof Union {",
                                  "    uint32 protected = 3;",
                                  "    uint32 Public = 4;",
                                  "  }",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir, []),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {ntestc,'Delete',2,{'Public',3}},
                        MEncoded  = M:encode_msg(OrigMsg),
                        GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                        MMDecoded = M:decode_msg(MEncoded, ntestc),
                        GMDecoded = gpb:decode_msg(MEncoded, ntestc, Defs),
                        MGDecoded = M:decode_msg(GEncoded, ntestc),
                        ?assertEqual(OrigMsg, MMDecoded),
                        ?assertEqual(OrigMsg, GMDecoded),
                        ?assertEqual(OrigMsg, MGDecoded)
                end)
      end).


nif_with_list_indata_for_bytes(features) -> [];
nif_with_list_indata_for_bytes(title) -> "Nif with list indata for bytes".
nif_with_list_indata_for_bytes() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_with_list_indata_for_bytes,
              DefsTxt = lf_lines(["message ntest5 {",
                                  "    required bytes s = 1;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsgList = {ntest5,[4,3,2,1]},
                        OrigMsgBin = {ntest5,<<4,3,2,1>>},
                        MEncoded  = M:encode_msg(OrigMsgList),
                        GEncoded  = gpb:encode_msg(OrigMsgList, Defs),
                        MMDecoded = M:decode_msg(MEncoded, ntest5),
                        GMDecoded = gpb:decode_msg(MEncoded, ntest5, Defs),
                        MGDecoded = M:decode_msg(GEncoded, ntest5),
                        ?assertEqual(OrigMsgBin, MMDecoded),
                        ?assertEqual(OrigMsgBin, GMDecoded),
                        ?assertEqual(OrigMsgBin, MGDecoded)
                end)
      end).


nif_with_non_normal_floats(features) -> [];
nif_with_non_normal_floats(title) -> "Nif and +-Inf/NaN".
nif_with_non_normal_floats() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_with_non_normal_floats,
              DefsTxt = lf_lines(["message nnf1 {",
                                  "    required float f = 1;",
                                  "    required double d = 2;",
                                  "}"]),
              Defs = parse_to_proto_defs(DefsTxt),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir,
                                                [strings_as_binaries]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        [begin
                             OrigMsg = {nnf1,Item,Item},
                             MEncoded  = M:encode_msg(OrigMsg),
                             GEncoded  = gpb:encode_msg(OrigMsg, Defs),
                             MMDecoded = M:decode_msg(MEncoded, nnf1),
                             GMDecoded = gpb:decode_msg(MEncoded, nnf1, Defs),
                             MGDecoded = M:decode_msg(GEncoded, nnf1),
                             ?assertEqual(OrigMsg, MMDecoded),
                             ?assertEqual(OrigMsg, GMDecoded),
                             ?assertEqual(OrigMsg, MGDecoded)
                         end
                         || Item <- [infinity, '-infinity', nan]]
                end)
      end).

error_if_both_translations_and_nif(features) -> [];
error_if_both_translations_and_nif(title) -> "Error if both Any translations "
                                                 ++ "and nif".
error_if_both_translations_and_nif() ->
    %% This is expected to fail, already at option verification, ie
    %% not produce any files at all, but should it accidentally
    %% succeed (due to a bug or so), it is useful to have it included
    %% under the ordinary nif handling umbrella.
    with_tmpdir(
      fun(_TmpDir) ->
              DefsTxt = lf_lines(["message ntest3 {",
                                  "    required string s = 1;",
                                  "}"]),
              Opts = [nif,
                      {any_translate,[{encode,{m,e,['$1']}},
                                      {decode,{m,d,['$1']}},
                                      {merge,{m,m,['$1','$2']}},
                                      {verify,{m,v,['$1','$errorf']}}]}],
              {{return,{error, _}},
               {output,Output1}} =
                  compile_file_get_output(DefsTxt, Opts),
              true = gpb_lib:is_substr("nif", Output1),
              true = gpb_lib:is_substr("translat", Output1),

              {{return,{error, _, []}},
               {output,""}} =
                  compile_file_get_output(DefsTxt, Opts ++ [return]),

              ok
      end).

bypass_wrappers_records_nif(features) -> [];
bypass_wrappers_records_nif(title) -> "Nif with the bypass_wrappers option".
bypass_wrappers_records_nif() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_bypass_wrappers_records,
              DefsTxt = lf_lines(["message bpw1 {",
                                  "    required uint32 f = 1;",
                                  "}"]),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir,
                                                [bypass_wrappers]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {bpw1,4711},
                        Encoded = M:encode_msg_bpw1(OrigMsg),
                        OrigMsg = M:decode_msg_bpw1(Encoded)
                end)
      end).

nif_with_packages_and_enums_protos() ->
    [{"gpb_pkg_a.proto",
      "
        syntax='proto2';
        import 'gpb_pkg_b.proto';
        package pkg.a;
        message MsgA {
          required pkg.b.MsgB submsg = 1;
          required ee1        e1 = 2;
          required ee2        e2 = 3;
          enum ee2 { ba = 0; bb = 1; }
        }
        enum ee1 { aa = 0; ab = 1; }
      "},
     {"gpb_pkg_b.proto",
      "
        syntax='proto2';
        package pkg.b;
        message MsgB { required uint32 f = 1; }
      "}].

nif_with_packages_and_enums(features) -> [];
nif_with_packages_and_enums(title) -> "Nif with packages and enums".
nif_with_packages_and_enums() ->
    with_tmpdir(
      fun(TmpDir) ->
              ProtoTexts = nif_with_packages_and_enums_protos(),
              M = gpb_pkg_a, % must match first ProtoText
              {ok, Code} = compile_nif_several_msg_defs(M, ProtoTexts, TmpDir,
                                                        [use_packages]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {'pkg.a.MsgA', {'pkg.b.MsgB', 17}, ab, ba},
                        Encoded = M:encode_msg(OrigMsg),
                        OrigMsg = M:decode_msg(Encoded, 'pkg.a.MsgA')
                end)
      end).

nif_with_renamings(features) -> [];
nif_with_renamings(title) -> "Nif with renamings".
nif_with_renamings() ->
    with_tmpdir(
      fun(TmpDir) ->
              ProtoTexts = nif_with_packages_and_enums_protos(),
              M = gpb_pkg_a, % must match first ProtoText
              RenamingOpts = [{rename, {msg_fqname, base_name}},
                              {rename, {msg_fqname, snake_case}}],
              {ok, Code} = compile_nif_several_msg_defs(
                             M, ProtoTexts, TmpDir,
                             [use_packages] ++ RenamingOpts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {msg_a, {msg_b, 17}, ab, ba},
                        Encoded = M:encode_msg(OrigMsg),
                        OrigMsg = M:decode_msg(Encoded, msg_a)
                end)
      end).

nif_with_opt_but_no_package(features) -> [];
nif_with_opt_but_no_package(title) -> "Nif with use_package option but no pkg".
nif_with_opt_but_no_package() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_opt_but_no_pkgs,
              DefsTxt = lf_lines(["message m1 {",
                                  "    required uint32 f = 1;",
                                  "}"]),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir,
                                                [use_packages]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {m1,4711},
                        Encoded = M:encode_msg(OrigMsg),
                        OrigMsg = M:decode_msg(Encoded, m1)
                end)
      end).

nif_without_mergers(features) -> [];
nif_without_mergers(title) -> "Nif with no mergers".
nif_without_mergers() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_no_mergers,
              DefsTxt = lf_lines(["message m1 {",
                                  "    required uint32 f = 1;",
                                  "}"]),
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir,
                                                [{gen_mergers,false},nif]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = {m1,4711},
                        Encoded = M:encode_msg(OrigMsg),
                        OrigMsg = M:decode_msg(Encoded, m1),
                        ?assertError(undef, M:merge_msgs(OrigMsg, OrigMsg))
                end)
      end).

compile_nif_msg_defs(M, MsgDefsOrIoList, TmpDir) ->
    compile_nif_msg_defs(M, MsgDefsOrIoList, TmpDir, []).

compile_nif_msg_defs(M, MsgDefsOrIoList, TmpDir, Opts) ->
    {MsgDefs, ProtoTxt} =
        case is_iolist(MsgDefsOrIoList) of
            true -> {parse_to_proto_defs(MsgDefsOrIoList,Opts), MsgDefsOrIoList};
            false -> {MsgDefsOrIoList, msg_defs_to_proto(MsgDefsOrIoList)}
        end,
    ProtoFile = lists:concat([M, ".proto"]),
    compile_nif_several_msg_defs_aux(M, [{ProtoFile, ProtoTxt}], MsgDefs,
                                     TmpDir, Opts).

compile_nif_several_msg_defs(M, [{Proto1,Text1}|_]=ProtoTexts, TmpDir, Opts) ->
    ?assertEqual(Proto1, atom_to_list(M)++".proto"),
    FNames = [FName || {FName, _Text} <- ProtoTexts],
    ImportFetcher = fun(FName) ->
                            case lists:keyfind(FName, 1, ProtoTexts) of
                                {_Base, Text} ->
                                    {ok, Text};
                                false ->
                                    {error, {enoent, {FName, FNames}}}
                            end
                    end,
    MoreOpts = [to_proto_defs, report_warnings,
                {import_fetcher, ImportFetcher}],
    AllOpts = Opts ++ MoreOpts,
    AllOpts2 = strip_renaming_opts(AllOpts),
    {ok, MsgDefs} = gpb_compile:string(M, Text1, AllOpts2),
    compile_nif_several_msg_defs_aux(M, ProtoTexts, MsgDefs, TmpDir, Opts).

compile_nif_several_msg_defs_aux(M, ProtoTexts, MsgDefs, TmpDir, Opts) ->
    ProtoPaths = [filename:join(TmpDir, Proto) || {Proto, _Txt} <- ProtoTexts],
    PbCcPaths = [change_ext(Path, ".proto", ".pb.cc") || Path <- ProtoPaths],
    PbOPaths  = [change_ext(Path, ".proto", ".pb.o") || Path <- ProtoPaths],
    [NifCcPath, NifOPath, NifSoPath] = NifFiles =
        [filename:join(TmpDir, lists:concat([M, Ext]))
         || Ext <- [".nif.cc", ".nif.o", ".nif.so"]],
    LoadNif = f("load_nif() -> erlang:load_nif(\"~s\", {{loadinfo}}).\n",
                [filename:join(TmpDir, lists:concat([M,".nif"]))]),
    LoadNifOpt = {load_nif, LoadNif},
    Opts2 = [binary, nif, LoadNifOpt] ++ Opts,
    {ok, Renamings} = gpb_names:compute_renamings(MsgDefs, Opts2),
    MsgDefs2 = gpb_names:apply_renamings(MsgDefs, Renamings),
    {ok, M, Codes} = gpb_compile:proto_defs(M, MsgDefs2, MsgDefs, Renamings,
                                            Opts2),
    Code = proplists:get_value(erl, Codes),
    NifTxt = proplists:get_value(nif, Codes),
    %%
    ok = file:write_file(NifCcPath, NifTxt),
    [ok = file:write_file(ProtoPath, ProtoTxt)
     || {ProtoPath, {_Base, ProtoTxt}} <- lists:zip(ProtoPaths, ProtoTexts)],
    %%
    CC = find_cplusplus_compiler(),
    Protoc = find_protoc(),
    CFlags = get_cflags(),
    LdFlags = get_ldflags(),
    CompileProtos = [f("'~s' --proto_path '~s' --cpp_out='~s' '~s'",
                       [Protoc, TmpDir, TmpDir, ProtoPath])
                     || ProtoPath <- ProtoPaths],
    CompileNif = f("'~s' -g -fPIC -Wall -O0 '-I~s' ~s -c -o '~s' '~s'",
                   [CC, TmpDir, CFlags, NifOPath, NifCcPath]),
    CompilePbs = [f("'~s' -g -fPIC -Wall -O0 '-I~s' ~s -c -o '~s' '~s'",
                    [CC, TmpDir, CFlags, PbOPath, PbCcPath])
                  || {PbOPath, PbCcPath} <- lists:zip(PbOPaths, PbCcPaths)],
    CompileSo = f("'~s' -g -fPIC -shared -Wall -O0 ~s"
                  "    -o '~s' '~s' ~s -lprotobuf",
                  [CC, LdFlags, NifSoPath, NifOPath,
                   [[" '", PbOPath, "'"] || PbOPath <- PbOPaths]]),
    CompileLines =
        lists:append(
          [[CompileProto || CompileProto <- CompileProtos],
           [CompileNif],
           [CompilePb || CompilePb <- CompilePbs],
           [CompileSo]]),
    %% Useful if debugging the nif code, see also with_tmpdir(save, Fun)
    Files = PbCcPaths ++ PbOPaths ++ NifFiles,
    ToClean = [filename:basename(F) || F <- Files,
                                       not lists:member(F, ProtoPaths)],
    file:write_file(
      filename:join(TmpDir, "Makefile"),
      iolist_to_binary(
        ["all:\n",
         [["\t", Line, "\n"] || Line <- CompileLines],
         "\n",
         "clean:\n",
         "\t", "$(RM)", [[" ",F] || F <- ToClean], "\n"])),
    ok = ccompile("~s", [["set -evx\n", gpb_lib:nl_join(CompileLines)]]),
    {ok, Code}.

lf_lines(Lines) ->
    [[L,"\n"] || L <- Lines].

is_iolist(X) ->
    try iolist_to_binary(X), true
    catch error:badarg -> false
    end.

change_ext(Path, OldExt, NewExt) ->
    filename:join(filename:dirname(Path),
                  filename:basename(Path, OldExt) ++ NewExt).

parse_to_proto_defs(Iolist) ->
    parse_to_proto_defs(Iolist, []).

parse_to_proto_defs(Iolist, Opts) ->
    Opts2 = strip_renaming_opts(Opts),
    B = iolist_to_binary(Iolist),
    {ok, ProtoDefs} = gpb_compile:file(
                        "X.proto",
                        [mk_fileop_opt([{read_file, fun(_) -> {ok, B} end}]),
                         {i,"."},
                         to_proto_defs, report_warnings] ++ Opts2),
    ProtoDefs.

strip_renaming_opts(Opts) ->
    lists:filter(fun gpb_names:is_not_renaming_opt/1, Opts).

%% Option to run with `save' for debugging nifs
with_tmpdir(Fun) ->
    with_tmpdir(dont_save, Fun).
with_tmpdir(Save, Fun) ->
    {ok, TmpDir} = get_tmpdir(),
    try Fun(TmpDir)
    after
        case Save of
            dont_save -> clean_tmpdir(TmpDir);
            save -> io:format(user, "~nSaved dir ~p~n", [TmpDir])
        end
    end.

get_tmpdir() ->
    rand_seed(),
    mktempdir(
      filename:join(case os:getenv("TMPDIR") of
                        false -> "/tmp";
                        TDir  -> TDir
                    end,
                    lists:concat([?MODULE,"-",os:getenv("LOGNAME"),"-",
                                  os:getpid(),"-"]))).

mktempdir(Base) ->
    D = Base ++ f("~8..0w", [rand_uniform(90000000)]),
    case file:make_dir(D) of
        ok             -> {ok, D};
        {error, exist} -> mktempdir(Base);
        Error          -> Error
    end.

clean_tmpdir(TmpDir) ->
    os:cmd(f("/bin/rm -rf '~s'", [TmpDir])).

in_separate_vm(TmpDir, Module, Code, Fun) ->
    %% With nifs in Erlang, one cannot control unloading of the nif,
    %% and thus cannot control when unloading of the protobuf library
    %% happens. I've seen cases where rapid unloading and reloading of
    %% nifs with new proto defs cause segvs (in strcmp), and from the
    %% stack trace, it looks like the protobuf library is unloading at
    %% the time when (or probably while) the next nif was being
    %% loaded. Since the loaded/linked protobuf dynamic library is a
    %% shared resource, if unloading and (re)loading happens in
    %% different threads, then there might be trouble.
    %%
    %% Therefore, to make the eunit tests stable, we run them in a
    %% separate vm.
    TmpCodeWrapperFun = fun() ->
                                try
                                    load_code(Module, Code),
                                    Fun()
                                after
                                    unload_code(Module)
                                end
                        end,
    FBin = term_to_binary(TmpCodeWrapperFun),
    FBinFile = filename:join(TmpDir, "fun-to-run"),
    FResFile = filename:join(TmpDir, "fun-run-result"),
    ok = file:write_file(FBinFile, FBin),
    DirOfThisBeam = filename:dirname(code:which(?MODULE)),
    GpbEbin = filename:dirname(code:which(gpb)),
    ModuleS = atom_to_list(?MODULE),
    CmdResult = run_cmd_collect_output(
                  "erl",
                  ["+B","-noinput",
                   "-boot","start_clean",
                   "-sasl","errlog_type","true",
                   "-pa",TmpDir,
                   "-pa",DirOfThisBeam,
                   "-pa",GpbEbin,
                   "-run",ModuleS,"main_in_separate_vm", FBinFile,FResFile]),
    analyze_output_from_separate_vm(CmdResult, FResFile).

main_in_separate_vm([FBinFile, FResFile]) ->
    {ok, FBin} = file:read_file(FBinFile),
    Fun = binary_to_term(FBin),
    Res = try Fun()
          catch ?STACKTRACE(Class,Reason,Stack) % ->
                  {'EXIT',{Class,Reason,Stack}}
          end,
    ResBin = term_to_binary(Res),
    WRes = file:write_file(FResFile, ResBin),
    io:format("Wrote result file (~p bytes) -> ~p~n", [byte_size(ResBin),WRes]),
    ensure_output_flushed_halt().

ensure_output_flushed_halt() ->
    case erlang:system_info(otp_release) of
        "R"++_ = Release ->
            %% Erlang R16 or earlier, attempt to support earlier releases
            %% if not too much work.
            if Release >= "R15B01" ->
                    %% R15B01 and later: halt waits until pending io has finished
                    halt(0);
               Release < "R15B01" ->
                    timer:sleep(100),
                    halt(0)
            end;
        _ ->
            %% Erlang 17 or later
            halt(0)
    end.

run_cmd_collect_output(Cmd, Args) ->
    case os:find_executable(Cmd) of
        false ->
            error({could_not_find_cmd,Cmd});
        CmdPath ->
            Port = erlang:open_port({spawn_executable,CmdPath},
                                    [use_stdio, stderr_to_stdout, binary,
                                     exit_status, {args,Args}]),
            collect_output(Port, [])
    end.

collect_output(Port, Acc) ->
    receive
        {Port, {data, Txt}} ->
            collect_output(Port, [Txt | Acc]);
        {Port, {exit_status,ExitCode}} ->
            {ExitCode, iolist_to_binary(lists:reverse(Acc))}
    end.

analyze_output_from_separate_vm({ExitCode, Output}, ResultFile) ->
    case file:read_file(ResultFile) of
        {ok, B} ->
            case binary_to_term(B) of
                {'EXIT',{Class,Reason,StackTrace}} ->
                    erlang:raise(Class, {in_separate_vm,Reason}, StackTrace);
                _Result ->
                    %% Anything not a crash is success, just like usual
                    ok
            end;
        {error, Reason} ->
            ?debugFmt("~nNo result from separate vm, output=~n~s~n", [Output]),
            error({no_result_file_from_separate_vm,{ResultFile,Reason},
                   {exit_code,ExitCode},
                   {execution_output,Output}})
    end.

test_nifs(Boolean) when is_boolean(Boolean) ->
    os:putenv("GPB_NIF_TESTS", lists:concat([Boolean])).

want_nif_tests() ->
    %% It can be useful to disable nif testing.
    %% Previously, it was very desirable, as described below,
    %% but since the move to test nifs in a separate vm,
    %% this is no longer as compelling a reason. It might still
    %% be desirable, eg if the c++ or protoc is not set up.
    %%
    %% Previously, when the nif tests were executed
    %% in the same vm as the eunit tests, then due to the
    %% behavior in the libprotoc, that if it detects loading
    %% a proto definition with the same name as it already
    %% has loaded, it will refuse, and may stop the entire
    %% erlang-vm. See the documentation in gpb_compile:c/1,2,
    %% the `nif' option, for further details. I have seen it
    %% halt the entire erlang-vm when tests failed, which is
    %% a pity e.g. when the vm is in an interactive inferior
    %% emacs window
    case os:getenv("GPB_NIF_TESTS") of
        false   -> true; %% default is to test nifs
        "true"  -> true;
        "false" -> false
    end.

find_cplusplus_compiler() ->
    case os:getenv("CXX") of
        false ->
            case os:find_executable("g++") of
                false -> os:find_executable("c++");
                Gxx   -> Gxx
            end;
        CxxCompiler ->
            CxxCompiler
    end.

find_protoc() ->
    case os:getenv("PROTOC") of
        false  -> os:find_executable("protoc");
        Protoc -> Protoc
    end.

check_protoc_can_do_oneof() ->
    cachingly_check('$cached_check_protoc_can_do_oneof',
                    %% oneof appeared in 2.6.0
                    fun() -> check_protoc_version_is_at_least([2,6]) end).

check_protoc_can_do_mapfields() ->
    cachingly_check('$cached_check_protoc_can_do_mapfields',
                    %% map<_,_> appeared in 3.0.0
                    fun() -> check_protoc_version_is_at_least([3,0]) end).

check_protoc_can_do_proto3() ->
    cachingly_check('$cached_check_protoc_can_do_proto3',
                    %% proto3 appeared in 3.0.0 :)
                    fun() -> check_protoc_version_is_at_least([3,0]) end).

check_protoc_can_do_json() ->
    cachingly_check('$cached_check_protoc_can_do_proto3',
                    %% proto3 appeared in 3.0.0 :)
                    fun() -> check_protoc_version_is_at_least([3,0]) end).

check_protoc_version_is_at_least(MinVsn) ->
    case cachingly_find_protoc_version() of
        {ok, Vsn} -> Vsn >= MinVsn;
        {error,_} -> false
    end.

cachingly_find_protoc_version() ->
    cachingly_check('$cached_protoc_version', fun find_protoc_version/0).

cachingly_check(CacheKey, F) ->
    case get(CacheKey) of
        undefined ->
            CanIt = F(),
            put(CacheKey, CanIt),
            CanIt;
        CanIt ->
            CanIt
    end.

find_protoc_version() ->
    Output = os:cmd(find_protoc() ++ " --version"),
    Words = gpb_lib:string_lexemes(Output, " \t\r\n"),
    case find_protoc_version_aux(Words, Output) of
        {ok, _}=Res -> Res;
        {error, X}=Res ->
            ?debugFmt("Trouble finding protoc version in ~s~n", [X]),
            Res
    end.

find_protoc_version_aux(["libprotoc", VersionStr | _], All) ->
    Components = gpb_lib:string_lexemes(VersionStr, "."),
    try {ok, [list_to_integer(X) || X <- Components]}
    catch error:badarg -> {error, {failed_to_interpret, VersionStr, All}}
    end;
find_protoc_version_aux([_ | Rest], All) ->
    find_protoc_version_aux(Rest, All);
find_protoc_version_aux([], All) ->
    {error, {no_version_string_found, All}}.

get_cflags() ->
    Root = code:root_dir(), %% e.g. /usr/lib/erlang
    CIncDir = filename:join([Root, "usr", "include"]),
    case os:getenv("CFLAGS") of
        false  -> "";
        CFlags -> CFlags
    end ++ case os:getenv("CXXFLAGS") of
               false    -> "";
               CxxFlags -> CxxFlags
           end ++ " " ++ f("-I'~s'", [CIncDir]).

platform_ldflags({unix, darwin}) ->
  " -undefined dynamic_lookup -dynamiclib";
platform_ldflags(_) ->
  "".

get_ldflags() ->
    case os:getenv("LDFLAGS") of
        false   -> "";
        LdFlags -> LdFlags
    end ++ platform_ldflags(os:type()).

msg_defs_to_proto(MsgDefs) ->
    iolist_to_binary(
      [maybe_syntaxdef(MsgDefs),
       lists:map(fun(M) -> msg_def_to_proto(M, MsgDefs) end, MsgDefs)]).

maybe_syntaxdef(MsgDefs) ->
    case proplists:get_value(syntax, MsgDefs) of
        undefined ->
            case contains_any_maptype_field(MsgDefs) of
                true  -> "syntax = \"proto2\";\n";
                false -> ""
            end;
        Syntax ->
            f("syntax = \"~s\";\n", [Syntax])
    end.

contains_any_maptype_field(MsgDefs) ->
    lists:any(fun(Fields) ->
                      lists:any(fun(#?gpb_field{type={map,_,_}}) -> true;
                                   (_) -> false
                                end,
                                Fields)
              end,
              [Fields || {{msg,_}, Fields} <- MsgDefs]).

msg_def_to_proto({{enum, Name}, EnumValues}, _MsgDefs) ->
    f("enum ~s {~n~s}~n~n",
      [Name, lists:map(fun format_enumerator/1, EnumValues)]);
msg_def_to_proto({{msg, Name}, Fields}, MsgDefs) ->
    IsProto3 = gpb:is_msg_proto3(Name, MsgDefs),
    f("message ~s {~n~s}~n~n",
      [Name, lists:map(fun(F) -> format_field(F, IsProto3) end, Fields)]);
msg_def_to_proto(_OtherElem, _MsgDefs) ->
    "".


format_enumerator({N,V}) ->
    f("  ~s = ~w;~n", [N, V]).

format_field(#?gpb_field{name=FName, fnum=FNum, type=Type,
                         occurrence=Occurrence},
             IsProto3) ->
    OccurrenceTxt = if Occurrence == repeated -> repeated;
                       IsProto3               -> "";
                       true                   -> Occurrence
                    end,
    case Type of
        {map,_,_} ->
            f("  ~s ~s = ~w;~n", [format_type(Type), FName, FNum]);
        _ ->
            f("  ~s ~s ~s = ~w;~n",
              [OccurrenceTxt, format_type(Type), FName, FNum])
    end;
format_field(#gpb_oneof{name=FName, fields=Fields}, _IsProto3) ->
    f("  oneof ~s {~n"
      "~s"
      "  };~n",
      [FName,
       [f("    ~s ~s = ~w;~n", [format_type(Type), OFName, FNum])
        || #?gpb_field{name=OFName, fnum=FNum, type=Type} <- Fields]]).

format_type({msg,Name})  -> Name;
format_type({enum,Name}) -> Name;
format_type({map,KeyType,ValueType}) ->
    f("map<~s,~s>", [format_type(KeyType), format_type(ValueType)]);
format_type(Type) ->
    Type.

ccompile(F, A) ->
    ccompile(F, A, verbose).

ccompile(F, A, ErrorVerbosity) ->
    Cmd = f(F, A),
    Output = os:cmd("LC_ALL=C; export LC_ALL; " ++ Cmd ++ "; echo $?\n"),
    [LastLine | _Rest] = lists:reverse(gpb_lib:string_lexemes(Output, "\r\n")),
    try list_to_integer(string_trim(LastLine)) of
        0 -> ok;
        _ -> case ErrorVerbosity of
                 verbose ->
                     ?debugFmt("Compilation failed!~nCmd=~p~nOutput:~n~ts~n~n",
                               [Cmd, Output]);
                 silent_on_compilation_error ->
                     ok
             end,
             {error, Output}
    catch error:badarg ->
            ?debugFmt("Compilation failed!~nCmd=~p~nOutput:~n~ts~n~n",
                      [Cmd, Output]),
            {error, Output}
    end.

mk_one_msg_field_of_each_type() ->
    EachType   = [sint32, sint64, int32, int64, uint32,
                  uint64, bool, fixed64, sfixed64,
                  double, string, bytes, fixed32, sfixed32,
                  float, {enum, ee}, {msg, submsg1}],
    EnumDef    = {{enum, ee}, [{en1, 1}, {en2, 2}]},
    SubMsgDef  = {{msg, submsg1}, mk_fields_of_type([uint32], required)},
    TopMsgDef1 = {{msg, topmsg1}, mk_fields_of_type(EachType, required)},
    TopMsgDef2 = {{msg, topmsg2}, mk_fields_of_type(EachType, repeated)},
    TopMsgDef3 = {{msg, topmsg3}, mk_fields_of_type(EachType, optional)},
    StringMsg = {{msg,strmsg}, mk_fields_of_type([string], required)},
    [EnumDef, SubMsgDef, TopMsgDef1, TopMsgDef2, TopMsgDef3, StringMsg].

mk_one_oneof_field_of_each_type() ->
    EachType   = [sint32, sint64, int32, int64, uint32,
                  uint64, bool, fixed64, sfixed64,
                  double, string, bytes, fixed32, sfixed32,
                  float, {enum, ee}, {msg, submsg1}],
    EnumDef    = {{enum, ee}, [{en1, 1}, {en2, 2}]},
    SubMsgDef  = {{msg, submsg1}, mk_fields_of_type([uint32], required)},
    OneofMsg1  = {{msg, oneof1},  mk_oneof_fields_of_type(EachType, 1)},
    [EnumDef, SubMsgDef, OneofMsg1].

mk_one_map_field_of_each_type() ->
    %% Reduced set of int types to shorten compilation times,
    %% while still cover all (most) code paths.
    ValueTypes = [sint32, sint64,
                  bool,
                  double, string, bytes,
                  float, {enum, ee}, {msg, submsg1}],
    ValueTypes2 = ValueTypes -- [sint64],
    KeyTypes   = [T || T <- ValueTypes, gpb:is_allowed_as_key_type(T)],
    %% Enum value in map must define 0 as the first value.
    EnumDef    = {{enum, ee}, [{en0, 0}, {en1, 1}, {en2, 2}]},
    SubMsgDef  = {{msg, submsg1}, mk_fields_of_type([uint32], required)},
    MapfldMsg1 = {{msg, map1},  mk_map_fields_of_type(KeyTypes, ValueTypes2)},
    [EnumDef, SubMsgDef, MapfldMsg1].

mk_proto3_fields() ->
    EachType   = [sint32, sint64, bool, double, string, bytes, {enum, ee}],
    MsgType    = {msg, submsg1},
    EnumDef    = {{enum, ee}, [{en0, 0}, {en1, 1}, {en2, 2}]},
    SubMsgDef  = {{msg, submsg1}, mk_fields_of_type([uint32], optional)},
    TopMsgDef1 = {{msg, topmsg1}, mk_fields_of_type(
                                    EachType ++ [MsgType],
                                    optional)},
    TopMsgDef2 = {{msg, topmsg2}, mk_fields_of_type(
                                    EachType ++ [MsgType],
                                    repeated,
                                    [{field_opts_f, fun maybe_packed/1}])},
    OneofMsg1  = {{msg, oneof1},  mk_oneof_fields_of_type([fixed32], 1)},
    [{syntax, "proto3"},
     {proto3_msgs, [topmsg1,topmsg2,oneof1,submsg1]},
     EnumDef, SubMsgDef, TopMsgDef1, TopMsgDef2, OneofMsg1].

mk_fields_of_type(Types, Occurrence) ->
    mk_fields_of_type(Types, Occurrence, []).

mk_fields_of_type(Types, Occurrence, Opts) ->
    FieldOptsF = proplists:get_value(field_opts_f, Opts, fun(_) -> [] end),
    Offset = proplists:get_value(offset, Opts, 0),
    Types1 = [Type || Type <- Types, can_do_nif_type(Type)],
    [#?gpb_field{name=list_to_atom(lists:concat([f, I + Offset])),
                 rnum=I + 1 + Offset,
                 fnum=I + Offset,
                 type=Type,
                 occurrence=Occurrence,
                 opts=FieldOptsF(Type)}
     || {I, Type} <- index_seq(Types1)].

mk_oneof_fields_of_type(Types, Pos) ->
    Types1 = [Type || Type <- Types, can_do_nif_type(Type)],
    [#gpb_oneof{
        name   = o,
        rnum   = Pos+1,
        fields = [#?gpb_field{name=list_to_atom(lists:concat([f,I])),
                              rnum=Pos+1,
                              fnum=I,
                              type=Type,
                              occurrence=optional,
                              opts=[]}
                  || {I, Type} <- index_seq(Types1)]}].

mk_map_fields_of_type(KeyTypes, ValueTypes) ->
    KeyTypes1 = [KT1 | _] = [T || T <- KeyTypes, can_do_nif_type(T)],
    ValueTypes1 = [VT1 | _] = [T || T <- ValueTypes, can_do_nif_type(T)],
    Fs1 = [#?gpb_field{type={map,KT1,VT}, occurrence=repeated, opts=[]}
           || VT <- ValueTypes1],
    Fs2 = [#?gpb_field{type={map,KT,VT1}, occurrence=repeated, opts=[]}
           || KT <- KeyTypes1],
    Fs3 = tl(Fs2), % avoid KT1,VT1 twice
    [F#?gpb_field{name=list_to_atom(lists:concat([f,I])), rnum=I+1, fnum=I}
     || {I, F} <- index_seq(Fs1 ++ Fs3)].

index_seq(L) -> lists:zip(lists:seq(1, length(L)), L).

maybe_packed({msg,_})   -> [];
maybe_packed({map,_,_}) -> [];
maybe_packed(string)    -> [];
maybe_packed(bytes)     -> [];
maybe_packed(_)         -> [packed].

can_do_nif_type(Type) ->
    if Type == int64;
       Type == sint64;
       Type == sfixed64 ->
            %% There's an issue with Erlang 17.0+ (will probably be
            %% fixed in 17.2): if compiled with gcc 4.9.0 (or newer, probably)
            %% and running on a 32-bit, there is an undefined behaviour
            %% which will make the test fail for nifs for sint64
            %% for INT64_MIN (-9223372036854775808). See also:
            %% http://erlang.org/pipermail/erlang-bugs/2014-July/004513.html
            case {is_erlvm_compiled_with_gcc490_or_later(), is_32_bit_os()} of
                {true, true} ->
                    OtpVsn = get_erlang_otp_major(),
                    if OtpVsn <  17 -> true;
                       OtpVsn == 17 -> false; % assume bug present
                       OtpVsn >  17 -> true   % assume fixed
                    end;
                _ ->
                    true
            end;
       true ->
            true
    end.

is_erlvm_compiled_with_gcc490_or_later() ->
    {Compiler, Version} = erlang:system_info(c_compiler_used),
    if Compiler == gnuc, is_tuple(Version) ->
            tuple_to_list(Version) >= [4,9,0];
       true ->
            undefined
    end.

is_32_bit_os() ->
    erlang:system_info({wordsize,external}) == 4. %% Erlang R14+

get_erlang_otp_major() ->
    case erlang:system_info(otp_release) of
        "R"++Rest -> % R16 or ealier
            list_to_integer(lists:takewhile(fun is_digit/1, Rest));
        RelStr ->
            %% In Erlang 17 the leading "R" was dropped,
            %% allow for some (possible?) variation
            try list_to_integer(RelStr)
            catch error:badarg ->
                    [NStr | _] = gpb_lib:string_lexemes(RelStr, ".-"),
                    try list_to_integer(NStr)
                    catch error:badarg -> error({unexpected_otp_version,RelStr})
                    end
            end
    end.

is_digit(C) when $0 =< C, C =< $9 -> true;
is_digit(_) -> false.

mk_msg(MsgName, Defs, Variant) ->
    {{msg, MsgName}, Fields} = lists:keyfind({msg, MsgName}, 1, Defs),
    R0 = erlang:make_tuple(length(Fields) + 1, undefined, [{1, MsgName}]),
    lists:foldl(fun(#?gpb_field{rnum=RNum}=Field, R) ->
                        Value = mk_field_value(Field, Defs, Variant),
                        setelement(RNum, R, Value);
                   (#gpb_oneof{rnum=RNum, fields=[OField1 | _]}, R) ->
                        #?gpb_field{name=Name} = OField1,
                        Value = mk_field_value(OField1, Defs, Variant),
                        setelement(RNum, R, {Name, Value})
                end,
                R0,
                Fields).

mk_field_value(#?gpb_field{occurrence=repeated}, _Defs, short) ->
    [];
mk_field_value(#?gpb_field{occurrence=repeated, type=T}=F, Defs, Variant) ->
    case T of
        {map, KeyType, ValueType} ->
            KF = F#?gpb_field{type=KeyType, occurrence=required},
            VF = F#?gpb_field{type=ValueType, occurrence=required},
            [begin
                 K = mk_field_value(KF, Defs, Variant),
                 V = mk_field_value(VF, Defs, Variant),
                 {K, V}
             end
             || _ <- lists:seq(1,10)];
        _ ->
            [mk_field_value(F#?gpb_field{occurrence=required}, Defs, Variant)]
    end;
mk_field_value(#?gpb_field{type=sint32}, _Defs, Vnt)   -> mk_sint(32, Vnt);
mk_field_value(#?gpb_field{type=sint64}, _Defs, Vnt)   -> mk_sint(64, Vnt);
mk_field_value(#?gpb_field{type=int32}, _Defs, Vnt)    -> mk_sint(32, Vnt);
mk_field_value(#?gpb_field{type=int64}, _Defs, Vnt)    -> mk_sint(64, Vnt);
mk_field_value(#?gpb_field{type=uint32}, _Defs, Vnt)   -> mk_uint(32, Vnt);
mk_field_value(#?gpb_field{type=uint64}, _Defs, Vnt)   -> mk_uint(64, Vnt);
mk_field_value(#?gpb_field{type=bool}, _Defs, Vnt)     -> mk_bool(Vnt);
mk_field_value(#?gpb_field{type=fixed64}, _Defs, Vnt)  -> mk_uint(64, Vnt);
mk_field_value(#?gpb_field{type=sfixed64}, _Defs, Vnt) -> mk_sint(64, Vnt);
mk_field_value(#?gpb_field{type=double}, _Defs, Vnt)   -> mk_float(64, Vnt);
mk_field_value(#?gpb_field{type=string}, _Defs, Vnt)   -> mk_string(Vnt);
mk_field_value(#?gpb_field{type=bytes}, _Defs, Vnt)    -> mk_bytes(Vnt);
mk_field_value(#?gpb_field{type=fixed32}, _Defs, Vnt)  -> mk_uint(32, Vnt);
mk_field_value(#?gpb_field{type=sfixed32}, _Defs, Vnt) -> mk_sint(32, Vnt);
mk_field_value(#?gpb_field{type=float}, _Defs, Vnt)    -> mk_float(32, Vnt);
mk_field_value(#?gpb_field{type={enum, E}}, Defs, Variant) ->
    {{enum, E}, [{E1 , _V1} | _Rest]=Es} = lists:keyfind({enum, E}, 1, Defs),
    case Variant of
        small_random ->
            element(1,random_nth(Es));
        _ ->
            E1
    end;
mk_field_value(#?gpb_field{type={msg, SubMsgName}}, Defs, Vnt) ->
    mk_msg(SubMsgName, Defs, Vnt).

mk_sint(32, small)        -> - (1 bsl 31);
mk_sint(32, big)          -> (1 bsl 31) - 1;
mk_sint(64, small)        -> - (1 bsl 63);
mk_sint(64, big)          -> (1 bsl 63) - 1;
mk_sint(_,  small_random) -> random_int(-100, 100);
mk_sint(_,  _)            -> 0.

mk_uint(32, big)          -> (1 bsl 32) - 1;
mk_uint(64, big)          -> (1 bsl 64) - 1;
mk_uint(_,  small_random) -> random_int(0, 100);
mk_uint(_,  _)            -> 0.

mk_bool(small)        -> false;
mk_bool(small_random) -> case random_int(0,1) of
                             0 -> false;
                             1 -> true
                         end;
mk_bool(_)            -> true.

mk_string(short)        -> "";
mk_string(big)          -> [16#10ffff];
mk_string(small_random) -> [random_int($a, $z) || _ <- lists:seq(1,10)];
mk_string(_)            -> "a".

mk_bytes(short)        -> <<>>;
mk_bytes(small_random) -> list_to_binary(mk_string(small_random));
mk_bytes(_)            -> <<"b">>.

mk_float(_, small_random) -> float(random_int(-10, 10));
mk_float(_, _)            -> 1.0.

random_nth(Seq) ->
    lists:nth(random_int(1, length(Seq)), Seq).

random_int(LowerLim, UpperLim) ->
    ensure_seeded(),
    rand_uniform(UpperLim - LowerLim + 1) + LowerLim - 1.

ensure_seeded() ->
    rand_seed().

%% --- command line options tests -----------------

cmdline_parses_include_opt_test() ->
    {ok, {[{i,"inc"}], []}} = gpb_compile:parse_opts_and_args(["-Iinc"]),
    {ok, {[{i,"inc"}], []}} = gpb_compile:parse_opts_and_args(["-I","inc"]),
    {error, _} = gpb_compile:parse_opts_and_args(["-I"]).

cmdline_parses_noarg_opt_test() ->
    {ok, {[defs_as_proplists], []}} =
         gpb_compile:parse_opts_and_args(["-pldefs"]).

cmdline_parses_string_opt_test() ->
    {ok, {[{o_erl, "src"}], []}} =
        gpb_compile:parse_opts_and_args(["-o-erl", "src"]),
    {error, _} = gpb_compile:parse_opts_and_args(["-o-erl"]).

cmdline_parses_alternatives_opt_test() ->
    {ok, {[{copy_bytes, true}], []}} =
        gpb_compile:parse_opts_and_args(["-c", "true"]),
    {ok, {[{copy_bytes, 1.25}], []}} =
        gpb_compile:parse_opts_and_args(["-c", "1.25"]).

cmdline_parses_files_test() ->
    {ok, {[], []}} = gpb_compile:parse_opts_and_args([]),
    {ok, {[], ["f.proto"]}} = gpb_compile:parse_opts_and_args(["f.proto"]).

cmdline_parses_also_non_proto_extensions_test() ->
    {ok, {[type_specs, {copy_bytes,auto}], ["a.x", "y.proto"]}} =
        gpb_compile:parse_opts_and_args(["-type", "-c", "auto",
                                         "a.x", "y.proto"]).

opt_test() ->
    %% Include dirs + out dirs
    {ok, {[{i, "include1"},
           {i, "include2"},
           {o, "out-dir"},
           {o_erl, "o-erl-dir"},
           {o_hrl, "o-hrl-dir"}],
          ["x.proto", "y.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-Iinclude1",
           "-I", "include2",
           "-o", "out-dir",
           "-o-erl", "o-erl-dir",
           "-o-hrl", "o-hrl-dir",
           "x.proto", "y.proto"]),
    %% nif related
    {ok, {[{o_nif_cc, "o-nif-cc-dir"},
           nif,
           {load_nif, "load-nif"}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-o-nif-cc", "o-nif-cc-dir",
           "-nif",
           "-load_nif", "load-nif",
           "x.proto"]),
    %% misc
    {ok, {[{verify, optionally},
           {verify, always},
           {verify, never},
           {copy_bytes, true},
           {copy_bytes, false},
           {copy_bytes, auto},
           {copy_bytes, 42}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-v", "optionally",
           "-v", "always",
           "-v", "never",
           "-c", "true",
           "-c", "false",
           "-c", "auto",
           "-c", "42",
           "x.proto"]),
    {ok, {[strings_as_binaries,
           use_packages,
           include_as_lib,
           type_specs,
           descriptor],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-strbin",
           "-pkgs",
           "-il",
           "-type",
           "-descr",
           "x.proto"]),
    {ok, {[{module_name_prefix, "mod_prefix_"},
           {module_name_suffix, "_mod_suffix"},
           {module_name, "abc"}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-modprefix", "mod_prefix_",
           "-modsuffix", "_mod_suffix",
           "-modname", "abc",
           "x.proto"]),
    {ok, {[defs_as_proplists,
           maps, msgs_as_maps, mapfields_as_maps, defs_as_maps],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-pldefs",
           "-maps", "-msgs-as-maps", "-mapfields-as-maps", "-defs-as-maps",
           "x.proto"]),
    {ok, {[maps, {maps_oneof, flat}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-maps", "-maps_oneof", "flat",
           "x.proto"]),
    {ok, {[maps, {maps_key_type, binary}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-maps", "-maps-key-type", "binary",
           "x.proto"]),
    {ok, {[{erlc_compile_options, "debug_info, inline_list_funcs"}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-erlc_compile_options", "debug_info, inline_list_funcs",
           "x.proto"]),
    {ok, {[epb_compatibility, epb_functions],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-epb", "-epb-functions",
           "x.proto"]),
    {ok, {[{target_erlang_version,18}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-for-version", "18",
           "x.proto"]),
    {ok, {[bypass_wrappers],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-bypass-wrappers",
           "x.proto"]),
    %% Help and version
    {ok, {[help, help,
           version, version],
          []}} =
        gpb_compile:parse_opts_and_args(
          ["-h", "--help",
           "-V", "--version"]).

any_translation_options_test() ->
    {ok, {[{any_translate,
            [{encode, {me,fe,['$1']}},
             {decode, {md,fd,['$1']}}]}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-any_translate", "e=me:fe,d=md:fd",
           "x.proto"]),
    %% Merge
    {ok, {[{any_translate,
            [{encode, {me,fe,['$1']}},
             {decode, {md,fd,['$1']}},
             {merge,  {mm,fm,['$1','$2']}}]}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-any_translate", "e=me:fe,d=md:fd,m=mm:fm",
           "x.proto"]),
    %% Verify
    {ok, {[{any_translate,
            [{encode, {me,fe,['$1']}},
             {decode, {md,fd,['$1']}},
             {verify, {mv,fv,['$1']}}]}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-any_translate", "e=me:fe,d=md:fd,V=mv:fv",
           "x.proto"]),
    %% old style verify
    {ok, {[{any_translate,
            [{encode, {me,fe,['$1']}},
             {decode, {md,fd,['$1']}},
             {verify, {mv,fv,['$1','$errorf']}}]}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-any_translate", "e=me:fe,d=md:fd,v=mv:fv",
           "x.proto"]).

renaming_options_test() ->
    %% Legacy renamings
    {ok, {[{msg_name_prefix,    "msg_prefix_"},
           {msg_name_suffix,    "_msg_suffix"},
           msg_name_to_lower],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-msgprefix", "msg_prefix_",
           "-msgsuffix", "_msg_suffix",
           "-msgtolower",
           "x.proto"]),
    %% Misc `What' renaming option values
    {ok, {[{rename,{pkg_name,{prefix,"pkg_prefix_"}}},
           {rename,{msg_name,{prefix,"msg_prefix_"}}},
           {rename,{msg_fqname,{prefix,"msg_prefix_"}}},
           {rename,{group_name,{prefix,"group_prefix_"}}},
           {rename,{group_fqname,{prefix,"group_prefix_"}}},
           {rename,{service_name,{prefix,"service_prefix_"}}},
           {rename,{service_fqname,{prefix,"serice_prefix_"}}},
           {rename,{rpc_name,{prefix,"rpc_prefix_"}}},
           {rename,{msg_typename,{prefix,"msgtype_"}}},
           {rename,{enum_typename,{prefix,"enumtype_"}}}] = WhatOpts,
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-rename", "pkg_name:prefix=pkg_prefix_",
           "-rename", "msg_name:prefix=msg_prefix_",
           "-rename", "msg_fqname:prefix=msg_prefix_",
           "-rename", "group_name:prefix=group_prefix_",
           "-rename", "group_fqname:prefix=group_prefix_",
           "-rename", "service_name:prefix=service_prefix_",
           "-rename", "service_fqname:prefix=serice_prefix_",
           "-rename", "rpc_name:prefix=rpc_prefix_",
           "-rename", "msg_typename:prefix=msgtype_",
           "-rename", "enum_typename:prefix=enumtype_",
           "x.proto"]),
    [] = lists:filter(fun gpb_names:is_not_renaming_opt/1, WhatOpts),

    %% Misc `How' renaming option values
    {ok, {[{rename,{msg_fqname,{suffix,"_msg_suffix"}}},
           {rename,{msg_fqname,lower_case}},
           {rename,{msg_fqname,snake_case}},
           {rename,{msg_fqname,dots_to_underscores}},
           {rename,{msg_fqname,base_name}},
           {rename,{msg_fqname,{prefix,
                                {by_proto, [{"myfile","myfile_prefix_"}]}}}},
           {rename,{msg_fqname,{prefix,
                                {by_proto, [{"f1","p1_"},{"f2","p2_"}]}}}}
          ] = HowOpts,
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-rename", "msg_fqname:suffix=_msg_suffix",
           "-rename", "msg_fqname:lower_case",
           "-rename", "msg_fqname:snake_case",
           "-rename", "msg_fqname:dots_to_underscores",
           "-rename", "msg_fqname:base_name",
           "-rename", "msg_fqname:proto=myfile:prefix=myfile_prefix_",
           "-rename", "msg_fqname:proto=f1:prefix=p1_,proto=f2:prefix=p2_",
           "x.proto"]),
    [] = lists:filter(fun gpb_names:is_not_renaming_opt/1, HowOpts),
    ok.

type_translation_options_test() ->
    {ok, {[{translate_type, {{msg,m},
                             [{encode, {me,fe,['$1']}},
                              {decode, {md,fd,['$1']}},
                              {merge,  {mm,fm,['$1','$2']}},
                              {verify, {mv,fv,['$1']}}]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_type", "type=msg:m,e=me:fe,d=md:fd,m=mm:fm,V=mv:fv",
           "x.proto"]),
    {ok, {[{translate_type, {{enum,ee}, [{encode, {me,fe,['$1']}} | _]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_type", "type=enum:ee,e=me:fe,d=md:fd,m=mm:fm,V=mv:fv",
           "x.proto"]),
    {ok, {[{translate_type, {int32, [{encode, {me,fe,['$1']}} | _]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_type", "type=int32,e=me:fe,d=md:fd,m=mm:fm,V=mv:fv",
           "x.proto"]),
    {ok, {[{translate_type, {{map,int32,{msg,m}},
                             [{encode, {me,fe,['$1']}} | _]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_type", "type=map<int32,msg:m>,e=me:fe,d=md:fd,V=mv:fv",
           "x.proto"]).

field_translation_options_test() ->
    {ok, {[{translate_field,
            {[m,f],
             [{encode, {me,fe,['$1']}},
              {decode, {md,fd,['$1']}},
              {merge,  {mm,fm,['$1','$2']}},
              {verify, {mv,fv,['$1']}},
              {decode_init_default, {mi,fi,[]}},
              {decode_repeated_add_elem, {ma,fa,['$1','$2']}},
              {decode_repeated_finalize, {mf,ff,['$1']}}]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_field",
           "field=m.f,e=me:fe,d=md:fd,m=mm:fm,V=mv:fv,i=mi:fi,a=ma:fa,f=mf:ff",
           "x.proto"]),
    {ok, {[{translate_field, {[m,f,[]], [{encode, {me,fe,['$1']}} | _]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_field", "field=m.f.[],e=me:fe,d=md:fd,m=mm:fm,V=mv:fv",
           "x.proto"]),
    {ok, {[{translate_field, {[m,c,a], [{encode, {me,fe,['$1']}} | _]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_field", "field=m.c.a,e=me:fe,d=md:fd,m=mm:fm,V=mv:fv",
           "x.proto"]),
    {ok, {[{translate_field, {[m], [{encode, {me,fe,['$1']}} | _]}}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-translate_field", "field=m,e=me:fe,d=md:fd,m=mm:fm,V=mv:fv",
           "x.proto"]).

no_type_specs_test() ->
    {ok, {[{type_specs, false}], ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(["-no_type", "x.proto"]).

no_gen_mergers_test() ->
    {ok, {[nif, {gen_mergers, false}], ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(["-nif", "-no-gen-mergers",
                                         "x.proto"]).

no_gen_intospections_test() ->
    {ok, {[{gen_introspect, false}], ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(["-no-gen-introspect", "x.proto"]).

dashes_and_underscores_are_interchangeable_in_options_test() ->
    {ok, {[{target_erlang_version,18}, {target_erlang_version,18}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(["-for-version", "18", % norm
                                         "-for_version", "18", % also accepted
                                         "x.proto"]),
    {ok, {[{erlc_compile_options, "debug_info, inline_list_funcs"},
           {erlc_compile_options, "debug_info, inline_list_funcs"}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-erlc_compile_options", "debug_info, inline_list_funcs", % norm
           "-erlc-compile-options", "debug_info, inline_list_funcs", % ok too
           "x.proto"]).


%% --- auxiliaries -----------------

%% vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
%% begin functions that imitates the interface of the gpb module
%% needed by the common/shared tests included above from gpb_tests.erl
-ifdef(gpb_compile_common_tests).
decode_msg(Bin, MsgName, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:decode_msg(Bin, MsgName)
    after unload_code(M)
    end.

encode_msg(Msg, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:encode_msg(Msg)
    after unload_code(M)
    end.

merge_msgs(Msg1, Msg2, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:merge_msgs(Msg1, Msg2)
    after unload_code(M)
    end.

verify_msg(Msg, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:verify_msg(Msg)
    after unload_code(M)
    end.
-endif. %% gpb_compile_common_tests
%% end of functions that imitates the interface of the gpb module
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

compile_defs(MsgDefs) ->
    compile_defs(MsgDefs, [{verify, always}]).

compile_defs(MsgDefs, ExtraOpts) ->
    Mod = find_unused_module(),
    Opts = [binary | ExtraOpts],
    {ok, Mod, Code} = gpb_compile:proto_defs(Mod, MsgDefs, Opts),
    load_code(Mod, Code),
    Mod.

compile_protos([{_, Main} | MoreProtos], Opts) ->
    compile_iolist(Main, [{test_extra_known_protos, MoreProtos} | Opts]).

compile_iolist(IoList) ->
    compile_iolist(IoList, []).

compile_iolist(IoList, ExtraOpts) ->
    compile_iolist_maybe_errors_or_warnings(IoList, ExtraOpts, must_succeed).

compile_iolist_maybe_errors_or_warnings(IoList, ExtraOpts0, OnFail) ->
    {TestOpts, GpbCompileOpts} =
        lists:partition(fun({test_extra_known_protos, _}) -> true;
                           (_GpbCompileOpt) -> false
                        end,
                        ExtraOpts0),
    ExtraProtos = proplists:get_value(test_extra_known_protos, TestOpts, []),
    Mod = find_unused_module(),
    ModProto = f("~s.proto", [Mod]),
    KnownProtos = [{ModProto, IoList} | ExtraProtos],
    ReadFile = fun(F) ->
                       B = filename:basename(F),
                       case lists:keyfind(B, 1, KnownProtos) of
                           {B, Contents} ->
                               {ok, iolist_to_binary([Contents])};
                           _ ->
                               file:read_file(F)
                       end
               end,
    ReadFileInfo = fun(F) ->
                           B = filename:basename(F),
                           case lists:keyfind(B, 1, KnownProtos) of
                               {B, _Contents} ->
                                   {ok, #file_info{access=read}};
                               _ ->
                                   file:read_file_info(F)
                           end
                   end,

    CompRes = gpb_compile:file(
                ModProto,
                [{file_op, [{read_file, ReadFile},
                            {read_file_info, ReadFileInfo},
                            {write_file, fun(_,_) -> ok end}]},
                 {i,"."},
                 binary, return_errors, return_warnings | GpbCompileOpts]),
    case OnFail of
        must_succeed ->
            %% Mod1 instead of Mod, since some options can change the
            %% module name (module_name_suffix, or epb_compatibility,
            %% for instance)
            {ok, Mod1, Code, []} = CompRes,
            load_code(Mod1, Code),
            Mod1;
        get_result ->
            case CompRes of
                {ok, Mod1, Code, Warnings} -> % Mod1 insead of Mod, see above
                    load_code(Mod1, Code),
                    {ok, Mod1, Warnings};
                {error, Reasons, Warnings} ->
                    {error, Reasons, Warnings}
            end;
        get_code ->
            case CompRes of
                {ok, Mod1, Code, Warnings} -> % Mod1 insead of Mod, see above
                    {ok, Mod1, Code, Warnings};
                {error, Reasons, Warnings} ->
                    {error, Reasons, Warnings}
            end
    end.

compile_iolist_get_errors_or_warnings(IoList) ->
    compile_iolist_get_errors_or_warnings(IoList, []).

compile_iolist_get_errors_or_warnings(IoList, ExtraOpts) ->
    compile_iolist_maybe_errors_or_warnings(IoList, ExtraOpts, get_result).

compile_to_string(Proto, Opts) ->
    Self = self(),
    FileOps = [{write_file, fun(FName,Data) ->
                                    case filename:extension(FName) of
                                        ".erl" -> Self ! {data, Data};
                                        _ -> ok
                                    end,
                                    ok
                            end}],
    PS = lists:flatten(Proto),
    ok = gpb_compile:string(some_module, PS, [Opts | [{file_op, FileOps}]]),
    {data,Bin} = ?recv({data,_}),
    binary_to_list(Bin).

compile_to_string_get_hrl(Proto, Opts) ->
    Self = self(),
    FileOps = [{write_file, fun(FName,Data) ->
                                    case filename:extension(FName) of
                                        ".hrl" -> Self ! {data, Data};
                                        _ -> ok
                                    end,
                                    ok
                            end}],
    PS = lists:flatten(Proto),
    ok = gpb_compile:string(some_module, PS, [Opts | [{file_op, FileOps}]]),
    {data,Bin} = ?recv({data,_}),
    case proplists:get_bool(strip_preprocessor_lines, Opts) of
        true ->
            %% Poor man's in-memory preprocessor
            binary_to_list(
              iolist_to_binary(
                [[Line,$\n] || Line <- binary:split(Bin, <<"\n">>, [global]),
                               not is_preprocessor_line(Line)]));
        false ->
            binary_to_list(Bin)
    end.

is_preprocessor_line(<<"-ifndef(", _/binary>>) -> true; % ")"
is_preprocessor_line(<<"-ifdef(", _/binary>>)  -> true; % ")"
is_preprocessor_line(<<"-define(", _/binary>>) -> true; % ")"
is_preprocessor_line(<<"-endif.", _/binary>>)  -> true;
is_preprocessor_line(_) -> false.

compile_erl_iolist(IoList) ->
    compile_erl_iolist(IoList, []).
compile_erl_iolist(IoList, ExtraOpts) ->
    Mod = find_unused_module(),
    Forms = iolist_to_forms([io_lib:format("-module(~p).\n",[Mod]), IoList]),
    ErlcOpts = [binary, return | ExtraOpts],
    {ok, Mod, Code, []} = compile:noenv_forms(Forms, ErlcOpts),
    load_code(Mod, Code),
    Mod.

iolist_to_forms(IoList) ->
    {ok, Toks, _End} = erl_scan:string(
                         unicode:characters_to_list(
                           unicode:characters_to_binary(IoList))),
    iol_to_forms2(Toks, [], []).

iol_to_forms2([{dot,_}=Dot | Rest], Curr, Acc) ->
    {ok, Form} = erl_parse:parse_form(lists:reverse([Dot | Curr])),
    iol_to_forms2(Rest, [], [Form | Acc]);
iol_to_forms2([Tok | Rest], Curr, Acc) ->
    iol_to_forms2(Rest, [Tok | Curr], Acc);
iol_to_forms2([], [], Acc) ->
    lists:reverse(Acc).



load_code(Mod, Code) ->
    unload_code(Mod),
    {module, Mod} = code:load_binary(Mod, "<nofile>", Code),
    ok.

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

find_unused_module() -> find_unused_module(1).

find_unused_module(N) ->
    find_unused_module("", N).

find_unused_module(Prefix, N) ->
    ModNameCandidate = list_to_atom(f("~s~s-tmp-~w", [Prefix, ?MODULE, N])),
    case code:is_loaded(ModNameCandidate) of
        false    -> ModNameCandidate;
        {file,_} -> find_unused_module(Prefix, N+1)
    end.

id(X) -> X.

f(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

-ifndef(NO_HAVE_RAND).
%% Erlang 19 or later
rand_uniform(Limit) -> rand:uniform(Limit).
rand_seed() -> _ = rand:uniform().
-else.
%% Erlang 18 or earlier
rand_uniform(Limit) -> random:uniform(Limit).
rand_seed() ->
    {A, B, C} = os:timestamp(),
    random:seed(erlang:phash2(A+B+C), erlang:phash2(B+C), erlang:phash2(A+C)).
-endif. % NO_HAVE_RAND

-ifndef(NO_HAVE_ERL20_STR_FUNCTIONS).

string_trim(Str) ->
    string:trim(Str).

-else.  % NO_HAVE_ERL20_STR_FUNCTIONS

string_trim(Str) ->
    string:strip(Str).

-endif. % NO_HAVE_ERL20_STR_FUNCTIONS
