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

-export([test_nifs/1]). %% set whether to test nifs or not

-export([compile_iolist/2]).
-export([compile_to_string_get_hrl/2]).
-export([compile_erl_iolist/1]).
-export([unload_code/1]).

%% NIF related
-export([nif_tests_check_prerequisites/1]).
-export([nif_oneof_tests_check_prerequisites/1]).
-export([nif_mapfield_tests_check_prerequisites/1]).
-export([nif_proto3_tests_check_prerequisites/1]).
-export([increase_timeouts/1]).
-export([with_tmpdir/1]).
-export([in_separate_vm/4]).
-export([compile_nif_msg_defs/3, compile_nif_msg_defs/4]).
-export([check_protoc_can_do_oneof/0]).
-export([check_protoc_can_do_mapfields/0]).
-export([check_protoc_can_do_proto3/0]).

%% internally used
-export([main_in_separate_vm/1]).

%% Translators for without user-data
-export([any_e_atom/1, any_d_atom/1, any_m_atom/2, any_v_atom/2]).
-export([any_v_atom/1]).
%% Translators for user-data
-export([any_e_atom/2, any_d_atom/2, any_m_atom/3, any_v_atom/3]).
%% Translators for user-data and op
-export([any_e_atom/3, any_d_atom/3, any_m_atom/4, any_v_atom/4]).

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
    {ok, [{{msg,'Msg'},[#?gpb_field{}]},
          {{msg_containment,"X"},['Msg']}]=MsgDefs} =
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

field_pass_as_params_test() ->
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

copy_bytes_unconditionally_test() ->
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                               [{copy_bytes, true}]),
            Data = M:encode_msg({m1, <<"d">>}),
            {m1, <<"d">>=Bs} = M:decode_msg(Data, m1),
            %% If the Bs has not been copied, then it is a sub-binary
            %% of a larger binary: of the message, ie of Data.
            %% So verify copying by verifying size of referenced data.
            ?assertEqual(byte_size(Bs), binary:referenced_byte_size(Bs)),
            unload_code(M);
       true ->
            %% nothing to test
            ok
    end.

copy_bytes_false_test() ->
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, false}]),
    Data = M:encode_msg({m1, <<"d">>}),
    {m1, <<"d">>=Bs} = M:decode_msg(Data, m1),
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            %% If the StrBin has not been copied, then it is a sub-binary
            %% of a larger binary: of the message, ie of Data.
            %% So verify copying by verifying size of referenced data.
            ?assertEqual(byte_size(Data), binary:referenced_byte_size(Bs));
       true ->
            ok
    end,
    unload_code(M).

copy_bytes_auto_test() ->
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, auto}]),
    Data = M:encode_msg({m1, <<"d">>}),
    {m1, <<"d">>=Bs} = M:decode_msg(Data, m1),
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    if HasBinary ->
            ?assertEqual(byte_size(Bs), binary:referenced_byte_size(Bs));
       true ->
            ok %% cannot test more if we don't have the binary module
    end,
    unload_code(M).

copy_bytes_fraction_test() ->
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    if HasBinary ->
            Proto = ["message m1 {",
                     "  required bytes f1 = 1;",
                     "  required bytes f2 = 2;",
                     "}"],
            M1 = compile_iolist(Proto, [{copy_bytes, 2}]),   % fraction as int
            M2 = compile_iolist(Proto, [{copy_bytes, 2.0}]), % fraction as float
            D1 = <<"d">>, %% small
            D2 = <<"dddddddddddddddddddddddddddd">>, %% large
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
            unload_code(M2);
       true ->
            ok
    end.

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

never_generates_unused_translator_functions_test_() ->
    %% On my slow machine (1.6 GHz Atom N270), it currently takes ~21 seconds
    {timeout,40,fun never_generates_unused_translator_functions_aux/0}.

never_generates_unused_translator_functions_aux() ->
    Enum   = {{enum,ee},[{a,0},{b,1}]},
    SubMsg = {{msg,s},[#?gpb_field{type=uint32,occurrence=required,
                                   fnum=1,rnum=2,opts=[]}]},
    BasicTypes = [int32, int64, sint32, sint64, uint32, uint64,
                  fixed32, fixed64, sfixed32, sfixed64,
                  {enum,ee},
                  bool, string, bytes, float, double,
                  {msg,s}],
    MapTypes = ([{map,string,VT} || VT <- BasicTypes]      % variable-sized key
                ++ [{map,fixed32,VT} || VT <- BasicTypes]), % fixed-size key
    Types = BasicTypes ++ MapTypes,
    M = find_unused_module(),
    [begin
         Field = case OInfo of
                     {Occ,Opts} ->
                         #?gpb_field{type=Type,occurrence=Occ,
                                     fnum=1,rnum=2,opts=Opts};
                     oneof ->
                         #gpb_oneof{
                            name=u, rnum=2,
                            fields=[#?gpb_field{type=Type,occurrence=optional,
                                                fnum=1,rnum=2,opts=[]}]}
                 end,
         Defs = ([{{msg,m},[Field]}]
                 ++ [Enum || needs_enum(Type)]
                 ++ [SubMsg || needs_submsg(Type)]),
         ?assertMatch(
            {{ok,M,_Code,[]=_Warns},_,_},
            {gpb_compile:proto_defs(M,Defs,[binary,return]), Type, OInfo})
     end
     || Type <- Types,
        OInfo <- case Type of
                     {map,_,_} ->
                         [{repeated,[]}];
                     _ ->
                         lists:flatten(
                           [{repeated,[]},
                            [{repeated,[packed]}
                             || gpb:is_type_packable(Type)],
                            {required,[]},
                            {optional,[]},
                            oneof])
                 end].

needs_enum({enum,ee}) -> true;
needs_enum({map,_,{enum,ee}}) -> true;
needs_enum(_) -> false.

needs_submsg({msg,s}) -> true;
needs_submsg({map,_,{msg,s}}) -> true;
needs_submsg(_) -> false.

%% --- misc ----------

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
         catch Class:Reason ->
                 Stack = erlang:get_stacktrace(),
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
    %% circular msg definitions ==> warning about omitting type specs
    [{{msg,m1}, [#?gpb_field{name=field11, type={msg,m2}, occurrence=optional,
                             fnum=1, rnum=2, opts=[]}]},
     {{msg,m2}, [#?gpb_field{name=field22, type={msg,m1}, occurrence=optional,
                             fnum=2, rnum=2, opts=[]}]}];
get_proto_defs(write_fails) ->
    get_proto_defs(clean_code).

get_proto_file(clean_code) ->
    "message m1 { optional uint32 field11 = 1; }\n" ++
    "message MessageInfo1 { optional uint32 field11 = 1; }\n";
get_proto_file(warningful_code) ->
    %% circular msg definitions ==> warning about omitting type specs
    ["message m1 { optional m2 field11 = 1; }\n"
     "message m2 { optional m1 field22 = 2; }\n"];
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
    increase_timeouts(
      nif_tests_check_prerequisites(
        [{"Verify errors in sepatarate vm are caught",
          fun verify_errors_in_separate_vm_are_caught/0},
         {"Nif compiles", fun nif_compiles/0},
         {"Nif encode decode", fun nif_encode_decode/0},
         increase_timeouts(
           nif_oneof_tests_check_prerequisites(
             [{"encode decode", fun nif_encode_decode_oneof/0}])),
         increase_timeouts(
           nif_mapfield_tests_check_prerequisites(
             [{"encode decode", fun nif_encode_decode_mapfields/0}])),
         increase_timeouts(
           nif_proto3_tests_check_prerequisites(
             [{"encode decode", fun nif_encode_decode_proto3/0}])),
         {"Nif enums in msgs", fun nif_enum_in_msg/0},
         {"Nif enums with pkgs", fun nif_enum_with_pkgs/0},
         {"Nif with groups", fun nif_with_groups/0},
         {"Nif with strbin", fun nif_with_strbin/0},
         {"Nif with booleans", fun nif_with_booleans/0},
         {"Nif with list indata for bytes",
          fun nif_with_list_indata_for_bytes/0},
         {"Nif and +-Inf/NaN", fun nif_with_non_normal_floats/0},
         {"Error if both Any translations and nif",
          fun error_if_both_any_translations_and_nif/0}])).

increase_timeouts({Descr, Tests}) ->
    %% On my slow 1.6 GHz Atom N270 machine, the map field test takes
    %% ~77 seconds to run, allow for a bit more
    PerTestTimeout = 140,
    {Descr,
     {timeout, PerTestTimeout * length(Tests),  %% timeout for all tests
      [{timeout, PerTestTimeout,
        [{TestDescr, TestFun}]}
       || {TestDescr, TestFun} <- Tests]}}.

nif_tests_check_prerequisites(Tests) ->
    case nif_verify_prerequisites() of
        ok            -> {"nif tests", Tests};
        {error, Text} -> {Text, []}
    end.

nif_verify_prerequisites() ->
    case {want_nif_tests(), find_protoc(), find_cplusplus_compiler()} of
        {false,_,_} -> {error, "Nif tests not wanted"};
        {_,false,_} -> {error, "Protoc not found, not trying to compile"};
        {_,_,false} -> {error, "No C++ compiler found, not trying to compile"};
        {_,_,_}     -> ok
    end.

'do_nif?'() ->
    nif_verify_prerequisites() == ok.

nif_oneof_tests_check_prerequisites(Tests) ->
    case 'do_nif?'() andalso check_protoc_can_do_oneof() of
        true  -> {"Nif with oneof fields", Tests};
        false -> {"Protoc < 2.6.0, not testing nifs with oneof", []}
    end.

nif_mapfield_tests_check_prerequisites(Tests) ->
    case 'do_nif?'() andalso check_protoc_can_do_mapfields() of
        true  -> {"Nif with map fields", Tests};
        false -> {"Protoc < 3.0.0, not testing nifs with map fields", []}
    end.

nif_proto3_tests_check_prerequisites(Tests) ->
    case 'do_nif?'() andalso check_protoc_can_do_proto3() of
        true  -> {"Nif with proto3", Tests};
        false -> {"Protoc < 3.0.0, not testing nifs with proto3", []}
    end.

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

nif_compiles() ->
    with_tmpdir(
      fun(TmpDir) ->
              NCM = gpb_nif_test_c1,
              Defs = mk_one_msg_field_of_each_type(),
              {ok, _Code} = compile_nif_msg_defs(NCM, Defs, TmpDir)
      end).

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

error_if_both_any_translations_and_nif() ->
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
              true = gpb_lib:is_substr("any_translate", Output1),

              {{return,{error, _, []}},
               {output,""}} =
                  compile_file_get_output(DefsTxt, Opts ++ [return]),

              ok
      end).

compile_nif_msg_defs(M, MsgDefsOrIoList, TmpDir) ->
    compile_nif_msg_defs(M, MsgDefsOrIoList, TmpDir, []).

compile_nif_msg_defs(M, MsgDefsOrIoList, TmpDir, Opts) ->
    {MsgDefs, ProtoTxt} =
        case is_iolist(MsgDefsOrIoList) of
            true -> {parse_to_proto_defs(MsgDefsOrIoList,Opts), MsgDefsOrIoList};
            false -> {MsgDefsOrIoList, msg_defs_to_proto(MsgDefsOrIoList)}
        end,
    [NifCcPath, PbCcPath, NifOPath, PbOPath, NifSoPath, ProtoPath] = Files  =
        [filename:join(TmpDir, lists:concat([M, Ext]))
         || Ext <- [".nif.cc", ".pb.cc", ".nif.o", ".pb.o", ".nif.so",
                    ".proto"]],
    LoadNif = f("load_nif() -> erlang:load_nif(\"~s\", {{loadinfo}}).\n",
                [filename:join(TmpDir, lists:concat([M,".nif"]))]),
    LoadNifOpt = {load_nif, LoadNif},
    Opts2 = [binary, nif, LoadNifOpt] ++ Opts,
    {ok, M, Codes} = gpb_compile:proto_defs(M, MsgDefs, Opts2),
    Code = proplists:get_value(erl, Codes),
    NifTxt = proplists:get_value(nif, Codes),
    %%
    ok = file:write_file(NifCcPath, NifTxt),
    ok = file:write_file(ProtoPath, ProtoTxt),
    %%
    CC = find_cplusplus_compiler(),
    Protoc = find_protoc(),
    CFlags = get_cflags(),
    LdFlags = get_ldflags(),
    CompileProto = f("'~s' --proto_path '~s' --cpp_out='~s' '~s'",
                     [Protoc, TmpDir, TmpDir, ProtoPath]),
    CompileNif = f("'~s' -g -fPIC -Wall -O0 '-I~s' ~s -c -o '~s' '~s'",
                   [CC, TmpDir, CFlags, NifOPath, NifCcPath]),
    CompilePb = f("'~s' -g -fPIC -Wall -O0 '-I~s' ~s -c -o '~s' '~s'",
                  [CC, TmpDir, CFlags, PbOPath, PbCcPath]),
    CompileSo = f("'~s' -g -fPIC -shared -Wall -O0 ~s"
                  "    -o '~s' '~s' '~s' -lprotobuf",
                  [CC, LdFlags, NifSoPath, NifOPath, PbOPath]),
    %% Useful if debugging the nif code, see also with_tmpdir(save, Fun)
    ToClean = [filename:basename(F) || F <- Files, F /= ProtoPath],
    file:write_file(filename:join(TmpDir, "Makefile"),
                    iolist_to_binary(
                      ["all:\n",
                       "\t", CompileProto, "\n",
                       "\t", CompileNif, "\n",
                       "\t", CompilePb, "\n",
                       "\t", CompileSo, "\n",
                       "\n",
                       "clean:\n",
                       "\t", "$(RM)", [[" ",F] || F <- ToClean], "\n"])),
    ok = ccompile("~s", ["set -evx\n"
                         ++ CompileProto ++ "\n"
                         ++ CompileNif ++ "\n"
                         ++ CompilePb ++ "\n"
                         ++ CompileSo]),
    {ok, Code}.

lf_lines(Lines) ->
    [[L,"\n"] || L <- Lines].

is_iolist(X) ->
    try iolist_to_binary(X), true
    catch error:badarg -> false
    end.

parse_to_proto_defs(Iolist) ->
    parse_to_proto_defs(Iolist, []).

parse_to_proto_defs(Iolist, Opts) ->
    B = iolist_to_binary(Iolist),
    {ok, ProtoDefs} = gpb_compile:file(
                        "X.proto",
                        [mk_fileop_opt([{read_file, fun(_) -> {ok, B} end}]),
                         {i,"."},
                         to_proto_defs, report_warnings] ++ Opts),
    ProtoDefs.

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
          catch Class:Reason -> {'EXIT',{Class,Reason,erlang:get_stacktrace()}}
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
    Cmd = f(F, A),
    Output = os:cmd("LC_ALL=C; export LC_ALL; " ++ Cmd ++ "; echo $?\n"),
    [LastLine | _Rest] = lists:reverse(gpb_lib:string_lexemes(Output, "\r\n")),
    try list_to_integer(string_trim(LastLine)) of
        0 -> ok;
        _ -> ?debugFmt("Compilation failed!~nCmd=~p~nOutput:~n~ts~n~n",
                       [Cmd, Output]),
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
    {ok, {[{msg_name_prefix,    "msg_prefix_"},
           {module_name_prefix, "mod_prefix_"},
           {msg_name_suffix,    "_msg_suffix"},
           {module_name_suffix, "_mod_suffix"},
           msg_name_to_lower,
           {module_name, "abc"}],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-msgprefix", "msg_prefix_",
           "-modprefix", "mod_prefix_",
           "-msgsuffix", "_msg_suffix",
           "-modsuffix", "_mod_suffix",
           "-msgtolower",
           "-modname", "abc",
           "x.proto"]),
    {ok, {[defs_as_proplists,
           maps, msgs_as_maps, mapfields_as_maps, defs_as_maps],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-pldefs",
           "-maps", "-msgs-as-maps", "-mapfields-as-maps", "-defs-as-maps",
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

no_type_specs_test() ->
    {ok, {[{type_specs, false}], ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(["-no_type", "x.proto"]).

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

compile_iolist(IoList) ->
    compile_iolist(IoList, []).

compile_iolist(IoList, ExtraOpts) ->
    compile_iolist_maybe_errors_or_warnings(IoList, ExtraOpts, must_succeed).

compile_iolist_maybe_errors_or_warnings(IoList, ExtraOpts, OnFail) ->
    Mod = find_unused_module(),
    Contents = iolist_to_binary(IoList),
    ModProto = f("~s.proto", [Mod]),
    ReadFile = fun(F) -> case filename:basename(F) of
                             ModProto -> {ok, Contents};
                             _ -> file:read_file(F)
                         end
               end,
    ReadFileInfo = fun(F) -> case filename:basename(F) of
                                 ModProto -> {ok, #file_info{access=read}};
                                 _ -> file:read_file_info(F)
                             end
                   end,

    CompRes = gpb_compile:file(
                ModProto,
                [{file_op, [{read_file, ReadFile},
                            {read_file_info, ReadFileInfo},
                            {write_file, fun(_,_) -> ok end}]},
                 {i,"."},
                 binary, return_errors, return_warnings | ExtraOpts]),
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
