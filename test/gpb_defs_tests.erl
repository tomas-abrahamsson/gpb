%%% Copyright (C) 2019  Tomas Abrahamsson
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

%% Test gpb_defs.erl and (somewhat implicitly) gpb_parse.yrl and gpb_scan.xrl

-module(gpb_defs_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

parses_syntax_test() ->
    {ok, [{syntax,"proto2"}]} = parse_lines(["syntax=\"proto2\";"]),
    {ok, [{syntax,"proto3"}]} = parse_lines(["syntax=\"proto3\";"]),
    ?assertError({parse_error,_,{_LNum,_Module,_EMsg}},
                 parse_lines(["syntax=\"totally-unknonwn\";"])).

parses_simple_msg_test() ->
    {ok, [{{msg,'Msg'}, [#?gpb_field{name=x, type=uint32, fnum=1,
                                     occurrence=optional, opts=[]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1;",
           "}"]).

parses_simple_oneof_test() ->
    {ok, [{{msg,'Msg'},
           [#gpb_oneof{
               name=x,
               fields=[#?gpb_field{name=a1, fnum=1, type=uint32,
                                   occurrence=optional},
                       #?gpb_field{name=a2, fnum=2, type=string,
                                   occurrence=optional}
                      ]}]}]=Defs} =
        parse_lines(
          ["message Msg {",
           "  oneof x {",
           "     uint32 a1 = 1;",
           "     string a2 = 2;",
           "  };",
           "}"]),
    %% Verify oneof fields are enumerated to get the same rnum
    %% (since they occupy the same record position)
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,'Msg'}, [#gpb_oneof{
                       name=x,
                       rnum=2,
                       fields=[#?gpb_field{name=a1, rnum=2},
                               #?gpb_field{name=a2, rnum=2}]}]},
     {{msg_containment, _}, _}] =
        do_process_sort_defs(Defs).

parses_default_value_test() ->
    {ok, [{{msg,'Msg'}, [#?gpb_field{name=x, type=uint32, fnum=1,
                                     occurrence=optional,
                                     opts=[{default,12}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1 [default = 12];",
           "}"]).

parses_default_value_for_bytes_test() ->
    {ok, Defs} = parse_lines(
                   ["message m {",
                    "  optional bytes  b = 1 [default = '\001\002\003'];",
                    "  optional string s = 2 [default = 'abc'];",
                    "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m}, [#?gpb_field{name=b, type=bytes,
                            opts=[{default,<<1,2,3>>}]},
                #?gpb_field{name=s, type=string,
                            opts=[{default,"abc"}]}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

parses_string_concatenation_test() ->
    {ok, [{{msg,'Msg'}, [#?gpb_field{name=x, type=string, fnum=1,
                                     occurrence=optional,
                                     opts=[{default,"abc"}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional string x=1 [default='a''b' 'c'];", %% = "abc" hopefully
           "}"]).

parses_nested_messages_test() ->
    {ok, [{{msg,'Msg'}, [{{msg,'Msg2'},[#?gpb_field{name=x}]},
                         #?gpb_field{name=y}]}]} =
        parse_lines(
          ["message Msg {",
           "  message Msg2 { optional uint32 x=1; }",
           "  repeated string y=1;",
           "}"]).

parses_relative_nested_messages_test() ->
    {ok, Elems} = parse_lines(["message m1 {",
                               "",
                               "  required m2 f2 = 1;",
                               "",
                               "  message m2 {",
                               "    message m3 {",
                               %%     The 'm5' refers to m1.m2.m5 (not m1.m5),
                               %%     and it is ok but not not necessary
                               %%     to write it with full path like this:
                               %%     required m1.m2.m5 f5 = 5;
                               "      required m5       f5 = 5;",
                               "      required m1.m2.m5 f6 = 6;",
                               "    }",
                               "    message m5 {",
                               "      required string fy = 1;",
                               "    }",
                               "  }",
                               "",
                               "  message m5 {",
                               "    required uint32 fx = 1;",
                               "  }",
                               "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1},         [#?gpb_field{name=f2, type={msg,'m1.m2'}}]},
     {{msg,'m1.m2'},    []},
     {{msg,'m1.m2.m3'}, [#?gpb_field{name=f5, type={msg,'m1.m2.m5'}},
                         #?gpb_field{name=f6, type={msg,'m1.m2.m5'}}]},
     {{msg,'m1.m2.m5'}, [#?gpb_field{name=fy}]},
     {{msg,'m1.m5'},    [#?gpb_field{name=fx}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Elems).

parses_relative_nested_messages_with_oneof_test() ->
    {ok, Elems} = parse_lines(["message t1 {",
                               "  message t2 {",
                               "    required uint32 bb = 18;",
                               "  }",
                               "}",
                               "message m1 {",
                               "  message m2 {",
                               "    required uint32 aa = 17;",
                               "  }",
                               "  oneof x {",
                               "    .t1.t2 xa1 = 2;",
                               "    m2     xa2 = 3;",
                               "  }",
                               "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1},      [#gpb_oneof{
                         name=x,
                         fields=[#?gpb_field{name=xa1, type={msg,'t1.t2'}},
                                 #?gpb_field{name=xa2, type={msg,'m1.m2'}}]}]},
     {{msg,'m1.m2'}, [#?gpb_field{}]},
     {{msg,'t1'},    []},
     {{msg,'t1.t2'}, [#?gpb_field{}]},
     {{msg_containment,_}, [m1, 'm1.m2', t1, 't1.t2']}] =
        do_process_sort_defs(Elems).

parses_circular_messages_test() ->
    {ok, Elems} = parse_lines(["message m1 {",
                               "  optional m1 f = 1;",
                               "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, [#?gpb_field{name=f, type={msg,m1}}]},
     {{msg_containment,_}, [m1]}] =
        do_process_sort_defs(Elems).

parses_enum_def_test() ->
    {ok, [{{enum,e1}, [{ee1,1},{ee2,2}]}]} =
        parse_lines(
          ["enum e1 {",
           "  ee1 = 1;",
           "  ee2 = 2;",
           "}"]).

parses_nested_enum_def_test() ->
    {ok, [{{msg,'Msg'}, [{{enum,e1}, [{ee1,1},{ee2,2}]},
                         #?gpb_field{name=ef}]}]} =
        parse_lines(
          ["message Msg {"
           "  enum e1 {",
           "    ee1 = 1;",
           "    ee2 = 2;",
           "  }",
           "  required e1 ef = 1;",
           "}"
          ]).

parses_dotted_references_test() ->
    {ok, [{{msg,'Msg3'}, [#?gpb_field{name=y,
                                      type={ref,['Msg','.','Msg2']}}]}]} =
        parse_lines(
          ["message Msg3 {",
           "  repeated Msg.Msg2 y=1;",
           "}"]).

whitespace_in_dotted_references_test() ->
    %% Motivation: this snippet:
    %%
    %%   google.ads.googleads.v0.enums.DisplayAdFormatSettingEnum
    %%       .DisplayAdFormatSetting format_setting = 13;
    %%
    %% Occurring here:
    %% https://github.com/googleapis/googleapis/blob/316b54c401ab9bc08ed71cb362915b9e7a23bb05/google/ads/googleads/v0/common/ad_type_infos.proto#L127-L129
    {ok, [{{msg,'Msg3'}, [#?gpb_field{name=y,
                                      type={ref,['Msg','.','Msg2']}}]}]} =
        parse_lines(
          ["message Msg3 {",
           "  repeated Msg\n"
           "      .Msg2 y=1;",
           "}"]).

parses_package_test() ->
    {ok, [{package,[p1,'.',p2]}, {{enum,e1}, _}]} =
        parse_lines(["package p1.p2;",
                     "enum e1 { a = 1; }"]).

parses_import_test() ->
    {ok, [{package,[p1,'.',p2]}, {import, "a/b/c.proto"}]} =
        parse_lines(["package p1.p2;",
                     "import \"a/b/c.proto\";"]).

parses_enum_option_test() ->
    {ok, Elems} = parse_lines(["enum e1 {",
                               "  option allow_alias = true;",
                               "  ee1 = 1;",
                               "  ee2 = 1;",
                               "}"]),
    [{file, _},
     {{enum,e1}, [{option, allow_alias, true}, {ee1,1},{ee2,1}]},
     {{enum_containment, _}, [_]},
     {{msg_containment,_}, []}] =
        do_process_sort_defs(Elems).

parses_enum_with_custom_option_test() ->
    {ok, Elems} = parse_lines(["enum e1 {",
                               "  option (my_e1_option) = true;",
                               "  option (my_e1_option).x = true;",
                               "  ee1 = 1 [(my_e_option) = 12];",
                               "  ee2 = 2 [(my_e_option).x = \"abc\"];",
                               "}"]),
    [{file, _},
     {{enum,e1}, [{option, my_e1_option, true},
                  {option, 'my_e1_option.x',true},
                  {ee1,1},{ee2,2}]},
     {{enum_containment, _}, [_]},
     {{msg_containment,_}, []}] =
        do_process_sort_defs(Elems).

parses_custom_option_test() ->
    AllDefs = parse_sort_several_file_lines(
                [{"descriptor.proto",
                  ["package google.protobuf;"
                   "message MessageOptions {", % dummy for this test inly
                   "}",
                   "message FieldOptions {", % dummy for this test inly
                   "}"]},
                 {"x.proto",
                  ["package x;",
                   "import \"descriptor.proto\";",
                   "extend google.protobuf.MessageOptions {",
                   "  optional string my_m_option = 51234;",
                   "}",
                   "extend google.protobuf.FieldOptions {",
                   "  optional string my_f_option = 51235;",
                   "}",
                   "",
                   "message t {",
                   "  option (my_m_option) = \"t1\";",
                   "  option (my_m_option) = \"t2\";",
                   "  message s {",
                   "    option (my_m_option) = \"s\";",
                   "  }",
                   "  required uint32 f1 = 22 [(my_f_option)];",
                   "  required uint32 f2 = 23 [(my_f_option).x = false];",
                   "}"]}],
                [use_packages]),
    [{file, _},
     {file, _},
     {package,'google.protobuf'},
     {package,x},
     {{enum_containment,"descriptor"}, []},
     {{enum_containment,"x"}, []},
     {{msg,'google.protobuf.FieldOptions'}, [#?gpb_field{name=my_f_option}]},
     {{msg,'google.protobuf.MessageOptions'}, [#?gpb_field{name=my_m_option}]},
     {{msg,'x.t'},[#?gpb_field{name=f1,opts=[{[my_f_option],true}]},
                   #?gpb_field{name=f2,opts=[{[my_f_option,x],false}]}]},
     {{msg,'x.t.s'},[]},
     {{msg_containment,_}, ['google.protobuf.FieldOptions',
                            'google.protobuf.MessageOptions']},
     {{msg_containment,_}, ['x.t', 'x.t.s']},
     {{msg_options,'x.t'},[{[my_m_option],"t1"},
                           {[my_m_option],"t2"}]},
     {{msg_options,'x.t.s'},[{[my_m_option],"s"}]},
     {{pkg_containment, "descriptor"}, 'google.protobuf'},
     {{pkg_containment, "x"}, x}] =
        AllDefs.

json_name_field_option_test() ->
    {ok, Elems} = parse_lines(
                    ["message m1 {",
                     "  required uint32 foo_bar = 1 [json_name='someName'];",
                     "  required uint32 g = 2 [json_name='something' 'Else'];",
                     "  required uint32 foo_bar_gazonk = 3;",
                     "}"]),
    [{file, _},
     {{enum_containment, _}, []},
     {{msg,m1},[#?gpb_field{opts=[{json_name, "someName"}]}=F1,
                #?gpb_field{opts=[{json_name, "somethingElse"}]},
                #?gpb_field{opts=[]}=F3]},
     {{msg_containment,_}, [_]}] =
        do_process_sort_defs(Elems),
    "someName" = gpb_lib:get_field_json_name(F1),
    "fooBarGazonk" = gpb_lib:get_field_json_name(F3).


generates_correct_absolute_names_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required .m1.m2 z = 2;",
                               "  required e1     w = 3;",
                               "}",
                               "message m3 {",
                               "  required m1.m2 b = 1;",
                               "}"]),
    [{file, _},
     {{enum,'m1.e1'}, [_]},
     {{enum_containment, _}, ['m1.e1']},
     {{msg,m1},       [#?gpb_field{name=y, type={msg,'m1.m2'}},
                       #?gpb_field{name=z, type={msg,'m1.m2'}},
                       #?gpb_field{name=w, type={enum,'m1.e1'}}]},
     {{msg,'m1.m2'},  [#?gpb_field{name=x}]},
     {{msg,m3},       [#?gpb_field{name=b, type={msg,'m1.m2'}}]},
     {{msg_containment,_}, [m1, 'm1.m2', m3]}] =
        do_process_sort_defs(Elems).

generates_correct_absolute_names_2_test() ->
    {ok, Elems} = parse_lines(["message m2 {",
                               "  message m4 { required uint32 x = 1; }",
                               "}",
                               "message m1 {",
                               "  message m2 {",
                               "    message m3 { required uint32 y = 1; }",
                               "  }",
                               "  required m1.m2 f1 = 1;", %% -> .m1.m2
                               "  required m2.m3 f2 = 2;", %% -> .m1.m2.m3
                               "  required m2.m4 f3 = 3;", %% -> .m2.m4
                               "}"]),
    [{file, _},
     {{enum_containment, _}, []},
     {{msg,m1},         [#?gpb_field{name=f1,type={msg,'m1.m2'}},
                         #?gpb_field{name=f2,type={msg,'m1.m2.m3'}},
                         #?gpb_field{name=f3,type={msg,'m2.m4'}}]},
     {{msg,'m1.m2'},    []},
     {{msg,'m1.m2.m3'}, [#?gpb_field{name=y}]},
     {{msg,m2},         []},
     {{msg,'m2.m4'},    [#?gpb_field{name=x}]},
     {{msg_containment,_}, [m1, 'm1.m2', 'm1.m2.m3', m2, 'm2.m4']}] =
        do_process_sort_defs(Elems).

resolve_refs_test() ->
    {ok, Elems} = parse_lines(["package p1;"
                               "import \"a/b/c.proto\";",
                               "message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required e1     z = 2;",
                               "  required uint32 w = 3;",
                               "}",
                               "message m3 {",
                               "  required m1.m2 b = 1;",
                               "}"]),
    [{file, _},
     {import, _},
     {package, p1},
     {{enum,'m1.e1'}, _},
     {{enum_containment, _}, ['m1.e1']},
     {{msg,m1},       [#?gpb_field{name=y, type={msg,'m1.m2'}},
                       #?gpb_field{name=z, type={enum,'m1.e1'}},
                       #?gpb_field{name=w}]},
     {{msg,'m1.m2'},  [#?gpb_field{name=x}]},
     {{msg,m3},       [#?gpb_field{name=b, type={msg,'m1.m2'}}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Elems).

resolve_map_valuetype_refs_test() ->
    {ok, Elems} = parse_lines(["message m1 {",
                               "  message m2 { required uint32 x = 1; }",
                               "  map<string, m2> b = 1;",
                               "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1},       [#?gpb_field{name=b, type={map,string,{msg,'m1.m2'}}}]},
     {{msg,'m1.m2'},  [#?gpb_field{name=x}]},
     {{msg_containment,_}, [m1, 'm1.m2']}] =
        do_process_sort_defs(Elems).

error_for_map_in_oneof_test() ->
    %% map<_,_> is encoded as if had it been a repeated (sub) message field,
    %% but inside a oneof, "optional" is implicit for all fields,
    %% so there cannot be a repeated field (such as a map<_,_>) inside
    %% the oneof.
    ?assertError(_, parse_lines(["message m1 {",
                                 "  oneof x {",
                                 "    map<string, m2> b = 1;",
                                 "  };",
                                 "}"])).

resolve_refs_with_packages_test() ->
    {ok, Elems} = parse_lines(["package p1;"
                               "import \"a/b/c.proto\";",
                               "message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required e1     z = 2;",
                               "  required uint32 w = 3;",
                               "}",
                               "message m3 {",
                               "  required m1.m2 b = 1;",
                               "}"]),
    [{file, _},
     {import, _},
     {package, p1},
     {{enum,'p1.m1.e1'}, _},
     {{enum_containment, _}, _},
     {{msg,'p1.m1'},    [#?gpb_field{name=y, type={msg,'p1.m1.m2'}},
                         #?gpb_field{name=z, type={enum,'p1.m1.e1'}},
                         #?gpb_field{name=w}]},
     {{msg,'p1.m1.m2'}, [#?gpb_field{name=x}]},
     {{msg,'p1.m3'},    [#?gpb_field{name=b, type={msg,'p1.m1.m2'}}]},
     {{msg_containment,_}, ['p1.m1', 'p1.m1.m2', 'p1.m3']},
     {{pkg_containment,"y"},p1}] =
        do_process_sort_defs(Elems, [use_packages]).

package_can_appear_anywhere_toplevelwise_test() ->
    %% The google protoc seems accepts one package specifiers anywhere
    %% at the top-level. It need not be at the beginning,
    %% yet it applies to all (non-imported) definitions in the proto file.
    {ok, Elems1} = parse_lines(["package p1;"
                                "message m1 { required uint32 x = 1; }",
                                "message m2 { required uint32 y = 2; }"]),
    {ok, Elems2} = parse_lines(["message m1 { required uint32 x = 1; }",
                                "package p1;"
                                "message m2 { required uint32 y = 2; }"]),
    {ok, Elems3} = parse_lines(["message m1 { required uint32 x = 1; }",
                                "message m2 { required uint32 y = 2; }",
                                "package p1;"]),
    [{file, _},
     {package, p1},
     {{enum_containment, _}, _},
     {{msg,'p1.m1'}, [#?gpb_field{}]},
     {{msg,'p1.m2'}, [#?gpb_field{}]},
     {{msg_containment,_}, _},
     {{pkg_containment,_}, _}] = Defs =
        do_process_sort_defs(Elems1, [use_packages]),
    ?assertEqual(Defs, do_process_sort_defs(Elems2, [use_packages])),
    ?assertEqual(Defs, do_process_sort_defs(Elems3, [use_packages])).


enumerates_msg_fields_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 11;",
                               "  required e1     z = 12;",
                               "}"]),
    [{file, _},
     {{enum,'m1.e1'}, _},
     {{enum_containment, _}, ['m1.e1']},
     {{msg,m1},       [#?gpb_field{name=y, fnum=11, rnum=2},
                       #?gpb_field{name=z, fnum=12, rnum=3}]},
     {{msg,'m1.m2'},  [#?gpb_field{name=x, fnum=1,  rnum=2}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Elems).

field_opt_normalization_test() ->
    {ok,Defs} = parse_lines(["message m1 {"
                             "  required uint32 f1=1 [packed=true,default=1];",
                             "  required uint32 f2=2 [packed=false];",
                             "  required uint32 f3=3 [packed,default=2];",
                             "  required uint32 f4=4 [deprecated=true];",
                             "  required uint32 f5=5 [deprecated=false];",
                             "  required uint32 f6=6 [deprecated];",
                             "  required bool   f7=7 [packed,default=true];",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, [#?gpb_field{name=f1, opts=[packed, {default,1}]},
                 #?gpb_field{name=f2, opts=[]},
                 #?gpb_field{name=f3, opts=[packed, {default,2}]},
                 #?gpb_field{name=f4, opts=[deprecated]},
                 #?gpb_field{name=f5, opts=[]},
                 #?gpb_field{name=f6, opts=[deprecated]},
                 #?gpb_field{name=f7, opts=[packed, {default,true}]}]},
     {{msg_containment,_}, [m1]}] =
        do_process_sort_several_defs([Defs]).

parses_empty_msg_field_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1 []; }"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, [#?gpb_field{name=f1, opts=[]}]},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_and_ignores_enum_field_options_test() ->
    {ok,_Defs} = parse_lines(["enum e1 { a=1 [x=y]; }"]).

parses_and_ignores_empty_enum_field_options_test() ->
    {ok,_Defs} = parse_lines(["enum e1 { a=1 []; }"]).

parses_msg_extensions_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1;",
                             "  extensions 100 to 199, 300, 400 to max, 250;",
                             "  extensions 251, 252;",
                             "  message m2 {",
                             "    required uint32 f2=2;",
                             "    extensions 233;",
                             "  }",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{extensions,m1},[{100,199},{250,250},{300,300},{400,max}]},
     {{extensions,m1},[{251,251},{252,252}]},
     {{extensions,'m1.m2'},[{233,233}]},
     {{msg,m1},      [#?gpb_field{name=f1}]},
     {{msg,'m1.m2'}, [#?gpb_field{name=f2}]},
     {{msg_containment,_}, [m1, 'm1.m2']}] =
        do_process_sort_defs(Defs).

parses_extending_msgs_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1 [default=17];",
                             "  extensions 200 to 299;",
                             "}",
                             "extend m1 {",
                             "  optional uint32 f2=2;",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{extensions,m1},[{200,299}]},
     {{msg,m1},       [#?gpb_field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                                   occurrence=required},
                       #?gpb_field{name=f2, fnum=2, rnum=3, opts=[],
                                   occurrence=optional}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

parses_extend_scope_rules_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1 = 1;",
                             "  extensions 100 to 199;",
                             "  message m1 {",
                             "    required uint32 f2 = 2;",
                             "    extensions 200 to 299;",
                             "    message m1 {",
                             "      required uint32 f3 = 3;",
                             "      extensions 300 to 399;",

                             %% This extends m1.m1.m1, ie the innermost m1
                             %% Located in m1.m1.m1
                             "      extend m1 {",
                             "        optional string e31 = 301;",
                             "      }",
                             "    }",
                             "  }",

                             %% This extends m1.m1, even though it is
                             %% located in m1, not in m1.m1
                             "  extend m1 {",
                             "    optional string e21 = 201;",
                             "  }",
                             "}",

                             "extend m1.m1 {",
                             "  optional string e22 = 202;",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{extensions,m1},[{100,199}]},
     {{extensions,'m1.m1'},[{200,299}]},
     {{extensions,'m1.m1.m1'},[{300,399}]},
     {{msg,m1},
      [#?gpb_field{name=f1,  fnum=1,   occurrence=required}]},
     {{msg,'m1.m1'},
      [#?gpb_field{name=f2,  fnum=2,   occurrence=required},
       #?gpb_field{name=e21, fnum=201, occurrence=optional},
       #?gpb_field{name=e22, fnum=202, occurrence=optional}]},
     {{msg,'m1.m1.m1'},
      [#?gpb_field{name=f3,  fnum=3,   occurrence=required},
       #?gpb_field{name=e31, fnum=301, occurrence=optional}]},
     {{msg_containment,_}, [m1, 'm1.m1', 'm1.m1.m1']}] =
        do_process_sort_defs(Defs).

parses_extending_msgs_with_nested_msg_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1 [default=17];",
                             "  extensions 200 to 299;",
                             "}",
                             "message m2 {",
                             "  optional uint32 num = 1;",
                             "}",
                             "extend m1 {",
                             "  optional m2 ext_m2 = 200;",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{extensions,m1},[{200,299}]},
     {{msg,m1},       [#?gpb_field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                                   occurrence=required},
                       #?gpb_field{name=ext_m2, fnum=200, rnum=3, opts=[],
                                   occurrence=optional, type={msg, m2}}]},
     {{msg,m2},       [#?gpb_field{name=num, fnum=1, rnum=2, opts=[],
                                   occurrence=optional}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

parses_nested_extending_msgs_in_package_test() ->
    {ok,Defs} = parse_lines(["package p1.p2;",
                             "message m1 {",
                             "  required uint32 f1=1 [default=17];",
                             "  extensions 200 to 299;",
                             "  extend m1 {",
                             "    optional uint32 f2=2;",
                             "  }",
                             "}"]),
    [{file, _},
     {package,'p1.p2'},
     {{enum_containment, _}, _},
     {{extensions,'p1.p2.m1'},[{200,299}]},
     {{msg,'p1.p2.m1'},
      [#?gpb_field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                   occurrence=required},
       #?gpb_field{name=f2, fnum=2, rnum=3, opts=[],
                   occurrence=optional}]},
     {{msg_containment,_}, ['p1.p2.m1']},
     {{pkg_containment,_}, 'p1.p2'}] =
        do_process_sort_defs(Defs, [use_packages]).

extend_msg_several_times_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1;",
                             "  extensions 200 to 299;",
                             "};",
                             "extend m1 {",
                             "  optional bool f2 = 202;",
                             "};",
                             "",
                             "extend m1 {",
                             "  optional string f3 = 203;",
                             "};"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{extensions,m1},[{200,299}]},
     {{msg,m1},
      [#?gpb_field{name=f1, fnum=1, rnum=2, type=uint32, occurrence=required},
       #?gpb_field{name=f2, fnum=202, rnum=3, type=bool, occurrence=optional},
       #?gpb_field{name=f3, fnum=203, rnum=4, type=string, occurrence=optional}
      ]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs, []).

extend_msg_in_other_package_test() ->
    AllDefs = parse_sort_several_file_lines(
                [{"foo.proto",
                  ["package foo;",
                   "message fm1 {",
                   "    required int32 f = 1;",
                   "    extensions 100 to 199;",
                   "}",
                   "message fm2 {",
                   "    required bool g = 1;",
                   "}"]},
                 {"bar.proto",
                  ["package bar;",
                   "import \"foo.proto\";",
                   "extend foo.fm1 {",
                   "    optional string b1 = 100;",
                   "    optional foo.fm2 b2 = 101;",
                   "}"]}],
                [use_packages]),
    [{file, _},
     {file, _},
     {package,bar},
     {package,foo},
     {{enum_containment, "bar"}, _},
     {{enum_containment, "foo"}, _},
     {{extensions,'foo.fm1'},[{100,199}]},
     {{msg,'foo.fm1'},
      [#?gpb_field{name=f, fnum=1, type=int32, occurrence=required},
       #?gpb_field{name=b1, fnum=100, type=string, occurrence=optional},
       #?gpb_field{name=b2, fnum=101, type={msg,'foo.fm2'},
                   occurrence=optional}]},
     {{msg,'foo.fm2'},
      [#?gpb_field{name=g, fnum=1, type=bool, occurrence=required}]},
     {{msg_containment,"bar"}, _}, % for bar.proto
     {{msg_containment,"foo"}, _}, % for foo.proto
     {{pkg_containment,"bar"},bar},
     {{pkg_containment,"foo"},foo}
    ] =
        AllDefs.

extending_and_resolving_ref_to_msg_in_enclosing_package_test() ->
    AllDefs = parse_sort_several_file_lines(
                [{"p/p.proto",
                  ["package p;",
                   "message err { required uint32 f = 1; };"]},
                 {"p/x/y.proto",
                  ["package p.x;",
                   "import \"p.proto\";",
                   "message a {",
                   "  extensions 200 to max;",
                   "  optional string g = 1;"
                   "  optional err    h = 2;"
                   "  extend a {",
                   "    repeated err errs = 200;",
                   "  }",
                   "}"]}],
                [use_packages]),
    [{file, _},
     {file, _},
     {package,p},
     {package,'p.x'},
     {{enum_containment, "p"}, _},
     {{enum_containment, "y"}, _},
     {{extensions,'p.x.a'},[{200,max}]},
     {{msg,'p.err'}, [#?gpb_field{name=f}]},
     {{msg,'p.x.a'},
      [#?gpb_field{name=g, fnum=1, type=string, occurrence=optional},
       #?gpb_field{name=h, fnum=2, type={msg,'p.err'}, occurrence=optional},
       #?gpb_field{name=errs, fnum=200, type={msg,'p.err'},
                   occurrence=repeated}]},
     {{msg_containment,"p"}, _},
     {{msg_containment,"y"}, _},
     {{pkg_containment,"p"},p},
     {{pkg_containment,"y"},'p.x'}] =
        AllDefs.

scope_when_resolving_extend_field_refs_test() ->
    AllDefs = parse_sort_several_file_lines(
                [{"a.proto",
                  ["package a;",
                   "message Foo {",
                   "  extensions 200 to max;",
                   "  optional string id = 1;",
                   "}"]},
                 {"b.proto",
                  ["package b;",
                   "import \"a.proto\";",
                   "message Bar {",
                   "  optional string id = 1;",
                   "}",
                   "",
                   "extend a.Foo {",
                   "  optional Bar b = 200;",
                   "}"]}],
                [use_packages]),
    [{file, _},
     {file, _},
     {package,a},
     {package,b},
     {{enum_containment, "a"}, _},
     {{enum_containment, "b"}, _},
     {{extensions,'a.Foo'},[{200,max}]},
     {{msg,'a.Foo'}, [#?gpb_field{name=id},
                      #?gpb_field{name=b, type={msg,'b.Bar'}}]},
     {{msg,'b.Bar'}, [#?gpb_field{name=id}]},
     {{msg_containment,"a"}, _},
     {{msg_containment,"b"}, _},
     {{pkg_containment,"a"}, a},
     {{pkg_containment,"b"}, b}] =
        AllDefs.

nested_extended_block_test() ->
    AllDefs = parse_sort_several_file_lines(
                [{"a.proto",
                  ["message A {",
                   "}",
                   "",
                   "message B {",
                   "    extend A {",
                   "        optional B b = 42;",
                   "    }",
                   "}"]}],
                []),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,'A'},[#?gpb_field{name=b,type={msg,'B'}}]},
     {{msg,'B'},[]},
     {{msg_containment,"a"},_}] =
        AllDefs.

group_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required m2 f = 1;",
                             "  required group g = 2 {",
                             "    required uint32 gf = 3;",
                             "  }",
                             "}",
                             "message m2 {",
                             "  required uint32 ff = 11;",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{group,'m1.g'},[#?gpb_field{name=gf,type=uint32,fnum=3,rnum=2,
                                  opts=[]}]},
     {{msg,m1},[#?gpb_field{name=f,type={msg,'m2'}},
                #?gpb_field{name=g,type={group,'m1.g'}}]},
     {{msg,m2},[#?gpb_field{name=ff}]},
     {{msg_containment,_}, [m1,m2]} % groups not included
    ] =
        do_process_sort_defs(Defs).

message_def_nested_in_group_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required m2 f = 1;",
                             "  message m2 {",
                             "    required uint32 ff = 11;",
                             "  }",
                             "  required group g = 2 {",
                             "    required uint32 gf = 3;",
                             "  }",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{group,'m1.g'},[#?gpb_field{name=gf,type=uint32,fnum=3,rnum=2,
                                  opts=[]}]},
     {{msg,m1},[#?gpb_field{name=f,type={msg,'m1.m2'}},
                #?gpb_field{name=g,type={group,'m1.g'}}]},
     {{msg,'m1.m2'},[#?gpb_field{name=ff}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

parses_service_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2);",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{rpc_containment,_}, [{s1, req}]},
     {{service,s1},[#?gpb_rpc{name=req, input=m1, output=m2}]},
     {{service_containment, _}, [s1]}] =
        do_process_sort_defs(Defs).

parses_multiple_services_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2);",
                             "}",
                             "service s2 {",
                             "  rpc req2(m2) returns (m1);",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{rpc_containment,_}, [_, _]},
     {{service,s1},[#?gpb_rpc{name=req,  input=m1, output=m2}]},
     {{service,s2},[#?gpb_rpc{name=req2, input=m2, output=m1}]},
     {{service_containment, _}, [_, _]}] =
        do_process_sort_defs(Defs).

parses_rpc_streams_and_options_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f1=1;}",
                             "service s1 {",
                             "  rpc r1(m1)        returns (m2);",
                             "  rpc r2(stream m1) returns (m2);",
                             "  rpc r3(m1)        returns (stream m2);",
                             "  rpc r4(stream m1) returns (stream m2);",
                             "  rpc r5(m1) returns (m2) { option a=1; };",
                             "  rpc r6(m1) returns (m2) { option (a).b=1; };",
                             "  rpc r7(m1) returns (m2) { option (a.b).c=1; };",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{rpc_containment,_}, _},
     {{service,s1},
      [#?gpb_rpc{name=r1, input_stream=false, output_stream=false},
       #?gpb_rpc{name=r2, input_stream=true, output_stream=false},
       #?gpb_rpc{name=r3, input_stream=false, output_stream=true},
       #?gpb_rpc{name=r4, input_stream=true, output_stream=true},
       #?gpb_rpc{name=r5, opts=[{'a',1}]},
       #?gpb_rpc{name=r6, opts=[{'a.b',1}]},
       #?gpb_rpc{name=r7, opts=[{'a.b.c',1}]}]=Rpcs},
     {{service_containment, _}, _}] =
        do_process_sort_defs(Defs),
    %% Check all input(arg)/output(return) messages too
    [{m1,m2}] = lists:usort([{A,R} || #?gpb_rpc{input=A,output=R} <- Rpcs]).

parses_service_ignores_empty_method_option_braces_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2) {};",
                             "}"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{rpc_containment, _}, _},
     {{service,s1},[#?gpb_rpc{name=req, input=m1, output=m2}]},
     {{service_containment, _}, _}] =
        do_process_sort_defs(Defs).


parses_empty_toplevel_statement_test() ->
    {ok,Defs} = parse_lines(["; message m1 { required uint32 f1=1; }; ; "]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_empty_message_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { ; ; required uint32 f1=1;;; }"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, [#?gpb_field{name=f1}]},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_empty_enum_statement_test() ->
    {ok,Defs} = parse_lines(["enum e1 { ; ; ee1=1;;; }"]),
    [{file, _},
     {{enum,e1}, [{ee1,1}]},
     {{enum_containment, _}, [e1]},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_empty_service_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { ; ; rpc r1(m1) returns (m1);;; }"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg_containment,_}, _},
     {{rpc_containment, _}, _},
     {{service,s1},[#?gpb_rpc{name=r1, input=m1, output=m1}]},
     {{service_containment, _}, _}] =
        do_process_sort_defs(Defs).

parses_empty_service_statement_method_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { rpc r1(m1) returns (m1){;;;}; ",
                             "             rpc r2(m1) returns (m1); }",
                             "service s2 { rpc r5(m1) returns (m1){}",
                             "             rpc r6(m1) returns (m1){} }"]),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,m1}, _},
     {{msg_containment,_}, _},
     {{rpc_containment,_}, _},
     {{service,s1},[#?gpb_rpc{name=r1, input=m1, output=m1},
                    #?gpb_rpc{name=r2, input=m1, output=m1}]},
     {{service,s2},[#?gpb_rpc{name=r5, input=m1, output=m1},
                    #?gpb_rpc{name=r6, input=m1, output=m1}]},
     {{service_containment, _}, _}] =
        do_process_sort_defs(Defs).

proto3_no_occurrence_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  uint32 f1=1;",
                             "  repeated uint32 f2=2;",
                             "}"]),
    [{file, _},
     {proto3_msgs,[m1]},
     {syntax,"proto3"},
     {{enum_containment, _}, _},
     {{msg,m1},
      [#?gpb_field{name=f1,fnum=1,occurrence=optional},
       #?gpb_field{name=f2,fnum=2,occurrence=repeated}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

proto3_sub_msgs_gets_occurrence_optional_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  s1 f1=1;",
                             "  repeated s1 f2=2;",
                             "}",
                             "message s1 { uint32 f1=1; }"]),
    [{file, _},
     {proto3_msgs,[m1,s1]},
     {syntax,"proto3"},
     {{enum_containment, _}, _},
     {{msg,m1},
      [#?gpb_field{name=f1,fnum=1,type={msg,s1},occurrence=optional},
       #?gpb_field{name=f2,fnum=2,type={msg,s1},occurrence=repeated}]},
     {{msg,s1},
      [#?gpb_field{name=f1,fnum=1,type=uint32,occurrence=optional}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

proto3_no_repeated_are_packed_by_default_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  repeated uint32 f2=2;", % to be packed
                             "  repeated uint32 f3=3 [packed=false];",
                             "  repeated uint32 f4=4 [packed=true];",
                             "  repeated uint32 f5=5 [packed];",
                             "}"]),
    [{file, _},
     {proto3_msgs,[m1]},
     {syntax,"proto3"},
     {{enum_containment, _}, _},
     {{msg,m1},
      [#?gpb_field{name=f2,fnum=2,occurrence=repeated,opts=[packed]},
       #?gpb_field{name=f3,fnum=3,occurrence=repeated,opts=[]},
       #?gpb_field{name=f4,fnum=4,occurrence=repeated,opts=[packed]},
       #?gpb_field{name=f5,fnum=5,occurrence=repeated,opts=[packed]}]},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

proto3_only_numeric_scalars_are_packed_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  repeated int32            i32  = 1;",
                             "  repeated int64            i64  = 2;",
                             "  repeated uint32           u32  = 3;",
                             "  repeated uint64           u64  = 4;",
                             "  repeated sint32           s32  = 5;",
                             "  repeated sint64           s64  = 6;",
                             "  repeated fixed32          f32  = 7;",
                             "  repeated fixed64          f64  = 8;",
                             "  repeated sfixed32         sf32 = 9;",
                             "  repeated sfixed64         sf64 = 10;",
                             "  repeated bool             bool = 11;",
                             "  repeated float            fl   = 12;",
                             "  repeated double           do   = 13;",
                             "  repeated string           str  = 14;",
                             "  repeated bytes            by   = 15;",
                             "  repeated ee               ef   = 16;",
                             "  repeated subm             subf = 17;",
                             "  map<int32,int32>          mapf = 18;",
                             "}",
                             "message subm { string f1 = 1; }",
                             "enum ee { e0 = 0; }"
                            ]),
    [{file, _},
     {proto3_msgs,[m1,subm]},
     {syntax,"proto3"},
     {{enum,ee},_},
     {{enum_containment, _}, _},
     {{msg,m1},
      [#?gpb_field{name=i32,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=i64,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=u32,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=u64,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=s32,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=s64,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=f32,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=f64,  occurrence=repeated, opts=[packed]},
       #?gpb_field{name=sf32, occurrence=repeated, opts=[packed]},
       #?gpb_field{name=sf64, occurrence=repeated, opts=[packed]},
       #?gpb_field{name=bool, occurrence=repeated, opts=[packed]},
       #?gpb_field{name=fl,   occurrence=repeated, opts=[packed]},
       #?gpb_field{name=do,   occurrence=repeated, opts=[packed]},
       #?gpb_field{name=str,  occurrence=repeated, opts=[]},
       #?gpb_field{name=by,   occurrence=repeated, opts=[]},
       #?gpb_field{name=ef,   occurrence=repeated, opts=[packed]},
       #?gpb_field{name=subf, occurrence=repeated, opts=[]},
       #?gpb_field{name=mapf,                      opts=[]}
      ]},
     {{msg,subm},_},
     {{msg_containment,_}, _}] =
        do_process_sort_defs(Defs).

mixing_proto2_and_proto3_test() ->
    {ok,Defs1} = parse_lines(["syntax=\"proto3\";",
                             %% import "f2.proto"; % (done another way below)
                             "message m1 {",
                             "  m2 f1=1;",
                             "  repeated uint32 f2=2;",
                             "}"]),
    {ok,Defs2} = parse_lines(["syntax=\"proto2\";",
                              "message m2 {",
                              "  repeated uint32 f3=3;",
                              "}"]),
    [{file, _},
     {file, _},
     {proto3_msgs,[m1]},
     {syntax,"proto2"},
     {syntax,"proto3"},
     {{enum_containment, _}, _},
     {{enum_containment, _}, _},
     {{msg,m1},
      [#?gpb_field{name=f1,type={msg,m2}},
       #?gpb_field{name=f2,occurrence=repeated,opts=[packed]}]},
     {{msg,m2},
      [#?gpb_field{name=f3,occurrence=repeated,opts=[]}]},
     {{msg_containment,_}, _},
     {{msg_containment,_}, _}] =
        do_process_sort_several_defs([Defs1, Defs2]).

proto3_reserved_numbers_and_names_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  uint32 f1=1;",
                             "  reserved 2, 15, 9 to 11;",
                             "  reserved \"foo\", \"bar\";",
                             "  message m2 {",
                             "    uint32 f2=2;",
                             "    reserved 17;",
                             "  }",
                             "}"]),
    [{file, _},
     {proto3_msgs,[m1,'m1.m2']},
     {syntax,"proto3"},
     {{enum_containment, _}, _},
     {{msg,m1},      [#?gpb_field{name=f1}]},
     {{msg,'m1.m2'}, [#?gpb_field{name=f2}]},
     {{msg_containment,_}, _},
     {{reserved_names,m1},       ["foo","bar"]},
     {{reserved_numbers,m1},     [2,15,{9,11}]},
     {{reserved_numbers,'m1.m2'},[17]}] =
        do_process_sort_defs(Defs).

file_attrs_for_each_file_test() ->
    AllDefs = parse_several_file_lines( % no sort!
                [{"f1.proto",
                  ["import \"f2\";",
                   "import \"f3.proto33\";",
                   "message M1 { };"]},
                 {"f2",
                  ["message M2 {}"]},
                 {"f3.proto33",
                  ["message M3 {}"]}],
                []),
    [{file, {"f1", "f1.proto"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M1'}, _},

     {file, {"f2", "f2"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M2'}, _},

     {file, {"f3", "f3.proto33"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M3'}, _}] = AllDefs.

basenameification_of_file_attrs_test() ->
    AllDefs = parse_several_file_lines( % no sort!
                [{"a/b/c/f1.proto",
                  ["import \"a/b/f1.proto\";",
                   "import \"x/y/z/f1.proto\";",
                   "import \"x/y/z/f1.proto\";",
                   "import \"f4.proto\";",
                   "message M1 { };"]},
                 {"a/b/f2.proto",
                  ["message M2 {}"]},
                 {"x/y/z/f1.proto",
                  ["message M3 {}"]},
                 {"f4.proto",
                  ["message M4 {}"]}],
                []),
    [{file, {"c/f1", "c/f1.proto"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M1'}, _},

     {file, {"f2", "f2.proto"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M2'}, _},

     {file, {"z/f1", "z/f1.proto"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M3'}, _},

     {file, {"f4", "f4.proto"}},
     {{msg_containment,_}, _},
     {{enum_containment,_}, _},
     {{msg,'M4'}, _}] = AllDefs.

fetches_imports_test() ->
    {ok, Elems} = parse_lines(["package p1;"
                               "import \"a/b/c.proto\";",
                               "import 'd/e/f.proto';",
                               "message m1 { required uint32 x = 1; }",
                               "enum    e1 { a = 17; }"]),
    ["a/b/c.proto", "d/e/f.proto"] = gpb_defs:fetch_imports(Elems).

verify_ignores_import_statements_test() ->
    ok = do_parse_verify_defs(["import \"Y.proto\";",
                               "message m2 { required uint32 x = 1; }"]).


verify_succeeds_for_defined_ref_in_message_test() ->
    ok = do_parse_verify_defs(["message m1 { required m2     x = 1; }",
                               "message m2 { required uint32 x = 1; }"]).

verify_catches_missing_ref_in_message_test() ->
    {error, [{ref_to_undefined_msg_or_enum, _}]} = Error =
        do_parse_verify_defs(["message m1 { required m2 f1 = 1; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "m2"]).

verify_succeeds_for_good_enum_default_value_test() ->
    ok = do_parse_verify_defs(
           ["enum e { e1 = 1; e2 = 2; }"
            "message m1 { required e f1 = 1 [default=e2]; }"]).

verify_catches_undefined_enum_value_in_default_test() ->
    {error, [_]} = Error = do_parse_verify_defs(
                             ["enum e { e1 = 1; e2 = 2; }"
                              "message m1 { required e f1 = 1 [default=e3];}"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "e3"]).

verify_succeeds_for_valid_integer_in_default_test() ->
    ok = do_parse_verify_defs(
           ["message m1 { required uint32 f1 = 1 [default=343]; }"]).

verify_catches_invalid_integer_in_default_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 f1 = 1 [default=-1]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "-1"]).

verify_catches_invalid_integer_in_default_2_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 f1 = 1 [default=e3]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "e3"]).

verify_catches_invalid_integer_in_default_3_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 f1 = 1 [default=\"abc\"]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1"]).

verify_succeeds_for_valid_string_in_default_test() ->
    ok = do_parse_verify_defs(
           ["message m1 { required string f1 = 1 [default=\"abc\"]; }"]),
    ok = do_parse_verify_defs(
           ["message m1 { required string f1 = 1 [default='abc']; }"]).

verify_catches_invalid_string_in_default_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required string f1 = 1 [default=344]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "344"]).

verify_succeeds_for_valid_float_in_default_test() ->
    ok = do_parse_verify_defs(
           ["message m1 { required float f1 = 1 [default=1.1]; }"]).

verify_succeeds_for_valid_int_as_double_in_default_test() ->
    ok = do_parse_verify_defs(
           ["message m1 { required double f1 = 1 [default=1]; }"]).

verify_catches_invalid_float_in_default_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required float f1 = 1 [default=\"abc\"]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "abc"]).

verify_succeeds_for_bool_in_default_test() ->
    ok = do_parse_verify_defs(
           ["message m1 { required bool f1 = 1 [default=true]; }"]),
    ok = do_parse_verify_defs(
           ["message m1 { required bool f1 = 1 [default=false]; }"]),
    ok = do_parse_verify_defs(
           ["message m1 { required bool f1 = 1 [default=1]; }"]),
    ok = do_parse_verify_defs(
           ["message m1 { required bool f1 = 1 [default=0]; }"]).

verify_catches_invalid_bool_in_default_1_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=\"abc\"]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "abc"]).

verify_catches_invalid_bool_in_default_2_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=TRUE]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "TRUE"]).

verify_catches_invalid_bool_in_default_3_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=2]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1"]).

verify_catches_invalid_bool_in_default_4_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=-1]; }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1"]).

verify_catches_invalid_rpc_return_type_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["enum e1 { a=1; }",
           "message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(m1) returns (e1); }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "e1", "return"]).

verify_catches_invalid_rpc_return_ref_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(m1) returns (m2); }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "m2", "return"]).

verify_catches_invalid_rpc_arg_type_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["enum e1 { a=1; }",
           "message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(e1) returns (m1); }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "e1", "arg"]).

verify_catches_invalid_rpc_arg_ref_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(m2) returns (m1); }"]),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "m2", "arg"]).

verify_hints_about_use_packages_option_test() ->
    {error, _} = Error1 =
        parse_several_file_lines(
          [{"a.proto", ["import \"b.proto\";",
                        "message MsgA { optional pkg.b.MsgB f1 = 1; }"]},
           {"b.proto", ["package pkg.b;",
                        "message MsgB { optional uint32 g = 1; }"]}],
          [],
          expect_error,
          verify_imports),
    Msg1 = verify_flat_string(gpb_parse:format_post_process_error(Error1)),
    verify_strings_present(Msg1, ["use_packages"]),
    %% Check when there are only refs from rpcs (different error values)
    {error, _} = Error2 =
        parse_several_file_lines(
          [{"a.proto", ["import \"b.proto\";",
                        "service S {",
                        "  rpc R(pkg.b.MsgB) returns (pkg.b.MsgB);",
                        "}"]},
           {"b.proto", ["package pkg.b;",
                        "message MsgB { optional uint32 g = 1; }"]}],
          [],
          expect_error,
          verify_imports),
    Msg2 = verify_flat_string(gpb_parse:format_post_process_error(Error2)),
    verify_strings_present(Msg2, ["use_packages"]),
    %% Check when there are only refs from extend (different error value)
    {error, _} = Error3 =
        parse_several_file_lines(
          [{"a.proto", ["import \"b.proto\";",
                        "extend pkg.b.msgB {",
                        "  optional uint32 f  = 4711;",
                        "}"]},
           {"b.proto", ["package pkg.b;",
                        "message MsgB { optional uint32 g = 1; }"]}],
          [],
          expect_error,
          verify_imports),
    Msg3 = verify_flat_string(gpb_parse:format_post_process_error(Error3)),
    verify_strings_present(Msg3, ["use_packages"]),
    %% Part of the heuristics for the hinting is that there are different
    %% package. Try two files, one imported, but with the same package.
    {error, _} = Error4 =
        parse_several_file_lines(
          [{"a.proto", ["import \"b.proto\";",
                        "package pkg.ab;",
                        "message MsgA { optional pkg.ab.BadMsgRef f1 = 1; }"]},
           {"b.proto", ["package pkg.ab;",
                        "message MsgB { optional uint32 g = 1; }"]}],
          [],
          expect_error,
          verify_imports),
    Msg4 = verify_flat_string(gpb_parse:format_post_process_error(Error4)),
    verify_strings_not_present(Msg4, ["use_packages"]),
    %% no hint about the option when it is already included
    {error, _} = Error5 =
        parse_several_file_lines(
          [{"a.proto", ["import \"b.proto\";",
                        "message MsgA { optional pkg.b.BadMsgRef f1 = 1; }"]},
           {"b.proto", ["package pkg.b;",
                        "message MsgB { optional uint32 g = 1; }"]}],
          [use_packages],
          expect_error,
          verify_imports),
    Msg5 = verify_flat_string(gpb_parse:format_post_process_error(Error5)),
    verify_strings_not_present(Msg5, ["use_packages"]).

verify_error_for_field_name_defined_twice_test() ->
    lists:foreach(
      fun({Id, ProtoLines, ErrorStringsToExpect}) ->
              io:format("Id=~p~n", [Id]),
              {error, _} = Error = do_parse_verify_defs(ProtoLines),
              Msg = gpb_defs:format_post_process_error(Error),
              verify_flat_string(Msg),
              if ErrorStringsToExpect /= [] ->
                      verify_strings_present(Msg, ErrorStringsToExpect);
                 true ->
                      ok
              end
      end,
      [{direct_field,
        ["message m1 {",
         "  required uint32 f1 = 1;",
         "  required uint32 f1 = 2;", % f1 used again
         "}"],
        ["m1", "f1"]},
       {defined_trice,
        ["message m1 {",
         "  required uint32 f1 = 1;",
         "  required uint32 f1 = 1;",
         "  required uint32 f1 = 1;",
         "}"],
        ["m1", "f1"]},
       {oneof_name_vs_simple_name,
        ["message m1 {",
         "  required uint32 f1 = 1;",
         "  oneof f1 {uint32 f2 = 2;}", % the oneof name, f1, used again
         "}"],
        ["m1", "f1"]},
       {oneof_names,
        ["message m1 {",
         "  oneof f1 {uint32 f2 = 1;}",
         "  oneof f1 {uint32 f3 = 2;}", % the oneof name, f1, used again
         "}"],
        ["m1", "f1"]},
       {oneof_field_names,
        ["message m1 {",
         "  oneof o1 {uint32 f1 = 1;}",
         "  oneof o2 {uint32 f1 = 2;}", % the oneof field, f1, used again
         "}"],
        ["m1", "f1"]}]).

verify_error_for_json_lowerCamelCased_field_name_defined_twice_test() ->
    ProtoLines =
        ["message m1 {",
         "  required uint32 some_name = 1;",
         "  required uint32 SomeName = 2;",
         "}"],
    Opts = [json],
    %% Should succeed if option json in not specified
    ok = do_parse_verify_defs(ProtoLines, []),
    %% Should fail  with the json option
    {error, _} = Error = do_parse_verify_defs(ProtoLines, Opts),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "some_name", "SomeName"]),
    %% Check also the json_name option
    ProtoLines2 =
        ["message m1 {",
         "  required uint32 some_name = 1;",
         "  required uint32 foo = 2 [json_name='some_name'];",
         "}"],
    {error, _} = Error2 = do_parse_verify_defs(ProtoLines2, Opts),
    Msg2 = verify_flat_string(gpb_defs:format_post_process_error(Error2)),
    verify_strings_present(Msg2, ["m1", "some_name", "foo"]),
    %% No error when json name and field name for a field coincides
    ProtoLines3 =
        ["message m1 {",
         "  required uint32 foo = 1;",
         "}"],
    ok = do_parse_verify_defs(ProtoLines3, Opts).

verify_valid_json_name_field_option_value_test() ->
    ProtoLines1 = ["message m1 {",
                   "  required uint32 f1 = 1 [json_name=10];",
                   "}"],
    Opts = [json],
    {error, _} = Error1 = do_parse_verify_defs(ProtoLines1, Opts),
    Msg1 = verify_flat_string(gpb_defs:format_post_process_error(Error1)),
    verify_strings_present(Msg1, ["m1", "f1"]),

    ProtoLines2 = ["message m1 {",
                   "  required uint32 f2 = 2 [json_name=false];",
                   "}"],
    {error, _} = Error2 = do_parse_verify_defs(ProtoLines2, Opts),
    Msg2 = verify_flat_string(gpb_defs:format_post_process_error(Error2)),
    verify_strings_present(Msg2, ["m1", "f2"]).

verify_error_for_field_number_defined_twice_test() ->
    lists:foreach(
      fun({Id, ProtoLines, ErrorStringsToExpect}) ->
              io:format("Id=~p~n", [Id]),
              {error, _} = Error = do_parse_verify_defs(ProtoLines),
              Msg = gpb_defs:format_post_process_error(Error),
              verify_flat_string(Msg),
              if ErrorStringsToExpect /= [] ->
                      verify_strings_present(Msg, ErrorStringsToExpect);
                 true ->
                      ok
              end
      end,
      [{direct_field,
        ["message m1 {",
         "  required uint32 f1 = 77;",
         "  required uint32 f2 = 77;", % 1 used again
         "}"],
        ["m1", "f1", "f2", "77"]},
       {defined_trice,
        ["message m1 {",
         "  required uint32 f1 = 77;",
         "  required uint32 f2 = 77;",
         "  required uint32 f3 = 77;",
         "}"],
        ["m1", "f1", "f2", "f3", "77"]},
       {oneof_field_vs_direct_field,
        ["message m1 {",
         "  required uint32 f1 = 77;",
         "  oneof f1 {uint32 f2 = 77;}",
         "}"],
        ["m1", "f1", "f2", "77"]},
       {oneof_fields,
        ["message m1 {",
         "  oneof o1 {uint32 f1 = 77;}",
         "  oneof o2 {uint32 f2 = 77;}",
         "}"],
        ["m1", "f1", "f2", "77"]}]).

verify_error_for_non_positive_field_number_test() ->
    ProtoLines = ["message m1 {"
                  "  required uint32 f1 = 0;",
                  "  required uint32 f2 = -1;",
                  "}"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "f2"]).

verify_error_for_message_already_defined_test() ->
    ProtoLines = ["message m1 { required uint32 f1 = 1; }",
                  "message m1 { required uint32 f2 = 2; }"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1"]).

verify_error_for_enum_already_defined_test() ->
    ProtoLines = ["enum e1 { a=0; }",
                  "enum e1 { b=1; }"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["e1"]).

verify_enum_must_have_at_least_one_value_test() ->
    ProtoLines = ["enum e1 { }"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["e1"]).

verify_error_for_rpc_name_defined_twice_test() ->
    ProtoLines =
        ["message m1 { required uint32 f1 = 1; }",
         "service s1 {"
         "  rpc req1(m1) returns (m1);",
         "  rpc req1(m1) returns (m1);",
         "}"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req1"]).

verify_error_for_services_already_defined_test() ->
    ProtoLines = ["message m1 { required uint32 f = 1; }",
                  "service s1 { rpc req1(m1) returns (m1); }",
                  "service s1 { rpc req2(m1) returns (m1); }"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1"]).

verify_multiple_errors_caught_test() ->
    ProtoLines = ["message m1 {"
                  "  required uint32 f1 = -77;",
                  "  required uint32 f1 = -77;",
                  "}"],
    {error, _} = Error = do_parse_verify_defs(ProtoLines),
    Msg = verify_flat_string(gpb_defs:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "-77"]).

do_parse_verify_defs(Lines) ->
    do_parse_verify_defs(Lines, []).

do_parse_verify_defs(Lines, Opts) ->
    {ok, Elems} = parse_lines(Lines),
    case post_process(Elems, Opts) of
        {ok, _} ->
            ok;
        {error, Reasons} ->
            {error, Reasons}
    end.

verify_flat_string(S) when is_list(S) ->
    case lists:all(fun is_integer/1, S) of
        true  -> S;
        false -> erlang:error(badstring, [S])
    end.

verify_strings_present(Str, StringsToTestFor) ->
    case [ToTest || ToTest <- StringsToTestFor, not is_present(Str, ToTest)] of
        []      -> ok;
        Missing -> erlang:error(missing_substring, [Missing, Str])
    end.

verify_strings_not_present(Str, StringsToTestFor) ->
    case [ToTest || ToTest <- StringsToTestFor, is_present(Str, ToTest)] of
        []      -> ok;
        Present -> erlang:error(unexpectedly_found, [Present, Str])
    end.

is_present(Str, ToTest) -> gpb_lib:is_substr(ToTest, Str).

%% test helpers
parse_sort_several_file_lines(ProtoLines, Opts) ->
    lists:sort(parse_several_file_lines(ProtoLines, Opts)).

parse_several_file_lines(ProtoLines, Opts) ->
    parse_several_file_lines(ProtoLines, Opts,
                             expect_success, filter_away_import_lines).

parse_several_file_lines(ProtoLines, Opts,
                         ExpectedResult, ImportLineHandling) ->
    {AllProtoNames, _AllLines} = lists:unzip(ProtoLines),
    AllProtoBases = lists:map(fun filename:basename/1, AllProtoNames),
    AllDefs1 = [begin
                    Lines1 = case ImportLineHandling of
                                 filter_away_import_lines ->
                                     filter_away_import_lines(Lines,
                                                              AllProtoBases);
                                 verify_imports ->
                                     verify_imports(Lines, AllProtoBases)
                             end,
                    {ok, Defs1} = parse_lines(Lines1),
                    {ok, Defs2} = gpb_defs:post_process_one_file(
                                    FName, Defs1, Opts),
                    Defs2
                end
                || {FName, Lines} <- ProtoLines],
    case ExpectedResult of
        expect_success ->
            {ok, AllDefs2} = gpb_parse:post_process_all_files(
                               lists:append(AllDefs1),
                               Opts),
            AllDefs2;
        expect_error ->
            {error, Reasons} = gpb_parse:post_process_all_files(
                                 lists:append(AllDefs1),
                                 Opts),
            {error, Reasons}
    end.

filter_away_import_lines(Lines, AllProtoNames) ->
    case extract_check_imports(Lines, AllProtoNames) of
        {ok, RestLines} ->
            RestLines;
        {error, Reason} ->
            error({bad_test, Reason})
    end.

verify_imports(Lines, AllProtoNames) ->
    case extract_check_imports(Lines, AllProtoNames) of
        {ok, _RestLines} ->
            Lines;
        {error, Reason} ->
            error({bad_test, Reason})
    end.

extract_check_imports(Lines, AllProtoNames) ->
    {ImportLines, RestLines} = lists:partition(fun is_import_line/1, Lines),
    Imports = lists:map(fun protobase_by_importline/1, ImportLines),
    StrayImports = lists:filter(
                     fun(I) -> not lists:member(I, AllProtoNames) end,
                     Imports),
    if StrayImports == [] -> {ok, RestLines};
       true -> {error, {stray_import_of_missing_proto, Imports, Lines}}
    end.

is_import_line("import \""++_) -> true;
is_import_line(_) -> false.

protobase_by_importline(Line) ->
    ["import"++_, ImportTxt | _] = gpb_lib:string_lexemes(Line, "\""),
    filename:basename(ImportTxt).


parse_lines(Lines) ->
    S = binary_to_list(iolist_to_binary([[L,"\n"] || L <- Lines])),
    case gpb_scan:string(S) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens++[{'$end',length(Lines)+1}]) of
                {ok, Result} ->
                    {ok, Result};
                {error, {LNum,_Module,EMsg}=Reason} ->
                    io:format("Parse error on line ~w:~n  ~p~n",
                              [LNum, {Tokens,EMsg}]),
                    erlang:error({parse_error,Lines,Reason})
            end;
        {error,Reason} ->
            io:format("Scan error:~n  ~p~n", [Reason]),
            erlang:error({scan_error,Lines,Reason})
    end.

do_process_sort_defs(Defs) ->
    do_process_sort_defs(Defs, []).

do_process_sort_defs(Defs, Opts) ->
    {ok, Defs2} = post_process(Defs, Opts),
    lists:sort(Defs2).

post_process(Elems, Opts) ->
    {ok, Elems2} = gpb_defs:post_process_one_file("y", Elems, Opts),
    gpb_defs:post_process_all_files(Elems2, Opts).

do_process_sort_several_defs(ListOfDefs) ->
    do_process_sort_several_defs(ListOfDefs, []).

do_process_sort_several_defs(ListOfDefs, Opts) ->
    lists:sort(do_process_several_defs(ListOfDefs, Opts)).

do_process_several_defs(ListOfDefs, Opts) ->
    AllElems =
        lists:append(
          [begin
               Filename = "z" ++ integer_to_list(I),
               {ok, Elems2} = gpb_defs:post_process_one_file(
                                Filename, Elems, Opts),
               Elems2
           end
           || {I, Elems} <- gpb_lib:index_seq(ListOfDefs)]),
    {ok, Defs2} = gpb_defs:post_process_all_files(AllElems, Opts),
    Defs2.
