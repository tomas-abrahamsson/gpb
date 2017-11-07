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

-module(gpb_parse_tests).

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
               fields=[#field{name=a1, fnum=1, type=uint32, occurrence=optional},
                       #field{name=a2, fnum=2, type=string, occurrence=optional}
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
    [{{msg,'Msg'}, [#gpb_oneof{
                       name=x,
                       rnum=2,
                       fields=[#field{name=a1, rnum=2},
                               #field{name=a2, rnum=2}]}]},
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
    [{{msg,m}, [#?gpb_field{name=b, type=bytes,
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
    [{{msg,m1},         [#?gpb_field{name=f2, type={msg,'m1.m2'}}]},
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
    [{{msg,m1},      [#gpb_oneof{
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
    [{{msg,m1}, [#?gpb_field{name=f, type={msg,m1}}]},
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
    [{{enum,e1}, [{option, allow_alias, true}, {ee1,1},{ee2,1}]},
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
    [{package,'google.protobuf'},
     {package,x},
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
     {{msg_options,'x.t.s'},[{[my_m_option],"s"}]}] =
        AllDefs.

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
    [{{enum,'m1.e1'}, [_]},
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
    [{{msg,m1},         [#?gpb_field{name=f1,type={msg,'m1.m2'}},
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
    [{import, _},
     {package, p1},
     {{enum,'m1.e1'}, _},
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
    [{{msg,m1},       [#?gpb_field{name=b, type={map,string,{msg,'m1.m2'}}}]},
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
    [{import, _},
     {package, p1},
     {{enum,'p1.m1.e1'}, _},
     {{msg,'p1.m1'},    [#?gpb_field{name=y, type={msg,'p1.m1.m2'}},
                         #?gpb_field{name=z, type={enum,'p1.m1.e1'}},
                         #?gpb_field{name=w}]},
     {{msg,'p1.m1.m2'}, [#?gpb_field{name=x}]},
     {{msg,'p1.m3'},    [#?gpb_field{name=b, type={msg,'p1.m1.m2'}}]},
     {{msg_containment,_}, ['p1.m1', 'p1.m1.m2', 'p1.m3']}] =
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
    [{package, p1},
     {{msg,'p1.m1'}, [#?gpb_field{}]},
     {{msg,'p1.m2'}, [#?gpb_field{}]},
     {{msg_containment,_}, _}] = Defs =
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
    [{{enum,'m1.e1'}, _},
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
                             "  required uint32 f6=5 [deprecated];",
                             "  required bool   f7=7 [packed,default=true];",
                             "}"]),
    [{{msg,m1}, [#?gpb_field{name=f1, opts=[packed, {default,1}]},
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
    [{{msg,m1}, [#?gpb_field{name=f1, opts=[]}]},
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
    [{{extensions,m1},[{100,199},{250,250},{300,300},{400,max}]},
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
    [{{extensions,m1},[{200,299}]},
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
    [{{extensions,m1},[{100,199}]},
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
    [{{extensions,m1},[{200,299}]},
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
    [{package,'p1.p2'},
     {{extensions,'p1.p2.m1'},[{200,299}]},
     {{msg,'p1.p2.m1'},
      [#?gpb_field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                   occurrence=required},
       #?gpb_field{name=f2, fnum=2, rnum=3, opts=[],
                   occurrence=optional}]},
     {{msg_containment,_}, ['p1.p2.m1']}] =
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
    [{{extensions,m1},[{200,299}]},
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
    [{package,bar},
     {package,foo},
     {{extensions,'foo.fm1'},[{100,199}]},
     {{msg,'foo.fm1'},
      [#?gpb_field{name=f, fnum=1, type=int32, occurrence=required},
       #?gpb_field{name=b1, fnum=100, type=string, occurrence=optional},
       #?gpb_field{name=b2, fnum=101, type={msg,'foo.fm2'},
                   occurrence=optional}]},
     {{msg,'foo.fm2'},
      [#?gpb_field{name=g, fnum=1, type=bool, occurrence=required}]},
     {{msg_containment,"bar"}, _}, % for bar.proto
     {{msg_containment,"foo"}, _}  % for foo.proto
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
    [{package,p},
     {package,'p.x'},
     {{extensions,'p.x.a'},[{200,max}]},
     {{msg,'p.err'}, [#?gpb_field{name=f}]},
     {{msg,'p.x.a'},
      [#?gpb_field{name=g, fnum=1, type=string, occurrence=optional},
       #?gpb_field{name=h, fnum=2, type={msg,'p.err'}, occurrence=optional},
       #?gpb_field{name=errs, fnum=200, type={msg,'p.err'},
                   occurrence=repeated}]},
     {{msg_containment,"p"}, _},
     {{msg_containment,"y"}, _}] =
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
    [{package,a},
     {package,b},
     {{extensions,'a.Foo'},[{200,max}]},
     {{msg,'a.Foo'}, [#?gpb_field{name=id},
                      #?gpb_field{name=b, type={msg,'b.Bar'}}]},
     {{msg,'b.Bar'}, [#?gpb_field{name=id}]},
     {{msg_containment,"a"}, _},
     {{msg_containment,"b"}, _}] =
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
    [{{group,'m1.g'},[#?gpb_field{name=gf,type=uint32,fnum=3,rnum=2,
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
    [{{group,'m1.g'},[#?gpb_field{name=gf,type=uint32,fnum=3,rnum=2,
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
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{service,s1},[#?gpb_rpc{name=req, input=m1, output=m2}]}] =
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
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{service,s1},[#?gpb_rpc{name=req,  input=m1, output=m2}]},
     {{service,s2},[#?gpb_rpc{name=req2, input=m2, output=m1}]}] =
        do_process_sort_defs(Defs).

parses_rpc_streams_and_options_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f1=1;}",
                             "service s1 {",
                             "  rpc r1(m1)        returns (m2);",
                             "  rpc r2(stream m1) returns (m2);",
                             "  rpc r3(m1)        returns (stream m2);",
                             "  rpc r4(stream m1) returns (stream m2);",
                             "  rpc ro(m1) returns (m2) { option a=1; };",
                             "  rpc ro(m1) returns (m2) { option (a).b=1; };",
                             "  rpc ro(m1) returns (m2) { option (a.b).c=1; };",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{service,s1},
      [#?gpb_rpc{name=r1, input_stream=false, output_stream=false},
       #?gpb_rpc{name=r2, input_stream=true, output_stream=false},
       #?gpb_rpc{name=r3, input_stream=false, output_stream=true},
       #?gpb_rpc{name=r4, input_stream=true, output_stream=true},
       #?gpb_rpc{name=ro, opts=[{'a',1}]},
       #?gpb_rpc{name=ro, opts=[{'a.b',1}]},
       #?gpb_rpc{name=ro, opts=[{'a.b.c',1}]}]=Rpcs}] =
        do_process_sort_defs(Defs),
    %% Check all input(arg)/output(return) messages too
    [{m1,m2}] = lists:usort([{A,R} || #?gpb_rpc{input=A,output=R} <- Rpcs]).

parses_service_ignores_empty_method_option_braces_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2) {};",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{msg_containment,_}, _},
     {{service,s1},[#?gpb_rpc{name=req, input=m1, output=m2}]}] =
        do_process_sort_defs(Defs).


parses_empty_toplevel_statement_test() ->
    {ok,Defs} = parse_lines(["; message m1 { required uint32 f1=1; }; ; "]),
    [{{msg,m1}, _},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_empty_message_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { ; ; required uint32 f1=1;;; }"]),
    [{{msg,m1}, [#?gpb_field{name=f1}]},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_empty_enum_statement_test() ->
    {ok,Defs} = parse_lines(["enum e1 { ; ; ee1=1;;; }"]),
    [{{enum,e1}, [{ee1,1}]},
     {{msg_containment,_}, _}] = do_process_sort_defs(Defs).

parses_empty_service_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { ; ; rpc r1(m1) returns (m1);;; }"]),
    [{{msg,m1}, _},
     {{msg_containment,_}, _},
     {{service,s1},[#?gpb_rpc{name=r1, input=m1, output=m1}]}] =
        do_process_sort_defs(Defs).

parses_empty_service_statement_method_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { rpc r1(m1) returns (m1){;;;}; ",
                             "             rpc r2(m1) returns (m1); }",
                             "service s2 { rpc r5(m1) returns (m1){}",
                             "             rpc r6(m1) returns (m1){} }"]),
    [{{msg,m1}, _},
     {{msg_containment,_}, _},
     {{service,s1},[#?gpb_rpc{name=r1, input=m1, output=m1},
                    #?gpb_rpc{name=r2, input=m1, output=m1}]},
     {{service,s2},[#?gpb_rpc{name=r5, input=m1, output=m1},
                    #?gpb_rpc{name=r6, input=m1, output=m1}]}] =
        do_process_sort_defs(Defs).

proto3_no_occurrence_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  uint32 f1=1;",
                             "  repeated uint32 f2=2;",
                             "}"]),
    [{proto3_msgs,[m1]},
     {syntax,"proto3"},
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
    [{proto3_msgs,[m1,s1]},
     {syntax,"proto3"},
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
    [{proto3_msgs,[m1]},
     {syntax,"proto3"},
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
    [{proto3_msgs,[m1,subm]},
     {syntax,"proto3"},
     {{enum,ee},_},
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
    [{proto3_msgs,[m1]},
     {syntax,"proto2"},
     {syntax,"proto3"},
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
    [{proto3_msgs,[m1,'m1.m2']},
     {syntax,"proto3"},
     {{msg,m1},      [#?gpb_field{name=f1}]},
     {{msg,'m1.m2'}, [#?gpb_field{name=f2}]},
     {{msg_containment,_}, _},
     {{reserved_names,m1},       ["foo","bar"]},
     {{reserved_numbers,m1},     [2,15,{9,11}]},
     {{reserved_numbers,'m1.m2'},[17]}] =
        do_process_sort_defs(Defs).

fetches_imports_test() ->
    {ok, Elems} = parse_lines(["package p1;"
                               "import \"a/b/c.proto\";",
                               "import 'd/e/f.proto';",
                               "message m1 { required uint32 x = 1; }",
                               "enum    e1 { a = 17; }"]),
    ["a/b/c.proto", "d/e/f.proto"] = gpb_parse:fetch_imports(Elems).

can_prefix_record_names_test() ->
    {ok, Defs} = parse_lines(["enum    e1 {a=1; b=2;}",
                              "message m1 {required e1 f1=1;}",
                              "message m2 {required m1 f2=1;}",
                              "service s1 {",
                              "  rpc req(m1) returns (m2) {};",
                              "}",
                              "extend m1 { optional uint32 fm2=2; }"]),
    [{{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{msg,p_m1}, [#?gpb_field{name=f1, type={enum,e1}},
                   #?gpb_field{name=fm2}]},
     %% type is a msg: to be prefixed
     {{msg,p_m2}, [#?gpb_field{type={msg,p_m1}}]},
     {{msg_containment,_}, _},
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=p_m1,  %% both argument ...
                 output=p_m2} %% ... and result msgs to be prefixed
      ]}] = do_process_sort_defs(Defs, [{msg_name_prefix, "p_"}]).

can_prefix_record_names_by_proto_test() ->
    Defs = parse_sort_several_file_lines(
             [{"proto1.proto", ["enum    e1 {a=1; b=2;}",
                                "message m1 {required e1 f1=1;}"]},
              {"proto2.proto", ["message m2 {required m1 f2=1;}",
                                "service s1 {",
                                "  rpc req(m1) returns (m2) {};",
                                "}",
                                "extend m1 { optional uint32 fm2=2; }"]},
              {"proto3.proto", ["message m3 {map<string, m1> m=1;}"]}],
             [{msg_name_prefix,
               {by_proto, [{proto1, "p1_"},
                           {proto2, "p2_"}]}}]),
    [{{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{msg,m3},   [#?gpb_field{name=m, type={map,string,{msg,p1_m1}}}]},
     {{msg,p1_m1}, [#?gpb_field{name=f1, type={enum,e1}},
                    #?gpb_field{name=fm2}]},
     %% type is a msg: to be prefixed
     {{msg,p2_m2}, [#?gpb_field{type={msg,p1_m1}}]},
     {{msg_containment,"proto1"}, [p1_m1]},
     {{msg_containment,"proto2"}, [p2_m2]},
     {{msg_containment,"proto3"}, [m3]},
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=p1_m1,  %% both argument ...
                 output=p2_m2} %% ... and result msgs to be prefixed
      ]}] = Defs.

can_suffix_record_names_test() ->
    {ok, Defs} = parse_lines(["enum    e1 {a=1; b=2;}",
                              "message m1 {required e1 f1=1;}",
                              "message m2 {required m1 f2=1;}",
                              "service s1 {",
                              "  rpc req(m1) returns (m2) {};",
                              "}",
                              "extend m1 { optional uint32 fm2=2; }"]),
    [{{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{msg,m1_s}, [#?gpb_field{name=f1, type={enum,e1}},
                   #?gpb_field{name=fm2}]},
     %% type is a msg: to be suffixed
     {{msg,m2_s}, [#?gpb_field{type={msg,m1_s}}]},
     {{msg_containment,_}, [m1_s, m2_s]},
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=m1_s,  %% both argument ...
                 output=m2_s} %% .. and result msgs to be prefixed
      ]}] = do_process_sort_defs(Defs, [{msg_name_suffix, "_s"}]).

can_tolower_record_names_test() ->
    {ok, Defs} = parse_lines(["message Msg1 {required Msg2   f1=1;}",
                              "message Msg2 {required uint32 g1=1;}",
                              "service Svc1 {",
                              "  rpc req(Msg1) returns (Msg2) {};",
                              "}",
                              "extend Msg1 { optional uint32 fm2=2; }"]),
    [{{msg,msg1}, [#?gpb_field{name=f1, type={msg,msg2}},
                   #?gpb_field{name=fm2}]},
     {{msg,msg2}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, [msg1, msg2]},
     {{service,svc1},
      [#?gpb_rpc{name=req,
                 input=msg1,  %% both argument ...
                 output=msg2} %% .. and result msgs to be to-lower
      ]}] = do_process_sort_defs(Defs, [msg_name_to_lower]).

can_tolower_record_names_with_oneof_test() ->
    {ok, Defs} = parse_lines(["message Msg1 {",
                              "  oneof u {",
                              "    Msg1   a = 1;",
                              "    uint32 b = 2;",
                              "  }",
                              "}"]),
    [{{msg,msg1}, [#gpb_oneof{fields=[#?gpb_field{name=a,type={msg,msg1}},
                                      #?gpb_field{name=b}]}]},
     {{msg_containment,_}, [msg1]}] =
        do_process_sort_defs(Defs, [msg_name_to_lower]).

can_tolower_record_names_with_map_test() ->
    {ok, Defs} = parse_lines(["message Msg1 {",
                              "  required uint32 f = 1;",
                              "}"
                              "message Msg2 {"
                              "  map<string,Msg1> m = 1;",
                              "}"]),
    [{{msg,msg1}, [#?gpb_field{}]},
     {{msg,msg2}, [#?gpb_field{type={map,string,{msg,msg1}}}]},
     {{msg_containment,_}, [msg1, msg2]}] =
        do_process_sort_defs(Defs, [msg_name_to_lower]).

can_to_snake_record_names_test() ->
    {ok, Defs} = parse_lines(["message MsgName1 {required MsgName2   f1=1;}",
                              "message MsgName2 {required uint32 g1=1;}",
                              "service SvcName1 {",
                              "  rpc req(MsgName1) returns (MsgName2) {};",
                              "}",
                              "extend MsgName1 { optional uint32 fm2=2; }"]),
    [{{msg,msg_name_1}, [#?gpb_field{name=f1, type={msg,msg_name_2}},
                   #?gpb_field{name=fm2}]},
     {{msg,msg_name_2}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, [msg_name_1, msg_name_2]},
     {{service,svc_name_1},
      [#?gpb_rpc{name=req,
                 input=msg_name_1,  %% both argument ...
                 output=msg_name_2} %% .. and result msgs to be to-lower
      ]}] = do_process_sort_defs(Defs, [msg_name_to_snake_case]).

can_tolower_record_names_with_packages_test() ->
    {ok, Defs} = parse_lines(["package Pkg1;",
                              "message Msg1 {required Msg2   f1=1;}",
                              "message Msg2 {required uint32 g1=1;}",
                              "service Svc1 {",
                              "  rpc req(Msg1) returns (Msg2) {};",
                              "}",
                              "extend Msg1 { optional uint32 fm2=2; }"]),
    [{package, 'pkg1'},
     {{msg,'pkg1.msg1'}, [#?gpb_field{name=f1, type={msg,'pkg1.msg2'}},
                          #?gpb_field{name=fm2}]},
     {{msg,'pkg1.msg2'}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, ['pkg1.msg1','pkg1.msg2']},
     {{service,'pkg1.svc1'},
      [#?gpb_rpc{name=req,
                 input='pkg1.msg1',  %% both argument ...
                 output='pkg1.msg2'} %% .. and result msgs to be to-lower
      ]}] = do_process_sort_defs(Defs, [msg_name_to_lower, use_packages]).

verify_ignores_import_statements_test() ->
    ok = do_parse_verify_defs(["import \"Y.proto\";",
                               "message m2 { required uint32 x = 1; }"]).


verify_succeeds_for_defined_ref_in_message_test() ->
    ok = do_parse_verify_defs(["message m1 { required m2     x = 1; }",
                               "message m2 { required uint32 x = 1; }"]).

verify_catches_missing_ref_in_message_test() ->
    {error, [{ref_to_undefined_msg_or_enum, _}]} = Error =
        do_parse_verify_defs(["message m1 { required m2 f1 = 1; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "m2"]).

verify_succeeds_for_good_enum_default_value_test() ->
    ok = do_parse_verify_defs(
           ["enum e { e1 = 1; e2 = 2; }"
            "message m1 { required e f1 = 1 [default=e2]; }"]).

verify_catches_undefined_enum_value_in_default_test() ->
    {error, [_]} = Error = do_parse_verify_defs(
                             ["enum e { e1 = 1; e2 = 2; }"
                              "message m1 { required e f1 = 1 [default=e3];}"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "e3"]).

verify_succeeds_for_valid_integer_in_default_test() ->
    ok = do_parse_verify_defs(
           ["message m1 { required uint32 f1 = 1 [default=343]; }"]).

verify_catches_invalid_integer_in_default_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 f1 = 1 [default=-1]; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "-1"]).

verify_catches_invalid_integer_in_default_2_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 f1 = 1 [default=e3]; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "e3"]).

verify_catches_invalid_integer_in_default_3_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 f1 = 1 [default=\"abc\"]; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
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
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
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
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
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
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "abc"]).

verify_catches_invalid_bool_in_default_2_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=TRUE]; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1", "TRUE"]).

verify_catches_invalid_bool_in_default_3_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=2]; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1"]).

verify_catches_invalid_bool_in_default_4_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required bool f1 = 1 [default=-1]; }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["m1", "f1"]).

verify_catches_invalid_rpc_return_type_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["enum e1 { a=1; }",
           "message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(m1) returns (e1); }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "e1", "return"]).

verify_catches_invalid_rpc_return_ref_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(m1) returns (m2); }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "m2", "return"]).

verify_catches_invalid_rpc_arg_type_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["enum e1 { a=1; }",
           "message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(e1) returns (m1); }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "e1", "arg"]).

verify_catches_invalid_rpc_arg_ref_test() ->
    {error, [_]} = Error =
        do_parse_verify_defs(
          ["message m1 { required uint32 x = 1; }",
           "service s1 { rpc req(m2) returns (m1); }"]),
    Msg = verify_flat_string(gpb_parse:format_post_process_error(Error)),
    verify_strings_present(Msg, ["s1", "req", "m2", "arg"]).

do_parse_verify_defs(Lines) ->
    {ok, Elems} = parse_lines(Lines),
    case post_process(Elems, []) of
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

is_present(Str, ToTest) -> gpb_lib:is_substr(ToTest, Str).

%% test helpers
parse_sort_several_file_lines(ProtoLines, Opts) ->
    {AllProtoNames, _AllLines} = lists:unzip(ProtoLines),
    AllProtoBases = lists:map(fun filename:basename/1, AllProtoNames),
    AllDefs1 = [begin
                    {ok, Defs1} = parse_lines(
                                    filter_away_import_lines(
                                      Lines, AllProtoBases)),
                    {ok, Defs2} = gpb_parse:post_process_one_file(
                                    FName, Defs1, Opts),
                    Defs2
                end
                || {FName, Lines} <- ProtoLines],
    {ok, AllDefs2} = gpb_parse:post_process_all_files(
                       lists:append(AllDefs1),
                       Opts),
    lists:sort(AllDefs2).

filter_away_import_lines(Lines, AllProtoNames) ->
    {ImportLines, RestLines} = lists:partition(fun is_import_line/1, Lines),
    Imports = lists:map(fun protobase_by_importline/1, ImportLines),
    StrayImports = lists:filter(
                     fun(I) -> not lists:member(I, AllProtoNames) end,
                     Imports),
    if StrayImports == [] -> RestLines;
       true -> error({bad_test, stray_import_of_missing_proto, Imports, Lines})
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
    {ok, Elems2} = gpb_parse:post_process_one_file("y", Elems, Opts),
    gpb_parse:post_process_all_files(Elems2, Opts).

do_process_sort_several_defs(ListOfDefs) ->
    do_process_sort_several_defs(ListOfDefs, []).

do_process_sort_several_defs(ListOfDefs, Opts) ->
    AllElems =
        lists:append(
          [begin
               {ok, Elems2} = gpb_parse:post_process_one_file("z", Elems, Opts),
               Elems2
           end
           || Elems <- ListOfDefs]),
    {ok, Defs2} = gpb_parse:post_process_all_files(AllElems, Opts),
    lists:sort(Defs2).
