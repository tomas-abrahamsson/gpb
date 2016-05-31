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
                               #field{name=a2, rnum=2}]}]}] =
        do_process_sort_defs(Defs).

parses_default_value_test() ->
    {ok, [{{msg,'Msg'}, [#?gpb_field{name=x, type=uint32, fnum=1,
                                     occurrence=optional,
                                     opts=[{default,12}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1 [default = 12];",
           "}"]).

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
     {{msg,'m1.m5'},    [#?gpb_field{name=fx}]}] =
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
     {{msg,'t1.t2'}, [#?gpb_field{}]}] =
        do_process_sort_defs(Elems).

parses_circular_messages_test() ->
    {ok, Elems} = parse_lines(["message m1 {",
                               "  optional m1 f = 1;",
                               "}"]),
    [{{msg,m1}, [#?gpb_field{name=f, type={msg,m1}}]}] =
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
    [{{enum,e1}, [{option, allow_alias, true}, {ee1,1},{ee2,1}]}] =
        do_process_sort_defs(Elems).

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
     {{msg,m3},       [#?gpb_field{name=b, type={msg,'m1.m2'}}]}] =
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
     {{msg,'m2.m4'},    [#?gpb_field{name=x}]}] =
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
     {{msg,m3},       [#?gpb_field{name=b, type={msg,'m1.m2'}}]}] =
        do_process_sort_defs(Elems).

resolve_map_valuetype_refs_test() ->
    {ok, Elems} = parse_lines(["message m1 {",
                               "  message m2 { required uint32 x = 1; }",
                               "  map<string, m2> b = 1;",
                               "}"]),
    [{{msg,m1},       [#?gpb_field{name=b, type={map,string,{msg,'m1.m2'}}}]},
     {{msg,'m1.m2'},  [#?gpb_field{name=x}]}] =
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
     {{msg,'p1.m3'},    [#?gpb_field{name=b, type={msg,'p1.m1.m2'}}]}] =
        do_process_sort_defs(Elems, [use_packages]).

package_can_appear_anywhere_toplevelwise_test() ->
    %% The google protoc seems accepts one package specifiers anywhere
    %% at the top-level. It must not be at the beginning,
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
     {{msg,'p1.m2'}, [#?gpb_field{}]}] = Defs =
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
     {{msg,'m1.m2'},  [#?gpb_field{name=x, fnum=1,  rnum=2}]}] =
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
                 #?gpb_field{name=f7, opts=[packed, {default,true}]}]}] =
        do_process_sort_defs(Defs).

parses_empty_msg_field_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1 []; }"]),
    [{{msg,m1}, [#?gpb_field{name=f1, opts=[]}]}] = do_process_sort_defs(Defs).

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
     {{msg,'m1.m2'}, [#?gpb_field{name=f2}]}] =
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
                                   occurrence=optional}]}] =
        do_process_sort_defs(Defs).

parses_nested_extending_msgs_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1 [default=17];",
                             "  extensions 200 to 299;",
                             "  extend m1 {",
                             "    optional uint32 f2=2;",
                             "  }",
                             "}"]),
    [{{extensions,m1},[{200,299}]},
     {{msg,m1},       [#?gpb_field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                                   occurrence=required},
                       #?gpb_field{name=f2, fnum=2, rnum=3, opts=[],
                                   occurrence=optional}]}] =
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
                                   occurrence=optional}]}] =
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
                   occurrence=optional}]}] =
        do_process_sort_defs(Defs, [use_packages]).

parses_service_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2);",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
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
     {{service,s1},[#?gpb_rpc{name=req,  input=m1, output=m2}]},
     {{service,s2},[#?gpb_rpc{name=req2, input=m2, output=m1}]}] =
        do_process_sort_defs(Defs).

parses_service_ignores_empty_method_option_braces_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2) {};",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{service,s1},[#?gpb_rpc{name=req, input=m1, output=m2}]}] =
        do_process_sort_defs(Defs).


parses_empty_toplevel_statement_test() ->
    {ok,Defs} = parse_lines(["; message m1 { required uint32 f1=1; }; ; "]),
    [{{msg,m1}, _}] = do_process_sort_defs(Defs).

parses_empty_message_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { ; ; required uint32 f1=1;;; }"]),
    [{{msg,m1}, [#?gpb_field{name=f1}]}] = do_process_sort_defs(Defs).

parses_empty_enum_statement_test() ->
    {ok,Defs} = parse_lines(["enum e1 { ; ; ee1=1;;; }"]),
    [{{enum,e1}, [{ee1,1}]}] = do_process_sort_defs(Defs).

parses_empty_service_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { ; ; rpc r1(m1) returns (m1);;; }"]),
    [{{msg,m1}, _},
     {{service,s1},[#?gpb_rpc{name=r1, input=m1, output=m1}]}] =
        do_process_sort_defs(Defs).

parses_empty_service_statement_method_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { rpc r1(m1) returns (m1){;;;}; }"]),
    [{{msg,m1}, _},
     {{service,s1},[#?gpb_rpc{name=r1, input=m1, output=m1}]}] =
        do_process_sort_defs(Defs).

proto3_no_occurrence_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  uint32 f1=1;",
                             "  repeated uint32 f2=2;",
                             "}"]),
    [{syntax,"proto3"},
     {{msg,m1},
      [#?gpb_field{name=f1,fnum=1,occurrence=required},
       #?gpb_field{name=f2,fnum=2,occurrence=repeated}]}] =
        do_process_sort_defs(Defs).

proto3_no_repeated_are_packed_by_default_test() ->
    {ok,Defs} = parse_lines(["syntax=\"proto3\";",
                             "message m1 {",
                             "  repeated uint32 f2=2;", % to be packed
                             "  repeated uint32 f3=3 [packed=false];",
                             "  repeated uint32 f4=4 [packed=true];",
                             "  repeated uint32 f5=5 [packed];",
                             "}"]),
    [{syntax,"proto3"},
     {{msg,m1},
      [#?gpb_field{name=f2,fnum=2,occurrence=repeated,opts=[packed]},
       #?gpb_field{name=f3,fnum=3,occurrence=repeated,opts=[]},
       #?gpb_field{name=f4,fnum=4,occurrence=repeated,opts=[packed]},
       #?gpb_field{name=f5,fnum=5,occurrence=repeated,opts=[packed]}
      ]}] =
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
     {{msg,p_m1}, [#?gpb_field{name=f1, type={enum,e1}}, #?gpb_field{name=fm2}]},
     {{msg,p_m2}, [#?gpb_field{type={msg,p_m1}}]}, %% type is a msg: to be prefixed
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=p_m1,  %% both argument ...
                 output=p_m2} %% ... and result msgs to be prefixed
      ]}] = do_process_sort_defs(Defs, [{msg_name_prefix, "p_"}]).

can_suffix_record_names_test() ->
    {ok, Defs} = parse_lines(["enum    e1 {a=1; b=2;}",
                              "message m1 {required e1 f1=1;}",
                              "message m2 {required m1 f2=1;}",
                              "service s1 {",
                              "  rpc req(m1) returns (m2) {};",
                              "}",
                              "extend m1 { optional uint32 fm2=2; }"]),
    [{{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{msg,m1_s}, [#?gpb_field{name=f1, type={enum,e1}}, #?gpb_field{name=fm2}]},
     {{msg,m2_s}, [#?gpb_field{type={msg,m1_s}}]}, %% type is a msg: to be prefixed
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
     {{service,svc1},
      [#?gpb_rpc{name=req,
                 input=msg1,  %% both argument ...
                 output=msg2} %% .. and result msgs to be to-lower
      ]}] = do_process_sort_defs(Defs, [msg_name_to_lower]).

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
           ["message m1 { required bool f1 = 1 [default=false]; }"]).

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
          ["message m1 { required bool f1 = 1 [default=1]; }"]),
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

is_present(Str, ToTest) -> string:str(Str, ToTest) > 0.

%% test helpers
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
    {ok, Elems2} = gpb_parse:post_process_one_file(Elems, Opts),
    gpb_parse:post_process_all_files(Elems2, Opts).

