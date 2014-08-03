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

parses_simple_msg_test() ->
    {ok, [{{msg,'Msg'}, [#field{name=x, type=uint32, fnum=1,
                                occurrence=optional, opts=[]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1;",
           "}"]).

parses_default_value_test() ->
    {ok, [{{msg,'Msg'}, [#field{name=x, type=uint32, fnum=1,
                                occurrence=optional, opts=[{default,12}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1 [default = 12];",
           "}"]).

parses_string_concatenation_test() ->
    {ok, [{{msg,'Msg'}, [#field{name=x, type=string, fnum=1,
                                occurrence=optional,
                                opts=[{default,"abc"}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional string x=1 [default='a''b' 'c'];", %% = "abc" hopefully
           "}"]).

parses_nested_messages_test() ->
    {ok, [{{msg,'Msg'}, [{{msg,'Msg2'},[#field{name=x}]},
                         #field{name=y}]}]} =
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
    [{{msg,m1},         [#field{name=f2, type={msg,'m1.m2'}}]},
     {{msg,'m1.m2'},    []},
     {{msg,'m1.m2.m3'}, [#field{name=f5, type={msg,'m1.m2.m5'}},
                         #field{name=f6, type={msg,'m1.m2.m5'}}]},
     {{msg,'m1.m2.m5'}, [#field{name=fy}]},
     {{msg,'m1.m5'},    [#field{name=fx}]}] =
        do_process_sort_defs(Elems).

parses_circular_messages_test() ->
    {ok, Elems} = parse_lines(["message m1 {",
                               "  optional m1 f = 1;",
                               "}"]),
    [{{msg,m1}, [#field{name=f, type={msg,m1}}]}] =
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
                         #field{name=ef}]}]} =
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
    {ok, [{{msg,'Msg3'}, [#field{name=y, type={ref,['Msg','.','Msg2']}}]}]} =
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
     {{msg,m1},       [#field{name=y, type={msg,'m1.m2'}},
                       #field{name=z, type={msg,'m1.m2'}},
                       #field{name=w, type={enum,'m1.e1'}}]},
     {{msg,'m1.m2'},  [#field{name=x}]},
     {{msg,m3},       [#field{name=b, type={msg,'m1.m2'}}]}] =
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
    [{{msg,m1},         [#field{name=f1,type={msg,'m1.m2'}},
                         #field{name=f2,type={msg,'m1.m2.m3'}},
                         #field{name=f3,type={msg,'m2.m4'}}]},
     {{msg,'m1.m2'},    []},
     {{msg,'m1.m2.m3'}, [#field{name=y}]},
     {{msg,m2},         []},
     {{msg,'m2.m4'},    [#field{name=x}]}] =
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
     {{msg,m1},       [#field{name=y, type={msg,'m1.m2'}},
                       #field{name=z, type={enum,'m1.e1'}},
                       #field{name=w}]},
     {{msg,'m1.m2'},  [#field{name=x}]},
     {{msg,m3},       [#field{name=b, type={msg,'m1.m2'}}]}] =
        do_process_sort_defs(Elems).

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
     {{msg,'p1.m1'},    [#field{name=y, type={msg,'p1.m1.m2'}},
                         #field{name=z, type={enum,'p1.m1.e1'}},
                         #field{name=w}]},
     {{msg,'p1.m1.m2'}, [#field{name=x}]},
     {{msg,'p1.m3'},    [#field{name=b, type={msg,'p1.m1.m2'}}]}] =
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
     {{msg,'p1.m1'}, [#field{}]},
     {{msg,'p1.m2'}, [#field{}]}] = Defs =
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
     {{msg,m1},       [#field{name=y, fnum=11, rnum=2},
                       #field{name=z, fnum=12, rnum=3}]},
     {{msg,'m1.m2'},  [#field{name=x, fnum=1,  rnum=2}]}] =
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
    [{{msg,m1}, [#field{name=f1, opts=[packed, {default,1}]},
                 #field{name=f2, opts=[]},
                 #field{name=f3, opts=[packed, {default,2}]},
                 #field{name=f4, opts=[deprecated]},
                 #field{name=f5, opts=[]},
                 #field{name=f6, opts=[deprecated]},
                 #field{name=f7, opts=[packed, {default,true}]}]}] =
        do_process_sort_defs(Defs).

parses_empty_msg_field_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1 []; }"]),
    [{{msg,m1}, [#field{name=f1, opts=[]}]}] = do_process_sort_defs(Defs).

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
     {{msg,m1},      [#field{name=f1}]},
     {{msg,'m1.m2'}, [#field{name=f2}]}] =
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
     {{msg,m1},       [#field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                              occurrence=required},
                       #field{name=f2, fnum=2, rnum=3, opts=[],
                              occurrence=optional}]}] =
        do_process_sort_defs(Defs).

parses_service_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2);",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{service,s1},[{rpc,req,m1,m2}]}] = do_process_sort_defs(Defs).

parses_service_ignores_empty_method_option_braces_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2) {};",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{service,s1},[{rpc,req,m1,m2}]}] = do_process_sort_defs(Defs).


parses_empty_toplevel_statement_test() ->
    {ok,Defs} = parse_lines(["; message m1 { required uint32 f1=1; }; ; "]),
    [{{msg,m1}, _}] = do_process_sort_defs(Defs).

parses_empty_message_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { ; ; required uint32 f1=1;;; }"]),
    [{{msg,m1}, [#field{name=f1}]}] = do_process_sort_defs(Defs).

parses_empty_enum_statement_test() ->
    {ok,Defs} = parse_lines(["enum e1 { ; ; ee1=1;;; }"]),
    [{{enum,e1}, [{ee1,1}]}] = do_process_sort_defs(Defs).

parses_empty_service_statement_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { ; ; rpc r1(m1) returns (m1);;; }"]),
    [{{msg,m1}, _},
     {{service,s1},[{rpc,r1,m1,m1}]}] = do_process_sort_defs(Defs).

parses_empty_service_statement_method_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1; }",
                             "service s1 { rpc r1(m1) returns (m1){;;;}; }"]),
    [{{msg,m1}, _},
     {{service,s1},[{rpc,r1,m1,m1}]}] = do_process_sort_defs(Defs).

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
     {{msg,p_m1}, [#field{name=f1, type={enum,e1}}, #field{name=fm2}]},
     {{msg,p_m2}, [#field{type={msg,p_m1}}]}, %% type is a msg: to be prefixed
     {{service,s1}, %% not prefixed
      [{rpc,req,p_m1,p_m2}]} %% both argument and result msgs to be prefixed
    ] =
        do_process_sort_defs(Defs, [{msg_name_prefix, "p_"}]).

can_suffix_record_names_test() ->
    {ok, Defs} = parse_lines(["enum    e1 {a=1; b=2;}",
                              "message m1 {required e1 f1=1;}",
                              "message m2 {required m1 f2=1;}",
                              "service s1 {",
                              "  rpc req(m1) returns (m2) {};",
                              "}",
                              "extend m1 { optional uint32 fm2=2; }"]),
    [{{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{msg,m1_s}, [#field{name=f1, type={enum,e1}}, #field{name=fm2}]},
     {{msg,m2_s}, [#field{type={msg,m1_s}}]}, %% type is a msg: to be prefixed
     {{service,s1}, %% not prefixed
      [{rpc,req,m1_s,m2_s}]} %% both argument and result msgs to be prefixed
    ] =
        do_process_sort_defs(Defs, [{msg_name_suffix, "_s"}]).

verify_ingores_import_statements_test() ->
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
    case gpb_parse:post_process(Elems, []) of
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
    {ok, Defs2} = gpb_parse:post_process(Defs, Opts),
    lists:sort(Defs2).
