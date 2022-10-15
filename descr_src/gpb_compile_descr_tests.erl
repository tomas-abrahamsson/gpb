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

-module(gpb_compile_descr_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gpb_descriptor.hrl").

-define(extension_range(StartIncl, EndExcl),
        #'DescriptorProto.ExtensionRange'{start = StartIncl,
                                          'end' = EndExcl}).
-define(reserved_range(StartIncl, EndExcl),
        #'DescriptorProto.ReservedRange'{start = StartIncl,
                                         'end' = EndExcl}).
-define(range_max, 536870912).

%% ------------------------------------------------------------------

individual_descriptor_test() ->
    {#'FileDescriptorSet'{
       file=[#'FileDescriptorProto'{}=B1,
             #'FileDescriptorProto'{}=B2]},
     [{"main",B1},
      {"aux",B2}]} = compile_descriptors(
                   [{"main.proto",
                     ["syntax=\"proto2\";",
                      "import \"aux.proto\";",
                      "message M { };"]},
                    {"aux.proto",
                     ["syntax=\"proto2\";",
                      "message A { optional uint32 g = 1; }"]}],
                   []).

oneof_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "message Ma {",
           "  oneof u1 { uint32 f11 = 11;",
           "             uint32 f12 = 12;",
           "             uint32 f13 = 13; };",
           "  oneof u2 { uint32 f21 = 21;",
           "             uint32 f22 = 22; };",
           "}",
           "message Mb {",
           "  oneof u2 { uint32 g11 = 11; };",
           "  oneof u1 { uint32 g21 = 21; };",
           "}"]}],
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="main.proto",
          package=undefined,
          message_type =
              [#'DescriptorProto'{
                  name="Ma",
                  field=[#'FieldDescriptorProto'{name="f11", oneof_index=0},
                         #'FieldDescriptorProto'{name="f12", oneof_index=0},
                         #'FieldDescriptorProto'{name="f13", oneof_index=0},
                         #'FieldDescriptorProto'{name="f21", oneof_index=1},
                         #'FieldDescriptorProto'{name="f22", oneof_index=1}],
                  oneof_decl=[#'OneofDescriptorProto'{name="u1"},
                              #'OneofDescriptorProto'{name="u2"}]},
               #'DescriptorProto'{
                  name="Mb",
                  field=[#'FieldDescriptorProto'{name="g11", oneof_index=0},
                         #'FieldDescriptorProto'{name="g21", oneof_index=1}],
                  oneof_decl=[#'OneofDescriptorProto'{name="u2"},
                              #'OneofDescriptorProto'{name="u1"}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []).

refs_have_pkg_name_test() -> % only refs have package, names do not
    ProtosAsTxts1 =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "import \"aux.proto\";",
           "package top.p1;",
           "message M { optional S f1 = 1;",
           "            optional E f2 = 2;",
           "            oneof u { S f3 = 3; };",
           "            map<uint32,S> f4 = 4;",
           "            map<uint64,E> f5 = 5; };",
           "message S { optional uint32 f1 = 1; };",
           "enum E { a=0;};",
           "service SomeService {",
           "  rpc rpc1 (M) returns (S);",
           "}"]},
         {"aux.proto",
          ["syntax=\"proto2\";",
           "package aux.p2;",
           "message A { optional B g = 1; }",
           "message B { optional uint32 h = 1; }"]}],

    %% Without the use_packges option
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="top/p1/main.proto",
          package="top.p1",
          message_type =
              [#'DescriptorProto'{
                  name="M", % name, not a ref
                  field=[#'FieldDescriptorProto'{type_name=".top.p1.S"},
                         #'FieldDescriptorProto'{type_name=".top.p1.E"},
                         #'FieldDescriptorProto'{type_name=".top.p1.S"},
                         #'FieldDescriptorProto'{type_name=MapField1Ref},
                         #'FieldDescriptorProto'{type_name=MapField2Ref}]},
               #'DescriptorProto'{name="S"},
               #'DescriptorProto'{
                  name=MapField1Name,
                  field=[#'FieldDescriptorProto'{type = 'TYPE_UINT32'},
                         #'FieldDescriptorProto'{type_name = ".top.p1.S"}]},
               #'DescriptorProto'{
                  name=MapField2Name,
                  field=[#'FieldDescriptorProto'{type = 'TYPE_UINT64'},
                         #'FieldDescriptorProto'{type_name = ".top.p1.E"}]}],
          enum_type = [#'EnumDescriptorProto'{name="E"}],
          service =
              [#'ServiceDescriptorProto'{
                  name="SomeService",
                  method = [#'MethodDescriptorProto'{
                               name = "rpc1",
                               input_type=".top.p1.M",
                               output_type=".top.p1.S"}]}]}},
      {"aux",
       #'FileDescriptorProto'{}}]} =
        Descriptors1 =
        compile_descriptors(ProtosAsTxts1, []),
    ?assertEqual(MapField1Ref, ".top.p1." ++ MapField1Name),
    ?assertEqual(MapField2Ref, ".top.p1." ++ MapField2Name),

    %% With the use_packages option
    Descriptors1 = compile_descriptors(ProtosAsTxts1, [use_packages]),

    %% A proto with no package definition
    ProtosAsTxts2 =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "message M { optional S f1 = 1;",
           "            optional E f2 = 2;",
           "            oneof u { S f3 = 3; }; };",
           "message S { optional uint32 f1 = 1; };",
           "enum E { a=0; };",
           "service SomeService {",
           "  rpc rpc1 (M) returns (S);",
           "}"]}],
    %% Without the use_packges option
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="main.proto",
          package=undefined,
          message_type =
              [#'DescriptorProto'{
                  name="M", % name, not a ref
                  field=[#'FieldDescriptorProto'{type_name=".S"},
                         #'FieldDescriptorProto'{type_name=".E"},
                         #'FieldDescriptorProto'{type_name=".S"}]},
               #'DescriptorProto'{name="S"}],
          enum_type = [#'EnumDescriptorProto'{name="E"}],
          service =
              [#'ServiceDescriptorProto'{
                  name="SomeService",
                  method = [#'MethodDescriptorProto'{
                               name = "rpc1",
                               input_type=".M",
                               output_type=".S"}]}]}}]} =
        Descriptors2 =
        compile_descriptors(ProtosAsTxts2, []),
    %% With the use_packages option
    Descriptors2 = compile_descriptors(ProtosAsTxts2, [use_packages]),
    ok.

service_rpc_streams_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto2';
            message M { };
            service S {
               rpc R1(stream M) returns (M);
               rpc R2(M) returns (stream M);
            };"]}],
    {_,
     [{_,
       #'FileDescriptorProto'{
          message_type = [_],
          service = [#'ServiceDescriptorProto'{method = Methods}]}}]} =
        compile_descriptors(ProtosAsTxts, []),
    [{"R1", true, false},
     {"R2", false, true}] =
        [{Name, ArgStream, RetStream}
         || #'MethodDescriptorProto'{name=Name,
                                     client_streaming=ArgStream,
                                     server_streaming=RetStream} <- Methods],
    ok.

nested_definitions_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax=\"proto2\";",
           "message M {",
           "  optional S f1 = 1;",
           "  optional E f2 = 2;",
           "  oneof u { S f3 = 3; };",
           "  message S { ",
           "    optional EE f1 = 1;",
           "    enum EE { aa=0; }",
           "    };",
           "  enum E { a=0; };",
           "}"]}],
    {_,
     [{"main",
       #'FileDescriptorProto'{
          name="main.proto",
          package=undefined,
          message_type =
              [#'DescriptorProto'{
                  name="M", % name, not a ref
                  field=[#'FieldDescriptorProto'{type_name=".M.S"},
                         #'FieldDescriptorProto'{type_name=".M.E"},
                         #'FieldDescriptorProto'{type_name=".M.S"}],
                  nested_type =
                      [#'DescriptorProto'{
                          name="S",
                          field=[#'FieldDescriptorProto'{type_name=".M.S.EE"}],
                          enum_type=[#'EnumDescriptorProto'{
                                        name="EE",
                                        value=[#'EnumValueDescriptorProto'{
                                                  name="aa",
                                                  number=0}]}]}],
                  enum_type = [#'EnumDescriptorProto'{
                                  name="E",
                                  value=[#'EnumValueDescriptorProto'{
                                            name="a",
                                            number=0}]}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []).

proto3_optional_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';",
           "message M {",
           "  uint32 f1 = 1;",
           "  optional uint32 f2 = 2;",
           "  oneof c { uint32 f3 = 3; }",
           "}"]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          message_type =
              [#'DescriptorProto'{
                  name="M",
                  field=[#'FieldDescriptorProto'{name="f1"},
                         #'FieldDescriptorProto'{name="f2", oneof_index=1},
                         #'FieldDescriptorProto'{name="f3", oneof_index=0}],
                  %% Synthetic names must come after any non-synthetic ones
                  oneof_decl=[#'OneofDescriptorProto'{name="c"},
                              #'OneofDescriptorProto'{}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []).

msg_options_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';
            message Mm {
              option deprecated=true;
              uint32 f = 1;
            };
           "]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          message_type= [#'DescriptorProto'{
                            options=MsgOptions}]}}]} =
        compile_descriptors(ProtosAsTxts, []),
    #'MessageOptions'{deprecated=true} = MsgOptions,
    ok.

msg_field_options_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';
            message Mm {
              repeated uint64 f_x = 1 [packed, deprecated=true,
                                       jstype=JS_STRING, json_name='f_x'];
              Mm g = 2 [lazy=true];

            };
           "]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          message_type=
              [#'DescriptorProto'{
                  field=[#'FieldDescriptorProto'{name="f_x",
                                                 options=FOptions1,
                                                 json_name=JsonName},
                         #'FieldDescriptorProto'{name="g",
                                                 options=FOptions2}]}]}}]} =
        compile_descriptors(ProtosAsTxts, []),
    "f_x" = JsonName,
    #'FieldOptions'{deprecated=true,
                    packed=true,
                    jstype='JS_STRING'} = FOptions1,
    #'FieldOptions'{lazy=true} = FOptions2,
    ok.

default_value_test() ->
    "1"         = fmt_proto_get_default("uint32", "1"),
    "-1"        = fmt_proto_get_default("sint32", "-1"),
    "1.125"++_  = fmt_proto_get_default("double", "1.125"),
    "-1.125"++_ = fmt_proto_get_default("double", "-1.125"),
    "inf"       = fmt_proto_get_default("double", "inf"),
    "-inf"      = fmt_proto_get_default("double", "-inf"),
    "nan"       = fmt_proto_get_default("double", "nan"),
    ok.

fmt_proto_get_default(Type, ProtoDefault) ->
    P = [{"main.proto",
          ["syntax='proto3';
            message M { "++Type++" f = 1 [default="++ProtoDefault++"]; }"]}],
    {_FileDescriptorSet,
     [{_, #'FileDescriptorProto'{
             message_type=[#'DescriptorProto'{
                              field=[#'FieldDescriptorProto'{
                                        default_value=DescrDefault}]}]}}]} =
        compile_descriptors(P, []),
    DescrDefault.

enum_options_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';
            message Mm { Ee f = 1; };
            enum Ee {
              option deprecated=true;
              option allow_alias=true;
              A = 0 [deprecated=true];
            }
           "]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          message_type= [#'DescriptorProto'{}],
          enum_type=[#'EnumDescriptorProto'{
                        value=[#'EnumValueDescriptorProto'{
                                  options=EnumValueOptions}],
                        options=EnumOptions}]}}]} =
        compile_descriptors(ProtosAsTxts, []),
    #'EnumOptions'{allow_alias=true, deprecated=true} = EnumOptions,
    #'EnumValueOptions'{deprecated=true} = EnumValueOptions,
    ok.

service_and_rpc_options_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';
            message M { };
            service S {
              option deprecated=true;
              rpc Req(M) returns(M) {
                option deprecated=true;
                option idempotency_level=NO_SIDE_EFFECTS;
              }
            }
           "]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          message_type= [#'DescriptorProto'{}],
          service=[#'ServiceDescriptorProto'{
                      method=[#'MethodDescriptorProto'{
                                 options=MethodOptions}],
                      options=ServiceOptions}]}}]} =
        compile_descriptors(ProtosAsTxts, []),
    #'ServiceOptions'{deprecated=true} = ServiceOptions,
    #'MethodOptions'{deprecated=true,
                     idempotency_level='NO_SIDE_EFFECTS'} = MethodOptions,
    ok.

file_options_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto3';
            option java_package='j.pkg';
            option java_outer_classname='OuterClassName';
            option java_multiple_files=true;
            option java_generate_equals_and_hash=false;
            option java_string_check_utf8=true;
            option optimize_for=CODE_SIZE;
            option go_package='g.pkg';
            option cc_generic_services=true;
            option java_generic_services=true;
            option py_generic_services=true;
            option php_generic_services=true;
            option deprecated=true;
            option cc_enable_arenas=false;
            option objc_class_prefix='prefix';
            option csharp_namespace='ns';
            option swift_prefix='prefix';
            option php_class_prefix='prefix';
            option php_namespace='ns';
            option php_metadata_namespace='mns';
            option ruby_package='r.pkg';
           "]}],
    {_FileDescriptorSet,
     [{"main",
       #'FileDescriptorProto'{
          options=FileOptions}}]} =
        compile_descriptors(ProtosAsTxts, []),
    #'FileOptions'{
       java_package="j.pkg",
       java_outer_classname="OuterClassName",
       java_multiple_files=true,
       java_generate_equals_and_hash=false,
       java_string_check_utf8=true,
       optimize_for='CODE_SIZE',
       go_package="g.pkg",
       cc_generic_services=true,
       java_generic_services=true,
       py_generic_services=true,
       php_generic_services=true,
       deprecated=true,
       cc_enable_arenas=false,
       objc_class_prefix="prefix",
       csharp_namespace="ns",
       swift_prefix="prefix",
       php_class_prefix="prefix",
       php_namespace="ns",
       php_metadata_namespace="mns",
       ruby_package="r.pkg"} = FileOptions,
    ok.

extend_proto() ->
    "message M1 {
       extensions 200 to 299;
       message M2 {
         extensions 201 to max;
       }
       extend M2 {optional uint32 e2 = 222;}
     };
     extend M1 {optional uint32 e11 = 211;}
     extend M1.M2 {optional uint32 e12 = 212;}".

extend_with_package_test() ->
    ProtosAsTxts = [{"main.proto", ["syntax='proto2';
                                     package p;
                                    " ++ extend_proto()]}],
    {FileDescriptorSet, [{_main1, MainProto}]} =
        compile_descriptors(ProtosAsTxts, []),
    #'FileDescriptorProto'{
       message_type =
           [#'DescriptorProto'{
               name="M1",
               field=[],
               extension=[#'FieldDescriptorProto'{extendee=".p.M1.M2",
                                                  name="e2"}],
               extension_range=[?extension_range(200, 300)],
               nested_type=
                   [#'DescriptorProto'{
                       name="M2",
                       field=[],
                       extension_range=[?extension_range(201, ?range_max)],
                       extension=[]}]}],
       extension=[#'FieldDescriptorProto'{extendee=".p.M1",
                                          name="e11"},
                  #'FieldDescriptorProto'{extendee=".p.M1.M2",
                                          name="e12"}]} = MainProto,

    %% Again, but with the use_packages option.
    %% The resulting descriptors should be the same.
    {FileDescriptorSet, [{_main2, MainProto}]} =
        compile_descriptors(ProtosAsTxts, [use_packages]),
    ok.

extend_no_package_test() ->
    ProtosAsTxts = [{"main.proto", ["syntax='proto2';\n" ++ extend_proto()]}],
    {FileDescriptorSet, [{_main1, MainProto}]} =
        compile_descriptors(ProtosAsTxts, []),
    #'FileDescriptorProto'{
       message_type =
           [#'DescriptorProto'{
               name="M1",
               field=[],
               extension=[#'FieldDescriptorProto'{extendee=".M1.M2"}],
               nested_type= [#'DescriptorProto'{name="M2",
                                                field=[]}]}],
       extension=[#'FieldDescriptorProto'{extendee=".M1"},
                  #'FieldDescriptorProto'{extendee=".M1.M2"}]} =
        MainProto,

    %% Again, but with the use_packags option.
    %% The resulting descriptors should be the same.
    {FileDescriptorSet, [{_main2, MainProto}]} =
        compile_descriptors(ProtosAsTxts, [use_packages]),
    ok.

reserved_test() ->
    ProtosAsTxts =
        [{"main.proto",
          ["syntax='proto2';
            message M {
              reserved 2, 11 to 13, 9, 15 to max;
              reserved 8;
              reserved 'foo', 'bar';
              reserved 'zzz';
            }"]}],
    {_FileDescriptorSet, [{_main1, MainDescr}]} =
        compile_descriptors(ProtosAsTxts, []),
    io:format("~nD:~p~n", [MainDescr]),
    #'FileDescriptorProto'{
       message_type =
           [#'DescriptorProto'{
               name="M",
               reserved_range=[?reserved_range( 2,  3),
                               ?reserved_range(11, 14),
                               ?reserved_range( 9, 10),
                               ?reserved_range(15, ?range_max),
                               ?reserved_range( 8,  9)],
               reserved_name=["foo", "bar", "zzz"]}]} = MainDescr,
    ok.

%% --helpers----------

compile_descriptors(IoLists, GpbCompileOpts) ->
    {ok, Defs, []=_Warns} = compile_files_as_iolists(IoLists, GpbCompileOpts),
    {Bin, PBins} = gpb_compile_descr:encode_defs_to_descriptors(Defs,
                                                                GpbCompileOpts),
    {gpb_descriptor:decode_msg(Bin, 'FileDescriptorSet'),
     [{ProtoName,gpb_descriptor:decode_msg(ProtoBin, 'FileDescriptorProto')}
      || {ProtoName, ProtoBin} <- PBins]}.

compile_files_as_iolists([{FName, _IoList} | _Rest]=IoLists, GpbCompileOpts) ->
    ReadFile = fun(F) ->
                       B = filename:basename(F),
                       case lists:keyfind(B, 1, IoLists) of
                           {B, Contents} ->
                               {ok, iolist_to_binary([Contents])};
                           _ ->
                               file:read_file(F)
                       end
               end,
    ReadFileInfo = fun(F) ->
                           B = filename:basename(F),
                           case lists:keyfind(B, 1, IoLists) of
                               {B, _Contents} ->
                                   {ok, #file_info{access=read}};
                               _ ->
                                   file:read_file_info(F)
                           end
                   end,
    LatestDefsVsn = lists:max(gpb_defs:supported_defs_versions()),
    gpb_compile:file(
      FName,
      [{file_op, [{read_file, ReadFile},
                  {read_file_info, ReadFileInfo},
                  {write_file, fun(_,_) -> ok end}]},
       {i,"."},
       to_proto_defs, {proto_defs_version, LatestDefsVsn},
       return_errors, return_warnings
       | GpbCompileOpts]).
