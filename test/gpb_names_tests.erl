%%% Copyright (C) 2017  Tomas Abrahamsson
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

-module(gpb_names_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

renames_just_msg_test() ->
    Defs0 = parse_sort_several_file_lines(x_proto(), [use_packages]),
    %% msg_name => don't touch Package part, just the msg name path.
    %% Over grpc, Package, Service and Rpc is exposed.
    [{file, _},
     {package,'TopPkg.SubPkg'},
     {{enum_containment, _}, _},
     {{msg,'TopPkg.SubPkg.msg_name_1'},
      [#?gpb_field{type={msg,'TopPkg.SubPkg.msg_name_1.msg_name_2'}},
       #?gpb_field{}]},
     {{msg,'TopPkg.SubPkg.msg_name_1.msg_name_2'},
      [#?gpb_field{}]},
     {{msg_containment,"x"},
      ['TopPkg.SubPkg.msg_name_1','TopPkg.SubPkg.msg_name_1.msg_name_2']},
     {{pkg_containment,"x"},'TopPkg.SubPkg'},
     {{rpc_containment,"x"},[{'TopPkg.SubPkg.SvcName1','RpcReq'}]},
     {{service,'TopPkg.SubPkg.SvcName1'},
      [#?gpb_rpc{name='RpcReq',
                 input='TopPkg.SubPkg.msg_name_1',
                 output='TopPkg.SubPkg.msg_name_1.msg_name_2'}]},
     {{service_containment,"x"}, ['TopPkg.SubPkg.SvcName1']}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs0,
                        [{rename, {msg_name, snake_case}}]))).

renames_msg_full_path_test() ->
    Defs0 = parse_sort_several_file_lines(x_proto(), [use_packages]),
    %% msg_fqname => transform also the Package---but only for messages,
    %% (the package names in other parts won't match, but if one wants to
    %% do this, then one presumably has no services)
    [{file, _},
     {package,'TopPkg.SubPkg'},
     {{enum_containment, _}, _},
     {{msg,'top_pkg.sub_pkg.msg_name_1'},
      [#?gpb_field{type={msg,'top_pkg.sub_pkg.msg_name_1.msg_name_2'}},
       #?gpb_field{}]},
     {{msg,'top_pkg.sub_pkg.msg_name_1.msg_name_2'},
      [#?gpb_field{}]},
     {{msg_containment,"x"},
      ['top_pkg.sub_pkg.msg_name_1','top_pkg.sub_pkg.msg_name_1.msg_name_2']},
     {{pkg_containment,"x"},'TopPkg.SubPkg'},
     {{rpc_containment,"x"},[{'TopPkg.SubPkg.SvcName1','RpcReq'}]},
     {{service,'TopPkg.SubPkg.SvcName1'},
      [#?gpb_rpc{name='RpcReq',
                 input='top_pkg.sub_pkg.msg_name_1',
                 output='top_pkg.sub_pkg.msg_name_1.msg_name_2'}]},
     {{service_containment,"x"}, ['TopPkg.SubPkg.SvcName1']}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs0,
                        [{rename, {msg_fqname, snake_case}}]))).

rename_package_affects_all_occurrences_test() ->
    Defs0 = parse_sort_several_file_lines(x_proto(), [use_packages]),
    [{file, _},
     {package,'top_pkg.sub_pkg'},
     {{enum_containment, _}, _},
     {{msg,'top_pkg.sub_pkg.MsgName1'},
      [#?gpb_field{type={msg,'top_pkg.sub_pkg.MsgName1.MsgName2'}},
       #?gpb_field{}]},
     {{msg,'top_pkg.sub_pkg.MsgName1.MsgName2'},
      [#?gpb_field{}]},
     {{msg_containment,"x"},
      ['top_pkg.sub_pkg.MsgName1','top_pkg.sub_pkg.MsgName1.MsgName2']},
     {{pkg_containment,"x"},'top_pkg.sub_pkg'},
     {{rpc_containment,"x"},[{'top_pkg.sub_pkg.SvcName1','RpcReq'}]},
     {{service,'top_pkg.sub_pkg.SvcName1'},
      [#?gpb_rpc{name='RpcReq',
                 input='top_pkg.sub_pkg.MsgName1',
                 output='top_pkg.sub_pkg.MsgName1.MsgName2'}]},
     {{service_containment,"x"}, ['top_pkg.sub_pkg.SvcName1']}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs0,
                        [{rename, {pkg_name, snake_case}},
                         use_packages]))).

dots_to_underscores_for_nested_msgs_test() ->
    Defs0 = parse_sort_several_file_lines(x_proto(), [use_packages]),
    %% msg_name => don't touch Package part, just the msg name path.
    %% Over grpc, Package, Service and Rpc is exposed.
    [{file, _},
     {package,'TopPkg.SubPkg'},
     {{enum_containment, _}, _},
     {{msg,'TopPkg.SubPkg.msg_name_1'},
      [#?gpb_field{type={msg,'TopPkg.SubPkg.msg_name_1_msg_name_2'}},
       #?gpb_field{}]},
     {{msg,'TopPkg.SubPkg.msg_name_1_msg_name_2'},
      [#?gpb_field{}]},
     {{msg_containment,"x"},
      ['TopPkg.SubPkg.msg_name_1','TopPkg.SubPkg.msg_name_1_msg_name_2']},
     {{pkg_containment,"x"},'TopPkg.SubPkg'},
     {{rpc_containment,"x"},[{'TopPkg.SubPkg.SvcName1','RpcReq'}]},
     {{service,'TopPkg.SubPkg.SvcName1'},
      [#?gpb_rpc{name='RpcReq',
                 input='TopPkg.SubPkg.msg_name_1',
                 output='TopPkg.SubPkg.msg_name_1_msg_name_2'}]},
     {{service_containment,"x"}, ['TopPkg.SubPkg.SvcName1']}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs0,
                        [{rename, {msg_name, snake_case}},
                         {rename, {msg_name, dots_to_underscores}}]))).

nested_enums_test() ->
    Defs0 = parse_sort_several_file_lines(
              [{"x.proto",
                ["package TopPkg.SubPkg;",
                 "message MsgName1 {",
                 "  required EnumName   f1=1;",
                 "  enum EnumName {a=0; b=1;}",
                 "};",
                 "service SvcName1 {",
                 "  rpc RpcReq(MsgName1) returns (MsgName1) {};",
                 "}"]}],
              [use_packages]),
    [{file,{"x","x.proto"}},
     {package,'TopPkg.SubPkg'},
     {{enum,'TopPkg.SubPkg.msg_name_1.EnumName'},[{a,0},{b,1}]},
     {{enum_containment,"x"},['TopPkg.SubPkg.msg_name_1.EnumName']},
     {{msg,'TopPkg.SubPkg.msg_name_1'},
      [#?gpb_field{name = f1,fnum = 1,rnum = 2,
                   type = {enum,'TopPkg.SubPkg.msg_name_1.EnumName'},
                   occurrence = required,opts = []}]},
     {{msg_containment,"x"},['TopPkg.SubPkg.msg_name_1']},
     {{pkg_containment,"x"},'TopPkg.SubPkg'},
     {{rpc_containment,"x"},
      [{'TopPkg.SubPkg.SvcName1','RpcReq'}]},
     {{service,'TopPkg.SubPkg.SvcName1'},
      [#?gpb_rpc{name = 'RpcReq',input = 'TopPkg.SubPkg.msg_name_1',
                 output = 'TopPkg.SubPkg.msg_name_1',input_stream = false,
                 output_stream = false,opts = []}]},
     {{service_containment,"x"},['TopPkg.SubPkg.SvcName1']}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs0,
                        [{rename, {msg_name, snake_case}},
                         {rename, {msg_name, dots_to_underscores}}]))).

base_name_test() ->
    Defs0 = parse_sort_several_file_lines(x_proto(), [use_packages]),
    [{file, _},
     {package,'TopPkg.SubPkg'},
     {{enum_containment, _}, _},
     {{msg,msg_name_1}, [#?gpb_field{type={msg, msg_name_2}},
                         #?gpb_field{}]},
     {{msg,msg_name_2}, [#?gpb_field{}]},
     {{msg_containment,"x"},[msg_name_1, msg_name_2]},
     {{pkg_containment,"x"},'TopPkg.SubPkg'},
     {{rpc_containment,"x"},[{'TopPkg.SubPkg.SvcName1','RpcReq'}]},
     {{service,'TopPkg.SubPkg.SvcName1'},
      [#?gpb_rpc{name='RpcReq',
                 input=msg_name_1,
                 output=msg_name_2}]},
     {{service_containment,"x"}, ['TopPkg.SubPkg.SvcName1']}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs0,
                        [{rename, {msg_fqname, base_name}},
                         {rename, {msg_fqname, snake_case}}]))).

x_proto() ->
    [{"x.proto",
      ["package TopPkg.SubPkg;",
       "message MsgName1 {",
       "  required MsgName2   f1=1;",
       "  message MsgName2 {required uint32 g1=1;}",
       "};",
       "service SvcName1 {",
       "  rpc RpcReq(MsgName1) returns (MsgName1.MsgName2) {};",
       "}",
       "extend MsgName1 { optional uint32 fm2=2; }"]}].

renames_groups_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["package TopPkg.SubPkg;",
                "message MsgName1 {",
                "  required group GROUP_NAME = 1 {",
                "    required MsgName2 gf1 = 11;",
                "  }",
                "  message MsgName2 {required uint32 m21=21;}"
                "};"]}],
             [use_packages]),
    [{file, _},
     {package,'TopPkg.SubPkg'},
     {{enum_containment, _}, _},
     {{group,'TopPkg.SubPkg.msgname1.group_name'},
      [#?gpb_field{type={msg,'TopPkg.SubPkg.MsgName1.MsgName2'}}]},
     {{msg,'TopPkg.SubPkg.MsgName1'},
      [#?gpb_field{name='GROUP_NAME',
                   type={group,'TopPkg.SubPkg.msgname1.group_name'}}]},
     {{msg,'TopPkg.SubPkg.MsgName1.MsgName2'}, [#?gpb_field{}]},
     {{msg_containment,"x"},
      ['TopPkg.SubPkg.MsgName1','TopPkg.SubPkg.MsgName1.MsgName2']},
     {{pkg_containment,"x"},'TopPkg.SubPkg'}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs,
                        [{rename, {group_name, lowercase}}]))).

rename_msg_by_proto_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"proto1.proto", ["enum    e1 {a=1; b=2;}",
                                "message m1 {required e1 f1=1;}"]},
              {"proto2.proto", ["message m2 {required m1 f2=1;}",
                                "service s1 {",
                                "  rpc req(m1) returns (m2) {};",
                                "}",
                                "extend m1 { optional uint32 fm2=2; }"]},
              {"proto3.proto", ["message m3 {map<string, m1> m=1;}"]}],
             []),
    [{file, _},
     {file, _},
     {file, _},
     {{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{enum_containment, _}, _},
     {{enum_containment, _}, _},
     {{enum_containment, _}, _},
     {{msg,m3},   [#?gpb_field{name=m, type={map,string,{msg,p1_m1}}}]},
     {{msg,p1_m1}, [#?gpb_field{name=f1, type={enum,e1}},
                    #?gpb_field{name=fm2}]},
     %% type is a msg: to be prefixed
     {{msg,p2_m2}, [#?gpb_field{type={msg,p1_m1}}]},
     {{msg_containment,"proto1"}, [p1_m1]},
     {{msg_containment,"proto2"}, [p2_m2]},
     {{msg_containment,"proto3"}, [m3]},
     {{rpc_containment,"proto2"}, [{s1,req}]},
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=p1_m1,  %% both argument ...
                 output=p2_m2} %% ... and result msgs to be prefixed
      ]},
     {{service_containment, "proto2"}, [s1]}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs,
                        [{msg_name_prefix,
                          {by_proto, [{proto1, "p1_"},
                                      {proto2, "p2_"}]}}]))).

prefix_record_names_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["enum    e1 {a=1; b=2;}",
                "message m1 {required e1 f1=1;}",
                "message m2 {required m1 f2=1;}",
                "service s1 {",
                "  rpc req(m1) returns (m2) {};",
                "}",
                "extend m1 { optional uint32 fm2=2; }"]}],
            []),
    [{file, _},
     {{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{enum_containment, _}, _},
     {{msg,p_m1}, [#?gpb_field{name=f1, type={enum,e1}},
                   #?gpb_field{name=fm2}]},
     %% type is a msg: to be prefixed
     {{msg,p_m2}, [#?gpb_field{type={msg,p_m1}}]},
     {{msg_containment,_}, _},
     {{rpc_containment,_}, [{s1,req}]},
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=p_m1,     %% both argument ...
                 output=p_m2}]}, %% ... and result msgs to be prefixed
     {{service_containment,_}, [s1]}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs,
                        [{msg_name_prefix, "p_"}]))).

can_suffix_record_names_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["enum    e1 {a=1; b=2;}",
                "message m1 {required e1 f1=1;}",
                "message m2 {required m1 f2=1;}",
                "service s1 {",
                "  rpc req(m1) returns (m2) {};",
                "}",
                "extend m1 { optional uint32 fm2=2; }"]}],
             []),
    [{file, _},
     {{enum,e1},  [{a,1},{b,2}]}, %% not prefixed
     {{enum_containment, _}, _},
     {{msg,m1_s}, [#?gpb_field{name=f1, type={enum,e1}},
                   #?gpb_field{name=fm2}]},
     %% type is a msg: to be suffixed
     {{msg,m2_s}, [#?gpb_field{type={msg,m1_s}}]},
     {{msg_containment,_}, [m1_s, m2_s]},
     {{rpc_containment,_}, [{s1,req}]},
     {{service,s1}, %% not prefixed
      [#?gpb_rpc{name=req,
                 input=m1_s,     %% both argument ...
                 output=m2_s}]}, %% .. and result msgs to be prefixed
     {{service_containment,_}, [s1]}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs,
                        [{msg_name_suffix, "_s"}]))).

can_tolower_record_names_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["message Msg1 {required Msg2   f1=1;}",
                "message Msg2 {required uint32 g1=1;}",
                "service Svc1 {",
                "  rpc req(Msg1) returns (Msg2) {};",
                "}",
                "extend Msg1 { optional uint32 fm2=2; }"]}],
            []),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,msg1}, [#?gpb_field{name=f1, type={msg,msg2}},
                   #?gpb_field{name=fm2}]},
     {{msg,msg2}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, [msg1, msg2]},
     {{rpc_containment,_}, [{svc1,req}]},
     {{service,svc1},
      [#?gpb_rpc{name=req,
                 input=msg1,     %% both argument ...
                 output=msg2}]}, %% .. and result msgs to be to-lower
     {{service_containment,_}, [svc1]}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_lower]))).

can_tolower_record_names_with_oneof_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["message Msg1 {",
                "  oneof u {",
                "    Msg1   a = 1;",
                "    uint32 b = 2;",
                "  }",
                "}"]}],
             []),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,msg1}, [#gpb_oneof{fields=[#?gpb_field{name=a,type={msg,msg1}},
                                      #?gpb_field{name=b}]}]},
     {{msg_containment,_}, [msg1]}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_lower]))).

can_tolower_record_names_with_map_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["message Msg1 {",
                "  required uint32 f = 1;",
                "}"
                "message Msg2 {"
                "  map<string,Msg1> m = 1;",
                "}"]}],
             []),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,msg1}, [#?gpb_field{}]},
     {{msg,msg2}, [#?gpb_field{type={map,string,{msg,msg1}}}]},
     {{msg_containment,_}, [msg1, msg2]}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_lower]))).

can_to_snake_record_names_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["message MsgName1 {required MsgName2   f1=1;}",
                "message MsgName2 {required uint32 g1=1;}",
                "service SvcName1 {",
                "  rpc req(MsgName1) returns (MsgName2) {};",
                "}",
                "extend MsgName1 { optional uint32 fm2=2; }"]}],
             []),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,msg_name_1}, [#?gpb_field{name=f1, type={msg,msg_name_2}},
                         #?gpb_field{name=fm2}]},
     {{msg,msg_name_2}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, [msg_name_1, msg_name_2]},
     {{rpc_containment,_}, [{svc_name_1, req}]},
     {{service,svc_name_1},
      [#?gpb_rpc{name=req,
                 input=msg_name_1,     %% both argument ...
                 output=msg_name_2}]}, %% .. and result msgs to be snake_cased
     {{service_containment,_},[svc_name_1]}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_snake_case]))).

to_snake_case_with_packages_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["package TopPkg.SubPkg;",
                "message MsgName1 {required MsgName2   f1=1;}",
                "message MsgName2 {required uint32 g1=1;}",
                "service SvcName1 {",
                "  rpc RpcReq(MsgName1) returns (MsgName2) {};",
                "}",
                "extend MsgName1 { optional uint32 fm2=2; }"]}],
             [use_packages]),
    [{file, _},
     {package, 'top_pkg.sub_pkg'},
     {{enum_containment, _}, _},
     {{msg,'top_pkg.sub_pkg.msg_name_1'},
      [#?gpb_field{name=f1, type={msg,'top_pkg.sub_pkg.msg_name_2'}},
       #?gpb_field{name=fm2}]},
     {{msg,'top_pkg.sub_pkg.msg_name_2'}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, ['top_pkg.sub_pkg.msg_name_1',
                            'top_pkg.sub_pkg.msg_name_2']},
     {{pkg_containment,"x"},'top_pkg.sub_pkg'},
     {{rpc_containment,_}, [{'top_pkg.sub_pkg.svc_name_1', rpc_req}]},
     {{service,'top_pkg.sub_pkg.svc_name_1'},
      [#?gpb_rpc{name=rpc_req,
                 input='top_pkg.sub_pkg.msg_name_1',
                 output='top_pkg.sub_pkg.msg_name_2'}]},
     {{service_containment,_}, ['top_pkg.sub_pkg.svc_name_1']}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_snake_case,
                                                   use_packages]))).

can_tolower_record_names_with_packages_with_legacy_opts_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["package Pkg1;",
                "message Msg1 {required Msg2   f1=1;}",
                "message Msg2 {required uint32 g1=1;}",
                "service Svc1 {",
                "  rpc req(Msg1) returns (Msg2) {};",
                "}",
                "extend Msg1 { optional uint32 fm2=2; }"]}],
             [use_packages]),
    [{file, _},
     {package, 'pkg1'},
     {{enum_containment, _}, _},
     {{msg,'pkg1.msg1'}, [#?gpb_field{name=f1, type={msg,'pkg1.msg2'}},
                          #?gpb_field{name=fm2}]},
     {{msg,'pkg1.msg2'}, [#?gpb_field{name=g1}]},
     {{msg_containment,_}, ['pkg1.msg1','pkg1.msg2']},
     {{pkg_containment,"x"},'pkg1'},
     {{rpc_containment,"x"},[{'pkg1.svc1',req}]},
     {{service,'pkg1.svc1'},
      [#?gpb_rpc{name=req,
                 input='pkg1.msg1',     %% both argument ...
                 output='pkg1.msg2'}]}, %% .. and result msgs to be to-lower
     {{service_containment,"x"},['pkg1.svc1']}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_lower,
                                                   use_packages]))).

can_tolower_with_package_def_but_without_use_package_opt_test() ->
    Defs = parse_sort_several_file_lines(
             [{"x.proto",
               ["package pkg1;",
                "message Msg1 {required uint32 f1=1;}"]}],
             [%% Not: use_packages even though there's package pkg1; line
             ]),
    [{file, _},
     {package, 'pkg1'},
     {{enum_containment, _}, _},
     {{msg,msg1}, [#?gpb_field{}]},
     {{msg_containment,_}, [msg1]}] =
        lists:sort(ok(gpb_names:rename_defs(Defs, [msg_name_to_lower]))).

error_for_duplicates_after_rename_test() ->
    Defs = parse_sort_several_file_lines(
             [{"upper.proto",
               ["package PKG1;",
                "message MSG1 {required MSG2   f1=1;}",
                "message MSG2 {required uint32 g1=1;}",
                "service SVC1 {",
                "  rpc REQ(MSG1) returns (MSG2) {};",
                "}",
                "extend MSG1 { optional uint32 fm2=2; }"]},
              {"lower.proto",
               ["package pkg1;",
                "message msg1 {required msg2   f1=1;}",
                "message msg2 {required uint32 g1=1;}",
                "service svc1 {",
                "  rpc req(msg1) returns (msg2) {};",
                "}",
                "extend msg1 { optional uint32 fm2=2; }"]}],
             [use_packages]),
    {error, Reason} = gpb_names:rename_defs(Defs, [msg_name_to_lower]),
    Txt = lists:flatten(gpb_names:format_error(Reason)),
    {true, true, true, false} =
        {gpb_lib:is_substr("same", Txt),
         gpb_lib:is_substr("MSG1", Txt),
         gpb_lib:is_substr("msg1", Txt),
         gpb_lib:is_substr("Unexpected error", Txt)}.

enum_type_names_test() ->
    Defs = parse_sort_several_file_lines(
             [{"xyz.proto",
               ["enum ee { a=0; b=1; };",
                "message msg1 {",
                "  required ee f1 = 1;",
                "  required group gg = 2 {",
                "    required uint32 f2 = 3;",
                "  }",
                "};"]}],
             []),
    {ok, Renamings1} = gpb_names:compute_renamings(Defs, []),
    ee = gpb_names:apply_msg_type_renaming(ee, Renamings1),
    msg1 = gpb_names:apply_msg_type_renaming(msg1, Renamings1),

    {ok, Renamings2} = gpb_names:compute_renamings(
                         Defs,
                         [{rename, {msg_typename, {prefix, msg_x_}}},
                          {rename, {enum_typename, {prefix, "enum_x_"}}}]),
    enum_x_ee = gpb_names:apply_enum_type_renaming(ee, Renamings2),
    msg_x_msg1 = gpb_names:apply_msg_type_renaming(msg1, Renamings2),
    ok.

no_error_for_same_rpc_name_in_different_services_test() ->
    Defs = parse_sort_several_file_lines(
             [{"a.proto",
               ["message M { required uint32 f1 = 1; };",
                "service Account { rpc Create (M) returns (M); }",
                "service Device  { rpc Create (M) returns (M); }"]}],
             []),
    [{file, _},
     {{enum_containment, _}, _},
     {{msg,'M'}, _},
     {{msg_containment,_},_},
     {{rpc_containment,_},[{account,create},{device,create}]},
     {{service,account}, [#?gpb_rpc{name=create}]},
     {{service,device},  [#?gpb_rpc{name=create}]},
     {{service_containment,_},[_,_]}] =
        lists:sort(ok(gpb_names:rename_defs(
                        Defs,
                        [{rename,{rpc_name,lowercase}},
                         {rename,{service_name,lowercase}}]))).

original_names_test() ->
    Protos = [{"x.proto",
               "package TopPkg.SubPkg;
                message MsgName1 {
                  required EE_a   f1 = 1;
                  required EE_b   f2 = 2;
                  required group G = 3 {
                    required uint32 f3 = 1;
                  }
                  enum EE_b { AA = 0; }
                }
                enum EE_a { BB = 0; }"}],
    RenameOpts = [{rename, {pkg_name, lowercase}},
                  {rename, {group_fqname, {suffix, '_gsuf'}}},
                  {rename, {msg_fqname, base_name}},
                  {rename, {msg_fqname, snake_case}}],

    %%% Test without use_packages option
    Defs1 = parse_sort_several_file_lines(Protos, []),
    {ok, Rs11} = gpb_names:compute_renamings(Defs1, []),
    'TopPkg.SubPkg' = gpb_names:original_pkg_name('TopPkg.SubPkg', Rs11),
    {ok, Rs12} = gpb_names:compute_renamings(Defs1, RenameOpts),
    '' = gpb_names:original_pkg_name('TopPkg.SubPkg', Rs12),

    %%% With the use_packages option
    ParseOpts = [use_packages],
    Defs2 = parse_sort_several_file_lines(Protos, ParseOpts),
    %% Test no_renamings
    {ok, Rs2} = gpb_names:compute_renamings(Defs2, ParseOpts), % no renamings
    'TopPkg.SubPkg' = gpb_names:original_pkg_name('TopPkg.SubPkg', Rs2),
    {ok, Rs3} = gpb_names:compute_renamings(Defs2, ParseOpts ++ RenameOpts),
    'TopPkg.SubPkg' = gpb_names:original_pkg_name('toppkg.subpkg', Rs3),
    'TopPkg.SubPkg.MsgName1' = gpb_names:original_msg_name(msg_name_1, Rs3),
    'TopPkg.SubPkg.MsgName1.G' = gpb_names:original_group_name(
                                   'toppkg.subpkg.MsgName1.G_gsuf', Rs3),
    'TopPkg.SubPkg.MsgName1.EE_b' = gpb_names:original_enum_name(
                                      'msg_name_1.EE_b', Rs3),
    'TopPkg.SubPkg.EE_a' = gpb_names:original_enum_name(
                             'toppkg.subpkg.EE_a', Rs3),
    ok.

%% test helpers
parse_sort_several_file_lines(ProtoLines, Opts) ->
    {AllProtoNames, _AllLines} = lists:unzip(ProtoLines),
    AllProtoBases = lists:map(fun filename:basename/1, AllProtoNames),
    AllDefs1 = [begin
                    {ok, Defs1} = parse_lines(
                                    filter_away_import_lines(
                                      Lines, AllProtoBases)),
                    {ok, Defs2} = gpb_defs:post_process_one_file(
                                    FName, Defs1, Opts),
                    Defs2
                end
                || {FName, Lines} <- ProtoLines],
    {ok, AllDefs2} = gpb_defs:post_process_all_files(
                       lists:append(AllDefs1),
                       Opts),
    lists:sort(AllDefs2).

filter_away_import_lines(Lines, AllProtoNames) ->
    Lines1 = ensure_list_of_lines(Lines),
    {ImportLines, RestLines} = lists:partition(fun is_import_line/1, Lines1),
    Imports = lists:map(fun protobase_by_importline/1, ImportLines),
    StrayImports = lists:filter(
                     fun(I) -> not lists:member(I, AllProtoNames) end,
                     Imports),
    if StrayImports == [] -> RestLines;
       true -> error({bad_test, stray_import_of_missing_proto, Imports, Lines})
    end.

ensure_list_of_lines(X) ->
    case is_flat_string(X) of
        true ->
            split_to_line_strip_leading_space(X);
        false ->
            X % already list of lines
    end.

is_flat_string(S) when is_list(S) -> lists:all(fun is_integer/1, S);
is_flat_string(_) -> false.

split_to_line_strip_leading_space(S) ->
    Lines = gpb_lib:string_lexemes(S, "\n"),
    Indent = count_indent(lists:last(Lines), 0),
    [strip_indent(Indent, Line) || Line <- Lines].

count_indent(" "++Rest, N) -> count_indent(Rest, N+1);
count_indent(_, N)         -> N.

strip_indent(N, " "++Rest) when N >= 1 -> strip_indent(N-1, Rest);
strip_indent(0, Rest) -> Rest;
strip_indent(_, Rest) -> Rest. % fewer indent spaces, possibly for first line

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

ok({ok, V}) -> V.
