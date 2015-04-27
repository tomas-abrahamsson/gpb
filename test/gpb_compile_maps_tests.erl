%%% Copyright (C) 2015  Tomas Abrahamsson
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

-module(gpb_compile_maps_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-ifndef(HAVE_MAPS).

no_maps_tests__test() ->
    %% rebar.config.script or the Makefile
    %% sets HAVE_MAPS if they detect that there is
    %% support for maps.
    ok.

-else. %% HAVE_MAPS

-import(gpb_compile_tests, [compile_iolist/2]).
-import(gpb_compile_tests, [unload_code/1]).

simple_maps_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "  required uint32 f2 = 2;",
                        "}"],
                       [maps, type_specs]),
    Data = M:encode_msg(#{f1 => "some string", f2 => 33}, m1),
    #{f1 := "some string", f2 := 33} = M:decode_msg(Data, m1),
    unload_code(M).

encode_decode_maps_with_opts_present_undefined_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "  optional uint32 f2 = 2;",
                        "  oneof o1 {",
                        "    uint32 f3 = 3;",
                        "    uint32 f4 = 4;",
                        "  };",
                        "}"],
                       [maps, {maps_unset_optional, present_undefined},
                        type_specs]),
    Data1 = M:encode_msg(#{f1 => "x", f2 => 33,        o1 => undefined}, m1),
    Data2 = M:encode_msg(#{f1 => "x", f2 => undefined, o1 => undefined}, m1),
    [{f1,"x"}, {f2,33}, {o1, undefined}] =
        lists:sort(maps:to_list(M:decode_msg(Data1, m1))),
    [{f1,"x"}, {f2,undefined}, {o1, undefined}] =
        lists:sort(maps:to_list(M:decode_msg(Data2, m1))),
    unload_code(M).

encode_decode_maps_with_opts_omitted_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "  optional uint32 f2 = 2;",
                        "  oneof o1 {",
                        "    uint32 f3 = 3;",
                        "    uint32 f4 = 4;",
                        "  };",
                        "}"],
                       [maps, {maps_unset_optional, omitted}, type_specs]),
    Data1 = M:encode_msg(#{f1 => "x", f2 => 33}, m1),
    Data2 = M:encode_msg(#{f1 => "x"}, m1),
    [{f1,"x"}, {f2,33}] = lists:sort(maps:to_list(M:decode_msg(Data1, m1))),
    [{f1,"x"}] = lists:sort(maps:to_list(M:decode_msg(Data2, m1))),
    unload_code(M).

%% merge ------------------------------------------------
merge_maps_with_opts_present_undefined_test() ->
    M = compile_iolist(["message m1 {",
                        "  required string f1 = 1;",
                        "  repeated string f2 = 2;",
                        "  optional uint32 f3 = 3;",
                        "  oneof o1 {",
                        "    m2     f11 = 11;",
                        "    uint32 f12 = 12;",
                        "  };",
                        "}",
                        "message m2 {",
                        "  repeated uint32 g1 = 1;",
                        "}"],
                       [maps, {maps_unset_optional, present_undefined},
                        type_specs]),
    [{f1,"y"}, {f2,["a","b","c"]}, {f3,393}, {o1,{f12,99}}] =
        lists:sort(
          maps:to_list(
            M:merge_msgs(#{f1 => "x", f2 => ["a"],
                           f3 => undefined,
                           o1 => undefined},
                         #{f1 => "y", f2 => ["b","c"],
                           f3 => 393, o1 => {f12, 99}},
                         m1))),
    [{f1,"y"}, {f2,[]}, {f3,111}, {o1,{f11,#{g1 := [1,2,3,4]}}}] =
        lists:sort(
          maps:to_list(
            M:merge_msgs(#{f1 => "x", f2 => [],
                           f3 => 111,
                           o1 => {f11,#{g1 => [1,2]}}},
                         #{f1 => "y", f2 => [], f3 => undefined,
                           o1 => {f11,#{g1 => [3,4]}}},
                         m1))),
    unload_code(M).

merge_maps_with_opts_omitted_test() ->
    M = compile_iolist(["message m1 {",
                        "  required string f1 = 1;",
                        "  repeated string f2 = 2;",
                        "  optional uint32 f3 = 3;",
                        "  oneof o1 {",
                        "    m2     f11 = 11;",
                        "    uint32 f12 = 12;",
                        "  };",
                        "}",
                        "message m2 {",
                        "  repeated uint32 g1 = 1;",
                        "}"],
                       [maps, {maps_unset_optional, omitted}, type_specs]),
    [{f1,"y"}, {f2,["a","b","c"]}, {f3,393}, {o1,{f12,99}}] =
        lists:sort(
          maps:to_list(
            M:merge_msgs(#{f1 => "x", f2 => ["a"]},
                         #{f1 => "y", f2 => ["b","c"],
                           f3 => 393, o1 => {f12, 99}},
                         m1))),
    [{f1,"y"}, {f2,[]}, {f3,111}, {o1,{f11,#{g1 := [1,2,3,4]}}}] =
        lists:sort(
          maps:to_list(
            M:merge_msgs(#{f1 => "x", f2 => [],
                           f3 => 111,
                           o1 => {f11,#{g1 => [1,2]}}},
                         #{f1 => "y", f2 => [],
                           o1 => {f11,#{g1 => [3,4]}}},
                         m1))),
    unload_code(M).


%% verify ------------------------------------------------
verify_maps_with_opts_present_undefined_test() ->
    M = compile_iolist(["message m1 {",
                        "  required string f1 = 1;",
                        "  repeated string f2 = 2;",
                        "  optional uint32 f3 = 3;",
                        "  oneof o1 {",
                        "    m2     f11 = 11;",
                        "    uint32 f12 = 12;",
                        "  };",
                        "}",
                        "message m2 {",
                        "  repeated uint32 g1 = 1;",
                        "}"],
                       [maps, {maps_unset_optional, present_undefined},
                        type_specs]),
    ok = M:verify_msg(#{f1 => "x", f2 => ["a"],
                        f3 => undefined, o1 => undefined},
                      m1),
    ok = M:verify_msg(#{f1 => "x", f2 => ["a"],
                        f3 => undefined, o1 => {f12,99}},
                      m1),
    ?assertError(_, M:verify_msg(#{f1 => "x", f2 => ["a"],
                                   f3 => undefined, o1 => fzx},
                                 m1)),
    ?assertError(_, M:verify_msg(#{f1 => "x", f2 => ["a"],
                                   f3 => undefined, o1 => {fzx,yy}},
                                 m1)),
    ?assertError(_, M:verify_msg(#{f1 => "x", f2 => ["a"],
                                   f3 => "abc", o1 => undefined},
                                 m1)),
    unload_code(M).

verify_maps_with_opts_omitted_test() ->
    M = compile_iolist(["message m1 {",
                        "  required string f1 = 1;",
                        "  repeated string f2 = 2;",
                        "  optional uint32 f3 = 3;",
                        "  oneof o1 {",
                        "    m2     f11 = 11;",
                        "    uint32 f12 = 12;",
                        "  };",
                        "}",
                        "message m2 {",
                        "  repeated uint32 g1 = 1;",
                        "}"],
                       [maps, {maps_unset_optional, omitted},
                        type_specs]),
    ok = M:verify_msg(#{f1 => "x", f2 => ["a"]},
                      m1),
    ok = M:verify_msg(#{f1 => "x", f2 => ["a"], o1 => {f12,99}},
                      m1),
    ?assertError(_, M:verify_msg(#{f1 => "x", f2 => ["a"], o1 => fzx},
                                 m1)),
    ?assertError(_, M:verify_msg(#{f1 => "x", f2 => ["a"], o1 => {fzx,yy}},
                                 m1)),
    ?assertError(_, M:verify_msg(#{f1 => "x", f2 => ["a"], f3 => "abc"},
                                 m1)),
    unload_code(M).


-endif. %% HAVE_MAPS
