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

-ifdef(NO_HAVE_MAPS).

no_maps_tests__test() ->
    %% rebar.config.script or the Makefile
    %% sets HAVE_MAPS if they detect that there is
    %% support for maps.
    ok.

-else. %% NO_HAVE_MAPS

-import(gpb_compile_tests, [compile_iolist/2]).
-import(gpb_compile_tests, [unload_code/1]).

-import(gpb_compile_tests, [nif_tests_check_prerequisites/1]).
-import(gpb_compile_tests, [nif_mapfield_tests_check_prerequisites/1]).
-import(gpb_compile_tests, [increase_timeouts/1]).
-import(gpb_compile_tests, [with_tmpdir/1]).
-import(gpb_compile_tests, [in_separate_vm/4]).
-import(gpb_compile_tests, [compile_nif_msg_defs/3, compile_nif_msg_defs/4]).
-import(gpb_compile_tests, [check_protoc_can_do_oneof/0]).

-define(verify_gpb_err(Expr), ?assertError({gpb_type_error, _}, Expr)).

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
                       [maps, {maps_unset_optional, omitted}, type_specs,
                        {field_pass_method, pass_as_record}]),
    Data1 = M:encode_msg(#{f1 => "x", f2 => 33}, m1),
    Data2 = M:encode_msg(#{f1 => "x"}, m1),
    [{f1,"x"}, {f2,33}] = lists:sort(maps:to_list(M:decode_msg(Data1, m1))),
    [{f1,"x"}] = lists:sort(maps:to_list(M:decode_msg(Data2, m1))),
    unload_code(M).

encode_decode_submsg_with_omitted_test() ->
    M = compile_iolist(["message t1 {",
                        "  optional m2 f2 = 3;",
                        "  required m2 f4 = 4;"
                        "};"
                        "message t2 {",
                        "  oneof o {",
                        "    m2 f2 = 3;",
                        "  }",
                        "};",
                        "message m2 {",
                        "  optional uint32 sf1 = 11;",
                        "};"],
                       [maps, {maps_unset_optional, omitted}, type_specs,
                        {field_pass_method,pass_as_params}]),
    BT1 = M:encode_msg(#{f2 => #{sf1 => 11}, f4 => #{}}, t1),
    [{f2,#{sf1:=11}},{f4,#{}}] = maps:to_list(M:decode_msg(BT1, t1)),
    BT2 = M:encode_msg(#{o => {f2, #{sf1 => 11}}}, t2),
    [{o,{f2,#{sf1:=11}}}] = maps:to_list(M:decode_msg(BT2, t2)),
    unload_code(M).

decode_merge_submsg_with_omitted_test() ->
    M = compile_iolist(["message t1 { oneof c {s2 a = 1;} };"
                        "message s2 { repeated uint32 f = 2; };"],
                       [maps, {maps_unset_optional, omitted}, type_specs,
                        {field_pass_method,pass_as_record}]),
    M1 = #{c => {a, #{f => [395]}}},
    M2 = #{c => {a, #{f => []}}},
    B1 = M:encode_msg(M1, t1),
    B2 = M:encode_msg(M2, t1),
    #{c := {a, #{f := [395]}}} = M:merge_msgs(M1, M2, t1),
    #{c := {a, #{f := [395]}}} = M:decode_msg(<<B1/binary, B2/binary>>, t1),
    unload_code(M).

maps_with_defaults_test() ->
    M = compile_iolist(["message t1 { optional uint32 f = 2 [default=2];};"],
                       [maps, type_specs, {maps_unset_optional, omitted}]),
    ?assertEqual(#{}, M:decode_msg(<<>>, t1)),
    unload_code(M).

-define(matches_either_or(ExpectedAlt1, ExpectedAlt2, Expr),
        begin
            Actual = Expr,
            try ExpectedAlt1 = Actual
            catch error:badmatch ->
                    try ExpectedAlt2 = Actual
                    catch error:badmatch ->
                            error({neither,
                                   ??ExpectedAlt1, 'nor', ??ExpectedAlt2,
                                   matched,Actual})
                    end
            end
        end).

map_type_test() ->
    M = compile_iolist(["message m1 { map<string,fixed32> a = 1; };"],
                       [maps, type_specs]),
    ?matches_either_or( % order of maps:to_list(#{x=>_,y=>_}) undefined
       <<10,8,10,1,"x", 21,17:32/little,
         10,8,10,1,"y", 21,18:32/little>>,
       <<10,8,10,1,"x", 21,18:32/little,
         10,8,10,1,"y", 21,17:32/little>>,
       M:encode_msg(#{a => #{"x" => 17,"y" => 18}}, m1)),

    #{a := #{"x" := 17,"y" := 18}} =
        M:decode_msg(
          %% A map with "x" => 16, (not to be included)
          %%            "x" => 17  (overrides "x" => 16)
          %%        and "y" => 18
          <<10,8,10,1,"x", 21,16:32/little,
            10,8,10,1,"x", 21,17:32/little,
            10,8,10,1,"y", 21,18:32/little>>,
          m1),

    #{a := #{"x" := 17, "y" := 18, "z" := 19}} =
        M:merge_msgs(#{a => #{"x" => 16, "y" => 18}},
                     #{a => #{"x" => 17, "z" => 19}},
                     m1),

    ok = M:verify_msg(#{a => #{"x" => 17, "y" => 18}}, m1),
    ?verify_gpb_err(M:verify_msg(#{a => not_a_map}, m1)),
    ?verify_gpb_err(M:verify_msg(#{a => #{16 => "x"}}, m1)), %% wrong key type
    ?verify_gpb_err(M:verify_msg(#{a => #{"x" => "wrong value type"}}, m1)),

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
                        "}"
                        "message m0 {",
                        "  optional m2 f2 = 2;",
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
    #{f2 := #{g1 := [1,2,3,4]}} =
        M:merge_msgs(#{f2 => #{g1 => [1,2]}},
                     #{f2 => #{g1 => [3,4]}},
                     m0),
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

%% verify ------------------------------------------------

nif_test_() ->
    increase_timeouts(
      nif_tests_check_prerequisites(
        [{"Nif encode decode with unset optionals present_undefined",
          fun nif_encode_decode_present_undefined/0},
         {"Nif encode decode with unset optionals omitted",
          fun nif_encode_decode_omitted/0},
         increase_timeouts(
           nif_mapfield_tests_check_prerequisites(
             [{"Encode decode", fun nif_encode_decode_mapfields/0}]))])).

nif_encode_decode_present_undefined() ->
    ProtocCanOneof = check_protoc_can_do_oneof(),
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_mpu_ed1,
              Defs = ["message x_mpu {\n",
                      "    optional uint32 o1 = 1;\n",
                      "    optional uint32 o2 = 2;\n",
                      [["    oneof oo1 {\n",
                        "        uint32 of1 = 3;\n",
                        "    }\n",
                        "    oneof oo2 {\n",
                        "        uint32 of2 = 4;\n",
                        "    }\n"] || ProtocCanOneof],
                      "    required uint32 rq = 5;\n",
                      "    repeated uint32 rp = 6;\n",
                      "}\n"],
              Opts = [maps, {maps_unset_optional, present_undefined}],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg01 = #{o1 => 1, o2 => undefined, rq => 4, rp => [5]},
                        Msg1 = if ProtocCanOneof ->
                                       Msg01#{oo1 => undefined, oo2 => {of2,4}};
                                  true ->
                                       Msg01
                               end,
                        Bin1 = M:encode_msg(Msg1, x_mpu),
                        Msg1 = M:decode_msg(Bin1, x_mpu),
                        ok
                end)
      end).

nif_encode_decode_omitted() ->
    ProtocCanOneof = check_protoc_can_do_oneof(),
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_mo_ed1,
              Defs = ["message x_mo {\n",
                      "    optional uint32 o1 = 1;\n",
                      "    optional uint32 o2 = 2;\n",
                      [["    oneof oo1 {\n",
                        "        uint32 of1 = 3;\n",
                        "    }\n",
                        "    oneof oo2 {\n",
                        "        uint32 of2 = 4;\n",
                        "    }\n"] || ProtocCanOneof],
                      "    required uint32 rq = 5;\n",
                      "    repeated uint32 rp = 6;\n",
                      "}\n"],
              Opts = [maps, {maps_unset_optional, omitted}],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg01 = #{o1 => 1, rq => 4, rp => [5]},
                        Msg1 = if ProtocCanOneof ->
                                       Msg01#{oo2 => {of2,4}};
                                  true ->
                                       Msg01
                               end,
                        Bin1 = M:encode_msg(Msg1, x_mo),
                        Msg1 = M:decode_msg(Bin1, x_mo),
                        ok
                end)
      end).

nif_encode_decode_mapfields() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_maps_with_mapfields_ed1,
              %% No need to test all types of keys/values, since that
              %% is already done in gpb_compile_tests.  Test only the
              %% map mechanism (as opposed to list of 2-tuples) here.
              Defs = ["message x {\n",
                      "   map<fixed32,string> i2s = 1;\n"
                      "}"],
              Opts = [maps],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg1 = #{i2s => #{11 => "aa",
                                          22 => "bb",
                                          33 => "cc"}},
                        Bin1 = M:encode_msg(Msg1, x),
                        Msg1 = M:decode_msg(Bin1, x),

                        Msg2 = #{i2s => #{}},
                        Bin2 = M:encode_msg(Msg2, x),
                        Msg2 = M:decode_msg(Bin2, x),

                        ok
                end)
      end).

-endif. %% NO_HAVE_MAPS
