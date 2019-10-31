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

-include("gpb_nif_test_helpers.hrl"). % the `?nif_if_supported(FnName)' macro

-export([dict_to_map/1, map_to_dict/1]).

%% Translators for {translate_field, {<Oneof>,...}} tests
-export([e_ipv4/1, d_ipv4/1, v_ipv4/1]).
-export([e_ipv6/1, d_ipv6/1, v_ipv6/1]).

-export([flat_map_prerequisites/1]).
-export([can_do_flat_oneof/0, can_do_flat_oneof/1]).

-import(gpb_compile_tests, [compile_iolist/2]).
-import(gpb_compile_tests, [compile_to_string_get_hrl/2]).
-import(gpb_compile_tests, [compile_erl_iolist/1]).
-import(gpb_compile_tests, [unload_code/1]).

-import(gpb_compile_tests, [nif_tests_check_prerequisites/1]).
-import(gpb_compile_tests, [with_tmpdir/2]).
-import(gpb_compile_tests, [in_separate_vm/4]).
-import(gpb_compile_tests, [compile_nif_msg_defs/4]).
-import(gpb_compile_tests, [check_protoc_can_do_oneof/0]).

-define(verify_gpb_err(Expr), ?assertError({gpb_type_error, _}, Expr)).

-define(recv(Pattern),
        (fun() -> receive Pattern=__V -> __V
                  after 4000 ->
                          error({receive_timed_out,
                                 {pattern,??Pattern},
                                 {message_queue,
                                  element(2,process_info(self(),messages))}})
                  end
         end)()).

-record(m1, {a}).

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

repeated_fields_can_be_omitted_on_input_with_opts_omitted_test() ->
    M = compile_iolist(
          ["message m1 {"
           "  repeated string    f1 = 1;",
           "  map<string,uint32> f2 = 2;", % a map<_,_> is a repeated field
           "}"],
          [maps, {maps_unset_optional, omitted}, type_specs]),
    Data1 = M:encode_msg(#{f1 => ["one", "two", "three"],
                           f2 => #{"a" => 1, "b" => 2}}, m1),
    [{f1,["one", "two", "three"]}, {f2, #{"a" := 1, "b" := 2}}] =
        map_to_sorted_list(M:decode_msg(Data1, m1)),
    %% repeated fields can be omitted on encoding
    Data2 = M:encode_msg(#{}, m1),
    [{f1,[]}, {f2, #{}}] = map_to_sorted_list(M:decode_msg(Data2, m1)),
    %% repeated fields always present on decoding (even if not preset in data)
    [{f1,[]}, {f2, #{}}] = map_to_sorted_list(M:decode_msg(<<>>, m1)),
    %% repeated fields can be omitted in calls to merge
    [] = map_to_sorted_list(M:merge_msgs(#{}, #{}, m1)),
    [{f1,["a"]}] = map_to_sorted_list(M:merge_msgs(#{f1 => ["a"]}, #{}, m1)),
    [{f1,["b"]}] = map_to_sorted_list(M:merge_msgs(#{}, #{f1 => ["b"]}, m1)),
    [{f1,["a","b"]}] = map_to_sorted_list(M:merge_msgs(#{f1 => ["a"]},
                                                       #{f1 => ["b"]}, m1)),
    [{f2,[{"a",1}]}] = map_to_sorted_list_r(
                         M:merge_msgs(#{f2 => #{"a" => 1}}, #{}, m1)),
    [{f2,[{"b",2}]}] = map_to_sorted_list_r(
                         M:merge_msgs(#{}, #{f2 => #{"b" => 2}}, m1)),
    [{f2,[{"a",1},{"b",2}]}] = map_to_sorted_list_r(
                                 M:merge_msgs(#{f2 => #{"a" => 1}},
                                              #{f2 => #{"b" => 2}},
                                              m1)),
    %% repeated fields can be omitted in calls to verify
    ok = M:verify_msg(#{}, m1),
    ok = M:verify_msg(#{f1 => []}, m1),
    ok = M:verify_msg(#{f2 => #{}}, m1),

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

map_type_with_unset_omitted_test() ->
    M = compile_iolist(["message m1 { map<string,fixed32> a = 1; };"],
                       [maps, type_specs, {maps_unset_optional,omitted}]),
    <<10,8,10,1,"x", 21,17:32/little>> = M:encode_msg(#{a => #{"x" => 17}},
                                                      m1),
    #{a := #{"x" := 17}} = M:decode_msg(<<10,8,10,1,"x", 21,17:32/little>>,
                                        m1),
    #{a := #{"x" := 17, "y" := 18, "z" := 19}} =
        M:merge_msgs(#{a => #{"x" => 16, "y" => 18}},
                     #{a => #{"x" => 17, "z" => 19}},
                     m1),
    ok = M:verify_msg(#{a => #{"x" => 17}}, m1),
    ?verify_gpb_err(M:verify_msg(#{a => not_a_map}, m1)),
    ?verify_gpb_err(M:verify_msg(#{a => #{16 => "x"}}, m1)), %% wrong key type
    ?verify_gpb_err(M:verify_msg(#{a => #{"x" => "wrong value type"}}, m1)),
    unload_code(M).

map_type_with_mapfields_as_maps_option_test() ->
    %% messages are records, but map<_,_> are maps
    M1 = compile_iolist(["message m1 { map<string,fixed32> a = 1; };"],
                        [mapfields_as_maps]),
    Msg1 = #m1{a = #{"x" => 17}},
    ok = M1:verify_msg(Msg1),
    B1 = M1:encode_msg(Msg1),
    Msg1 = M1:decode_msg(B1, m1),
    Msg1 = M1:merge_msgs(Msg1, Msg1),
    #m1{a=EmptyMap} = M1:decode_msg(<<>>, m1),
    0 = maps:size(EmptyMap),
    unload_code(M1),

    %% messages are maps, but map<_,_> are 2-tuples
    M2 = compile_iolist(["message m2 { map<string,fixed32> a = 1; };"],
                        [msgs_as_maps, {mapfields_as_maps, false}]),
    Msg2 = #{a => [{"x",17}]},
    ok = M2:verify_msg(Msg2, m2),
    B2 = M2:encode_msg(Msg2, m2),
    Msg2 = M2:decode_msg(B2, m2),
    Msg2 = M2:merge_msgs(Msg2, Msg2, m2),
    unload_code(M2).

flat_oneof_maps_test_() ->
    flat_map_prerequisites(
      [{"pass as params", fun() -> flat_oneof_maps_test_aux(pass_as_params) end},
       {"pass as record", fun() -> flat_oneof_maps_test_aux(pass_as_record) end}]).

flat_oneof_maps_test_aux(FieldPass) ->
    M = compile_iolist(["message m1 {",
                        "  oneof x {",
                        "    uint32 a = 1;",
                        "    string b = 2;",
                        "    m3     c = 3;",
                        "    m4     d = 4;",
                        "  }",
                        "}",
                        "message m3 { required uint32 f = 3; }",
                        "message m4 { repeated uint32 g = 4; }"],
                        [maps, {maps_unset_optional,omitted},
                         {maps_oneof, flat},
                         {field_pass_method, FieldPass}]),
    <<8,100>>     = B1A = M:encode_msg(#{a => 100}, m1),
    <<18, 1,"x">> = B1B = M:encode_msg(#{b => "x"}, m1),
    ok = M:verify_msg(#{a => 1}, m1),
    ok = M:verify_msg(#{c => #{f => 103}}, m1),
    ?verify_gpb_err(M:verify_msg(#{a => 1, b => 2}, m1)), % dup fields
    ?verify_gpb_err(M:verify_msg(#{x => {a, 1}}, m1)), % x is extraneous
    ?verify_gpb_err(M:verify_msg(#{x => {a, 1}}, m1)), % x is extraneous
    #{a := 100} = M:decode_msg(B1A, m1),
    #{b := "x"} = M:decode_msg(B1B, m1),
    ?assertEqual(#{b => "x"},
                 M:decode_msg(<<B1A/binary, B1B/binary>>, m1)), % merge -> b
    ?assertEqual(#{a => 100},
                 M:decode_msg(<<B1B/binary, B1A/binary>>, m1)), % merge -> a
    ?assertEqual(#{b => "x"},
                 M:merge_msgs(#{a => 100}, #{b => "x"}, m1)),
    ?assertEqual(#{a => 100},
                 M:merge_msgs(#{b => "x"}, #{a => 100}, m1)),

    %% -- Now for when there are sub msgs --
    %%
    <<26,2, 24,103>> = B1Ca = M:encode_msg(#{c => #{f => 103}}, m1),
    <<26,2, 24,113>> = B1Cb = M:encode_msg(#{c => #{f => 113}}, m1),
    #{c := #{f := 103}}     = M:decode_msg(B1Ca, m1),
    <<34,2, 32,104>> = B1Da  = M:encode_msg(#{d => #{g => [104]}}, m1),
    <<34,2, 32,114>> = B1Db  = M:encode_msg(#{d => #{g => [114]}}, m1),
    %% merging same oneof
    ?assertEqual(#{c => #{f => 103}},
                 M:decode_msg(<<B1Cb/binary, B1Ca/binary>>, m1)),
    ?assertEqual(#{d => #{g => [104,114]}},
                 M:decode_msg(<<B1Da/binary, B1Db/binary>>, m1)),
    %% merging different oneofs
    ?assertEqual(#{d => #{g => [104]}},
                 M:decode_msg(<<B1Ca/binary, B1Da/binary>>, m1)),
    ?assertEqual(#{b => "x"},
                 M:decode_msg(<<B1Ca/binary, B1B/binary>>, m1)),

    ?assertEqual(#{c => #{f => 113}},
                 M:merge_msgs(#{c => #{f => 103}}, #{c => #{f => 113}}, m1)),
    ?assertEqual(#{d => #{g => [104]}},
                 M:merge_msgs(#{c => #{f => 103}}, #{d => #{g => [104]}}, m1)),
    ?assertEqual(#{d => #{g => [104,114]}},
                 M:merge_msgs(#{d => #{g => [104]}}, #{d => #{g => [114]}},
                              m1)),
    ?assertEqual(#{b => "x"},
                 M:merge_msgs(#{c => #{f => 103}}, #{b => "x"}, m1)),
    unload_code(M).

required_group_test() ->
    Mod = compile_iolist(
            ["message m1 {",
             "  required group g = 30 { required fixed32 gf = 35; }",
             "}"],
            [maps, type_specs]),
    M = #{g => #{gf => 17}},
    D = "f3 01 9d 02   11 00 00 00 f4 01",
    B = hexundump(D),
    B = Mod:encode_msg(M, m1),
    M = Mod:decode_msg(B, m1),
    ok = Mod:verify_msg(#{g => #{gf => 17}}, m1),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1.g.gf'}]}},
                 Mod:verify_msg(#{g => #{gf => x}}, m1)),
    unload_code(Mod).

repeated_and_optional_group_test() ->
    Mod = compile_iolist(
            ["message m1 {",
             "  repeated group g = 31 { required fixed32 gf = 36; };",
             "  optional group h = 32 { required fixed32 hf = 37; };",
             "}"],
            [maps, {maps_unset_optional, omitted}, type_specs]),
    M1 = #{g => [#{gf => 17},#{gf => 18}]},
    B1 = hexundump("fb 01 a5 02   11 00 00 00 fc 01"
                   "fb 01 a5 02   12 00 00 00 fc 01"),
    B1 = Mod:encode_msg(M1, m1),
    M1 = Mod:decode_msg(B1, m1),
    %% Now with merge
    M2 = #{g => [], h => #{hf => 99}},
    B21 = hexundump("83 02 ad 02   11 00 00 00 84 02"), % 17 to be over-merged
    B22 = hexundump("83 02 ad 02   63 00 00 00 84 02"), % 99 to replace
    B22 = Mod:encode_msg(M2, m1),
    M2  = Mod:decode_msg(<<B21/binary, B22/binary>>, m1),
    M2  = Mod:merge_msgs(#{g => []}, M2, m1),
    ok = Mod:verify_msg(#{g => []}, m1),
    ok = Mod:verify_msg(#{g => [#{gf => 17},#{gf => 18}]}, m1),
    ok = Mod:verify_msg(#{g => [], h => #{hf => 4711}}, m1),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1.g'}]}},
                 Mod:verify_msg(#{g => #{gf => x}}, m1)),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1.g.gf'}]}},
                 Mod:verify_msg(#{g => [#{gf => x}]}, m1)),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1.h.hf'}]}},
                 Mod:verify_msg(#{g => [], h => #{hf => x}}, m1)),
    unload_code(Mod).

%%hexdump(B) ->
%%    string:to_lower(lists:concat([integer_to_list(C,16) || <<C:4>> <= B])).
hexundump(S) ->
    <<<<(list_to_integer([C],16)):4>> || C <- S, is_hex_digit(C)>>.
is_hex_digit(D) when $0 =< D, D =< $9 -> true;
is_hex_digit(D) when $a =< D, D =< $f -> true;
is_hex_digit(D) when $A =< D, D =< $F -> true;
is_hex_digit(_) -> false.

fetch_rpc_def_test() ->
    M = compile_iolist(["message m { required uint32 f = 1; };\n",
                        "service s { rpc r(m) returns(m); }\n"],
                       [maps, type_specs]),
    #{name := r, input := m, output := m} = M:fetch_rpc_def(s,r),
    ?assertError(_, M:fetch_rpc_def(s,some_bad_rpc_name)),
    unload_code(M).

defs_as_maps_means_no_include_of_gpb_hrl_test() ->
    Proto = "message m1 { required uint32 f = 1; }",
    Self = self(),
    ok = gpb_compile:string(
           dummy_defs_as_maps,
           Proto,
           [defs_as_maps,
            {file_op, [{write_file, fun(FName,Data) ->
                                            case filename:extension(FName) of
                                                ".erl" -> Self ! {data, Data};
                                                _ -> ok
                                            end,
                                            ok
                                    end}]}]),
    {data,Bin} = ?recv({data,_}),
    %% Check that (only) one -include line is present:
    %% "dummy_defs_as_maps.hrl" (since no option msgs_as_maps)
    %% but not: "gpb.hrl" (since we've option defs_as_maps)
    Lines = gpb_lib:string_lexemes(binary_to_list(Bin), "\n"),
    [Inc] = lines_matching("-include", Lines),
    {true,false,_} = {gpb_lib:is_substr("dummy_defs_as_maps.hrl",Inc),
                      gpb_lib:is_substr("gpb.hrl", Inc),
                      Inc}.

lines_matching(Text, Lines) ->
    [Line || Line <- Lines, gpb_lib:is_substr(Text, Line)].

map_to_sorted_list(M) ->
    lists:sort(maps:to_list(M)).

map_to_sorted_list_r(M) ->
    [{K,if is_map(V) -> map_to_sorted_list_r(V);
           true      -> V
        end}
     || {K, V} <- lists:sort(maps:to_list(M))].

%% --- default values --------------

default_value_handling_test_() ->
    {timeout,30,fun 'default_value_handling_test_aux'/0}.

default_value_handling_test_aux() ->
    Proto = ["message m {",
             "  optional uint32 f1 = 1;",
             "  optional uint32 f2 = 2 [default=2];",
             "}"],
    FieldNames = [f1,f2],
    [begin
         AllOpts = Opts ++ OptVariation1 ++ OptVariation2 ++ [maps],
         M = compile_iolist(Proto, AllOpts),
         FVs = lists:zip(FieldNames, tl(tuple_to_list(BaseExpected))),
         FVs2 = case proplists:get_value(maps_unset_optional, AllOpts) of
                    present_undefined -> FVs;
                    omitted           -> [{F,V} || {F,V} <- FVs, V /= undefined]
                end,
         Expected = maps:from_list(FVs2),
         ?assertMatch({Expected,_}, {M:decode_msg(<<>>, m), Opts}),
         unload_code(M)
     end
     || {BaseExpected, Opts} <-
            [{{m,undefined,undefined}, []},
             {{m,0,2},         [defaults_for_omitted_optionals,
                                type_defaults_for_omitted_optionals]},
             {{m,undefined,2}, [defaults_for_omitted_optionals]},
             {{m,0,0},         [type_defaults_for_omitted_optionals]}],
        OptVariation1 <- [[pass_as_params],
                          [pass_as_record]],
        OptVariation2 <- [[{maps_unset_optional,present_undefined}],
                          [{maps_unset_optional,omitted}]]].

defaults_for_proto3_fields_test() ->
    Proto = ["syntax=\"proto3\";\n",
             "message m {",
             "  map<int32, int32> mii = 41;",
             "}"],
    P3M = compile_erl_iolist(
             ["-export([new_m_msg/0]).\n",
              compile_to_string_get_hrl(
                Proto,
                [type_specs, strip_preprocessor_lines, mapfields_as_maps]),
              "new_m_msg() -> #m{}.\n"]),
    {m, #{}} = P3M:new_m_msg(),
    unload_code(P3M).

defaults_for_unset_proto3_submessages_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";\n",
           "\n",
           "message Cfg {",
           "  XList l = 1;",
           "  message XList {",
           "    repeated Item elem = 1;",
           "  }",
           "}",
           "message Item {",
           "  uint32 f1 = 1;",
           "}"],
          [maps %%  {maps_unset_optional, omitted} is default
          ]),
    Cfg = M:decode_msg(<<>>, 'Cfg'),
    {0,_} = {maps:size(Cfg),Cfg},
    unload_code(M).

defaults_for_unset_proto3_oneof_test() ->
    M = compile_iolist(
          ["syntax=\"proto3\";\n",
           "\n",
           "message Msg {",
           "  oneof c {",
           "    uint32 a = 1;",
           "    uint32 b = 2;",
           "  }",
           "}"],
          [maps %%  {maps_unset_optional, omitted} is default
          ]),
    Msg = M:decode_msg(<<>>, 'Msg'),
    {0,_} = {maps:size(Msg),Msg},
    unload_code(M).

%% -- translations
%%-
translate_maptype_test() ->
    %% For this tests, the internal format of the map<_,_> is a dict()
    M = compile_iolist(
          ["message m {",
           "  map<int32,string> m = 1;"
           "}"],
          [maps,
           {translate_type,
            {{map,int32,string},
             [{encode,{?MODULE, dict_to_map, ['$1']}},
              {decode,{?MODULE, map_to_dict, ['$1']}},
              {verify,{gpb_compile_tests, is_dict, ['$1']}}]}}]),
    D0 = dict:from_list([{1,"one"},{2,"two"}]),
    M1 = #{m => D0},
    ok = M:verify_msg(M1, m),
    B1 = M:encode_msg(M1, m),
    #{m := D1} = M:decode_msg(B1, m),
    ?assertEqual(lists:sort(dict:to_list(D0)),
                 lists:sort(dict:to_list(D1))),
    ?assertError({gpb_type_error, _}, M:verify_msg(#{m => not_a_dict}, m)),
    unload_code(M).

dict_to_map(D) -> maps:from_list(dict:to_list(D)).
map_to_dict(M) -> dict:from_list(maps:to_list(M)).

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
          [maps,
           {translate_field,
            {[m,ip], [{encode, {gpb_compile_tests, e_ipv4or6, ['$1']}},
                      {decode, {gpb_compile_tests, d_ipv4or6, ['$1']}},
                      {verify, {gpb_compile_tests, v_ipv4or6, ['$1']}}]}}]),
    M1 = #{ip => {127,0,0,1}},
    M2 = #{ip => {0,0,0,0, 0,0,0,1}},
    ok = M:verify_msg(M1, m),
    ok = M:verify_msg(M2, m),
    <<13, _/bits>>    = B1 = M:encode_msg(M1, m), % check field tag+wiretype
    <<18,16, _/bits>> = B2 = M:encode_msg(M2, m), % check field tag+wiretype+len
    M1 = M:decode_msg(B1, m),
    M2 = M:decode_msg(B2, m),
    ?assertError({gpb_type_error, _},
                 M:verify_msg(#{ip => {1,2,3,4,5,6}}, m)), % wrong tuple size
    unload_code(M).

translate_flat_oneof_maps_test_() ->
    flat_map_prerequisites(
      [{"translate flat oneof", fun translate_flat_oneof_test_aux/0}]).

translate_flat_oneof_test_aux() ->
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
          [maps,
           {maps_oneof, flat},
           {translate_field,
            {[m,ip,ipv4],
             [{encode, {?MODULE, e_ipv4, ['$1']}},
              {decode, {?MODULE, d_ipv4, ['$1']}},
              {verify, {?MODULE, v_ipv4, ['$1']}}]}},
           {translate_field,
            {[m,ip,ipv6],
             [{encode, {?MODULE, e_ipv6, ['$1']}},
              {decode, {?MODULE, d_ipv6, ['$1']}},
              {verify, {?MODULE, v_ipv6, ['$1']}}]}}]),
    M1 = #{ipv4 => {127,0,0,1}},
    M2 = #{ipv6 => {0,0,0,0, 0,0,0,1}},
    ok = M:verify_msg(M1, m),
    ok = M:verify_msg(M2, m),
    <<13, _/bits>>    = B1 = M:encode_msg(M1, m), % check field tag+wiretype
    <<18,16, _/bits>> = B2 = M:encode_msg(M2, m), % check field tag+wiretype+len
    M1 = M:decode_msg(B1, m),
    M2 = M:decode_msg(B2, m),
    ?assertError({gpb_type_error, _},
                 M:verify_msg(#{ipv4 => {1,2,3,4,5,6}}, m)),
    ?assertError({gpb_type_error, _},
                 M:verify_msg(#{ipv6 => {1,2,3,4,5,6}}, m)),
    unload_code(M).

e_ipv4(Ip) -> element(2,gpb_compile_tests:e_ipv4or6(Ip)).
d_ipv4(Ip) -> gpb_compile_tests:d_ipv4or6({ipv4,Ip}).
v_ipv4(Ip) -> gpb_compile_tests:v_ipv4or6(Ip).

e_ipv6(Ip) -> element(2,gpb_compile_tests:e_ipv4or6(Ip)).
d_ipv6(Ip) -> gpb_compile_tests:d_ipv4or6({ipv6,Ip}).
v_ipv6(Ip) -> gpb_compile_tests:v_ipv4or6(Ip).

%% --- target versions ----------------------------------

type_syntax_for_required_fields_test() ->
    %% In Erlang/OTP 19, required and optional fields for maps can be defined
    %% using ":=" and "=>" respectively, like this:
    %%
    %%     -type m() :: #{req := integer(),
    %%                    opt => string()}.
    %%
    %% For Erlang/OTP 18 and earlier, only "=>" is available, so do
    %% the best we can, and generate type specs like this:
    %%
    %%     -type m() :: #{req => integer()
    %%                    %% opt => string()
    %%                   }.
    %%
    Proto = "message m { required uint32 f = 1; }",
    CommonOpts = [type_specs, maps],

    S1 = compile_to_string(Proto, [{target_erlang_version,18} | CommonOpts]),
    T1 = get_type(S1),
    [true, false] = [gpb_lib:is_substr(X, T1) || X <- ["=>", ":="]],

    S2 = compile_to_string(Proto, [{target_erlang_version,19} | CommonOpts]),
    T2 = get_type(S2),
    [false, true] = [gpb_lib:is_substr(X, T2) || X <- ["=>", ":="]].

compile_to_string(Proto, Opts) ->
    Self = self(),
    FileOps = [{write_file, fun(FName,Data) ->
                                    case filename:extension(FName) of
                                        ".erl" -> Self ! {data, Data};
                                        _ -> ok
                                    end,
                                    ok
                            end}],
    ok = gpb_compile:string(some_module, Proto, [Opts | [{file_op, FileOps}]]),
    {data,Bin} = ?recv({data,_}),
    binary_to_list(Bin).

get_type(S) -> get_type_2(gpb_lib:string_lexemes(S, "\n")).

get_type_2(["-type"++_=S | Rest]) -> get_type_3(Rest, [S]);
get_type_2([_ | Rest])            -> get_type_2(Rest);
get_type_2([])                    -> "".

get_type_3([Line | Rest], Acc) ->
    case Line of
        "  "++_ -> get_type_3(Rest, [Line | Acc]);
        _       -> gpb_lib:nl_join(lists:reverse(Acc))
    end;
get_type_3([], Acc) ->
    gpb_lib:nl_join(lists:reverse(Acc)).

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

merge_x_test() ->
    M = compile_iolist(["message m2 {",
                        "  map<bool,m1>  b = 2;",
                        "}",
                        "message m1 {",
                        "  required bool a = 1;"
                        "}"],
                       [maps, {maps_unset_optional, omitted}]),
    M2a = #{b => #{}},
    M2b = #{b => #{false => #{a=> false}}},
    %% verify round-triping first
    M2a = M:decode_msg(M:encode_msg(M2a, m2), m2),
    M2b = M:decode_msg(M:encode_msg(M2b, m2), m2),
    %% verify merging
    MM = M:merge_msgs(M2a, M2b, m2),
    MM = M:decode_msg(<<(M:encode_msg(M2a, m2))/binary,
                        (M:encode_msg(M2b, m2))/binary>>,
                      m2),
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

extraneous_keys_with_opts_omitted_test() ->
    M = compile_iolist(["message m1 {",
                        "  optional uint32 bar_value = 3;",
                        "  oneof o1 {",
                        "    uint32 a = 11;",
                        "  };",
                        "}"],
                       [maps, {maps_unset_optional, omitted},
                        type_specs]),
    ok = M:verify_msg(#{}, m1),
    ok = M:verify_msg(#{bar_value => 33}, m1),
    ok = M:verify_msg(#{o1 => {a,111}}, m1),
    ?assertError(_, M:verify_msg(#{bar => 33}, m1)), % misspelling of bar_value
    ?assertError(_, M:verify_msg(#{o2  => {a,11}}, m1)), % misspelling of o1
    unload_code(M).

%% --- binary() map keys ------------

basic_binary_map_keys_test() ->
    M = compile_iolist(["message m {",
                        "  required uint32 a = 1;",
                        "  optional bool   b = 2;",
                        "  repeated uint32 c = 3;",
                        "}"],
                       [maps, {maps_key_type, binary}]),
    %% encode and decode
    M1 = #{<<"a">> => 1, <<"b">> => true, <<"c">> => [3,4,5]},
    B1 = M:encode_msg(M1, m),
    ?assertEqual(M1, M:decode_msg(B1, m)),
    %% merge
    M2 = #{<<"a">> => 11, <<"c">> => [33,44,55]},
    B2 = M:encode_msg(M2, m),
    M2M = #{<<"a">> => 11, <<"b">> => true, <<"c">> => [3,4,5,33,44,55]},
    ?assertEqual(M2M, M:merge_msgs(M1, M2, m)),
    ?assertEqual(M2M, M:decode_msg(<<B1/binary, B2/binary>>, m)),
    %% verify
    ?assertError(_, M:verify_msg(#{}, m)), % required field not set
    ?assertError(_, M:verify_msg(#{a => 1}, m)), % wrong key type
    ?assertError(_, M:verify_msg(#{<<"a">> => "abc"}, m)), % wrong value type
    unload_code(M).

oneof_binary_map_keys_test() ->
    M = compile_iolist(["message m {",
                        "  required uint32 a  = 1;",
                        "  oneof c {",
                        "    string x = 22;",
                        "    uint32 y = 23;",
                        "  }",
                        "}"],
                       [maps, {maps_key_type, binary}]),
    M1 = #{<<"a">> => 11}, % oneof field not set
    B1 = M:encode_msg(M1, m),
    ?assertEqual(M1, M:decode_msg(B1, m)),

    M2 = #{<<"a">> => 11, <<"c">> => {x, "x"}}, % oneof field set
    B2 = M:encode_msg(M2, m),
    ?assertEqual(M2, M:decode_msg(B2, m)),
    unload_code(M).

binary_map_keys_not_mixed_with_map_types_test() ->
    M = compile_iolist(["message m {",
                        "  required uint32 a  = 1;",
                        "  map<uint32,string> mm = 2;",
                        "}"],
                       [maps, {maps_key_type, binary}]),
    M1 = #{<<"a">> => 11, <<"mm">> => #{}},
    B1 = M:encode_msg(M1, m),
    ?assertEqual(M1, M:decode_msg(B1, m)),

    M2 = #{<<"a">> => 12, <<"mm">> => #{1 => "one", 2 => "two"}},
    B2 = M:encode_msg(M2, m),
    ?assertEqual(M2, M:decode_msg(B2, m)),

    M3 = #{<<"a">> => 12, <<"mm">> => #{3 => "three"}},
    B3 = M:encode_msg(M3, m),
    M3M = #{<<"a">> => 12, <<"mm">> => #{1 => "one", 2 => "two", 3 => "three"}},
    ?assertEqual(M3M, M:decode_msg(<<B2/binary, B3/binary>>, m)),
    ?assertEqual(M3M, M:merge_msgs(M2, M3, m)),

    unload_code(M).

verify_extraneous_keys_for_binary_map_keys_test() ->
    M = compile_iolist(["message m {",
                        "  required uint32 a = 1;",
                        "  optional bool   b = 2;",
                        "  repeated uint32 c = 3;",
                        "}"],
                       [maps, {maps_key_type, binary}]),
    %% encode and decode
    M1 = #{<<"a">> => 1, <<"b">> => true, <<"c">> => [3,4,5]},
    ok = M:verify_msg(M1, m),
    ?assert(is_binary(M:encode_msg(M1, m, [verify]))),
    ?assertError({gpb_type_error,_}, M:verify_msg(M1#{<<"d">> => 4}, m)),
    ?assertError({gpb_type_error,_}, M:encode_msg(M1#{<<"d">> => 4}, m,
                                                  [verify])),
    unload_code(M).

verify_missing_nonoptional_key_for_binary_map_keys_test() ->
    M = compile_iolist(["message m {",
                        "  required uint32 a = 1;",
                        "  optional bool   b = 2;",
                        "  repeated uint32 c = 3;",
                        "}"],
                       [maps, {maps_key_type, binary}]),
    %% encode and decode
    M1 = #{<<"a">> => 1, <<"b">> => true, <<"c">> => [3,4,5]},
    ok = M:verify_msg(M1, m),
    ?assert(is_binary(M:encode_msg(M1, m, [verify]))),
    ?assertError({gpb_type_error,{{missing_fields,[<<"a">>],m},_}},
                 M:verify_msg(#{}, m)),
    ?assertError({gpb_type_error,{{missing_fields,[<<"a">>],m},_}},
                 M:encode_msg(#{}, m, [verify])),
    unload_code(M).

bypassed_wrappers_maps_test() ->
    DefsM1 = "message m1 { required uint32 a = 1; }\n",
    DefsNoMsgs = "enum ee { a = 0; }\n",

    Mod1 = compile_iolist(DefsM1, [bypass_wrappers, maps]),
    M1 = #{a => 1234},
    B1 = Mod1:encode_msg_m1(M1),
    B1 = Mod1:encode_msg_m1(M1, undefined),
    ?assertMatch(true, is_binary(B1)),
    M1 = Mod1:decode_msg_m1(B1),
    M1 = Mod1:decode_msg_m1(B1, undefined),
    unload_code(Mod1),

    %% verify no compatibility functions generated with no compat options
    Mod2 = compile_iolist(DefsM1, [maps]),
    ?assertError(undef, Mod2:encode_msg_m1(M1)),
    ?assertError(undef, Mod2:decode_msg_m1(B1)),
    unload_code(Mod2),

    %% verify functions generated ok when no msgs specified
    Mod3 = compile_iolist(DefsNoMsgs, [maps]),
    _ = Mod3:module_info(),
    unload_code(Mod3).


%% nif ------------------------------------------------

nif_test_() ->
    nif_tests_check_prerequisites(
      [?nif_if_supported(nif_encode_decode_present_undefined),
       ?nif_if_supported(nif_encode_decode_omitted),
       ?nif_if_supported(nif_encode_decode_flat_oneof),
       ?nif_if_supported(nif_encode_decode_flat_oneof_proto3),
       ?nif_if_supported(nif_encode_decode_binary_keys),
       ?nif_if_supported(nif_encode_decode_mapfields),
       ?nif_if_supported(nif_with_mapfields_as_maps),
       ?nif_if_supported(bypass_wrappers_maps_nif)]).

nif_encode_decode_present_undefined(features) -> [oneof];
nif_encode_decode_present_undefined(title) ->
    "Nif encode decode with unset optionals present_undefined".
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

nif_encode_decode_omitted(features) -> [oneof];
nif_encode_decode_omitted(title) ->
    "Nif encode decode with unset optionals omitted".
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

nif_encode_decode_flat_oneof(features) -> [oneof];
nif_encode_decode_flat_oneof(extra_checks) -> [fun can_do_flat_oneof/1];
nif_encode_decode_flat_oneof(title) -> "Nif encode decode with flat oneof".
nif_encode_decode_flat_oneof() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_fo_ed1,
              Defs = ["message x_fo {",
                      "  oneof x {",
                      "    uint32 a = 1;",
                      "    string b = 2;",
                      "    x_fo3  c = 3;",
                      "    x_fo4  d = 4;",
                      "  }",
                      "}",
                      "message x_fo3 { required uint32 f = 3; }",
                      "message x_fo4 { repeated uint32 g = 4; }"],
              Opts = [maps, {maps_unset_optional,omitted}, {maps_oneof, flat}],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg1 = #{a => 100},
                        <<8, 100>>  = B1A = M:encode_msg(Msg1, x_fo),
                        ?assertEqual(Msg1, M:decode_msg(B1A, x_fo)),

                        Msg2 = #{c => #{f => 13}},
                        <<26,2, 24,13>> = B1C = M:encode_msg(Msg2, x_fo),
                        ?assertEqual(Msg2, M:decode_msg(B1C, x_fo)),

                        Msg3 = #{d => #{g => [14,24]}},
                        <<34,4, 32,14, 32,24>> = B1D = M:encode_msg(Msg3, x_fo),
                        ?assertEqual(Msg3, M:decode_msg(B1D, x_fo)),
                        ok
                end)
      end).

nif_encode_decode_flat_oneof_proto3(features) -> [oneof, proto3];
nif_encode_decode_flat_oneof_proto3(extra_checks) -> [fun can_do_flat_oneof/1];
nif_encode_decode_flat_oneof_proto3(title) -> "Flat oneof with proto3".
nif_encode_decode_flat_oneof_proto3() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_fo_ed1,
              Defs = ["syntax=\"proto3\";",
                      "message x_fo {",
                      "  oneof x {",
                      "    uint32 a = 1;",
                      "    string b = 2;",
                      "    x_fo3  c = 3;",
                      "    x_fo4  d = 4;",
                      "  }",
                      "}",
                      "message x_fo3 { uint32 f = 3; }",
                      "message x_fo4 { repeated uint32 g = 4; }"],
              Opts = [maps, {maps_unset_optional,omitted}, {maps_oneof, flat}],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg1 = #{a => 100},
                        <<8, 100>>  = B1A = M:encode_msg(Msg1, x_fo),
                        ?assertEqual(Msg1, M:decode_msg(B1A, x_fo)),

                        Msg2 = #{c => #{f => 13}},
                        <<26,2, 24,13>> = B1C = M:encode_msg(Msg2, x_fo),
                        ?assertEqual(Msg2, M:decode_msg(B1C, x_fo)),

                        Msg3 = #{d => #{g => [14,24]}},
                        <<34,4, 34,2, 14,24>> = B1D = M:encode_msg(Msg3, x_fo),
                        ?assertEqual(Msg3, M:decode_msg(B1D, x_fo)),
                        ok
                end)
      end).


nif_encode_decode_binary_keys(features) -> [];
nif_encode_decode_binary_keys(title) -> "Nif encode decode with binary keys".
nif_encode_decode_binary_keys() ->
    ProtocCanOneof = check_protoc_can_do_oneof(),
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_bk_ed1,
              Defs = ["message x_bk {\n",
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
              Opts = [maps, {maps_key_type, binary}],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg01 = #{<<"o1">> => 1,
                                  <<"rq">> => 4,
                                  <<"rp">> => [5]},
                        Msg1 = if ProtocCanOneof ->
                                       Msg01#{<<"oo2">> => {of2,4}};
                                  true ->
                                       Msg01
                               end,
                        Bin1 = M:encode_msg(Msg1, x_bk),
                        Msg1 = M:decode_msg(Bin1, x_bk),
                        ok
                end)
      end).


nif_encode_decode_mapfields(features) -> [mapfields];
nif_encode_decode_mapfields(title) -> "Encode decode map<_,_> fields".
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

nif_with_mapfields_as_maps(features) -> [mapfields];
nif_with_mapfields_as_maps(title) -> "The mapfields_as_maps option".
nif_with_mapfields_as_maps() ->
    Defs = ["message m1 {\n",
            "   map<fixed32,string> a = 1;\n"
            "}"],
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_nomaps_with_mapfields_as_maps_ed1,
              Opts = [mapfields_as_maps],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg1 = #m1{a = #{11 => "aa"}},
                        Bin1 = M:encode_msg(Msg1),
                        Msg1 = M:decode_msg(Bin1, m1)
                end)
      end),
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_nif_test_maps_but_not_mapfields_as_maps_ed1,
              Opts = [msgs_as_maps, {mapfields_as_maps,false}],
              {ok, Code} = compile_nif_msg_defs(M, Defs, TmpDir, Opts),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        Msg1 = #{a => [{11,"aa"}]},
                        Bin1 = M:encode_msg(Msg1, m1),
                        Msg1 = M:decode_msg(Bin1, m1)
                end)
      end).

bypass_wrappers_maps_nif(features) -> [];
bypass_wrappers_maps_nif(title) -> "The bypass_wrappers option".
bypass_wrappers_maps_nif() ->
    with_tmpdir(
      fun(TmpDir) ->
              M = gpb_bypass_wrappers_maps,
              DefsTxt = ["message bpw1 {\n"
                         "    required uint32 f = 1;\n"
                         "}"],
              {ok, Code} = compile_nif_msg_defs(M, DefsTxt, TmpDir,
                                                [bypass_wrappers, maps]),
              in_separate_vm(
                TmpDir, M, Code,
                fun() ->
                        OrigMsg = #{f => 4712},
                        Encoded = M:encode_msg_bpw1(OrigMsg),
                        OrigMsg = M:decode_msg_bpw1(Encoded)
                end)
      end).

-compile({nowarn_unused_function, with_tmpdir/1}).
with_tmpdir(F) ->
    with_tmpdir(dont_save, F). % -import()ed from gpb_compile_tests

flat_map_prerequisites(Tests) ->
    case can_do_flat_oneof() of
        ok -> {"flat oneof for maps", Tests};
        {error, Text} -> {Text, []}
    end.

can_do_flat_oneof(_Features) ->
    can_do_flat_oneof().

can_do_flat_oneof() ->
    CanDoFlatMaps = gpb_lib:target_can_do_flat_oneof_for_maps([]),
    MayFailCompilation =
        gpb_lib:target_may_fail_compilation_for_flat_oneof_for_maps([]),
    if CanDoFlatMaps,
       MayFailCompilation ->
            {error, "flat oneof for maps skipped (may hit compiler error)"};
       CanDoFlatMaps,
       not MayFailCompilation ->
            ok;
       not CanDoFlatMaps ->
            {error, "flat oneof for maps skipped (Erlang 17 or earlier)"}
    end.

-endif. %% NO_HAVE_MAPS
