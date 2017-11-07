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
-import(gpb_compile_tests, [compile_to_string_get_hrl/2]).
-import(gpb_compile_tests, [compile_erl_iolist/1]).
-import(gpb_compile_tests, [unload_code/1]).

-import(gpb_compile_tests, [nif_tests_check_prerequisites/1]).
-import(gpb_compile_tests, [nif_mapfield_tests_check_prerequisites/1]).
-import(gpb_compile_tests, [increase_timeouts/1]).
-import(gpb_compile_tests, [with_tmpdir/1]).
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
    {timeout,15,fun 'default_value_handling_test_aux'/0}.

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
             [{"Encode decode", fun nif_encode_decode_mapfields/0},
              {"mapfields_as_maps", fun nif_with_mapfields_as_maps/0}]))])).

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

-endif. %% NO_HAVE_MAPS
