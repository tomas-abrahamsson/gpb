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

-module(gpb_json_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-import(gpb_compile_tests, [compile_iolist/2]).
-import(gpb_compile_tests, [unload_code/1]).

object_format_test() ->
    Proto = "
        message Msg {
          optional uint32 i = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_object_format, eep18}]),
    [{<<"i">>, 17}] = M1:to_json({'Msg', 17}),
    [{}]            = M1:to_json({'Msg', undefined}),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_object_format, {proplist}}]),
    {[{<<"i">>, 17}]} = M2:to_json({'Msg', 17}),
    {[]}              = M2:to_json({'Msg', undefined}),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_object_format, {x,proplist}}]),
    {x, [{<<"i">>, 17}]} = M3:to_json({'Msg', 17}),
    {x, []}              = M3:to_json({'Msg', undefined}),
    unload_code(M3),

    M4 = compile_iolist(Proto, [json, {json_object_format, {struct,proplist}}]),
    {struct, [{<<"i">>, 17}]} = M4:to_json({'Msg', 17}),
    {struct, []}              = M4:to_json({'Msg', undefined}),
    unload_code(M4).

-ifndef(NO_HAVE_MAPS).
object_format_map_test() ->
    Proto = "
        message Msg {
          optional uint32 i = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_object_format, map}]),
    ?assertEqual(#{<<"i">> => 17}, M1:to_json({'Msg', 17})),
    ?assertEqual(#{},              M1:to_json({'Msg', undefined})),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

key_format_test() ->
    Proto = "
        message Msg {
          optional uint32 i = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_key_format, binary}]),
    [{<<"i">>, 17}] = M1:to_json({'Msg', 17}),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_key_format, atom}]),
    [{i, 17}] = M2:to_json({'Msg', 17}),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_key_format, string}]),
    [{"i", 17}] = M3:to_json({'Msg', 17}),
    unload_code(M3).

array_format_test() ->
    Proto = "
        message Msg {
          repeated uint32 l = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_array_format, list}]),
    [{<<"l">>, [17,18]}] = M1:to_json({'Msg', [17,18]}),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_array_format, {array,list}}]),
    [{<<"l">>, {array,[17,18]}}] = M2:to_json({'Msg', [17,18]}),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_array_format, {x,list}}]),
    [{<<"l">>, {x,[17,18]}}] = M3:to_json({'Msg', [17,18]}),
    unload_code(M3).


string_format_test() ->
    Proto = "
        message Msg {
          optional string s = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_string_format, binary}]),
    [{<<"s">>, <<"abc">>}] = M1:to_json({'Msg', "abc"}),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_string_format, list}]),
    [{<<"s">>, "abc"}] = M2:to_json({'Msg', "abc"}),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_string_format, list},
                                strings_as_binaries]),
    [{<<"s">>, "abc"}] = M3:to_json({'Msg', <<"abc">>}),
    unload_code(M3).

optional_requred_repeated_test() ->
    Proto = ["message Msg {",
             "  optional uint32 op = 1;",
             "  required uint32 rq = 2;",
             "  repeated uint32 rp = 3;",
             "}"],
    %% Default
    M0 = compile_iolist(Proto, [json]),
    [{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}] =
        M0:to_json({'Msg', 10, 11, [12]}),
    unload_code(M0),
    %% Jsx
    M1 = compile_iolist(Proto, [json, {json_format, jsx}]),
    [{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}] =
        M1:to_json({'Msg', 10, 11, [12]}),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_format, mochijson2}]),
    {struct, [{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}]} =
        M2:to_json({'Msg', 10, 11, [12]}),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_format, jiffy}]),
    {[{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}]} =
        M3:to_json({'Msg', 10, 11, [12]}),
    unload_code(M3),
    ok.

-ifndef(NO_HAVE_MAPS).
optional_requred_repeated_maps_test() ->
    Proto = ["message Msg {",
             "  optional uint32 op = 1;",
             "  required uint32 rq = 2;",
             "  repeated uint32 rp = 3;",
             "}"],
    MM1 = compile_iolist(Proto, [json, {json_format, maps}]),
    #{<<"op">> := 10, <<"rq">> := 11, <<"rp">> := [12]} =
        MM1:to_json({'Msg', 10, 11, [12]}),
    unload_code(MM1),
    %% Maps is the default json format for option maps:
    MM2 = compile_iolist(Proto, [json, maps]),
    #{<<"op">> := 10, <<"rq">> := 11, <<"rp">> := [12]} =
        MM2:to_json(#{op => 10, rq => 11, rp => [12]}, 'Msg'),
    unload_code(MM2),
    ok.
-endif. % -ifndef(NO_HAVE_MAPS).

verify_option_test() ->
    Proto = ["message Msg {",
             "  optional uint32 f = 1;",
             "}"],
    M1 = compile_iolist(Proto, [json]),
    ?assertError({gpb_type_error, _},
                 M1:to_json({'Msg', invalid_integer}, [verify])),
    [{<<"f">>, inv}] = M1:to_json({'Msg', inv}, []),
    unload_code(M1),
    %% Verify always
    M2 = compile_iolist(Proto, [json, {verify, always}]),
    ?assertError({gpb_type_error, _},
                 M2:to_json({'Msg', invalid_integer}, [])),
    unload_code(M2),
    %% Verify never
    M3 = compile_iolist(Proto, [json, {verify, never}]),
    [{<<"f">>, inv}] = M3:to_json({'Msg', inv}, [verify]),
    unload_code(M3),
    ok.

-ifndef(NO_HAVE_MAPS).
verify_option_maps_test() ->
    Proto = ["message Msg {",
             "  optional uint32 f = 1;",
             "}"],
    MM = compile_iolist(Proto, [json, maps]),
    ?assertError({gpb_type_error, _},
                 MM:to_json(#{f => invalid_integer}, 'Msg', [verify])),
    #{<<"f">> := inv} = MM:to_json(#{f => inv}, 'Msg', []),
    unload_code(MM),
    ok.
-endif. % -ifndef(NO_HAVE_MAPS).

various_types_proto() ->
    "message MsgMsg    { optional Sub    f = 1; };
     message EnumMsg   { optional EE     f = 1; };
     message BoolMsg   { optional bool   f = 1; };
     message StringMsg { optional string f = 1; };
     message BytesMsg  { optional bytes  f = 1; };
     message Int32Msg  { optional int32  f = 1; };
     message Int64Msg  { optional int64  f = 1; };
     message FloatMsg  { optional float  f = 1; };
     enum EE { A=0; B=1; };
     message Sub { required uint32 s = 1; };".

various_types_test() ->
    M1 = compile_iolist(various_types_proto(), [json]),
    [{<<"f">>, [{<<"s">>, 11}]}] = M1:to_json({'MsgMsg', {'Sub', 11}}),
    [{<<"f">>, <<"A">>}] = M1:to_json({'EnumMsg', 'A'}),
    [{<<"f">>, true}] = M1:to_json({'BoolMsg', true}),
    [{<<"f">>, false}] = M1:to_json({'BoolMsg', false}),
    %% string: check that it accepts iodata
    [{<<"f">>, <<"abc">>}] = M1:to_json({'StringMsg', "abc"}),
    [{<<"f">>, <<"abc">>}] = M1:to_json({'StringMsg', <<"abc">>}),
    [{<<"f">>, <<"abc">>}] = M1:to_json({'StringMsg', ["a", [<<"b">>], $c]}),
    %% bytes: "standard base64 encoding with paddings.": (also accept iodata)
    [{<<"f">>, <<"AAECBA==">>}] = M1:to_json({'BytesMsg', [0,1,2,4]}),
    [{<<"f">>, <<"AAECBA==">>}] = M1:to_json({'BytesMsg', <<0,1,2,4>>}),
    [{<<"f">>, <<"AAECBA==">>}] = M1:to_json({'BytesMsg', [0,<<1,2>>,4]}),
    %% int32 to be encoded as integers
    [{<<"f">>, 10}] = M1:to_json({'Int32Msg', 10}),
    [{<<"f">>, -10}] = M1:to_json({'Int32Msg', -10}),
    %% int32 to be encoded as strings
    %% (presumably because max javascript int is often approx 2^53-1,
    %% although the json format as such does not have an explicit max)
    %% https://stackoverflow.com/questions/307179/what-is-javascripts-highest-integer-value-that-a-number-can-go-to-without-losin
    [{<<"f">>, <<"10">>}] = M1:to_json({'Int64Msg', 10}),
    [{<<"f">>, <<"-10">>}] = M1:to_json({'Int64Msg', -10}),
    %% Float
    [{<<"f">>, 0.125}]           = M1:to_json({'FloatMsg', 0.125}),
    [{<<"f">>, 10}]              = M1:to_json({'FloatMsg', 10}),
    [{<<"f">>, <<"Infinity">>}]  = M1:to_json({'FloatMsg', infinity}),
    [{<<"f">>, <<"-Infinity">>}] = M1:to_json({'FloatMsg', '-infinity'}),
    [{<<"f">>, <<"NaN">>}]       = M1:to_json({'FloatMsg', 'nan'}),

    %% Omitted optional values
    [{}] = M1:to_json({'MsgMsg', undefined}),
    [{}] = M1:to_json({'EnumMsg', undefined}),
    [{}] = M1:to_json({'StringMsg', undefined}),
    [{}] = M1:to_json({'BytesMsg', undefined}),
    [{}] = M1:to_json({'Int32Msg', undefined}),
    [{}] = M1:to_json({'Int64Msg', undefined}),
    [{}] = M1:to_json({'FloatMsg', undefined}),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
various_types_maps_test() ->
    M1 = compile_iolist(various_types_proto(), [json, maps]),
    ?assertEqual(#{<<"f">> => #{<<"s">> => 11}},
                 M1:to_json(#{f => #{s => 11}}, 'MsgMsg')),
    %% Omitted optional values
    ?assertEqual(#{}, M1:to_json(#{}, 'MsgMsg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'EnumMsg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'BoolMsg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'StringMsg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'BytesMsg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'Int32Msg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'Int64Msg')),
    ?assertEqual(#{}, M1:to_json(#{}, 'FloatMsg')),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

types_defaults_proto() ->
    "syntax=\"proto3\";\n"
        ++ various_types_proto().

type_defaults_test() ->
    M1 = compile_iolist(types_defaults_proto(), [json]),
    M2 = compile_iolist(types_defaults_proto(),
                        [json, json_always_print_primitive_fields]),
    [{}]                 = M1:to_json({'EnumMsg', 'A'}),
    [{<<"f">>, <<"A">>}] = M2:to_json({'EnumMsg', 'A'}),
    [{}]                 = M1:to_json({'BoolMsg', false}),
    [{<<"f">>, false}]   = M2:to_json({'BoolMsg', false}),
    [{}]                 = M1:to_json({'StringMsg', ""}),
    [{<<"f">>, <<>>}]    = M2:to_json({'StringMsg', <<>>}),
    [{}]                 = M1:to_json({'BytesMsg', <<>>}),
    [{<<"f">>, <<>>}]    = M2:to_json({'BytesMsg', <<>>}),
    [{}]                 = M1:to_json({'Int32Msg', 0}),
    [{<<"f">>, 0}]       = M2:to_json({'Int32Msg', 0}),
    [{}]                 = M1:to_json({'Int64Msg', 0}),
    [{<<"f">>, <<"0">>}] = M2:to_json({'Int64Msg', 0}),
    [{}]                 = M1:to_json({'FloatMsg', 0.0}),
    [{<<"f">>, 0.0}]     = M2:to_json({'FloatMsg', 0.0}),
    unload_code(M1),
    unload_code(M2).

-ifndef(NO_HAVE_MAPS).
type_defaults_maps_test() ->
    M1 = compile_iolist(types_defaults_proto(), [json, maps]),
    M2 = compile_iolist(types_defaults_proto(),
                        [json, json_always_print_primitive_fields, maps]),
    ?assertEqual(#{},                   M1:to_json(#{f => 'A'}, 'EnumMsg')),
    ?assertEqual(#{<<"f">> => <<"A">>}, M2:to_json(#{f => 'A'}, 'EnumMsg')),
    ?assertEqual(#{},                   M1:to_json(#{f => false}, 'BoolMsg')),
    ?assertEqual(#{<<"f">> => false},   M2:to_json(#{f => false}, 'BoolMsg')),
    ?assertEqual(#{},                   M1:to_json(#{f => ""}, 'StringMsg')),
    ?assertEqual(#{<<"f">> => <<>>},    M2:to_json(#{f => <<>>}, 'StringMsg')),
    ?assertEqual(#{},                   M1:to_json(#{f => <<>>}, 'BytesMsg')),
    ?assertEqual(#{<<"f">> => <<>>},    M2:to_json(#{f => <<>>}, 'BytesMsg')),
    ?assertEqual(#{},                   M1:to_json(#{f => 0}, 'Int32Msg')),
    ?assertEqual(#{<<"f">> => 0},       M2:to_json(#{f => 0}, 'Int32Msg')),
    ?assertEqual(#{},                   M1:to_json(#{f => 0}, 'Int64Msg')),
    ?assertEqual(#{<<"f">> => <<"0">>}, M2:to_json(#{f => 0}, 'Int64Msg')),
    ?assertEqual(#{},                   M1:to_json(#{f => 0.0}, 'FloatMsg')),
    ?assertEqual(#{<<"f">> => 0.0},     M2:to_json(#{f => 0.0}, 'FloatMsg')),
    unload_code(M1),
    unload_code(M2).
-endif. % -ifndef(NO_HAVE_MAPS).

oneof_proto() ->
    "syntax=\"proto3\";
    message Msg {
      oneof c {
        uint32 a = 1;
        bool   b = 2;
      }
    }".

oneof_test() ->
    M1 = compile_iolist(oneof_proto(), [json]),
    [{}] = M1:to_json({'Msg', undefined}),
    [{<<"a">>, 10}]   = M1:to_json({'Msg', {a, 10}}),
    [{<<"a">>, 0}]    = M1:to_json({'Msg', {a, 0}}), % though type-default
    [{<<"b">>, true}] = M1:to_json({'Msg', {b, true}}),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
oneof_maps_test() ->
    M1 = compile_iolist(oneof_proto(), [json, maps]),
    ?assertEqual(#{},                M1:to_json(#{}, 'Msg')),
    ?assertEqual(#{<<"a">> => 10},   M1:to_json(#{c => {a, 10}}, 'Msg')),
    ?assertEqual(#{<<"a">> => 0},    M1:to_json(#{c => {a, 0}}, 'Msg')),
    ?assertEqual(#{<<"b">> => true}, M1:to_json(#{c => {b, true}}, 'Msg')),
    unload_code(M1).

flat_oneof_maps_test_() ->
    flat_map_prerequisites(
      [{"flat oneof", fun flat_oneof_maps_test_aux/0}]).

flat_map_prerequisites(Tests) ->
    CanDoFlatMaps = gpb_lib:target_can_do_flat_oneof_for_maps([]),
    MayFailCompilation =
        gpb_lib:target_may_fail_compilation_for_flat_oneof_for_maps([]),
    if CanDoFlatMaps,
       MayFailCompilation ->
            {"flat oneof for maps skipped (may hit compiler error)", []};
       CanDoFlatMaps,
       not MayFailCompilation ->
            {"flat oneof for maps", Tests};
       not CanDoFlatMaps ->
            {"flat oneof for maps skipped (Erlang 17 or earlier)", []}
    end.

flat_oneof_maps_test_aux() ->
    M1 = compile_iolist(oneof_proto(), [json, maps, {maps_oneof, flat}]),
    ?assertEqual(#{},                M1:to_json(#{}, 'Msg')),
    ?assertEqual(#{<<"a">> => 10},   M1:to_json(#{a => 10}, 'Msg')),
    ?assertEqual(#{<<"a">> => 0},    M1:to_json(#{a => 0}, 'Msg')),
    ?assertEqual(#{<<"b">> => true}, M1:to_json(#{b => true}, 'Msg')),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

%% map<_,_> tests
mapfield_proto() ->
    "
    message I32ToStr  { map<int32,string>  f = 1; };
    message I64ToStr  { map<int64,string>  f = 1; };
    message BoolToStr { map<bool,string>   f = 1; };
    message StrToStr  { map<string,string> f = 1; };
    message StrToSub  { map<string,Sub>    f = 1; };
    message Sub { uint32 s = 1; }
    ".

mapfield_test() ->
    M1 = compile_iolist(mapfield_proto(), [json]),
    [{<<"f">>, [{<<"0">>, <<"abc">>},
                {<<"1">>, <<"def">>}]}] =
        M1:to_json({'I32ToStr', [{0,"abc"},{1,"def"}]}),
    [{<<"f">>, [{<<"0">>, <<"abc">>},
                {<<"1">>, <<"def">>}]}] =
        M1:to_json({'I64ToStr', [{0,"abc"},{1,"def"}]}),
    [{<<"f">>, [{<<"true">>,  <<"abc">>},
                {<<"false">>, <<"def">>}]}] =
        M1:to_json({'BoolToStr', [{true,"abc"},{false,"def"}]}),
    [{<<"f">>, [{<<"x">>, <<"abc">>},
                {<<"y">>, <<"def">>}]}] =
        M1:to_json({'StrToStr', [{"x","abc"},{"y","def"}]}),
    [{<<"f">>, [{<<"x">>, [{<<"s">>, 10}]},
                {<<"y">>, [{<<"s">>, 20}]}]}] =
        M1:to_json({'StrToSub', [{"x",{'Sub',10}},{"y",{'Sub',20}}]}),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
mapfield_map_test() ->
    M1 = compile_iolist(mapfield_proto(), [json, maps]),
    ?assertEqual(#{<<"f">> => #{<<"0">> => <<"abc">>,
                                <<"1">> => <<"def">>}},
                 M1:to_json(#{f => #{0 => "abc",
                                     1 => "def"}},
                            'I32ToStr')),
    ?assertEqual(#{<<"f">> => #{<<"0">> => <<"abc">>,
                                <<"1">> => <<"def">>}},
                 M1:to_json(#{f => #{0 => "abc",
                                     1 => "def"}},
                            'I64ToStr')),
    ?assertEqual(#{<<"f">> => #{<<"true">> =>  <<"abc">>,
                                <<"false">> => <<"def">>}},
                 M1:to_json(#{f => #{true => "abc",
                                     false => "def"}},
                            'BoolToStr')),
    ?assertEqual(#{<<"f">> => #{<<"x">> => <<"abc">>,
                                <<"y">> => <<"def">>}},
                 M1:to_json(#{f => #{"x" => "abc",
                                     "y" => "def"}},
                            'StrToStr')),
    ?assertEqual(#{<<"f">> => #{<<"x">> => #{<<"s">> => 10},
                                <<"y">> => #{<<"s">> => 20}}},
                 M1:to_json(#{f => #{"x" => #{s => 10},
                                     "y" => #{s => 20}}},
                            'StrToSub')),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

lower_camel_case_test() ->
    %% "Message field names are mapped to lowerCamelCase ..."
    Proto = "
       syntax=\"proto3\";
       message Msg {
          uint32 foo_bar = 1;
          uint32 some_other_32_value = 2;
       }
       ",
    M1 = compile_iolist(Proto, [json]),
    [{<<"fooBar">>, 1},
     {<<"someOther32Value">>, 2}] = M1:to_json({'Msg', 1, 2}),
    unload_code(M1).

preserve_proto_field_names_test() ->
    Proto = "
       syntax=\"proto3\";
       message Msg {
          uint32 foo_bar = 1;
          uint32 some_other_32_value = 2;
       }
       ",
    M1 = compile_iolist(Proto, [json, json_preserve_proto_field_names]),
    [{<<"foo_bar">>, 1},
     {<<"some_other_32_value">>, 2}] = M1:to_json({'Msg', 1, 2}),
    unload_code(M1).

no_msgs_test() ->
    Proto = "
       syntax=\"proto3\";
       enum Ee { A=0; }
       ",
    M1 = compile_iolist(Proto, [json]),
    unload_code(M1).

no_msg_fields_test() ->
    Proto = "
       syntax=\"proto3\";
       message Msg {  }
       ",
    M1 = compile_iolist(Proto, [json]),
    [{}] = M1:to_json({'Msg'}),
    unload_code(M1).

msg_with_only_groups_test() ->
    Proto = "
       message Msg {
          optional group g = 11 { required fixed32 gf = 1; };
          repeated group h = 21 { required fixed64 hf = 2; };
          required group i = 31 { }; // empty group
       }
       ",
    M1 = compile_iolist(Proto, [json]),
    [{<<"g">>, [{<<"gf">>, 1}]},
     {<<"h">>, [[{<<"hf">>, <<"2">>}]]},
     {<<"i">>, [{}]}] =
        M1:to_json({'Msg', {'Msg.g', 1}, [{'Msg.h',2}], {'Msg.i'}}),
    unload_code(M1).

cmdline_json_opt_test() ->
    {ok, {[json],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-json",
           "x.proto"]),

    %% Json format
    [{ok, {[json, {json_format, Expected}],
           ["x.proto"]}} =
         gpb_compile:parse_opts_and_args(
           ["-json", "-json-format", Str,
            "x.proto"])
     || {Expected, Str} <- [{jsx,        "jsx"},
                            {mochijson2, "mochijson2"},
                            {jiffy,      "jiffy"},
                            {maps,       "maps"}]],

    %% Json object format
    [{ok, {[json, {json_object_format, Expected}],
           ["x.proto"]}} =
         gpb_compile:parse_opts_and_args(
           ["-json", "-json-object-format", Str,
            "x.proto"])
     || {Expected, Str} <- [{eep18,             "eep18"},
                            {{proplist},        "tpl"},
                            {{struct,proplist}, "tpl:struct"},
                            {{x,proplist},      "tpl:x"},
                            {map,               "map"}]],
    %% Json key format
    [{ok, {[json, {json_key_format, Expected}],
           ["x.proto"]}} =
         gpb_compile:parse_opts_and_args(
           ["-json", "-json-key-format", Str,
            "x.proto"])
     || {Expected, Str} <- [{binary,            "binary"},
                            {atom,              "atom"},
                            {string,            "string"}]],

    %% Json array format
    [{ok, {[json, {json_array_format, Expected}],
           ["x.proto"]}} =
         gpb_compile:parse_opts_and_args(
           ["-json", "-json-array-format", Str,
            "x.proto"])
     || {Expected, Str} <- [{list,              "list"},
                            {{array,list},      "tl:array"}]],

    %% Json string format
    [{ok, {[json, {json_string_format, Expected}],
           ["x.proto"]}} =
         gpb_compile:parse_opts_and_args(
           ["-json", "-json-string-format", Str,
            "x.proto"])
     || {Expected, Str} <- [{binary,            "binary"},
                            {list,              "list"}]],

    %% Json null format
    [{ok, {[json, {json_null, Expected}],
           ["x.proto"]}} =
         gpb_compile:parse_opts_and_args(
           ["-json", "-json-null", Str,
            "x.proto"])
     || {Expected, Str} <- [{null,             "null"},
                            {nil,              "nil"},
                            {undefined,        "undefined"}]],

    %% Misc options
    {ok, {[json,
           json_always_print_primitive_fields,
           json_preserve_proto_field_names],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-json", "-json-always-print-primitive-fields",
           "-json-preserve-proto-field-names",
           "x.proto"]),
    ok.
