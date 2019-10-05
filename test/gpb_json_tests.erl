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
-include("gpb_nif_test_helpers.hrl"). % the `?nif_if_supported(FnName)' macro

-import(gpb_compile_tests, [compile_iolist/2]).
-import(gpb_compile_tests, [compile_protos/2]).
-import(gpb_compile_tests, [unload_code/1]).

-import(gpb_compile_tests, [nif_tests_check_prerequisites/1]).
-import(gpb_compile_tests, [guess_features/1]).
-import(gpb_compile_tests, [with_tmpdir/2]).
-import(gpb_compile_tests, [in_separate_vm/4]).
-import(gpb_compile_tests, [compile_nif_msg_defs/4]).

-import(gpb_compile_maps_tests, [flat_map_prerequisites/1]).

-export([json_encode/1, json_decode/1]). % for debugging

%% For testing translations
-export([float_to_duration/1, duration_to_float/1]).
-export([fraction_to_nanos/1, nanos_to_fraction/1]).
-export([string_to_int/1, int_to_string/1]).
-export([id/1]).

-ifdef('OTP_RELEASE').
%% ?assertEqual/3 appeared in Erlang 20 already,
%% but it is easier to test for 'OTP_RELEASE' which
%% appeared in Erlang 21, and we need a fallback to
%% support older releases anyway...
-define(assert_eq_3(ExpectedExpr, ActualExpr, DebugInfo),
        ?assertEqual(ExpectedExpr, ActualExpr, DebugInfo)).
-else. % -ifdef('OTP_RELEASE').
%% Fallback
-define(assert_eq_3(ExpectedExpr, ActualExpr, DebugInfo),
        ((fun() ->
                  X@@ = (ExpectedExpr),
                  case (ActualExpr) of
                      X@@ -> ok;
                      Actual@@ ->
                          error({not_equal, [{line, ?LINE},
                                             {expr,??ActualExpr},
                                             {debug, DebugInfo},
                                             {expected,X@@},
                                             {actual,Actual@@}]})
                  end

          end)())).
-endif. % -ifdef('OTP_RELEASE').


object_format_test() ->
    Proto = "
        message Msg {
          optional uint32 i = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_object_format, eep18}]),
    [{<<"i">>, 17}] = M1:to_json({'Msg', 17}),
    [{}]            = M1:to_json({'Msg', undefined}),
    {'Msg', 17}        = M1:from_json([{<<"i">>, 17}], 'Msg'),
    {'Msg', undefined} = M1:from_json([{}], 'Msg'),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_object_format, {proplist}}]),
    {[{<<"i">>, 17}]} = M2:to_json({'Msg', 17}),
    {[]}              = M2:to_json({'Msg', undefined}),
    {'Msg', 17}        = M2:from_json({[{<<"i">>, 17}]}, 'Msg'),
    {'Msg', undefined} = M2:from_json({[]}, 'Msg'),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_object_format, {x,proplist}}]),
    {x, [{<<"i">>, 17}]} = M3:to_json({'Msg', 17}),
    {x, []}              = M3:to_json({'Msg', undefined}),
    {'Msg', 17}        = M3:from_json({x, [{<<"i">>, 17}]}, 'Msg'),
    {'Msg', undefined} = M3:from_json({x, []}, 'Msg'),
    unload_code(M3),

    M4 = compile_iolist(Proto, [json, {json_object_format, {struct,proplist}}]),
    {struct, [{<<"i">>, 17}]} = M4:to_json({'Msg', 17}),
    {struct, []}              = M4:to_json({'Msg', undefined}),
    {'Msg', 17}        = M4:from_json({struct, [{<<"i">>, 17}]}, 'Msg'),
    {'Msg', undefined} = M4:from_json({struct, []}, 'Msg'),
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
    {'Msg', 17}        = M1:from_json(#{<<"i">> => 17}, 'Msg'),
    {'Msg', undefined} = M1:from_json(#{}, 'Msg'),
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
    {'Msg', 17} = M1:from_json([{<<"i">>, 17}], 'Msg'),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_key_format, atom}]),
    [{i, 17}] = M2:to_json({'Msg', 17}),
    {'Msg', 17} = M2:from_json([{i, 17}], 'Msg'),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_key_format, string}]),
    [{"i", 17}] = M3:to_json({'Msg', 17}),
    {'Msg', 17} = M3:from_json([{"i", 17}], 'Msg'),
    unload_code(M3).

array_format_test() ->
    Proto = "
        message Msg {
          repeated uint32 l = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_array_format, list}]),
    [{<<"l">>, [17,18]}] = M1:to_json({'Msg', [17,18]}),
    {'Msg', [17,18]} = M1:from_json([{<<"l">>, [17,18]}], 'Msg'),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_array_format, {array,list}}]),
    [{<<"l">>, {array,[17,18]}}] = M2:to_json({'Msg', [17,18]}),
    {'Msg', [17,18]} = M2:from_json([{<<"l">>, {array,[17,18]}}], 'Msg'),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_array_format, {x,list}}]),
    [{<<"l">>, {x,[17,18]}}] = M3:to_json({'Msg', [17,18]}),
    {'Msg', [17,18]} = M3:from_json([{<<"l">>, {x,[17,18]}}], 'Msg'),
    unload_code(M3).


string_format_test() ->
    Proto = "
        message Msg {
          optional string s = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_string_format, binary}]),
    [{<<"s">>, <<"abc">>}] = M1:to_json({'Msg', "abc"}),
    {'Msg', "abc"} = M1:from_json([{<<"s">>, <<"abc">>}], 'Msg'),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_string_format, list}]),
    [{<<"s">>, "abc"}] = M2:to_json({'Msg', "abc"}),
    {'Msg', "abc"} = M2:from_json([{<<"s">>, "abc"}], 'Msg'),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_string_format, list},
                                strings_as_binaries]),
    [{<<"s">>, "abc"}] = M3:to_json({'Msg', <<"abc">>}),
    {'Msg', <<"abc">>} = M3:from_json([{<<"s">>, "abc"}], 'Msg'),
    unload_code(M3).

null_test() ->
    Proto = "
        message Msg {
          optional sint32 i = 1;
        }
    ",
    M1 = compile_iolist(Proto, [json, {json_null, nil}]),
    {'Msg', undefined} = M1:from_json([{<<"s">>, nil}], 'Msg'),
    unload_code(M1).

optional_requred_repeated_test() ->
    Proto = ["message Msg {",
             "  optional uint32 op = 1;",
             "  required uint32 rq = 2;",
             "  repeated uint32 rp = 3;",
             "}"],
    Msg = {'Msg', 10, 11, [12]},
    %% Default
    M0 = compile_iolist(Proto, [json]),
    [{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}] = J0 =
        M0:to_json(Msg),
    Msg = M0:from_json(J0, 'Msg'),
    unload_code(M0),
    %% Jsx
    M1 = compile_iolist(Proto, [json, {json_format, jsx}]),
    [{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}] = J1 =
        M1:to_json({'Msg', 10, 11, [12]}),
    Msg = M1:from_json(J1, 'Msg'),
    unload_code(M1),

    M2 = compile_iolist(Proto, [json, {json_format, mochijson2}]),
    {struct, [{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}]} = J2 =
        M2:to_json({'Msg', 10, 11, [12]}),
    Msg = M2:from_json(J2, 'Msg'),
    unload_code(M2),

    M3 = compile_iolist(Proto, [json, {json_format, jiffy}]),
    {[{<<"op">>, 10}, {<<"rq">>, 11}, {<<"rp">>, [12]}]} = J3 =
        M3:to_json({'Msg', 10, 11, [12]}),
    Msg = M3:from_json(J3, 'Msg'),
    unload_code(M3),
    ok.

-ifndef(NO_HAVE_MAPS).
optional_requred_repeated_maps_test() ->
    Proto = ["message Msg {",
             "  optional uint32 op = 1;",
             "  required uint32 rq = 2;",
             "  repeated uint32 rp = 3;",
             "}"],
    Msg1 = {'Msg', 10, 11, [12]},
    MM1 = compile_iolist(Proto, [json, {json_format, maps}]),
    #{<<"op">> := 10, <<"rq">> := 11, <<"rp">> := [12]} = J1 =
        MM1:to_json(Msg1),
    Msg1 = MM1:from_json(J1, 'Msg'),
    unload_code(MM1),
    %% Maps is the default json format for option maps:
    Msg2 = #{op => 10, rq => 11, rp => [12]},
    MM2 = compile_iolist(Proto, [json, maps]),
    #{<<"op">> := 10, <<"rq">> := 11, <<"rp">> := [12]} = J2 =
        MM2:to_json(Msg2, 'Msg'),
    ?assertEqual(Msg2,  MM1:from_json(J2, 'Msg')),
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
    {'MsgMsg', {'Sub', 11}} = M1:from_json([{<<"f">>, [{<<"s">>, 11}]}],
                                           'MsgMsg'),
    {'EnumMsg', 'A'} = M1:from_json([{<<"f">>, <<"A">>}], 'EnumMsg'),
    %% bool: accept also boolean values as strings (case insensitively)
    {'BoolMsg', true} = M1:from_json([{<<"f">>, true}], 'BoolMsg'),
    {'BoolMsg', false} = M1:from_json([{<<"f">>, false}], 'BoolMsg'),
    {'BoolMsg', true} = M1:from_json([{<<"f">>, <<"true">>}], 'BoolMsg'),
    {'BoolMsg', true} = M1:from_json([{<<"f">>, <<"TRUe">>}], 'BoolMsg'),
    {'BoolMsg', true} = M1:from_json([{<<"f">>, <<"1">>}], 'BoolMsg'),
    %% string: check that it accepts iodata
    [{<<"f">>, <<"abc">>}] = M1:to_json({'StringMsg', "abc"}),
    [{<<"f">>, <<"abc">>}] = M1:to_json({'StringMsg', <<"abc">>}),
    [{<<"f">>, <<"abc">>}] = M1:to_json({'StringMsg', ["a", [<<"b">>], $c]}),
    {'StringMsg', "abc"} = M1:from_json([{<<"f">>, <<"abc">>}], 'StringMsg'),
    %% bytes: "standard base64 encoding with paddings.": (also accept iodata)
    [{<<"f">>, <<"AAECBA==">>}] = M1:to_json({'BytesMsg', [0,1,2,4]}),
    [{<<"f">>, <<"AAECBA==">>}] = M1:to_json({'BytesMsg', <<0,1,2,4>>}),
    [{<<"f">>, <<"AAECBA==">>}] = M1:to_json({'BytesMsg', [0,<<1,2>>,4]}),
    {'BytesMsg', <<0,1,2,4>>} = M1:from_json([{<<"f">>, <<"AAECBA==">>}],
                                             'BytesMsg'),
    {'BytesMsg', <<255>>} = M1:from_json([{<<"f">>, <<"/+==">>}], %base64
                                         'BytesMsg'),
    {'BytesMsg', <<255>>} = M1:from_json([{<<"f">>, <<"_-==">>}], %base64
                                         'BytesMsg'),
    %% int32 to be encoded as integers
    [{<<"f">>, 10}] = M1:to_json({'Int32Msg', 10}),
    [{<<"f">>, -10}] = M1:to_json({'Int32Msg', -10}),
    {'Int32Msg', 10}  = M1:from_json([{<<"f">>, 10}], 'Int32Msg'),
    {'Int32Msg', 10}  = M1:from_json([{<<"f">>, <<"10">>}], 'Int32Msg'),
    {'Int32Msg', -10} = M1:from_json([{<<"f">>, -10}], 'Int32Msg'),
    {'Int32Msg', -10} = M1:from_json([{<<"f">>, <<"-10">>}], 'Int32Msg'),
    %% Check some other representations of integers: Leading 0 not to
    %% be treated as octal, and leading plus seems to be allowed
    {'Int32Msg', 377} = M1:from_json([{<<"f">>, <<"0377">>}], 'Int32Msg'),
    {'Int32Msg', 10} = M1:from_json([{<<"f">>, <<"+10">>}], 'Int32Msg'),
    %% int32 to be encoded as strings
    %% (presumably because max javascript int is often approx 2^53-1,
    %% although the json format as such does not have an explicit max)
    %% https://stackoverflow.com/questions/307179/what-is-javascripts-highest-integer-value-that-a-number-can-go-to-without-losin
    [{<<"f">>, <<"10">>}] = M1:to_json({'Int64Msg', 10}),
    [{<<"f">>, <<"-10">>}] = M1:to_json({'Int64Msg', -10}),
    {'Int64Msg', 10}  = M1:from_json([{<<"f">>, <<"10">>}], 'Int64Msg'),
    {'Int64Msg', -10} = M1:from_json([{<<"f">>, <<"-10">>}], 'Int64Msg'),
    %% Float
    [{<<"f">>, 0.125}]           = M1:to_json({'FloatMsg', 0.125}),
    [{<<"f">>, 10}]              = M1:to_json({'FloatMsg', 10}),
    [{<<"f">>, <<"Infinity">>}]  = M1:to_json({'FloatMsg', infinity}),
    [{<<"f">>, <<"-Infinity">>}] = M1:to_json({'FloatMsg', '-infinity'}),
    [{<<"f">>, <<"NaN">>}]       = M1:to_json({'FloatMsg', 'nan'}),
    Fl = 'FloatMsg',
    {Fl, 0.125}       = M1:from_json([{<<"f">>, 0.125}], Fl),
    {Fl, 0.125}       = M1:from_json([{<<"f">>, <<"0.125">>}], Fl),
    {Fl, 0.125}       = M1:from_json([{<<"f">>, <<"1.25e-1">>}], Fl),
    {Fl, 10.0}        = M1:from_json([{<<"f">>, 10}], Fl),
    {Fl, 10.0}        = M1:from_json([{<<"f">>, <<"10">>}], Fl),
    {Fl, infinity}    = M1:from_json([{<<"f">>, <<"Infinity">>}], Fl),
    {Fl, '-infinity'} = M1:from_json([{<<"f">>, <<"-Infinity">>}], Fl),
    {Fl, 'nan'}       = M1:from_json([{<<"f">>, <<"NaN">>}], Fl),
    %% Protobuf also allows the following:
    {Fl, 1.0}         = M1:from_json([{<<"f">>, <<"+1">>}], Fl),
    {Fl, 0.125}       = M1:from_json([{<<"f">>, <<".125">>}], Fl),
    {Fl, 1.0}         = M1:from_json([{<<"f">>, <<"1.">>}], Fl),
    {Fl, 10.0}        = M1:from_json([{<<"f">>, <<"1.e1">>}], Fl),
    {Fl, -1.0}        = M1:from_json([{<<"f">>, <<"-.1e1">>}], Fl),
    {Fl, 1.0}         = M1:from_json([{<<"f">>, <<"+.1e1">>}], Fl),

    %% Omitted optional values
    [{}] = M1:to_json({'MsgMsg', undefined}),
    [{}] = M1:to_json({'EnumMsg', undefined}),
    [{}] = M1:to_json({'StringMsg', undefined}),
    [{}] = M1:to_json({'BytesMsg', undefined}),
    [{}] = M1:to_json({'Int32Msg', undefined}),
    [{}] = M1:to_json({'Int64Msg', undefined}),
    [{}] = M1:to_json({'FloatMsg', undefined}),
    {'MsgMsg', undefined} = M1:from_json([{<<"f">>, null}], 'MsgMsg'),
    {'MsgMsg', undefined} = M1:from_json([{}], 'MsgMsg'),
    {'EnumMsg', undefined} = M1:from_json([{<<"f">>, null}], 'EnumMsg'),
    {'EnumMsg', undefined} = M1:from_json([{}], 'EnumMsg'),
    {'StringMsg', undefined} = M1:from_json([{<<"f">>, null}], 'StringMsg'),
    {'StringMsg', undefined} = M1:from_json([{}], 'StringMsg'),
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
    ?assertEqual(#{}, M1:from_json(#{<<"f">> => null}, 'MsgMsg')),
    ?assertEqual(#{}, M1:from_json(#{}, 'MsgMsg')),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

enums_proto() ->
    "
     syntax=\"proto3\";
     message EnumMsg   { EE f = 1; };
     enum EE { EE_A=0; EE_B=1; };
    ".

decoding_enums_test() ->
    M1 = compile_iolist(enums_proto(), [json]),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"EE_B">>}], 'EnumMsg'),
    %% enums can be given as integers too, even ints represented as strings
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"1">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, 1}], 'EnumMsg'),
    %% gpb normally decodes unknown enums as integers
    {'EnumMsg', 9} = M1:from_json([{<<"f">>, 9}], 'EnumMsg'),
    %% Unparsables or unknowns: when string: treat the field as not present
    %% In proto2 should decode to undefined instead of EE_A
    %% but need some more handling to do that...
    {'EnumMsg', 'EE_A'} = M1:from_json([{<<"f">>, <<" 1">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_A'} = M1:from_json([{<<"f">>, <<"9">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_A'} = M1:from_json([{<<"f">>, <<"EE/B">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_A'} = M1:from_json([{<<"f">>, <<"EE/B">>}], 'EnumMsg'),
    unload_code(M1).

decoding_enums_case_insensitively_test() ->
    M1 = compile_iolist(enums_proto(),
                        [json, json_case_insensitive_enum_parsing]),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"EE_B">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"EE-B">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"ee_b">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"ee_B">>}], 'EnumMsg'),
    {'EnumMsg', 'EE_B'} = M1:from_json([{<<"f">>, <<"ee-B">>}], 'EnumMsg'),
    unload_code(M1).

types_defaults_proto() ->
    "syntax=\"proto3\";\n"
        ++ strip_occurrence(various_types_proto()).

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
    {'EnumMsg', 'A'}     = M1:from_json([{}], 'EnumMsg'),
    {'BoolMsg', false}   = M1:from_json([{}], 'BoolMsg'),
    {'StringMsg', ""}    = M1:from_json([{}], 'StringMsg'),
    {'BytesMsg', <<>>}   = M1:from_json([{}], 'BytesMsg'),
    {'Int32Msg', 0}      = M1:from_json([{}], 'Int32Msg'),
    {'Int64Msg', 0}      = M1:from_json([{}], 'Int64Msg'),
    {'FloatMsg', 0.0}    = M1:from_json([{}], 'FloatMsg'),
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
    ?assertEqual(#{f => 'A'},           M1:from_json(#{}, 'EnumMsg')),
    ?assertEqual(#{f => false},         M1:from_json(#{}, 'BoolMsg')),
    ?assertEqual(#{f => ""},            M1:from_json(#{}, 'StringMsg')),
    ?assertEqual(#{f => <<>>},          M1:from_json(#{}, 'BytesMsg')),
    ?assertEqual(#{f => 0},             M1:from_json(#{}, 'Int32Msg')),
    ?assertEqual(#{f => 0},             M1:from_json(#{}, 'Int64Msg')),
    ?assertEqual(#{f => 0.0},           M1:from_json(#{}, 'FloatMsg')),
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
    {'Msg', undefined} = M1:from_json([{}], 'Msg'),
    {'Msg', {a, 10}}   = M1:from_json([{<<"a">>, 10}], 'Msg'),
    {'Msg', {a, 0}}    = M1:from_json([{<<"a">>, 0}], 'Msg'),
    {'Msg', {b, true}} = M1:from_json([{<<"b">>, true}], 'Msg'),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
oneof_maps_test() ->
    M1 = compile_iolist(oneof_proto(), [json, maps]),
    ?assertEqual(#{},                M1:to_json(#{}, 'Msg')),
    ?assertEqual(#{<<"a">> => 10},   M1:to_json(#{c => {a, 10}}, 'Msg')),
    ?assertEqual(#{<<"a">> => 0},    M1:to_json(#{c => {a, 0}}, 'Msg')),
    ?assertEqual(#{<<"b">> => true}, M1:to_json(#{c => {b, true}}, 'Msg')),
    ?assertEqual(#{},                M1:from_json(#{}, 'Msg')),
    ?assertEqual(#{c => {a, 10}},   M1:from_json(#{<<"a">> => 10}, 'Msg')),
    ?assertEqual(#{c => {a, 0}},    M1:from_json(#{<<"a">> => 0}, 'Msg')),
    ?assertEqual(#{c => {b, true}}, M1:from_json(#{<<"b">> => true}, 'Msg')),
    unload_code(M1).

flat_oneof_maps_test_() ->
    flat_map_prerequisites(
      [{"flat oneof", fun flat_oneof_maps_test_aux/0}]).

flat_oneof_maps_test_aux() ->
    M1 = compile_iolist(oneof_proto(), [json, maps, {maps_oneof, flat}]),
    ?assertEqual(#{},                M1:to_json(#{}, 'Msg')),
    ?assertEqual(#{<<"a">> => 10},   M1:to_json(#{a => 10}, 'Msg')),
    ?assertEqual(#{<<"a">> => 0},    M1:to_json(#{a => 0}, 'Msg')),
    ?assertEqual(#{<<"b">> => true}, M1:to_json(#{b => true}, 'Msg')),
    ?assertEqual(#{},          M1:from_json(#{}, 'Msg')),
    ?assertEqual(#{a => 10},   M1:from_json(#{<<"a">> => 10}, 'Msg')),
    ?assertEqual(#{a => 0},    M1:from_json(#{<<"a">> => 0}, 'Msg')),
    ?assertEqual(#{b => true}, M1:from_json(#{<<"b">> => true}, 'Msg')),
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
    %% -- internal -> json
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

    %% -- json -> internal
    {'I32ToStr', [{0,"abc"},{1,"def"}]} =
        M1:from_json([{<<"f">>, [{<<"0">>, <<"abc">>},
                                 {<<"1">>, <<"def">>}]}],
                     'I32ToStr'),
    {'I64ToStr', [{0,"abc"},{1,"def"}]} =
        element2sort(
          M1:from_json([{<<"f">>, [{<<"0">>, <<"abc">>},
                                   {<<"1">>, <<"def">>}]}],
                       'I64ToStr')),
    {'BoolToStr', [{false,"def"},{true,"abc"}]} =
        element2sort(
          M1:from_json([{<<"f">>, [{<<"true">>,  <<"abc">>},
                                   {<<"false">>, <<"def">>}]}],
                       'BoolToStr')),
    {'StrToStr', [{"x","abc"},{"y","def"}]} =
        element2sort(
          M1:from_json([{<<"f">>, [{<<"x">>, <<"abc">>},
                                   {<<"y">>, <<"def">>}]}],
                       'StrToStr')),
    {'StrToSub', [{"x",{'Sub',10}},{"y",{'Sub',20}}]} =
        element2sort(
          M1:from_json([{<<"f">>, [{<<"x">>, [{<<"s">>, 10}]},
                                   {<<"y">>, [{<<"s">>, 20}]}]}],
                       'StrToSub')),
    unload_code(M1).

element2sort(Tuple) ->
    setelement(2, Tuple, lists:sort(element(2, Tuple))).

-ifndef(NO_HAVE_MAPS).
mapfield_map_test() ->
    M1 = compile_iolist(mapfield_proto(), [json, maps]),
    %% -- internal -> json
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

    %% -- json -> internal
    ?assertEqual(#{f => #{0 => "abc",
                          1 => "def"}},
                 M1:from_json(#{<<"f">> => #{<<"0">> => <<"abc">>,
                                             <<"1">> => <<"def">>}},
                              'I32ToStr')),
    ?assertEqual(#{f => #{0 => "abc",
                          1 => "def"}},
                 M1:from_json(#{<<"f">> => #{<<"0">> => <<"abc">>,
                                             <<"1">> => <<"def">>}},
                              'I64ToStr')),
    ?assertEqual(#{f => #{true => "abc",
                          false => "def"}},
                 M1:from_json(#{<<"f">> => #{<<"true">> =>  <<"abc">>,
                                             <<"false">> => <<"def">>}},
                              'BoolToStr')),
    ?assertEqual(#{f => #{"x" => "abc",
                          "y" => "def"}},
                 M1:from_json(#{<<"f">> => #{<<"x">> => <<"abc">>,
                                             <<"y">> => <<"def">>}},
                              'StrToStr')),
    ?assertEqual(#{f => #{"x" => #{s => 10},
                          "y" => #{s => 20}}},
                 M1:from_json(#{<<"f">> => #{<<"x">> => #{<<"s">> => 10},
                                             <<"y">> => #{<<"s">> => 20}}},
                              'StrToSub')),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

p3wellknown_duration_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/duration.proto';
        message D {
           google.protobuf.Duration f = 1;
        }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    F = <<"f">>,
    Duration = 'google.protobuf.Duration',
    [{F, <<"0s">>}] = M1:to_json({'D', {Duration, 0, 0}}),
    [{F, <<"1s">>}] = M1:to_json({'D', {Duration, 1, 0}}),
    %% Negative (also in range 0..1 where seconds cannot be negative)
    [{F, <<"-1s">>}] = M1:to_json({'D', {Duration, -1, 0}}),
    [{F, <<"-1.100s">>}] = M1:to_json({'D', {Duration, -1, -100000000}}),
    [{F, <<"-0.100s">>}] = M1:to_json({'D', {Duration, 0, -100000000}}),
    %% with 3 6 or 9 decimals as appropriate
    [{F, <<"1.000000123s">>}] = M1:to_json({'D', {Duration, 1, 123}}),
    [{F, <<"1.000123s">>}] = M1:to_json({'D', {Duration, 1, 123000}}),
    [{F, <<"1.123s">>}] = M1:to_json({'D', {Duration, 1, 123000000}}),
    %% with optionals omitted
    [{F, <<"0s">>}] = M1:to_json({'D', {Duration, undefined, undefined}}),
    [{F, <<"1s">>}] = M1:to_json({'D', {Duration, 1, undefined}}),
    [{F, <<"0.000000123s">>}] = M1:to_json({'D', {Duration, undefined, 123}}),
    %% too large
    ?assertError(_, M1:to_json({'D', {Duration, 0, 2111222333}})),
    ?assertError(_, M1:to_json({'D', {Duration, 0, 1000000000}})),
    %% Decoding ---
    {'D', {Duration, 0, 0}} = M1:from_json([{F, <<"0s">>}], 'D'),
    %% negative: both nanos and seconds must be negative
    {'D', {Duration, -1, -1}} = M1:from_json([{F, <<"-1.000000001s">>}], 'D'),
    {'D', {Duration, 0, -1}}  = M1:from_json([{F, <<"-0.000000001s">>}], 'D'),
    %% Only seconds, no nanos
    {'D', {Duration, 1, 0}} = M1:from_json([{F, <<"1s">>}], 'D'),
    %% with 3 6 or 9 decimals as appropriate
    {'D', {Duration, 1, 123}} = M1:from_json([{F, <<"1.000000123s">>}], 'D'),
    {'D', {Duration, 1, 123000}} = M1:from_json([{F, <<"1.000123s">>}], 'D'),
    {'D', {Duration, 1, 123000000}} = M1:from_json([{F, <<"1.123s">>}], 'D'),
    %% "Accepted are any fractional digits (also none) as long as they fit
    %% into nano-seconds"
    {'D', {Duration, 1, 1230}} = M1:from_json([{F, <<"1.00000123s">>}], 'D'),
    {'D', {Duration, 1, 12300}} = M1:from_json([{F, <<"1.0000123s">>}], 'D'),
    {'D', {Duration, 1, 12300000}} = M1:from_json([{F, <<"1.0123s">>}], 'D'),
    %% invalid
    ?assertError(_, M1:from_json([{F, <<"1.2111222333s">>}], 'D')), % overflow
    ?assertError(_, M1:from_json([{F, <<".1s">>}], 'D')),
    %% with Google protobuf, "1.s" seems valid (but ".1s" does not)
    {'D', {Duration, 1, 0}} = M1:from_json([{F, <<"1.s">>}], 'D'),
    %% done
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
p3wellknown_duration_map_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/duration.proto';
        message D {
           google.protobuf.Duration f = 1;
        }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json, maps]),
    ?assertEqual(#{<<"f">> => <<"1s">>},
                 M1:to_json(#{f => #{seconds => 1, nanos => 0}}, 'D')),
    ?assertEqual(#{<<"f">> => <<"1s">>},
                 M1:to_json(#{f => #{seconds => 1}}, 'D')),
    ?assertEqual(#{f => #{seconds => 1, nanos => 0}},
                 M1:from_json(#{<<"f">> => <<"1s">>}, 'D')),
    unload_code(M1),
    M2 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json,
                         maps, {maps_unset_optional, present_undefined}]),
    ?assertEqual(#{<<"f">> => <<"1s">>},
                 M2:to_json(#{f => #{seconds => 1, nanos => 0}}, 'D')),
    ?assertEqual(#{<<"f">> => <<"1s">>},
                 M2:to_json(#{f => #{seconds => 1, nanos => undefined}}, 'D')),
    ?assertEqual(#{f => #{seconds => 1, nanos => 0}},
                 M2:from_json(#{<<"f">> => <<"1s">>}, 'D')),
    unload_code(M2).
-endif. % -ifndef(NO_HAVE_MAPS).

p3wellknown_duration_with_translations_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/duration.proto';
        message D {
           google.protobuf.Duration f = 1;
        }
        ",
    TranslateDfOpt =
        {translate_field,
         {['D',f], [{encode, {?MODULE, float_to_duration,['$1']}},
                    {decode, {?MODULE, duration_to_float,['$1']}}]}},
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json, TranslateDfOpt]),
    F = <<"f">>,
    [{F, <<"1.125s">>}] = M1:to_json({'D', 1.125}),
    {'D', 1.125} = M1:from_json([{F, <<"1.125s">>}], 'D'),
    unload_code(M1),

    Duration = 'google.protobuf.Duration',
    TranslateDurationFieldOpts =
        [{translate_field,
          {[Duration,seconds],
           [{encode, {?MODULE, string_to_int,['$1']}},
            {decode, {?MODULE, int_to_string,['$1']}}]}},
         {translate_field,
          {[Duration,nanos],
           [{encode, {?MODULE, fraction_to_nanos,['$1']}},
            {decode, {?MODULE, nanos_to_fraction,['$1']}}]}}],
    M2 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json | TranslateDurationFieldOpts]),
    [{F, <<"1.125s">>}] = M2:to_json({'D', {Duration, "1", 0.125}}),
    {'D', {Duration, "1", 0.125}} = M2:from_json([{F, <<"1.125s">>}], 'D'),
    unload_code(M2).

float_to_duration(Fl) ->
    Seconds = trunc(Fl),
    Nanos = fraction_to_nanos(Fl - Seconds),
    {'google.protobuf.Duration', Seconds, Nanos}.

duration_to_float({'google.protobuf.Duration', Seconds, Nanos}) ->
    Seconds + nanos_to_fraction(Nanos).

fraction_to_nanos(Fl) when 0.0 =< Fl, Fl < 1.0 ->
    trunc(Fl * 1000000000).

nanos_to_fraction(Nanos) when 0 =< Nanos, Nanos < 1000000000 ->
    Nanos / 1000000000.

string_to_int(S) -> list_to_integer(S).

int_to_string(I) -> integer_to_list(I).

p3wellknown_timestamp_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/timestamp.proto';
        message T {
           google.protobuf.Timestamp f = 1;
        }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    F = <<"f">>,
    Timestamp = 'google.protobuf.Timestamp',
    [{F, <<"1970-01-01T00:00:00Z">>}] = M1:to_json({'T', {Timestamp, 0, 0}}),
    [{F, <<"1969-12-31T23:59:50Z">>}] = M1:to_json({'T', {Timestamp, -10, 0}}),
    %% with 3 6 or 9 decimals as appropriate
    [[{F, <<"1970-01-01T00:00:01.000000123Z">>}],
     [{F, <<"1970-01-01T00:00:01.000123Z">>}],
     [{F, <<"1970-01-01T00:00:01.123Z">>}]] =
        [M1:to_json({'T', {Timestamp, 1, 123}}),
         M1:to_json({'T', {Timestamp, 1, 123000}}),
         M1:to_json({'T', {Timestamp, 1, 123000000}})],
    %% with optionals omitted
    [[{F, <<"1970-01-01T00:00:00Z">>}],
     [{F, <<"1970-01-01T00:00:01Z">>}],
     [{F, <<"1970-01-01T00:00:00.000000123Z">>}]] =
        [M1:to_json({'T', {Timestamp, undefined, undefined}}),
         M1:to_json({'T', {Timestamp, 1, undefined}}),
         M1:to_json({'T', {Timestamp, undefined, 123}})],
    %% too large
    ?assertError(_, M1:to_json({'T', {Timestamp, 0, 2111222333}})),
    ?assertError(_, M1:to_json({'T', {Timestamp, 0, 1000000000}})),
    %% Decoding ---
    {'T', {Timestamp, 0, 0}} =
        M1:from_json([{F, <<"1970-01-01T00:00:00Z">>}], 'T'),
    {'T', {Timestamp, 482196050, 520000000}} =
        M1:from_json([{F, <<"1985-04-12T23:20:50.52Z">>}], 'T'),
    %% with offset
    [{'T',{'google.protobuf.Timestamp',851042397,0}},
     {'T',{'google.protobuf.Timestamp',851042397,0}}] =
        [M1:from_json([{F, <<"1996-12-19T16:39:57-08:00">>}], 'T'),
         M1:from_json([{F, <<"1996-12-20T00:39:57Z">>}], 'T')],
    %% with nano seconds and offset
    {'T',{'google.protobuf.Timestamp',851042397,520000000}} =
        M1:from_json([{F, <<"1996-12-19T16:39:57.52-08:00">>}], 'T'),
    %% with 3 6 or 9 decimals as appropriate
    [{'T', {Timestamp, 1, 123}},
     {'T', {Timestamp, 1, 123000}},
     {'T', {Timestamp, 1, 123000000}}] =
        [M1:from_json([{F, <<"1970-01-01T00:00:01.000000123Z">>}], 'T'),
         M1:from_json([{F, <<"1970-01-01T00:00:01.000123Z">>}], 'T'),
         M1:from_json([{F, <<"1970-01-01T00:00:01.123Z">>}], 'T')],
    %% "Accepted are any fractional digits (also none) as long as they fit
    %% into nano-seconds"
    [{'T', {Timestamp, 1, 1230}},
     {'T', {Timestamp, 1, 12300}},
     {'T', {Timestamp, 1, 12300000}}] =
        [M1:from_json([{F, <<"1970-01-01T00:00:01.00000123Z">>}], 'T'),
         M1:from_json([{F, <<"1970-01-01T00:00:01.0000123Z">>}], 'T'),
         M1:from_json([{F, <<"1970-01-01T00:00:01.0123Z">>}], 'T')],
    %% done
    unload_code(M1).

p3wellknown_wrappers_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/wrappers.proto';
        message Double { google.protobuf.DoubleValue f = 1; }
        message Float  { google.protobuf.FloatValue f = 1; }
        message I64    { google.protobuf.Int64Value f = 1; }
        message U64    { google.protobuf.UInt64Value f = 1; }
        message I32    { google.protobuf.Int32Value f = 1; }
        message U32    { google.protobuf.UInt32Value f = 1; }
        message Bool   { google.protobuf.BoolValue f = 1; }
        message String { google.protobuf.StringValue f = 1; }
        message Bytes  { google.protobuf.BytesValue f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    F = <<"f">>,
    DoubleValue = 'google.protobuf.DoubleValue',
    FloatValue = 'google.protobuf.FloatValue',
    [{F, 0.125}] = M1:to_json({'Double', {DoubleValue, 0.125}}),
    [{}]         = M1:to_json({'Double', undefined}),
    [{F, 0.125}] = M1:to_json({'Float', {FloatValue, 0.125}}),
    [{}]         = M1:to_json({'Float', undefined}),
    I64Value = 'google.protobuf.Int64Value',
    U64Value = 'google.protobuf.UInt64Value',
    I32Value = 'google.protobuf.Int32Value',
    U32Value = 'google.protobuf.UInt32Value',
    [{F, <<"123">>}] = M1:to_json({'I64', {I64Value, 123}}),
    [{}]             = M1:to_json({'I64', undefined}),
    [{F, <<"123">>}] = M1:to_json({'U64', {U64Value, 123}}),
    [{}]             = M1:to_json({'U64', undefined}),
    [{F, 123}]       = M1:to_json({'I32', {I32Value, 123}}),
    [{}]             = M1:to_json({'I32', undefined}),
    [{F, 123}]       = M1:to_json({'U32', {U32Value, 123}}),
    [{}]             = M1:to_json({'U32', undefined}),
    BoolValue   = 'google.protobuf.BoolValue',
    StringValue = 'google.protobuf.StringValue',
    BytesValue  = 'google.protobuf.BytesValue',
    [{F, true}]       = M1:to_json({'Bool', {BoolValue, true}}),
    [{}]              = M1:to_json({'Bool', undefined}),
    [{F, <<"abc">>}]  = M1:to_json({'String', {StringValue, <<"abc">>}}),
    [{}]              = M1:to_json({'String', undefined}),
    [{F, <<"YQA=">>}] = M1:to_json({'Bytes', {BytesValue, <<"a",0>>}}),
    [{}]              = M1:to_json({'Bytes', undefined}),
    %% Decoding ---
    {'Double', {DoubleValue, 0.0}}   = M1:from_json([{}],         'Double'),
    {'Double', {DoubleValue, 0.0}}   = M1:from_json([{F, null}],  'Double'),
    {'Double', {DoubleValue, 0.125}} = M1:from_json([{F, 0.125}], 'Double'),
    {'Float', {FloatValue, 0.0}}   = M1:from_json([{}],         'Float'),
    {'Float', {FloatValue, 0.0}}   = M1:from_json([{F, null}],  'Float'),
    {'Float', {FloatValue, 0.125}} = M1:from_json([{F, 0.125}], 'Float'),
    {'I64', {I64Value, 0}}  = M1:from_json([{}],            'I64'),
    {'I64', {I64Value, 0}}  = M1:from_json([{F, null}],     'I64'),
    {'I64', {I64Value, 17}} = M1:from_json([{F, <<"17">>}], 'I64'),
    {'U64', {U64Value, 0}}  = M1:from_json([{}],            'U64'),
    {'U64', {U64Value, 0}}  = M1:from_json([{F, null}],     'U64'),
    {'U64', {U64Value, 17}} = M1:from_json([{F, <<"17">>}], 'U64'),
    {'I32', {I32Value, 0}}  = M1:from_json([{}],        'I32'),
    {'I32', {I32Value, 0}}  = M1:from_json([{F, null}], 'I32'),
    {'I32', {I32Value, 17}} = M1:from_json([{F, 17}],   'I32'),
    {'U32', {U32Value, 0}}  = M1:from_json([{}],        'U32'),
    {'U32', {U32Value, 0}}  = M1:from_json([{F, null}], 'U32'),
    {'U32', {U32Value, 17}} = M1:from_json([{F, 17}],   'U32'),
    {'Bool', {BoolValue, false}} = M1:from_json([{}],         'Bool'),
    {'Bool', {BoolValue, false}} = M1:from_json([{F, null}],  'Bool'),
    {'Bool', {BoolValue, false}} = M1:from_json([{F, false}], 'Bool'),
    {'Bool', {BoolValue, true}}  = M1:from_json([{F, true}],  'Bool'),
    {'String', {StringValue, ""}}  = M1:from_json([{}],           'String'),
    {'String', {StringValue, ""}}  = M1:from_json([{F, null}],    'String'),
    {'String', {StringValue, "a"}} = M1:from_json([{F, <<"a">>}], 'String'),
    {'Bytes', {BytesValue, <<>>}}  = M1:from_json([{}],           'Bytes'),
    {'Bytes', {BytesValue, <<>>}}  = M1:from_json([{F, null}],    'Bytes'),
    {'Bytes', {BytesValue, <<"a",0>>}} = M1:from_json([{F, <<"YQA=">>}],
                                                      'Bytes'),
    %% done
    unload_code(M1).

p3wellknown_null_value_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/struct.proto';
        message N { google.protobuf.NullValue f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    F = <<"f">>,
    E1 = {'N', 'NULL_VALUE'},
    J1 = [{F, null}],
    J1 = M1:to_json(E1),
    E1 = M1:from_json(J1, 'N'),
    E1 = M1:from_json([{F, 0}], 'N'),
    E1 = M1:from_json([{F, 0.0}], 'N'),
    E1 = M1:from_json([{F, <<>>}], 'N'),
    unload_code(M1).

p3wellknown_struct_list_value_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/struct.proto';
        message L { google.protobuf.ListValue f = 1; }
        message V { google.protobuf.Value f = 1; }
        message S { google.protobuf.Struct f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    ListValue = 'google.protobuf.ListValue',
    Value = 'google.protobuf.Value',
    Struct = 'google.protobuf.Struct',
    E1 = {ListValue, [{Value, {null_value, 'NULL_VALUE'}},
                                {Value, {number_value, 12}},
                                {Value, {number_value, 1.25e3}},
                                {Value, {string_value, "abc"}},
                                {Value, {bool_value, true}},
                                {Value, {struct_value, {Struct, []}}},
                                {Value, {list_value, {ListValue,[]}}}]},
    J1 = [null, 12, 1.25e3, <<"abc">>, true, [{}], []],
    J1 = M1:to_json(E1),
    E1 = change_term(M1:from_json(J1, ListValue),
                     {number_value,12.0}, % double decodes to float, fix cmp
                     {number_value,12}),

    F = <<"f">>,
    J2 = [{F, [{<<"s">>, <<"abc">>},
          {<<"b">>, true},
          {<<"n">>, null}]}],
    E2 = {'S', {Struct, M21=[{"s", {Value, {string_value, "abc"}}},
                             {"b", {Value, {bool_value, true}}},
                             {"n", {Value, {null_value, 'NULL_VALUE'}}}]}},
    J2 = M1:to_json(E2),
    {'S', {Struct, M22}} = M1:from_json(J2, 'S'),
    ?assertEqual(lists:sort(M21),
                 lists:sort(M22)),
    unload_code(M1).

p3wellknown_listvalue_with_translation_test() ->
    %% Test mk_msg with translations for repeated fields.
    %% This the google.protobuf.ListValue is a repeated field.
    Proto = "
        syntax='proto3';
        import 'google/protobuf/struct.proto';
        message L { google.protobuf.ListValue f = 1; }
        ",
    ListValue = 'google.protobuf.ListValue',
    TranslateOpt =
        {translate_field,
         {[ListValue,values],
          %% Internal format for the list is a set in this test,
          %% so translate to/from a set.
          [{encode, {sets, to_list, ['$1']}},
           {decode_init_default, {sets, new, []}},
           {decode_repeated_add_elem, {sets, add_element,['$1', '$2']}},
           {decode_repeated_finalize, {?MODULE, id, ['$1']}}]}},
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json, TranslateOpt]),
    %% true occurs twice in the input, but will be only once in the set
    {ListValue, S} = M1:from_json([null, <<"abc">>, true, true], ListValue),
    [{'google.protobuf.Value',{bool_value,true}},
     {'google.protobuf.Value',{null_value,'NULL_VALUE'}},
     {'google.protobuf.Value',{string_value,"abc"}}] =
        lists:sort(sets:to_list(S)),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
p3wellknown_struct_list_value_map_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/struct.proto';
        message L { google.protobuf.ListValue f = 1; }
        message V { google.protobuf.Value f = 1; }
        message S { google.protobuf.Struct f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json, maps]),
    E1 = #{values => [#{kind => {null_value, 'NULL_VALUE'}},
                      #{kind => {number_value, 12}},
                      #{kind => {number_value, 1.25e3}},
                      #{kind => {string_value, "abc"}},
                      #{kind => {bool_value, true}},
                      #{kind => {struct_value, #{fields => #{}}}},
                      #{kind => {list_value, #{values => []}}}]},
    J1 = [null, 12, 1.25e3, <<"abc">>, true, #{}, []],
    ?assertEqual(J1, M1:to_json(E1, 'google.protobuf.ListValue')),
    ?assertEqual(E1, change_term(M1:from_json(J1, 'google.protobuf.ListValue'),
                                 {number_value,12.0},
                                 {number_value,12})),
    E2 = #{f => #{fields => #{"s" => #{kind => {string_value, "abc"}},
                              "b" => #{kind => {bool_value, true}},
                              "n" => #{kind => {null_value, 'NULL_VALUE'}}}}},
    J2 = #{<<"f">> => #{<<"s">> => <<"abc">>,
                        <<"b">> => true,
                        <<"n">> => null}},
    ?assertEqual(J2, M1:to_json(E2, 'S')),
    ?assertEqual(E2, M1:from_json(J2, 'S')),
    unload_code(M1).

p3wellknown_struct_list_value_flat_map_test_() ->
    flat_map_prerequisites(
      [{"flat oneof", fun p3wellknown_struct_list_value_flat_map_aux/0}]).

p3wellknown_struct_list_value_flat_map_aux() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/struct.proto';
        message L { google.protobuf.ListValue f = 1; }
        message V { google.protobuf.Value f = 1; }
        message S { google.protobuf.Struct f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json, maps, {maps_oneof, flat}]),
    E1 = #{values => [#{null_value => 'NULL_VALUE'},
                      #{number_value => 12},
                      #{number_value => 1.25e3},
                      #{string_value => "abc"},
                      #{bool_value => true},
                      #{struct_value => #{fields => #{}}},
                      #{list_value => #{values => []}}]},
    J1 = [null, 12, 1.25e3, <<"abc">>, true, #{}, []],
    ?assertEqual(J1, M1:to_json(E1, 'google.protobuf.ListValue')),
    ?assertEqual(E1, change_term(M1:from_json(J1, 'google.protobuf.ListValue'),
                                 #{number_value => 12.0},
                                 #{number_value => 12})),
    E2 = #{f => #{fields => #{"s" => #{string_value => "abc"},
                              "b" => #{bool_value => true},
                              "n" => #{null_value => 'NULL_VALUE'}}}},
    J2 = #{<<"f">> => #{<<"s">> => <<"abc">>,
                        <<"b">> => true,
                        <<"n">> => null}},
    ?assertEqual(J2, M1:to_json(E2, 'S')),
    ?assertEqual(E2, M1:from_json(J2, 'S')),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

change_term(Old, Old, New) -> New;
change_term([H | T], Old, New) ->
    [change_term(H, Old, New) | change_term(T, Old, New)];
change_term(T, Old, New) when is_tuple(T) ->
    list_to_tuple(change_term(tuple_to_list(T), Old, New));
change_term(X, Old, New) ->
    maybe_change_map_term(X, Old, New).

-ifndef(NO_HAVE_MAPS).
maybe_change_map_term(M, Old, New) when is_map(M) ->
    maps:from_list([{change_term(K, Old, New), change_term(V, Old, New)}
                    || {K, V} <- maps:to_list(M)]);
maybe_change_map_term(X, _Old, _New) -> % simple/scalar term, no change
    X.
-else. % -ifndef(NO_HAVE_MAPS).
maybe_change_map_term(X, _Old, _New) -> % simple/scalar term, no change
    X.
-endif. % -ifndef(NO_HAVE_MAPS).

p3wellknown_empty_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/empty.proto';
        message E { google.protobuf.Empty f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    E1 = {'E', {'google.protobuf.Empty'}},
    J1 = [{<<"f">>, [{}]}],
    J1 = M1:to_json(E1),
    E1 = M1:from_json(J1, 'E'),
    unload_code(M1).

p3wellknown_field_mask_test() ->
    Proto = "
        syntax='proto3';
        import 'google/protobuf/field_mask.proto';
        message F { google.protobuf.FieldMask f = 1; }
        ",
    M1 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json]),
    FieldMask = 'google.protobuf.FieldMask',
    F = <<"f">>,
    E1 = {'F', {FieldMask, ["user.display_name", "photo"]}},
    J1 = [{F, <<"user.displayName,photo">>}],
    J1 = M1:to_json(E1),
    E1 = M1:from_json(J1, 'F'),
    %% spurious commas
    {FieldMask, ["f", "g"]} = M1:from_json(<<"f,g">>, FieldMask),
    {FieldMask, ["f", "g"]} = M1:from_json(<<"f,,,g">>, FieldMask),
    {FieldMask, ["f", "g"]} = M1:from_json(<<",,f,g">>, FieldMask),
    {FieldMask, ["f", "g"]} = M1:from_json(<<"f,g,,">>, FieldMask),
    %% spurious dots preserved
    {FieldMask, ["f...g"]} = M1:from_json(<<"f...g">>, FieldMask),
    %% unlowercase handling
    {FieldMask, ["d_name"]} = M1:from_json(<<"dName">>, FieldMask),
    {FieldMask, ["d1_name"]} = M1:from_json(<<"d1Name">>, FieldMask),
    {FieldMask, ["d1name"]}  = M1:from_json(<<"d1name">>, FieldMask),
    {FieldMask, ["d_n_name"]} = M1:from_json(<<"dNName">>, FieldMask),
    {FieldMask, ["d_abc_name"]} = M1:from_json(<<"dABCName">>, FieldMask),
    {FieldMask, ["d_name"]} = M1:from_json(<<"dNAME">>, FieldMask),
    {FieldMask, ["dname"]} = M1:from_json(<<"DNAME">>, FieldMask),
    unload_code(M1),

    %% Even with the option json_preserve_proto_field_names,
    %% field_mask names are still to be lowerCamelCased,
    %% at least in google's libprotoc (3.6.1)
    M2 = compile_protos([{"<gen>.proto", Proto}],
                        [use_packages, json, json_preserve_proto_field_names]),
    J1 = M2:to_json(E1),
    J1 = [{F, <<"user.displayName,photo">>}],
    unload_code(M2).


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

    %% Check decodable as both original field name and as lowerCamelCase
    {'Msg', 1, _} = M1:from_json([{<<"fooBar">>, 1}], 'Msg'),
    {'Msg', 1, _} = M1:from_json([{<<"foo_bar">>, 1}], 'Msg'),
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

field_pass_as_record_test() ->
    %% Analysis may decide decoding should pass fields in a record
    %% instead of as params for performance reasons,
    %% but this could make json decoding get into trouble
    %% due to the joint depencency on gpb_decoders_lib.
    Proto = "
       message Msg {
          uint32 f1 = 1;
          uint32 f2 = 2;
          map<string,uint32> f3 = 3;
       }
       ",
    M1 = compile_iolist(Proto,
                        [json, {field_pass_method, pass_as_record}]),
    [{<<"f1">>, 1},
     {<<"f2">>, 2},
     {<<"f3">>, [{<<"a">>, 1},{<<"b">>, 2}]}] =
         M1:to_json({'Msg', 1, 2, [{"a", 1}, {"b", 2}]}),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
json_maps_and_records_with_mapfields_test() ->
    Proto = "
       message Msg {
          uint32 f1 = 1;
          uint32 f2 = 2;
          map<string,uint32> f3 = 3;
       }
       ",
    M1 = compile_iolist(Proto, [json, {json_format, maps}]),
    Msg = {'Msg', 1, 2, [{"a", 1}, {"b", 2}]},
    #{<<"f1">> := 1,
      <<"f2">> := 2,
      <<"f3">> := #{<<"a">> := 1, <<"b">> := 2}} = M1:to_json(Msg),
    Msg2 = M1:decode_msg(M1:encode_msg(Msg, 'Msg'), 'Msg'),
    %% order undefined, so compare after sort:
    ?assertEqual(lists:sort(element(4, Msg)),
                 lists:sort(element(4, Msg2))),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

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

ignores_unknown_items_on_decoding_test() ->
    Proto = "
         message Msg {
           optional uint32 f = 1;
         }
    ",
    M1 = compile_iolist(Proto, [json]),
    {'Msg', 17} = M1:from_json([{<<"foo">>, 18},
                                {<<"bar">>, 19},
                                {<<"f">>,   17}], 'Msg'),
    unload_code(M1).

bypass_wrappers_proto() ->
    "
         syntax=\"proto2\";
         message Msg {
           optional uint32 f = 1;
         }
    ".

bypass_wrappers_test() ->
    M1 = compile_iolist(bypass_wrappers_proto(), [json, bypass_wrappers]),
    [{<<"f">>, 17}] = M1:to_json_msg_Msg({'Msg', 17}),
    {'Msg', 17} = M1:from_json_msg_Msg([{<<"f">>, 17}]),
    unload_code(M1).

-ifndef(NO_HAVE_MAPS).
bypass_wrappers_maps_test() ->
    M1 = compile_iolist(bypass_wrappers_proto(),
                        [json, maps, bypass_wrappers]),
    ?assertEqual(#{<<"f">> => 17}, M1:to_json_msg_Msg(#{f => 17})),
    ?assertEqual(#{f => 17}, M1:from_json_msg_Msg(#{<<"f">> => 17})),
    unload_code(M1).
-endif. % -ifndef(NO_HAVE_MAPS).

json_name_proto() ->
    "
         syntax='proto2';
         message Msg {
           optional uint32 foo_bar = 1 [json_name='x_y_z'];
         }
    ".

json_name_test() ->
    Proto = json_name_proto(),
    M1 = compile_iolist(Proto, [json]),
    [{<<"x_y_z">>, 17}] = M1:to_json({'Msg', 17}),
    {'Msg', 17} = M1:from_json([{<<"x_y_z">>, 17}], 'Msg'),
    {'Msg', 17} = M1:from_json([{<<"foo_bar">>, 17}], 'Msg'),
    unload_code(M1).

aliased_enums_test() ->
    Proto = "
         syntax='proto2';
         message Msg {
           optional ee f = 1;
         }
         enum ee {
           option allow_alias = true;
           E0 = 0;
           E1_A = 1;
           E1_B = 1;
         }
    ",
    M1 = compile_iolist(Proto, [json]),
    [{<<"f">>, <<"E0">>}] = M1:to_json({'Msg', 'E0'}),
    [{<<"f">>, <<"E1_A">>}] = M1:to_json({'Msg', 'E1_A'}),
    [{<<"f">>, <<"E1_B">>}] = M1:to_json({'Msg', 'E1_B'}),
    {'Msg', 'E0'} = M1:from_json([{<<"f">>, <<"E0">>}], 'Msg'),
    {'Msg', 'E1_A'} = M1:from_json([{<<"f">>, <<"E1_A">>}], 'Msg'),
    {'Msg', 'E1_A'} = M1:from_json([{<<"f">>, <<"E1_B">>}], 'Msg'),
    {'Msg', 'E0'}   = M1:from_json([{<<"f">>, <<"0">>}], 'Msg'),
    {'Msg', 'E1_A'} = M1:from_json([{<<"f">>, <<"1">>}], 'Msg'),
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
           json_preserve_proto_field_names,
           json_case_insensitive_enum_parsing],
          ["x.proto"]}} =
        gpb_compile:parse_opts_and_args(
          ["-json", "-json-always-print-primitive-fields",
           "-json-preserve-proto-field-names",
           "-json-case-insensitive-enum-parsing",
           "x.proto"]),
    ok.

%% nif ------------------------------------------------

nif_test_() ->
    nif_tests_check_prerequisites(
      [?nif_if_supported(nif_misc_types_rec),
       ?nif_if_supported(nif_field_names),
       ?nif_if_supported(nif_type_defaults),
       ?nif_if_supported(nif_oneof_rec),
       ?nif_if_supported(nif_mapfields_rec),
       ?nif_if_supported(nif_bypass_wrappers),
       ?nif_if_supported(nif_json_name)]).


-ifndef(NO_HAVE_MAPS).
nif_maps_test_() ->
    nif_tests_check_prerequisites(
      [?nif_if_supported(nif_misc_types_maps),
       ?nif_if_supported(nif_oneof_maps),
       ?nif_if_supported(nif_oneof_flat_maps),
       ?nif_if_supported(nif_mapfields_maps)]).
-endif. % -ifndef(NO_HAVE_MAPS).

nif_misc_types_proto() ->
    "
        syntax=\"proto2\";
        message M {
          optional uint32 op = 1;
          required uint32 rq = 2;
          repeated uint32 rp = 3;
        }
        message Top {
          optional Sub f = 1;
        }
        message Sub {
          optional uint32 f = 1;
        }
        message Ints {
          optional int32   i32 = 1;
          optional fixed32 f32 = 2;
          optional int64   i64 = 3;
          optional fixed64 f64 = 4;
        }
        message Floats {
          optional float   f = 1;
          optional double  d = 2;
        }
        message Bool {
          optional bool    b = 1;
        }
        message Strs {
          optional string  s = 1;
          optional bytes   b = 2;
        }
        message Enums {
          optional EE      e = 1;
        }
        enum EE { EE_A = 0; EE_B = 1; EE_C = 2; }
        ".

nif_misc_types_rec(features) ->
    [json | guess_features(nif_misc_types_proto())];
nif_misc_types_rec(title) ->
    "types (records)".
nif_misc_types_rec() ->
    nif_to_from_json_aux(
      dont_save,
      nif_misc_types_proto(),
      fun(NifM, ErlM) ->
              Bigint = (1 bsl 60) - 1, % bigger than 2^53
              j_roundtrip({'Top', {'Sub', 4711}}, NifM, ErlM),
              j_roundtrip({'M', 12, 13, [14]}, NifM, ErlM),
              j_roundtrip({'Ints', 11, 12, 4711, Bigint}, NifM, ErlM),
              j_roundtrip({'Floats', -2.0, 1.25e-1}, NifM, ErlM),
              j_roundtrip({'Bool', true}, NifM, ErlM),
              j_roundtrip({'Bool', false}, NifM, ErlM),
              j_roundtrip({'Strs', "abc", <<255,1,0>>}, NifM, ErlM),
              j_roundtrip({'Enums', 'EE_A'}, NifM, ErlM),
              j_roundtrip({'Enums', 'EE_B'}, NifM, ErlM),
              ok
      end,
      [json]).

-ifndef(NO_HAVE_MAPS).
nif_misc_types_maps(features) -> [json|guess_features(nif_misc_types_proto())];
nif_misc_types_maps(title) -> "types (maps)".
nif_misc_types_maps() ->
    nif_to_from_json_aux(
      dont_save,
      nif_misc_types_proto(),
      fun(NifM, ErlM) ->
              Bigint = (1 bsl 60) - 1, % bigger than 2^53
              j_map_roundtrip(#{f => #{f => 4711}},
                              'Top', NifM, ErlM),
              j_map_roundtrip(#{op => 12, rq => 13, rp => [14]},
                              'M', NifM, ErlM),
              j_map_roundtrip(#{i32 => 11, f32 => 12,
                                i64 => 4711, f64 => Bigint},
                              'Ints', NifM, ErlM),
              j_map_roundtrip(#{f => -2.0, d => 1.25e-1}, 'Floats', NifM, ErlM),
              j_map_roundtrip(#{b => true}, 'Bool', NifM, ErlM),
              j_map_roundtrip(#{b => false}, 'Bool', NifM, ErlM),
              j_map_roundtrip(#{s => "abc", b => <<255,1,0>>},
                              'Strs', NifM, ErlM),
              j_map_roundtrip(#{e => 'EE_A'}, 'Enums', NifM, ErlM),
              j_map_roundtrip(#{e => 'EE_B'}, 'Enums', NifM, ErlM),
              ok
      end,
      [json, maps]).
-endif. % -ifndef(NO_HAVE_MAPS).

field_names_and_defaults_proto() ->
    "
    syntax=\"proto3\";
    message IntMsg  { uint32 foo_bar_int = 1; };
    ".

nif_field_names(features) -> [json, json_preserve_proto_field_names, proto3];
nif_field_names(title) -> "field names".
nif_field_names() ->
    Msg1 = {'IntMsg', 1},
    nif_to_from_json_aux(
      dont_save,
      field_names_and_defaults_proto(),
      fun(NifM, _ErlM) ->
              [{<<"fooBarInt">>,  _}] = json_decode(NifM:to_json(Msg1)),
              ok
      end,
      [json]),
    nif_to_from_json_aux(
      dont_save,
      field_names_and_defaults_proto(),
      fun(NifM, _ErlM) ->
              [{<<"foo_bar_int">>,  _}] = json_decode(NifM:to_json(Msg1)),
              ok
      end,
      [json, json_preserve_proto_field_names]).

nif_type_defaults(features) -> [json, proto3];
nif_type_defaults(title) -> "type defaults".
nif_type_defaults() ->
    Msg0 = {'IntMsg', 0},
    Msg1 = {'IntMsg', 1},
    nif_to_from_json_aux(
      dont_save,
      field_names_and_defaults_proto(),
      fun(NifM, _ErlM) ->
              [{}]           = json_decode(NifM:to_json(Msg0)),
              [{_,  1}]      = json_decode(NifM:to_json(Msg1)),
              ok
      end,
      [json]),
    nif_to_from_json_aux(
      dont_save,
      field_names_and_defaults_proto(),
      fun(NifM, _ErlM) ->
              [{_,  0}]       = json_decode(NifM:to_json(Msg0)),
              [{_,  1}]       = json_decode(NifM:to_json(Msg1)),
              ok
      end,
      [json, json_always_print_primitive_fields]).

nif_oneof_rec(features) -> [json | guess_features(oneof_proto())];
nif_oneof_rec(title) -> "oneof (records)".
nif_oneof_rec() ->
    nif_to_from_json_aux(
      dont_save,
      oneof_proto(),
      fun(NifM, ErlM) ->
              j_roundtrip({'Msg', {a, 10}}, NifM, ErlM),
              j_roundtrip({'Msg', {b, true}}, NifM, ErlM),
              ok
      end,
      [json]).

-ifndef(NO_HAVE_MAPS).
nif_oneof_maps(features) -> [json | guess_features(oneof_proto())];
nif_oneof_maps(title) -> "oneof (maps)".
nif_oneof_maps() ->
    nif_to_from_json_aux(
      dont_save,
      oneof_proto(),
      fun(NifM, ErlM) ->
              j_map_roundtrip(#{c => {a, 10}}, 'Msg', NifM, ErlM),
              j_map_roundtrip(#{c => {b, true}}, 'Msg', NifM, ErlM),
              ok
      end,
      [json, maps]).

nif_oneof_flat_maps(features) -> [json | guess_features(oneof_proto())];
nif_oneof_flat_maps(extra_checks) -> [fun can_do_flat_oneof/1];
nif_oneof_flat_maps(title) -> "oneof (flat maps)".
nif_oneof_flat_maps() ->
    nif_to_from_json_aux(
      dont_save,
      oneof_proto(),
      fun(NifM, ErlM) ->
              j_map_roundtrip(#{a => 10}, 'Msg', NifM, ErlM),
              j_map_roundtrip(#{b => true}, 'Msg', NifM, ErlM),
              ok
      end,
      [json, maps, {maps_oneof, flat}]).
-endif. % -ifndef(NO_HAVE_MAPS).

mapfields_proto() ->
    "
    syntax=\"proto2\";
    message I32ToStr  { map<int32,string>  f = 1; };
    message BoolToStr { map<bool,string>   f = 1; };
    message StrToStr  { map<string,string> f = 1; };
    message I64ToSub  { map<int64,Sub>     f = 1; };
    message Sub       { optional uint32    i = 1; };
    ".

nif_mapfields_rec(features) -> [json | guess_features(mapfields_proto())];
nif_mapfields_rec(title) -> "map<_,_> fields (records)".
nif_mapfields_rec() ->
    nif_to_from_json_aux(
      dont_save,
      mapfields_proto(),
      fun(NifM, ErlM) ->
              j_roundtrip({'I32ToStr', [{0,"a"},{1,"b"}]}, NifM, ErlM,
                          fun element2sort/1),
              j_roundtrip({'BoolToStr', [{false,"a"},{true,"b"}]}, NifM, ErlM,
                          fun element2sort/1),
              j_roundtrip({'StrToStr', [{"a","z"},{"b","x"}]}, NifM, ErlM,
                          fun element2sort/1),
              j_roundtrip({'I64ToSub', [{1,{'Sub',11}}, {2,{'Sub',22}}]},
                          NifM, ErlM, fun element2sort/1),
              ok
      end,
      [json]).

-ifndef(NO_HAVE_MAPS).
nif_mapfields_maps(features) -> [json | guess_features(mapfields_proto())];
nif_mapfields_maps(title) -> "map<_,_> fields (maps)".
nif_mapfields_maps() ->
    nif_to_from_json_aux(
      dont_save,
      mapfields_proto(),
      fun(NifM, ErlM) ->
              j_map_roundtrip(#{f => #{0 => "a",
                                       1 => "b"}},
                              'I32ToStr', NifM, ErlM),
              j_map_roundtrip(#{f => #{false => "a",
                                       true => "b"}},
                              'BoolToStr', NifM, ErlM),
              j_map_roundtrip(#{f => #{"a" => "z",
                                       "b" => "x"}},
                              'StrToStr', NifM, ErlM),
              j_map_roundtrip(#{f => #{1 => #{i => 11},
                                       2 => #{i => 22}}},
                              'I64ToSub', NifM, ErlM),
              ok
      end,
      [json, maps]).
-endif. % -ifndef(NO_HAVE_MAPS).

nif_bypass_wrappers(features) -> [json |
                                  guess_features(bypass_wrappers_proto())];
nif_bypass_wrappers(title) -> "bypass_wrappers".
nif_bypass_wrappers() ->
    nif_to_from_json_aux(
      dont_save,
      bypass_wrappers_proto(),
      fun(NifM, _ErlM) ->
              JsonTxt = NifM:to_json_msg_Msg({'Msg', 17}),
              {'Msg', 17} = NifM:from_json_msg_Msg(JsonTxt)
      end,
      [json, bypass_wrappers]).

nif_to_from_json_aux(SaveOrNot, Proto, TestF, ExtraOpts) ->
    with_tmpdir(
      SaveOrNot,
      fun(TmpDir) ->
              NifM = gpb_jnif_test_ed1,
              NifOpts = [nif] ++ ExtraOpts,
              {ok, Code} = compile_nif_msg_defs(NifM, Proto, TmpDir, NifOpts),
              in_separate_vm(
                TmpDir, NifM, Code,
                fun() ->
                        ErlOpts = NifOpts -- [nif],
                        ErlM = compile_iolist(Proto, ErlOpts),
                        TestF(NifM, ErlM)
                end)
      end).

nif_json_name(features) -> [json | guess_features(json_name_proto())];
nif_json_name(title) -> "json_name field option".
nif_json_name() ->
    nif_to_from_json_aux(
      dont_save,
      json_name_proto(),
      fun(NifM, _ErlM) ->
              [{<<"x_y_z">>, 17}] = json_decode(NifM:to_json({'Msg', 17})),
              {'Msg', 17} = NifM:from_json(<<"{\"x_y_z\": 17}">>, 'Msg'),
              {'Msg', 17} = NifM:from_json(<<"{\"foo_bar\": 17}">>, 'Msg')
      end,
      [json]).

-compile({nowarn_unused_function, with_tmpdir/1}).
with_tmpdir(F) ->
    with_tmpdir(dont_save, F). % -import()ed from gpb_compile_tests

-ifndef(NO_HAVE_MAPS).
can_do_flat_oneof(Features) ->
    gpb_compile_maps_tests:can_do_flat_oneof(Features).
-endif. % -ifndef(NO_HAVE_MAPS).


%% roundtrip for records/tuples
j_roundtrip(Msg, NifModule, ErlModule) ->
    j_roundtrip(Msg, NifModule, ErlModule, fun id/1).
j_roundtrip(Msg, NifModule, ErlModule, PrepF) when is_tuple(Msg) ->
    j_roundtrip_nif_to_erl(Msg, NifModule, ErlModule, PrepF),
    j_roundtrip_erl_to_nif(Msg, NifModule, ErlModule, PrepF).

j_roundtrip_nif_to_erl(Msg, NifModule, ErlModule, PrepF) ->
    JStr = NifModule:to_json(Msg),
    JRepr = json_decode(JStr),
    MsgName = element(1, Msg),
    DebugInfo = [nif_to_erl, {jstr, JStr}, {jrepr, JRepr}],
    ?assert_eq_3(Msg, PrepF(ErlModule:from_json(JRepr, MsgName)), DebugInfo).

j_roundtrip_erl_to_nif(Msg, NifModule, ErlModule, PrepF) ->
    JRepr = ErlModule:to_json(Msg),
    JStr = json_encode(JRepr),
    MsgName = element(1, Msg),
    DebugInfo = [erl_to_nif, {jstr, JStr}, {jrepr, JRepr}],
    ?assert_eq_3(Msg, PrepF(NifModule:from_json(JStr, MsgName)), DebugInfo).

id(X) -> X.

-ifndef(NO_HAVE_MAPS).
%% roundtrip for maps
j_map_roundtrip(Msg, MsgName, NifModule, ErlModule) when is_map(Msg) ->
    j_map_roundtrip_nif_to_erl(Msg, MsgName, NifModule, ErlModule),
    j_map_roundtrip_erl_to_nif(Msg, MsgName, NifModule, ErlModule).

j_map_roundtrip_nif_to_erl(Msg, MsgName, NifModule, ErlModule) ->
    JStr = NifModule:to_json(Msg, MsgName),
    JRepr = pl_to_map(json_decode(JStr)),
    DebugInfo = [nif_to_erl, {jstr, JStr}, {jrepr, JRepr}],
    ?assert_eq_3(Msg, ErlModule:from_json(JRepr, MsgName), DebugInfo).

j_map_roundtrip_erl_to_nif(Msg, MsgName, NifModule, ErlModule) ->
    JRepr = ErlModule:to_json(Msg, MsgName),
    JStr = json_encode(map_to_pl(JRepr)),
    DebugInfo = [erl_to_nif, {jstr, JStr}, {jrepr, JRepr}],
    ?assert_eq_3(Msg, NifModule:from_json(JStr, MsgName), DebugInfo).
-endif. % -ifndef(NO_HAVE_MAPS).

%% -- Simplified json encoder/decoder for subset that occurs here --
json_encode(Obj) -> iolist_to_binary(je(Obj)).

je([{}]) ->
    <<"{}">>;
je(L) when is_list(L), is_tuple(hd(L)) ->
    [${, comma_join([[$\",K,$\",$:, je(V)] || {K,V} <- L]), $}];
je(L) when is_list(L) ->
    [$[, comma_join([je(Elem) || Elem <- L]), $]];
je(I) when is_integer(I) -> integer_to_list(I);
je(F) when is_float(F) -> float_to_list(F);
je(S) when is_binary(S) -> [$\",S,$\"]; % string
je(true) -> <<"true">>;
je(false) -> <<"false">>;
je(null) -> <<"null">>.

comma_join([]) -> []; % gpb_lib:comma_join introduces spaces, so use our own
comma_join([Hd|Tl]) -> [Hd | [[",",Elem] || Elem <- Tl]].

%% Expect well-conforming input as a binary.
%% - assume object keys always strings
%% - assume floating point numbers well formed (eg leading digit, no leading +)
%% - assume strings have no escaped chars
json_decode(B) ->
    {JTerm, ""} = jd(binary_to_list(B)),
    JTerm.

%% Assume any preceding whitespace has been stripped (see sp/1)
%% Return {Token, Rest} where Rest has been stripped of leading whitespace
-define(is_digit(C), $0 =< C, C =< $9).
jd("{}"++Rest) -> {[{}], sp(Rest)};
jd("{"++Rest) -> jd_obj(Rest, []);
jd("\""++Rest) -> jd_str(Rest, "");
jd("["++Rest) -> jd_array(Rest, []);
jd("-"++[D|Rest]) when ?is_digit(D) -> jd_num(Rest, [D,$-]);
jd([D|_]=Rest) when ?is_digit(D) -> jd_num(Rest, []);
jd("true"++Rest) -> {true, sp(Rest)};
jd("false"++Rest) -> {false, sp(Rest)};
jd("null"++Rest) -> {null, sp(Rest)}.

jd_obj(S, Acc) ->
    {K, ":"++Rest} = jd(S),
    case jd(sp(Rest)) of
        {V, ","++Rest2} -> jd_obj(sp(Rest2), [{K,V} | Acc]);
        {V, "}"++Rest2} -> {lists:reverse([{K,V} | Acc]), sp(Rest2)}
    end.

jd_array(S, Acc) ->
    case jd(S) of
        {Elem, ","++Rest} -> jd_obj(sp(Rest), [Elem | Acc]);
        {Elem, "]"++Rest} -> {lists:reverse([Elem | Acc]), sp(Rest)}
    end.

jd_str("\""++Rest, Acc) -> {list_to_binary(lists:reverse(Acc)), sp(Rest)};
jd_str("\\"++Rest, Acc) -> error({oops, didnt_expect_escaped, Rest, Acc});
jd_str([C|Rest], Acc)   -> jd_str(Rest, [C | Acc]).

jd_num([D|Rest], Acc) when ?is_digit(D) -> jd_num(Rest, [D | Acc]);
jd_num("."++Rest, Acc) -> jd_frac(Rest, "." ++ Acc);
jd_num("E"++Rest, Acc) -> jd_exp(Rest, lists:reverse(".0e", Acc));
jd_num("e"++Rest, Acc) -> jd_exp(Rest, lists:reverse(".0e", Acc));
jd_num(Rest, Acc) -> {list_to_integer(lists:reverse(Acc)), sp(Rest)}.

jd_frac([D | Rest], Acc) when ?is_digit(D) -> jd_frac(Rest, [D | Acc]);
jd_frac("e"++Rest, Acc) -> jd_exp(Rest, "e" ++ Acc);
jd_frac("E"++Rest, Acc) -> jd_exp(Rest, "e" ++ Acc);
jd_frac(Rest, Acc) -> {list_to_float(lists:reverse(Acc)), sp(Rest)}.

jd_exp([D | Rest], Acc) when ?is_digit(D) -> jd_exp_d(Rest, [D | Acc]);
jd_exp("+"++Rest, Acc) -> jd_exp_d(Rest, "+" ++ Acc);
jd_exp("-"++Rest, Acc) -> jd_exp_d(Rest, "-" ++ Acc).

jd_exp_d([D|Rest], Acc) when ?is_digit(D) -> jd_exp_d(Rest, [D | Acc]);
jd_exp_d(Rest, Acc) -> {list_to_float(lists:reverse(Acc)), sp(Rest)}.

sp(" "++Rest) -> sp(Rest);
sp("\t"++Rest) -> sp(Rest);
sp("\n"++Rest) -> sp(Rest);
sp(Rest) -> Rest.

%% Extra helpers for maps
-ifndef(NO_HAVE_MAPS).
map_to_pl(M) when is_map(M) ->
    case [{K, map_to_pl(V)} || {K,V} <- maps:to_list(M)] of
        [] -> [{}];
        Pl -> Pl
    end;
map_to_pl(L) when is_list(L) ->
    [map_to_pl(Elem) || Elem <- L];
map_to_pl(X) ->
    X.

pl_to_map([{}]) ->
    #{};
pl_to_map(L) when is_list(L), is_tuple(hd(L)) ->
    maps:from_list([{K, pl_to_map(V)} || {K,V} <- L]);
pl_to_map(L) when is_list(L) ->
    [pl_to_map(Elem) || Elem <- L];
pl_to_map(X) ->
    X.
-endif. % -ifndef(NO_HAVE_MAPS).

strip_occurrence("optional" ++ Rest) -> strip_occurrence(Rest);
strip_occurrence("required" ++ Rest) -> strip_occurrence(Rest);
strip_occurrence([C | Rest])         -> [C | strip_occurrence(Rest)];
strip_occurrence("")                 -> "".
