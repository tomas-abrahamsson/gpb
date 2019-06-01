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

-include_lib("eunit/include/eunit.hrl").

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
