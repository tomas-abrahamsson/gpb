%%% Copyright (C) 2026  Tomas Abrahamsson
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
-module(gpb_compile_native_records_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-ifdef(NO_HAVE_NATIVE_RECORDS).

no_native_records_tests__test() ->
    %% rebar.config.script or the Makefile
    %% sets NO_HAVE_NATIVE_RECORDS if they detect that there is
    %% no support for native records.
    ok.

-else. %% NO_HAVE_NATIVE_RECORDS

-import(gpb_compile_tests, [compile_iolist/2]).
-import(gpb_compile_tests, [unload_code/1]).

simple_native_records_test() ->
    M = compile_iolist(
          """
          syntax="proto2";
          message m1 {
            required string f1 = 1;
            required uint32 f2 = 2;
          }
          """,
          [{msg_format, native_records}]),
    R = records:create(M, m1, [{f1, "some string"}, {f2, 33}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    Data = M:encode_msg(R, m1),
    Data = M:encode_msg(R, []),
    Data = M:encode_msg(R, [{verify, true}]),
    Data = M:encode_msg(R, [{verify, false}]),
    Data = M:encode_msg(R, m1, []),
    R = M:decode_msg(Data, m1),
    R = M:decode_msg(Data, m1, []),
    R = M:merge_msgs(R, R),
    R = M:merge_msgs(R, R, m1),
    R = M:merge_msgs(R, R, m1, []),
    ok = M:verify_msg(R),
    ok = M:verify_msg(R, m1),
    ok = M:verify_msg(R, m1, []),
    ?assertError({gpb_type_error, _}, M:verify_msg(R#_{f1 = x})),
    ?assertError({gpb_type_error, _}, M:verify_msg(R#_{f1=undefined})),
    [#{name := f1}, % expect maps format by default
     #{name := f2}] = M:fetch_msg_def(m1),
    unload_code(M).

oneof_test() ->
    M = compile_iolist(
          """
          syntax="proto2";
          message m1 {
            oneof c {
              uint32  a1 = 1;
              fixed32 a2 = 2;
            }
          }
          """,
          [{msg_format, native_records}]),
    R = records:create(M, m1, [{c, {a1, 11}}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    ok = M:verify_msg(R),
    ?assertError({gpb_type_error, _}, M:verify_msg(R#_{c = x})),
    ?assertError({gpb_type_error, _}, M:verify_msg(R#_{c = {a_no, 12}})),
    unload_code(M).

mapfield_test() ->
    M = compile_iolist(
          """
          syntax="proto2";
          message m1 {
            map<uint32, string> f = 1;
          }
          """,
          [{msg_format, native_records}]),
    R = records:create(M, m1, [{f, #{1 => "one", 2 => "two"}}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    ok = M:verify_msg(R),
    unload_code(M).

unset_value_test() ->
    Unset = uuu_uuu,
    M = compile_iolist(
          """
          syntax="proto2";
          message m1 {
            optional string o = 1;
            required uint32 r = 2;
          }
          """,
          [{msg_format, native_records},
           {native_records_unset, Unset}]),
    R1 = records:create(M, m1, [{o, Unset}, {r, 33}],
                        #{is_exported => true}),
    R2 = records:create(M, m1, [{o, "abc"}, {r, 33}],
                        #{is_exported => true}),
    Data1 = M:encode_msg(R1),
    Data2 = M:encode_msg(R2),
    ?assertNotEqual(R1, R2, #{r1 => R1, r2 => R2}),
    ?assert(byte_size(Data1) < byte_size(Data2),
            #{r1 => R1, r2 => R2, data1 => Data1, data2 => Data2}),
    R1 = M:decode_msg(Data1, m1),
    R2 = M:decode_msg(Data2, m1),
    %% On decoding, if no value is included, even for required fields,
    %% it should decode to the undefined value. Also for pass_as_record,
    %% we'd need to be able to have it set to the undefined value.
    #_{o=Unset, r=Unset} = M:decode_msg(<<>>, m1),
    unload_code(M).

pass_as_records_test() ->
    M = compile_iolist(
          """
          syntax="proto2";
          message m1 {
            optional string o = 1;
            required uint32 r = 2;
          }
          """,
          [{msg_format, native_records},
           {field_pass_method, pass_as_record}]),
    R = records:create(M, m1, [{o, undefined}, {r, 44}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    #_{o=undefined, r=undefined} = M:decode_msg(<<>>, m1),
    unload_code(M).


default_value_handling_test() ->
    Proto = """
            message m {
              optional uint32 f1 = 1;
              optional uint32 f2 = 2 [default=2];
            }
            """,
    FieldNames = [f1,f2],
    [begin
         AllOpts = Opts ++ OptVariation ++ [{msg_format, native_records}],
         M = compile_iolist(Proto, AllOpts),
         FVs = lists:zip(FieldNames, tl(tuple_to_list(BaseExpected))),
         Expected = records:create(M, m, FVs, #{is_exported => true}),
         ?assertMatch(Expected, M:decode_msg(<<>>, m),
                      #{opts => Opts,
                        expected => Expected}),
         unload_code(M)
     end
     || {BaseExpected, Opts} <-
            [{{m,undefined,undefined}, []},
             {{m,0,2},         [defaults_for_omitted_optionals,
                                type_defaults_for_omitted_optionals]},
             {{m,undefined,2}, [defaults_for_omitted_optionals]},
             {{m,0,0},         [type_defaults_for_omitted_optionals]}],
        OptVariation <- [[{field_pass_method, pass_as_params}],
                         [{field_pass_method, pass_as_record}]]].

type_default_option_should_be_ignored_for_proto3_test() ->
    Proto = """
            syntax="proto3";
            message m { uint32 f1 = 1; };
            """,
    M = compile_iolist(Proto, [{type_defaults_for_omitted_optionals, false}]),
    {m,0} = M:decode_msg(<<>>, m),
    unload_code(M).

exports_records_in_erl_test() ->
    Proto = """
            syntax="proto2";
            message m { optional uint32 f1 = 1; };
            """,
    #{".erl" := Erl} = filename_extensions(
                         compile_proto_get_written_files(
                           mod,
                           Proto,
                           [{msg_format, native_records}])),
    assert_regexp_present_in("-record #m", Erl),
    assert_regexp_present_in("-export_record.*\\bm\\b", Erl),
    ok.

default_values_for_records_fields_test() ->
    Proto = """
            syntax="proto2";
            message m {
              optional uint32 f1 = 1;
              oneof ch {
                uint32 f2 = 2;
              }
            };
            """,
    #{".erl" := Erl} = filename_extensions(
                         compile_proto_get_written_files(
                           mod,
                           Proto,
                           [{msg_format, native_records}])),
    assert_regexp_present_in("f1 = undefined", Erl),
    assert_regexp_present_in("ch = undefined", Erl),
    ok.

modified_unset_value_for_records_fields_test() ->
    Proto = """
            syntax="proto2";
            message m {
              optional uint32 f1 = 1;
              oneof ch {
                uint32 f2 = 2;
                uint32 f3 = 3;
              }
            };
            """,
    #{".erl" := Erl} = filename_extensions(
                         compile_proto_get_written_files(
                           mod,
                           Proto,
                           [{msg_format, native_records},
                            {native_records_unset, uuu_uuu}])),
    assert_regexp_present_in("f1 = uuu_uuu", Erl),
    assert_regexp_present_in("ch = uuu_uuu", Erl),
    ok.

modified_unset_value_decoding_with_oneof_test() ->
    Proto = """
            syntax="proto2";
            message m1 {
              optional uint32 f1 = 1;
              oneof c {
                uint32 a2 = 2;
                uint32 a3 = 3;
              }
            };
            """,
    [begin
         M = compile_iolist(Proto,
                            [{msg_format, native_records},
                             {native_records_unset, '$undef'},
                             FieldPassOpt]),
         R = records:create(M, m1, [{f1, 1}, {c, {a2, 11}}],
                            #{is_exported => true}),
         Data = M:encode_msg(R),
         R = M:decode_msg(Data, m1),
         #_{f1='$undef', c='$undef'} = M:decode_msg(<<>>, m1),
         unload_code(M)
     end
     || FieldPassOpt <- [{field_pass_method, pass_as_params},
                         {field_pass_method, pass_as_record}]],
    ok.

imports_records_in_hrl_test() ->
    Proto = """
            syntax="proto2";
            message m { optional uint32 f1 = 1; };
            """,
    #{".hrl" := Hrl} = filename_extensions(
                         compile_proto_get_written_files(
                           mod,
                           Proto,
                           [{msg_format, native_records}])),
    assert_regexp_present_in("-import_record.*mod(.|\\n)*\\bm\\b", Hrl),
    ok.

error_if_both_native_records_and_nif_test() ->
    Opts = [{msg_format, native_records}, nif],
    {error, {invalid_options, _, _}} = Error =
        gpb_compile:string(dummy, "message dummy { }", Opts),
    Txt = lists:flatten(gpb_compile:format_error(Error)),
    assert_regexp_present_in("native.*records", Txt),
    assert_regexp_present_in("nif", Txt),
    ok.

no_defaults_for_required_fields_w_repeated_test() ->
    Proto = """
            syntax="proto2";
            message m1 {
              required uint32 f1 = 1;
              repeated uint32 f2 = 2;
            };
            """,
    M = compile_iolist(Proto,
                       [{msg_format, native_records},
                        {native_records_required_default, none},
                        {field_pass_method, pass_as_record}]),
    R = records:create(M, m1, [{f1, 1}, {f2, [44]}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    %% missing required field:
    ?assertError({gpb_error, _}, M:decode_msg(<<>>, m1)),
    unload_code(M).

no_defaults_for_required_fields_w_submsgs_test() ->
    Proto = """
            syntax="proto2";
            message m1 {
              required uint32 f1 = 1;
              optional submsg f2 = 2;
            };
            message submsg {};
            """,
    M = compile_iolist(Proto,
                       [{msg_format, native_records},
                        {native_records_required_default, none},
                        {field_pass_method, pass_as_record}]),
    SubMsg = records:create(M, submsg, [], #{is_exported => true}),
    R = records:create(M, m1, [{f1, 1}, {f2, SubMsg}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    %% missing required field:
    ?assertError({gpb_error, _}, M:decode_msg(<<>>, m1)),
    unload_code(M).

no_defaults_for_required_fields_w_oneof_test() ->
    Proto = """
            syntax="proto2";
            message m1 {
              required uint32 f1 = 1;
              oneof c {
                uint32 a2 = 2;
                uint32 a3 = 3;
              }
            };
            """,
    M = compile_iolist(Proto,
                       [{msg_format, native_records},
                        {native_records_required_default, none},
                        {field_pass_method, pass_as_record}]),
    R = records:create(M, m1, [{f1, 1}, {c, {a2, 2}}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    %% missing required field:
    ?assertError({gpb_error, _}, M:decode_msg(<<>>, m1)),
    unload_code(M).

no_defaults_for_required_fields_w_oneof_submsg_test() ->
    Proto = """
            syntax="proto2";
            message m1 {
              required uint32 f1 = 1;
              oneof c {
                submsg a2 = 2;
                submsg a3 = 3;
              }
            };
            message submsg {};
            """,
    M = compile_iolist(Proto,
                       [{msg_format, native_records},
                        {native_records_required_default, none},
                        {field_pass_method, pass_as_record}]),
    SubMsg = records:create(M, submsg, [], #{is_exported => true}),
    R = records:create(M, m1, [{f1, 1}, {c, {a2, SubMsg}}],
                       #{is_exported => true}),
    Data = M:encode_msg(R),
    R = M:decode_msg(Data, m1),
    %% missing required field:
    ?assertError({gpb_error, _}, M:decode_msg(<<>>, m1)),
    unload_code(M).

no_defaults_with_modified_unset_value_decoding_with_oneof_test() ->
    Proto = """
            syntax="proto2";
            message m1 {
              required uint32 f1 = 1;
              oneof c {
                uint32 a2 = 2;
                uint32 a3 = 3;
              }
            };
            """,
    [begin
         M = compile_iolist(Proto,
                            [{msg_format, native_records},
                             {native_records_required_default, none},
                             {native_records_unset, '$undef'},
                             FieldPassOpt]),
         R = records:create(M, m1, [{f1, 1}, {c, {a2, 11}}],
                            #{is_exported => true}),
         Data = M:encode_msg(R),
         R = M:decode_msg(Data, m1),
         R2 = records:create(M, m1, [{f1, 11}, {c, '$undef'}],
                            #{is_exported => true}),
         Data2 = M:encode_msg(R2),
         #_{f1=11, c='$undef'} = M:decode_msg(Data2, m1),

         unload_code(M)
     end
     || FieldPassOpt <- [{field_pass_method, pass_as_params},
                         {field_pass_method, pass_as_record}]],
    ok.

%% ----------------------------------------------------------------------

compile_proto_get_written_files(Mod, Proto, Opts) ->
    Tc = self(),
    ok = gpb_compile:string(
           Mod,
           Proto,
           Opts ++ [{file_op,
                     [{write_file, fun(FName, Data) ->
                                           Tc ! {file_write, FName, Data},
                                           ok
                                   end}]}]),
    maps:from_list(collect_files_written()).

filename_extensions(Collected) ->
    maps:fold(
      fun(FileName, Data, Acc) ->
              Acc#{filename:extension(FileName) => Data}
      end,
      #{},
      Collected).

assert_regexp_present_in(Re, Str) ->
    case re:run(Str, Re) of
        nomatch ->
            io:format("--vv---- Str ----------------~n"
                      "~s~n"
                      "--^^-------------------------~n",
                      [Str]),
            error({re_not_present, Re, Str});
        {match, _} ->
            ok
    end.

collect_files_written() ->
    receive
        {file_write, FName, Data} ->
            [{FName, Data} | collect_files_written()]
    after 10 ->
            []
    end.

-endif. %% NO_HAVE_NATIVE_RECORDS
