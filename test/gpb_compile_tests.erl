%%% Copyright (C) 2010-2011  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-module(gpb_compile_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-export([test_nifs/1]). %% set whether to test nifs or not

%% Include a bunch of tests from gpb_tests.
%% The shared tests are for stuff that must work both
%% for gpb and for the code that gpb_compile generates.
%% (I know it is a bit unorthodox to include .erl files,
%% but actually seems to work. Better than duplicating tests)
-define(gpb_compile_common_tests, true).
-ifdef(gpb_compile_common_tests).
-include("gpb_tests.erl").
-else.  %% gpb_compile_common_tests
-record(m1,{a}).
-endif. %% gpb_compile_common_tests

parses_non_importing_file_test() ->
    Contents = iolist_to_binary(
                 ["message Msg { required uint32 field1 = 1; }\n"]),
    ok = gpb_compile:file(
           "X.proto",
           [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
            mk_defs_probe_sender_opt(self()),
            {i,"."}]),
    [{{msg,'Msg'},_}] = receive_filter_sort_msgs_defs().


parses_importing_file_test() ->
    ContentsX = iolist_to_binary(
                  ["import \"Y.proto\";\n"
                   "message X { required Y f1 = 1; }\n"]),
    ContentsY = iolist_to_binary(
                  ["message Y { required uint32 f1 = 1; }\n"]),
    ok = gpb_compile:file(
           "X.proto",
           [mk_fileop_opt([{read_file, fun("X.proto") -> {ok, ContentsX};
                                          ("Y.proto") -> {ok, ContentsY}
                                       end}]),
            mk_defs_probe_sender_opt(self()),
            {i,"."}]),
    [{{msg,'X'},_}, {{msg,'Y'},_}] = receive_filter_sort_msgs_defs().


parses_file_to_binary_test() ->
    Contents = <<"message Msg { required uint32 field1 = 1; }\n">>,
    {ok, 'X', Code, []} =
        gpb_compile:file(
          "X.proto",
          [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
           mk_defs_probe_sender_opt(self()),
           {i,"."},
           binary, return_warnings]),
    true = is_binary(Code),
    [{{msg,'Msg'},_}] = receive_filter_sort_msgs_defs().

parses_file_to_msg_defs_test() ->
    Contents = <<"message Msg { required uint32 field1 = 1; }\n">>,
    {ok, [{{msg,'Msg'},[#field{}]}]=MsgDefs} =
        gpb_compile:file(
          "X.proto",
          [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
           {i,"."},
           to_msg_defs, report_warnings]),
    %% Check that the returned msgdefs are usable
    M = compile_defs(MsgDefs),
    ?assertMatch(<<_/binary>>, M:encode_msg({'Msg',33})),
    unload_code(M).

parses_msgdefs_to_binary_test() ->
    Defs = [{{msg,'Msg'},
             [#field{name=field1, rnum=2, fnum=1, type=uint32,
                     occurrence=required, opts=[]}]}],
    M = find_unused_module(),
    {ok, M, Code} = gpb_compile:msg_defs(M, Defs, [binary]),
    true = is_binary(Code).

parses_and_generates_good_code_also_for_reserved_keywords_test() ->
    %% use erlang reserved words on as many .proto locations as possible
    %% to verify that the generated code compiles and works.
    M = compile_iolist(["enum if { begin=1; end=2; }"
                        "message catch { required if case = 1; }\n"]),
    ?assertMatch(true, is_binary(M:encode_msg({'catch', 'begin'}))),
    unload_code(M).

parses_and_generates_good_code_also_for_empty_msgs_test() ->
    M = compile_iolist(["message m1 { }\n"]),
    ?assertMatch(true, is_binary(M:encode_msg({m1}))),
    ?assertMatch({m1}, M:decode_msg(M:encode_msg({m1}), m1)),
    unload_code(M).

field_pass_as_params_test() ->
    MsgDef = ["message m2 { required uint32 f22 = 1; }"
              "message m1 { required uint32  f1 = 1;",
              "             optional fixed32 f2 = 2;",
              "             repeated fixed32 f3 = 3;",
              "             repeated fixed32 f4 = 4 [packed];",
              "             repeated uint32  f5 = 5;",
              "             repeated uint32  f6 = 5 [packed];",
              "             optional string  f7 = 7;"
              "             optional m2      f8 = 8; }"],
    Msg = {m1, 4711, undefined,      %% f1,f2
           [4713,4714], [4715,4716], %% f3,f4
           [4717,4718], [4719,4720], %% f5,f6
           "abc", {m2,33}},
    lists:foreach(
      fun(Opts) ->
              ?assertMatch({Msg,_},
                           {encode_decode_round_trip(MsgDef, Msg, Opts), Opts})
      end,
      [[{field_pass_method,pass_as_params}],
       [{field_pass_method,pass_as_record}],
       [{{field_pass_method,m1},pass_as_record},
        {{field_pass_method,m2},pass_as_params}],
       [{{field_pass_method,m1},pass_as_params},
        {{field_pass_method,m2},pass_as_record}]]).

encode_decode_round_trip(MsgDefAsIoList, Msg, Opts) ->
    M = compile_iolist(MsgDefAsIoList, Opts),
    MsgName = element(1, Msg),
    Result = M:decode_msg(M:encode_msg(Msg), MsgName),
    unload_code(M),
    Result.

mk_fileop_opt(NonDefaults) ->
    {file_op,
     fun(read_file_info, [FileName]) ->
             case proplists:get_value(read_file_info, NonDefaults, '$no') of
                 '$no' -> {ok, #file_info{access=read}};
                 Fn    -> Fn(filename:basename(FileName))
             end;
        (read_file, [FileName]) ->
             case proplists:get_value(read_file, NonDefaults, '$no') of
                 '$no' -> {error, enoent};
                 Fn    -> Fn(filename:basename(FileName))
             end;
        (write_file, [FileName, Bin]) ->
             case proplists:get_value(write_file, NonDefaults, '$no') of
                 '$no' -> ok;
                 Fn    -> Fn(filename:basename(FileName), Bin)
             end
     end}.

mk_defs_probe_sender_opt(SendTo) ->
    {probe_defs, fun(Defs) -> SendTo ! {defs, Defs} end}.

receive_filter_sort_msgs_defs() ->
    lists:sort([Msg || {{msg,_},_} = Msg <- receive {defs, Defs} -> Defs end]).

-record(m9,{aa, bb, cc, dd}).
-record(m10,{aaa}).

code_generation_when_submsg_size_is_known_at_compile_time_test() ->
    KnownSizeM9 =
        [{{msg,m9}, [#field{name=aa, type={enum,e}, occurrence=required,
                            fnum=1, rnum=#m9.aa, opts=[]},
                     #field{name=bb, type=fixed32, occurrence=required,
                            fnum=2, rnum=#m9.bb, opts=[]},
                     #field{name=cc, type=fixed64, occurrence=required,
                            fnum=3, rnum=#m9.cc, opts=[]},
                     #field{name=dd, type={msg,m10}, occurrence=required,
                            fnum=4, rnum=#m9.dd, opts=[]}]}],
    UnknownSizeM9 =
        [{{msg,m9}, [#field{name=aa, type={enum,e}, occurrence=optional,
                            fnum=1, rnum=#m9.aa, opts=[]},
                     #field{name=bb, type=fixed32, occurrence=optional,
                            fnum=2, rnum=#m9.bb, opts=[]},
                     #field{name=cc, type=fixed64, occurrence=optional,
                            fnum=3, rnum=#m9.cc, opts=[]},
                     #field{name=dd, type={msg,m10}, occurrence=required,
                            fnum=4, rnum=#m9.dd, opts=[]}]}],

    CommonDefs =
        [{{msg,m1}, [#field{name=a, type={msg,m9}, occurrence=required,
                            fnum=1, rnum=2, opts=[]}]},
         {{msg,m10},[#field{name=aaa, type=bool, occurrence=required,
                            fnum=1, rnum=#m10.aaa, opts=[]}]},
         {{enum,e}, [{x1, 1}, {x2, 2}]} %% all enum values same encode size
        ],

    M1 = compile_defs(CommonDefs++KnownSizeM9),
    M2 = compile_defs(CommonDefs++UnknownSizeM9),
    Msg = #m1{a=#m9{aa=x1, bb=33, cc=44, dd=#m10{aaa=true}}},
    Encoded1 = M1:encode_msg(Msg),
    Encoded2 = M2:encode_msg(Msg),
    Encoded1 = Encoded2,
    unload_code(M1),
    unload_code(M2).

%% --- introspection ---------------

introspection_package_name_test() ->
    M = compile_iolist(["package foo.bar;",
                        "message M { required uint32 f1=1; }"]),
    'foo.bar' = M:get_package_name(),
    unload_code(M),
    M = compile_iolist(["message M { required uint32 f1=1; }"]),
    undefined = M:get_package_name(),
    unload_code(M).

introspection_msgs_test() ->
    M = compile_iolist(["message msg1 { required uint32 f1=1; }"]),
    [msg1] = M:get_msg_names(),
    [#field{name=f1, type=uint32, fnum=1}] = M:find_msg_def(msg1),
    [#field{name=f1, type=uint32, fnum=1}] = M:fetch_msg_def(msg1),
    error = M:find_msg_def(msg_ee),
    ?assertError(_, M:fetch_msg_def(msg_ee)),
    unload_code(M).

introspection_enums_test() ->
    %% Names
    M = compile_iolist(["enum e1 { n1=1; n2=2; }",
                        "message msg1 { required uint32 f1=1; }"]),
    [e1] = M:get_enum_names(),
    %% find and fetch
    [{n1,1},{n2,2}] = M:find_enum_def(e1),
    error = M:find_enum_def(ee),
    [{n1,1},{n2,2}] = M:fetch_enum_def(e1),
    ?assertError(_, M:fetch_enum_def(ee)),
    %% symbol <--> value mapping
    n1 = M:enum_symbol_by_value(e1, 1),
    n2 = M:enum_symbol_by_value(e1, 2),
    n1 = M:enum_symbol_by_value_e1(1),
    n2 = M:enum_symbol_by_value_e1(2),
    1  = M:enum_value_by_symbol(e1, n1),
    2  = M:enum_value_by_symbol(e1, n2),
    1  = M:enum_value_by_symbol_e1(n1),
    2  = M:enum_value_by_symbol_e1(n2),
    unload_code(M).

introspection_defs_as_proplists_test() ->
    Proto = ["message msg1 { required uint32 f1=1; }"],
    %% With the defs_as_proplists option
    M = compile_iolist(Proto, [defs_as_proplists]),
    [[{name,       f1},
      {fnum,       1},
      {rnum,       2},
      {type,       uint32},
      {occurrence, required},
      {opts,       []}]] = PL = M:find_msg_def(msg1),
    [{{msg, msg1}, PL}] = M:get_msg_defs(),
    unload_code(M),

    %% No defs_as_proplists option
    M = compile_iolist(Proto, [{defs_as_proplists, false}]),
    [#field{name       = f1,
            fnum       = 1,
            rnum       = 2,
            type       = uint32,
            occurrence = required,
            opts       = []}] = Fs = M:find_msg_def(msg1),
    [{{msg, msg1}, Fs}] = Defs = M:get_msg_defs(),
    unload_code(M),

    %% make sure the generated erl file does not -include[_lib] "gpb.hrl"
    Master = self(),
    ReportOutput = fun(FName, Contents) ->
                           Master ! {filename:extension(FName), Contents},
                           ok
                   end,
    FileOpOpt = mk_fileop_opt([{write_file, ReportOutput}]),
    ok = gpb_compile:msg_defs(M, Defs, [FileOpOpt, defs_as_proplists]),
    receive {".hrl", Hrl1} -> nomatch = re:run(Hrl1, "\"gpb.hrl\"") end,
    receive {".erl", Erl1} -> nomatch = re:run(Erl1, "\"gpb.hrl\"") end,
    ok = gpb_compile:msg_defs(M, Defs, [FileOpOpt, defs_as_proplists,
                                        include_as_lib]),
    receive {".hrl", Hrl2} -> nomatch = re:run(Hrl2, "\"gpb.hrl\"") end,
    receive {".erl", Erl2} -> nomatch = re:run(Erl2, "\"gpb.hrl\"") end.

%% --- decoder tests ---------------

decodes_overly_long_varints_test() ->
    M = compile_defs([{{msg,m1}, [#field{name=a, type=int32, fnum=1, rnum=#m1.a,
                                         occurrence=required, opts=[]}]}]),
    #m1{a=54} = M:decode_msg(<<8, 54>>, m1), %% canonically encoded
    #m1{a=54} = M:decode_msg(<<8, (128+54), 128, 128, 0>>, m1),
    unload_code(M).

%% --- scoped messages ---------------

dotted_names_gives_no_compilation_error_test() ->
    %% make sure dotted names does not give compilation errors,
    %% for instance if some generated code would rely on names
    %% having the same syntax as erlang atoms, or, when prepended
    %% with an upper case character, having the same syntax as an
    %% erlang variable
    M = compile_iolist(["message m1 {"
                        "  message m2 { required uint32 x = 1; }",
                        "  enum    e1 { ea = 17; eb = 18; }",
                        "  required m2     y = 1;",
                        "  required .m1.m2 z = 2;",
                        "  required e1     w = 3;",
                        "}",
                        "message m3 { required m1.m2 b = 1; }"]),
    M1Msg = {m1, {'m1.m2', 1}, {'m1.m2', 2}, ea},
    Data = M:encode_msg(M1Msg),
    M1Msg = M:decode_msg(Data, m1),
    unload_code(M).

%% --- bytes ----------

copy_bytes_unconditionally_test() ->
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                               [{copy_bytes, true}]),
            Data = M:encode_msg({m1, <<"d">>}),
            {m1, <<"d">>=Bs} = M:decode_msg(Data, m1),
            %% If the Bs has not been copied, then it is a sub-binary
            %% of a larger binary: of the message, ie of Data.
            %% So verify copying by verifying size of referenced data.
            ?assertEqual(byte_size(Bs), binary:referenced_byte_size(Bs)),
            unload_code(M);
       true ->
            %% nothing to test
            ok
    end.

copy_bytes_false_test() ->
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, false}]),
    Data = M:encode_msg({m1, <<"d">>}),
    {m1, <<"d">>=Bs} = M:decode_msg(Data, m1),
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            %% If the StrBin has not been copied, then it is a sub-binary
            %% of a larger binary: of the message, ie of Data.
            %% So verify copying by verifying size of referenced data.
            ?assertEqual(byte_size(Data), binary:referenced_byte_size(Bs));
       true ->
            ok
    end,
    unload_code(M).

copy_bytes_auto_test() ->
    M = compile_iolist(["message m1 { required bytes f1 = 1; }"],
                       [{copy_bytes, auto}]),
    Data = M:encode_msg({m1, <<"d">>}),
    {m1, <<"d">>=Bs} = M:decode_msg(Data, m1),
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    if HasBinary ->
            ?assertEqual(byte_size(Bs), binary:referenced_byte_size(Bs));
       true ->
            ok %% cannot test more if we don't have the binary module
    end,
    unload_code(M).

copy_bytes_fraction_test() ->
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    if HasBinary ->
            Proto = ["message m1 {",
                     "  required bytes f1 = 1;",
                     "  required bytes f2 = 2;",
                     "}"],
            M1 = compile_iolist(Proto, [{copy_bytes, 2}]),   % fraction as int
            M2 = compile_iolist(Proto, [{copy_bytes, 2.0}]), % fraction as float
            D1 = <<"d">>, %% small
            D2 = <<"dddddddddddddddddddddddddddd">>, %% large
            Data = M1:encode_msg({m1, D1, D2}),
            ?assert(byte_size(Data) > (2 * byte_size(D1))),
            ?assert(byte_size(Data) < (2 * byte_size(D2))),
            {m1, D1Bs, D2Bs} = M1:decode_msg(Data, m1),
            ?assertEqual(D1, D1Bs),
            ?assertEqual(D2, D2Bs),
            %% The small data should have been copied, but not the larger
            ?assertEqual(byte_size(D1Bs), binary:referenced_byte_size(D1Bs)),
            ?assertEqual(byte_size(Data), binary:referenced_byte_size(D2Bs)),

            {m1, D3Bs, D4Bs} = M2:decode_msg(Data, m1),
            ?assertEqual(D1, D3Bs),
            ?assertEqual(D2, D4Bs),
            ?assertEqual(byte_size(D3Bs), binary:referenced_byte_size(D3Bs)),
            ?assertEqual(byte_size(Data), binary:referenced_byte_size(D4Bs)),

            unload_code(M1),
            unload_code(M2);
       true ->
            ok
    end.

%% --- strings ----------

strings_as_binaries_option_produces_bins_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "}"],
                       [strings_as_binaries]),
    Data = M:encode_msg({m1, "some string"}),
    {m1, <<"some string">>} = M:decode_msg(Data, m1),
    unload_code(M).

strings_as_lists_is_the_default_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "}"],
                       []),
    Data = M:encode_msg({m1, "some string"}),
    {m1, "some string"} = M:decode_msg(Data, m1),
    unload_code(M).

strings_as_binaries_opt_together_with_copy_bytes_opt_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "}"],
                       [strings_as_binaries, {copy_bytes, auto}]),
    Data = M:encode_msg({m1, "some string"}),
    {m1, <<"some string">>=StrBin} = M:decode_msg(Data, m1),
    HasBinary = (catch binary:copy(<<1>>)) == <<1>>, % binary exists since R14A
    if HasBinary ->
            ?assertEqual(byte_size(StrBin),
                         binary:referenced_byte_size(StrBin));
       true ->
            ok
    end,
    unload_code(M).

accepts_both_strings_and_binaries_as_input_test() ->
    M = compile_iolist(["message m1 {"
                        "  required string f1 = 1;",
                        "  required string f2 = 2;",
                        "}"]),
    Data = M:encode_msg({m1, "some string", <<"some other string">>}),
    {m1, "some string", "some other string"} = M:decode_msg(Data, m1),
    unload_code(M).


%% --- Returning/reporting warnings/errors tests ----------
%% ... when compiling to file/binary
%% ... when there are/aren't warnings/errors

report_or_return_warnings_or_errors_test_() ->
    %% Without increased timeout, this test sometimes times
    %% out on my slow machine (1.6 GHz Atom N270)
    {timeout,58,fun report_or_return_warnings_or_errors_test_aux/0}.

report_or_return_warnings_or_errors_test_aux() ->
    [rwre_go(Options, CompileTo, SrcType, SrcQuality)
     || Options    <- [[report_warnings, report_errors],
                       [report_warnings],
                       [report_errors],
                       [return_warnings, return_errors],
                       [return_warnings],
                       [return_errors],
                       [report_warnings, return_errors],
                       [return_warnings, report_errors],
                       []
                      ],
        CompileTo  <- [to_binary, to_file, to_msg_defs],
        SrcType    <- [from_file, from_defs],
        SrcQuality <- [clean_code, warningful_code, erroneous_code,
                       write_fails],
        %% Exclude a few combos
        not (SrcQuality == erroneous_code andalso SrcType == from_defs),
        not (SrcQuality == write_fails andalso CompileTo == to_binary),
        not (SrcQuality == write_fails andalso CompileTo == to_msg_defs)].

rwre_go(Options, CompileTo, SrcType, SrcQuality) ->
    ExpectedReturn = compute_expected_return(Options, CompileTo, SrcQuality),
    ExpectedOutput = compute_expected_output(Options, SrcQuality),
    {{return,Returned},
     {output,Output}} = compile_the_code(Options, CompileTo,
                                         SrcType, SrcQuality),
    eval_return(ExpectedReturn, Returned, Output,
                Options, CompileTo, SrcType, SrcQuality),
    eval_output(ExpectedOutput, Output, Returned,
                Options, CompileTo, SrcType, SrcQuality),
    ok.

compute_expected_return(Options, CompileTo, write_fails) ->
    compute_expected_return(Options, CompileTo, erroneous_code);
compute_expected_return(Options, to_file, SrcQuality) ->
    WarnOpt = get_warning_opt(Options),
    ErrOpt = get_error_opt(Options),
    case {WarnOpt, ErrOpt, SrcQuality} of
        {report, report, clean_code}      -> ok;
        {report, report, warningful_code} -> ok;
        {report, report, erroneous_code}  -> {error, '_'};
        {return, return, clean_code}      -> {ok, []};
        {return, return, warningful_code} -> {ok, non_empty_list};
        {return, return, erroneous_code}  -> {error, '_', []};
        {report, return, clean_code}      -> ok;
        {report, return, warningful_code} -> ok;
        {report, return, erroneous_code}  -> {error, '_'};
        {return, report, clean_code}      -> {ok, []};
        {return, report, warningful_code} -> {ok, non_empty_list};
        {return, report, erroneous_code}  -> {error, '_', []}
    end;
compute_expected_return(Options, to_binary, SrcQuality) ->
    WarnOpt = get_warning_opt(Options),
    ErrOpt = get_error_opt(Options),
    case {WarnOpt, ErrOpt, SrcQuality} of
        {report, report, clean_code}      -> {ok, mod, binary};
        {report, report, warningful_code} -> {ok, mod, binary};
        {report, report, erroneous_code}  -> {error, '_'};
        {return, return, clean_code}      -> {ok, mod, binary, []};
        {return, return, warningful_code} -> {ok, mod, binary, non_empty_list};
        {return, return, erroneous_code}  -> {error, '_', []};
        {report, return, clean_code}      -> {ok, mod, binary};
        {report, return, warningful_code} -> {ok, mod, binary};
        {report, return, erroneous_code}  -> {error, '_'};
        {return, report, clean_code}      -> {ok, mod, binary, []};
        {return, report, warningful_code} -> {ok, mod, binary, non_empty_list};
        {return, report, erroneous_code}  -> {error, '_', []}
    end;
compute_expected_return(Options, to_msg_defs, SrcQuality) ->
    WarnOpt = get_warning_opt(Options),
    ErrOpt = get_error_opt(Options),
    case {WarnOpt, ErrOpt, SrcQuality} of
        {report, report, clean_code}      -> {ok, non_empty_list};
        {report, report, warningful_code} -> {ok, non_empty_list};
        {report, report, erroneous_code}  -> {error, '_'};
        {return, return, clean_code}      -> {ok, non_empty_list, []};
        {return, return, warningful_code} -> {ok, non_empty_list,non_empty_list};
        {return, return, erroneous_code}  -> {error, '_', []};
        {report, return, clean_code}      -> {ok, non_empty_list};
        {report, return, warningful_code} -> {ok, non_empty_list};
        {report, return, erroneous_code}  -> {error, '_'};
        {return, report, clean_code}      -> {ok, non_empty_list, []};
        {return, report, warningful_code} -> {ok, non_empty_list,non_empty_list};
        {return, report, erroneous_code}  -> {error, '_', []}
    end.

compute_expected_output(_Options, clean_code) ->
    "";
compute_expected_output(Options, warningful_code) ->
    WarnOpt = get_warning_opt(Options),
    ErrOpt = get_error_opt(Options),
    case {WarnOpt, ErrOpt} of
        {report, report} -> non_empty_list;
        {report, return} -> non_empty_list;
        {return, report} -> "";
        {return, return} -> ""
    end;
compute_expected_output(Options, write_fails) ->
    compute_expected_output(Options, erroneous_code);
compute_expected_output(Options, erroneous_code) ->
    WarnOpt = get_warning_opt(Options),
    ErrOpt = get_error_opt(Options),
    case {WarnOpt, ErrOpt} of
        {report, report} -> non_empty_list;
        {report, return} -> "";
        {return, report} -> non_empty_list;
        {return, return} -> ""
    end.

get_warning_opt(Opts) ->
    case {member(return_warnings, Opts), member(report_warnings, Opts)} of
        {false, false} -> report; %% default
        {true,  false} -> return;
        {false,  true} -> report
    end.

get_error_opt(Opts) ->
    case {member(return_errors, Opts), member(report_errors, Opts)} of
        {false, false} -> report; %% default
        {true,  false} -> return;
        {false,  true} -> report
    end.

member(Elem, List) ->
    lists:member(Elem, List).

compile_the_code(Options, CompileTo, from_defs, SrcQuality) ->
    compile_msg_defs_get_output(get_proto_defs(SrcQuality),
                                compute_compile_opts(Options, CompileTo,
                                                     SrcQuality));
compile_the_code(Options, CompileTo, from_file, SrcQuality) ->
    compile_file_get_output(get_proto_file(SrcQuality),
                            compute_compile_opts(Options, CompileTo,
                                                 SrcQuality)).

get_proto_defs(clean_code) ->
    [{{msg,m1}, [#field{name=field11, type=uint32, occurrence=optional,
                        fnum=1, rnum=2, opts=[]}]}];
get_proto_defs(warningful_code) ->
    %% circular msg definitions ==> warning about omitting type specs
    [{{msg,m1}, [#field{name=field11, type={msg,m2}, occurrence=optional,
                        fnum=1, rnum=2, opts=[]}]},
     {{msg,m2}, [#field{name=field22, type={msg,m1}, occurrence=optional,
                        fnum=2, rnum=2, opts=[]}]}];
get_proto_defs(write_fails) ->
    get_proto_defs(clean_code).

get_proto_file(clean_code) ->
    "message m1 { optional uint32 field11 = 1; }\n";
get_proto_file(warningful_code) ->
    %% circular msg definitions ==> warning about omitting type specs
    ["message m1 { optional m2 field11 = 1; }\n"
     "message m2 { optional m1 field22 = 2; }\n"];
get_proto_file(erroneous_code) ->
    "g&~#";
get_proto_file(write_fails) ->
    get_proto_file(clean_code).

compute_compile_opts(Options, CompileTo, write_fails) ->
    compute_compile_opts_2(Options, CompileTo) ++ mk_failing_write_option();
compute_compile_opts(Options, CompileTo, _SrcQuality) ->
    compute_compile_opts_2(Options, CompileTo).

compute_compile_opts_2(Opts, to_binary)   -> [binary, type_specs | Opts];
compute_compile_opts_2(Opts, to_msg_defs) -> [to_msg_defs, type_specs | Opts];
compute_compile_opts_2(Opts, to_file)     -> [type_specs | Opts].

mk_failing_write_option() ->
    [fail_write].

compile_msg_defs_get_output(MsgDefs, Opts) ->
    Opts2 = case lists:member(fail_write, Opts) of
                false ->
                    Opts;
                true ->
                    RestOpts = Opts -- [fail_write],
                    FOpt = mk_fileop_opt([{write_file,fun(_,_) -> {error,eacces}
                                                      end}]),
                    [FOpt | RestOpts]
            end,
    capture_stdout(fun() -> gpb_compile:msg_defs('x', MsgDefs, Opts2) end).

compile_file_get_output(Txt, Opts) ->
    Contents = iolist_to_binary(Txt),
    FailWrite = lists:member(fail_write, Opts),
    RestOpts = Opts -- [fail_write],
    FileOpOpts = if FailWrite -> mk_fileop_opt(
                                   [{read_file, fun(_) -> {ok, Contents} end},
                                    {write_file,fun(_, _) -> {error,eacces} end}
                                   ]);
                    true -> mk_fileop_opt(
                              [{read_file, fun(_) -> {ok, Contents} end}])
                 end,
    capture_stdout(
      fun() ->
              gpb_compile:file("X.proto", [FileOpOpts, {i,"."} | RestOpts])
      end).

eval_return(Expected, Actual, Output,
            Options, CompileTo, SrcType, SrcQuality) ->
    case match_values(Expected, Actual) of
        true ->
            ok;
        false ->
            erlang:error({bad_return,Expected,Actual,
                          [{output,Output},
                           {setup,{Options, CompileTo,
                                   SrcType, SrcQuality}}]})
    end.

eval_output(Expected, Actual, Returned,
            Options, CompileTo, SrcType, SrcQuality) ->
    case match_value(Expected, Actual) of
        true ->
            ok;
        false ->
            erlang:error({bad_output,Expected,Actual,
                          [{returned,Returned},
                           {setup,{Options, CompileTo,
                                   SrcType, SrcQuality}}]})
    end.


match_values(X, X) ->
    true;
match_values([E | ERest], [A | ARest]) ->
    case match_value(E, A) of
        true  -> match_values(ERest, ARest);
        false -> false
    end;
match_values(ET, AT) when is_tuple(ET), is_tuple(AT),
                          tuple_size(ET) == tuple_size(AT) ->
    match_values(tuple_to_list(ET), tuple_to_list(AT));
match_values(_, _) ->
    false.

match_value('_', _) ->
    true;
match_value(non_empty_list, X) when is_list(X), X /= [] ->
    true;
match_value(binary, X) when is_binary(X) ->
    true;
match_value(mod, X) when is_atom(X) ->
    true;
match_value(atom, X) when is_atom(X) ->
    true;
match_value(X, X)  ->
    true;
match_value(ET, AT) when is_tuple(ET), is_tuple(AT),
                          tuple_size(ET)==tuple_size(AT) ->
    match_values(ET, AT);
match_value(_, _) ->
    false.

capture_stdout_actually_works_test() ->
    Ret = x,
    {{return, Ret},
     {output, "z"}} = capture_stdout(fun() -> io:format("~s", [z]), Ret end).

capture_stdout(Fun) ->
    {_Pid,MRef} = spawn_monitor(
                   fun() ->
                           EvalExitWithResult = fun() -> exit(Fun()) end,
                           group_leader(self(), self()),
                           {_Pid, MRef} = spawn_monitor(EvalExitWithResult),
                           handle_io_requests(MRef, [])
                   end),
    receive
        {'DOWN', MRef, _, _, {{return, _Ret}, {output, _Output}}=Res} ->
            Res
    end.

handle_io_requests(MRef, Acc) ->
    receive
        {'DOWN', MRef, _, _, FunRes} ->
            exit({{return, FunRes},
                  {output, lists:flatten(lists:reverse(Acc))}});
        {io_request, From, ReplyAs, Req} ->
            {IoRes, Output} = handle_io_req(Req),
            From ! {io_reply, ReplyAs, IoRes},
            handle_io_requests(MRef, [Output | Acc])
    end.

handle_io_req({put_chars, Mod, Fun, Args}) ->
    {ok, apply(Mod, Fun, Args)};
handle_io_req({put_chars, _Enc, Mod, Fun, Args}) ->
    {ok, apply(Mod, Fun, Args)};
handle_io_req({put_chars, Txt}) ->
    {ok, Txt};
handle_io_req({put_chars, _Enc, Txt}) ->
    {ok, Txt};
handle_io_req({setopts, _}) ->
    {ok, ""};
handle_io_req({requests, IoRequests}) ->
    handle_io_reqs(IoRequests, []);
handle_io_req(_) ->
    %% {get_geometry, _??}
    %% {get_password, Prompt}
    %% {get_password, Enc, Prompt}
    %% {get_until, Prompt, Mod, Fun, Args}
    %% {get_until, Prompt, Enc, Mod, Fun, Args}
    %% {get_line, Prompt}
    %% {get_line, Enc, Prompt}
    %% {get_chars, Prompt, N}
    %% {get_chars, Enc, Prompt, N}
    {{error, enotsup}, ""}.

handle_io_reqs([Req | Rest], Acc) ->
    {_Res, Output} = handle_io_req(Req),
    handle_io_reqs(Rest, [Output | Acc]);
handle_io_reqs([], Acc) ->
    {ok, lists:flatten(lists:reverse(Acc))}.

failure_to_write_output_files_not_ignored_test() ->
    Contents = <<"message m1 { optional uint32 field11 = 1; }\n">>,
    CommonFileOpOpts = [{read_file, fun(_) -> {ok, Contents} end}],
    CommonOpts = [{i,"."}, return],
    WriteErlFailsOpts =
        [mk_fileop_opt([{write_file, fun("X.erl", _) -> ok;
                                        ("X.hrl", _) -> {error, eacces}
                                     end} | CommonFileOpOpts]) | CommonOpts],
    WriteHrlFailsOpts =
        [mk_fileop_opt([{write_file, fun("X.erl", _) -> ok;
                                        ("X.hrl", _) -> {error, eacces}
                                     end} | CommonFileOpOpts]) | CommonOpts],
    {error, _Reason, []}=Err1 = gpb_compile:file("X.proto", WriteErlFailsOpts),
    {error, _Reason, []}=Err2 = gpb_compile:file("X.proto", WriteHrlFailsOpts),
    gpb_compile:format_error(Err1),
    gpb_compile:format_error(Err2).

%% --- format_error tests ----------

format_error_works_for_scan_errors_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["message Msg ~~ required uint32 field1 = & }\n"],
      [".proto:1: "]).

format_error_works_for_parse_errors_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["message Msg { required uint32 field1 = }\n"],
      [".proto:1: ", "syntax error"]).

format_error_works_when_failed_to_read_import_file_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["import \"ZZ.proto\";\n",
       "message Msg { required uint32 field1 = 2;}\n"],
      [{read_file, [{"ZZ.proto", {error, eacces}}]}],
      ["read", "permission denied"]).

format_error_works_when_import_file_not_found_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["import \"ZZ.proto\";\n",
       "message Msg { required uint32 field1 = 2;}\n"],
      [{read_file_info, [{"ZZ.proto", {error, enoent}}]}],
      ["import", "not"]).

format_error_works_for_verification_erros_test() ->
    compile_and_assert_that_format_error_produces_iolist(
      ["message Msg1 { required Msg2 field1 = 2;}\n"],
      ["Msg2", "Msg1", "field1"]).

compile_and_assert_that_format_error_produces_iolist(Contents, ExpectedWords) ->
    compile_and_assert_that_format_error_produces_iolist(
      Contents, [], ExpectedWords).

compile_and_assert_that_format_error_produces_iolist(Contents,
                                                     ExtraFileOpReturnValues,
                                                     ExpectedPhrases) ->
    FileContents = iolist_to_binary(Contents),
    FileRetriever = mk_file_retriever(FileContents, ExtraFileOpReturnValues),
    FileInfoReader = mk_read_file_info("X.proto", ExtraFileOpReturnValues),
    Res = gpb_compile:file(
            "X.proto",
            [mk_fileop_opt([{read_file, FileRetriever},
                            {read_file_info, FileInfoReader}]),
             mk_defs_probe_sender_opt(self()),
             {i,"."}]),
    ?assertMatch({error,_}, Res),
    Txt = gpb_compile:format_error(Res),
    IsIoList = io_lib:deep_char_list(Txt),
    ?assertMatch({true, _}, {IsIoList, Txt}),
    FlatTxt = lists:flatten(Txt),
    PhrasesFound = [string:str(FlatTxt, Word) > 0 || Word <- ExpectedPhrases],
    AllPhrasesFound = lists:all(fun id/1, PhrasesFound),
    ?assertMatch({true,_,_}, {AllPhrasesFound, FlatTxt, PhrasesFound}).

mk_file_retriever(MainProtoFileContents, ExtraFileOpReturnValues) ->
    ExtraFileReturnValues =
        proplists:get_value(read_file, ExtraFileOpReturnValues, []),
    fun(FileName) ->
            case lists:keysearch(FileName, 1, ExtraFileReturnValues) of
                {value, {FileName, ReturnValue}} ->
                    ReturnValue;
                false ->
                    {ok, MainProtoFileContents}
            end
    end.

mk_read_file_info(_MainProtoFileName, ExtraFileOpReturnValues) ->
    ExtraFileReturnValues =
        proplists:get_value(read_file_info, ExtraFileOpReturnValues, []),
    fun(FileName) ->
            case lists:keysearch(FileName, 1, ExtraFileReturnValues) of
                {value, {FileName, ReturnValue}} ->
                    ReturnValue;
                false ->
                    {ok, #file_info{access=read}}
            end
    end.


%% --- nif generation tests -----------------

generates_nif_as_binary_and_file_test() ->
    Defs = mk_one_msg_field_of_each_type(),
    M = gpb_nif_test,
    LoadNif = "load_nif() -> erlang:load_nif({{nifbase}}, {{loadinfo}}).\n",
    LoadNifOpt = {load_nif, LoadNif},
    {ok, M, Codes} = gpb_compile:msg_defs(M, Defs, [binary, nif, LoadNifOpt]),
    Nif1 = proplists:get_value(nif, Codes),
    Master = self(),
    ReportWriteCc = fun(FName, Contents) ->
                            case filename:extension(FName) of
                                ".cc" -> Master ! {cc, Contents}, ok;
                                _     -> ok
                            end
                    end,
    FileOpOpt = mk_fileop_opt([{write_file, ReportWriteCc}]),
    ok = gpb_compile:msg_defs(M, Defs, [nif, FileOpOpt, LoadNifOpt]),
    Nif2 = receive {cc, Cc} -> Cc end,
    ?assertMatch(Nif1, Nif2).

nif_code_test_() ->
    {Descr, Tests} =
        case {want_nif_tests(), find_protoc(), find_cplusplus_compiler()} of
            {false,_,_} -> {"Protoc tests not wanted", []};
            {_,false,_} -> {"Protoc not found, not trying to compile", []};
            {_,_,false} -> {"No C++ compiler found, not trying to compile", []};
            {_,_,_}     -> {"nif tests",
                            [{"Nif compiles", fun nif_compiles/0},
                             {"Nif encode decode", fun nif_encode_decode/0}]}
        end,
    %% Without increased timeout, this test frequently times
    %% out on my slow laptop (1.6 GHz Atom N270)
    {Descr,
     {timeout, 300,  %% timeout for all tests
      [{timeout, 100, %% timeout for each test
        [{TestDescr, TestFun}]}
       || {TestDescr, TestFun} <- Tests]}}.

nif_compiles() ->
    with_tmpdir(
      fun(TmpDir) ->
              NCM = gpb_nif_test_c1,
              Defs = mk_one_msg_field_of_each_type(),
              {ok, _Code} = compile_msg_defs(NCM, Defs, TmpDir)
      end).

nif_encode_decode() ->
    with_tmpdir(
      fun(TmpDir) ->
              NEDM = gpb_nif_test_ed1,
              Defs = mk_one_msg_field_of_each_type(),
              {ok, Code} = compile_msg_defs(NEDM, Defs, TmpDir),
              with_tmpcode(
                NEDM, Code,
                fun() -> nif_encode_decode_test_it(NEDM, Defs) end)
      end).

nif_encode_decode_test_it(NEDM, Defs) ->
    MsgNames = [MsgName || {{msg, MsgName}, _Fields} <- Defs],
    Variants = [small, big, short, long],
    lists:foreach(fun({MsgName, Variant}) ->
                          OrigMsg = mk_msg(MsgName, Defs, Variant),
                          Encoded = NEDM:encode_msg(OrigMsg),
                          Decoded = NEDM:decode_msg(Encoded, MsgName),
                          ?assertEqual(Decoded, OrigMsg)
                  end,
                  [{MsgName,Variant} || MsgName <- MsgNames,
                                        Variant <- Variants]).

compile_msg_defs(M, MsgDefs, TmpDir) ->
    [NifCcPath, PbCcPath, NifOPath, PbOPath, NifSoPath, ProtoPath] =
        [filename:join(TmpDir, lists:concat([M, Ext]))
         || Ext <- [".nif.cc", ".pb.cc", ".nif.o", ".pb.o", ".nif.so",
                    ".proto"]],
    LoadNif = f("load_nif() -> erlang:load_nif(\"~s\", {{loadinfo}}).\n",
                [filename:join(TmpDir, lists:concat([M,".nif"]))]),
    LoadNifOpt = {load_nif, LoadNif},
    Opts = [binary, nif, LoadNifOpt],
    {ok, M, Codes} = gpb_compile:msg_defs(M, MsgDefs, Opts),
    Code = proplists:get_value(erl, Codes),
    NifTxt = proplists:get_value(nif, Codes),
    ProtoTxt = msg_defs_to_proto(MsgDefs),
    %%
    ok = file:write_file(NifCcPath, NifTxt),
    ok = file:write_file(ProtoPath, ProtoTxt),
    %%
    CC = find_cplusplus_compiler(),
    Protoc = find_protoc(),
    CFlags = get_cflags(),
    LdFlags = get_ldflags(),
    ok = ccompile("'~s' --proto_path '~s' --cpp_out='~s' '~s'",
                  [Protoc, TmpDir, TmpDir, ProtoPath]),
    ok = ccompile("'~s' -g -fPIC -Wall -O3 '-I~s' ~s -c -o '~s' '~s'",
                  [CC, TmpDir, CFlags, NifOPath, NifCcPath]),
    ok = ccompile("'~s' -g -fPIC -Wall -O3 '-I~s' ~s -c -o '~s' '~s'",
                  [CC, TmpDir, CFlags, PbOPath, PbCcPath]),
    ok = ccompile("'~s' -g -fPIC -shared -Wall -O3 ~s"
                  "    -o '~s' '~s' '~s' -lprotobuf",
                  [CC, LdFlags, NifSoPath, NifOPath, PbOPath]),
    {ok, Code}.

with_tmpdir(Fun) ->
    {ok, TmpDir} = get_tmpdir(),
    try Fun(TmpDir)
    after clean_tmpdir(TmpDir)
    end.

get_tmpdir() ->
    {A, B, C} = now(),
    random:seed(erlang:phash2(A+B+C), erlang:phash2(B+C), erlang:phash2(A+C)),
    mktempdir(
      filename:join(case os:getenv("TMPDIR") of
                        false -> "/tmp";
                        TDir  -> TDir
                    end,
                    lists:concat([?MODULE,"-",os:getenv("LOGNAME"),"-",
                                  os:getpid(),"-"]))).

mktempdir(Base) ->
    D = Base ++ f("~8..0w", [random:uniform(90000000)]),
    case file:make_dir(D) of
        ok             -> {ok, D};
        {error, exist} -> mktempdir(Base);
        Error          -> Error
    end.

clean_tmpdir(TmpDir) ->
    os:cmd(f("/bin/rm -rf '~s'", [TmpDir])).

with_tmpcode(Module, Code, Fun) ->
    try
        load_code(Module, Code),
        Fun()
    after
        unload_code(Module)
    end.

test_nifs(Boolean) when is_boolean(Boolean) ->
    os:putenv("GPB_NIF_TESTS", lists:concat([Boolean])).

want_nif_tests() ->
    %% It can be useful to disable nif testing due to the
    %% behaviour in the libprotoc, that if it detects loading
    %% a proto definition with the same name as it already
    %% has loaded, it will refuse, and may stop the entire
    %% erlang-vm. See the documentation in gpb_compile:c/1,2,
    %% the `nif' option, for further details. I have seen it
    %% halt the entire erlang-vm when tests failed, which is
    %% a pity e.g. when the vm is in an interactive inferior
    %% emacs window
    case os:getenv("GPB_NIF_TESTS") of
        false   -> true; %% default is to test nifs
        "true"  -> true;
        "false" -> false
    end.

find_cplusplus_compiler() ->
    case os:getenv("CXX") of
        false ->
            case os:find_executable("g++") of
                false -> os:find_executable("c++");
                Gxx   -> Gxx
            end;
        CxxCompiler ->
            CxxCompiler
    end.

find_protoc() ->
    case os:getenv("PROTOC") of
        false  -> os:find_executable("protoc");
        Protoc -> Protoc
    end.

get_cflags() ->
    case os:getenv("CFLAGS") of
        false  -> "";
        CFlags -> CFlags
    end ++ case os:getenv("CXXFLAGS") of
               false    -> "";
               CxxFlags -> CxxFlags
           end.

get_ldflags() ->
    case os:getenv("LDFLAGS") of
        false   -> "";
        LdFlags -> LdFlags
    end.

msg_defs_to_proto(MsgDefs) ->
    iolist_to_binary(lists:map(fun msg_def_to_proto/1, MsgDefs)).

msg_def_to_proto({{enum, Name}, EnumValues}) ->
    f("enum ~s {~n~s}~n~n",
      [Name, lists:map(fun format_enumerator/1, EnumValues)]);
msg_def_to_proto({{msg, Name}, Fields}) ->
    f("message ~s {~n~s}~n~n",
      [Name, lists:map(fun format_field/1, Fields)]).

format_enumerator({N,V}) ->
    f("  ~s = ~w;~n", [N, V]).

format_field(#field{name=FName, fnum=FNum, type=Type, occurrence=Occurrence}) ->
    TypeStr = case Type of
                  {msg,Name2}  -> Name2;
                  {enum,Name2} -> Name2;
                  Type         -> Type
              end,
    f("  ~s ~s ~s = ~w;~n", [Occurrence, TypeStr, FName, FNum]).


ccompile(F, A) ->
    Cmd = f(F, A),
    Output = os:cmd(Cmd ++ "; echo $?\n"),
    [LastLine | _Rest] = lists:reverse(string:tokens(Output, "\r\n")),
    try list_to_integer(string:strip(LastLine)) of
        0 -> ok;
        _ -> ?debugFmt("Compilation failed!~nCmd=~p~nOutput:~n~s~n~n",
                       [Cmd, Output]),
             {error, Output}
    catch error:badarg ->
            ?debugFmt("Compilation failed!~nCmd=~p~nOutput:~n~s~n~n",
                      [Cmd, Output]),
            {error, Output}
    end.

mk_one_msg_field_of_each_type() ->
    EnumDef    = {{enum, ee}, [{en1, 1}, {en2, 2}]},
    SubMsgDef  = {{msg, submsg1}, mk_fields_of_type([uint32], required)},
    TopMsgDef1 = {{msg, topmsg1}, mk_fields_of_type(
                                    [sint32, sint64, int32, int64, uint32,
                                     uint64, bool, fixed64, sfixed64,
                                     double, string, bytes, fixed32, sfixed32,
                                     float, {enum, ee}, {msg, submsg1}],
                                    required)},
    TopMsgDef2 = {{msg, topmsg2}, mk_fields_of_type(
                                    [sint32, sint64, int32, int64, uint32,
                                     uint64, bool, fixed64, sfixed64,
                                     double, string, bytes, fixed32, sfixed32,
                                     float, {enum, ee}, {msg, submsg1}],
                                    repeated)},
    TopMsgDef3 = {{msg, topmsg3}, mk_fields_of_type(
                                    [sint32, sint64, int32, int64, uint32,
                                     uint64, bool, fixed64, sfixed64,
                                     double, string, bytes, fixed32, sfixed32,
                                     float, {enum, ee}, {msg, submsg1}],
                                    optional)},
    [EnumDef, SubMsgDef, TopMsgDef1, TopMsgDef2, TopMsgDef3].

mk_fields_of_type(Types, Occurrence) ->
    [#field{name=list_to_atom(lists:concat([f,integer_to_list(I)])),
            rnum=I + 1,
            fnum=I,
            type=Type,
            occurrence=Occurrence,
            opts=[]}
     || {I, Type} <- lists:zip(lists:seq(1,length(Types)), Types)].

mk_msg(MsgName, Defs, Variant) ->
    {{msg, MsgName}, Fields} = lists:keyfind({msg, MsgName}, 1, Defs),
    R0 = erlang:make_tuple(length(Fields) + 1, undefined, [{1, MsgName}]),
    lists:foldl(fun(#field{rnum=RNum}=Field, R) ->
                        Value = mk_field_value(Field, Defs, Variant),
                        setelement(RNum, R, Value)
                end,
                R0,
                Fields).

mk_field_value(#field{occurrence=repeated}, _Defs, short) ->
    [];
mk_field_value(#field{occurrence=repeated}=F, Defs, Variant) ->
    [mk_field_value(F#field{occurrence=required}, Defs, Variant)];
mk_field_value(#field{type=sint32}, _Defs, Vnt)   -> mk_sint(32, Vnt);
mk_field_value(#field{type=sint64}, _Defs, Vnt)   -> mk_sint(64, Vnt);
mk_field_value(#field{type=int32}, _Defs, Vnt)    -> mk_sint(32, Vnt);
mk_field_value(#field{type=int64}, _Defs, Vnt)    -> mk_sint(64, Vnt);
mk_field_value(#field{type=uint32}, _Defs, Vnt)   -> mk_uint(32, Vnt);
mk_field_value(#field{type=uint64}, _Defs, Vnt)   -> mk_uint(64, Vnt);
mk_field_value(#field{type=bool}, _Defs, Vnt)     -> mk_bool(Vnt);
mk_field_value(#field{type=fixed64}, _Defs, Vnt)  -> mk_uint(64, Vnt);
mk_field_value(#field{type=sfixed64}, _Defs, Vnt) -> mk_sint(64, Vnt);
mk_field_value(#field{type=double}, _Defs, Vnt)   -> mk_float(64, Vnt);
mk_field_value(#field{type=string}, _Defs, Vnt)   -> mk_string(Vnt);
mk_field_value(#field{type=bytes}, _Defs, Vnt)    -> mk_bytes(Vnt);
mk_field_value(#field{type=fixed32}, _Defs, Vnt)  -> mk_uint(32, Vnt);
mk_field_value(#field{type=sfixed32}, _Defs, Vnt) -> mk_sint(32, Vnt);
mk_field_value(#field{type=float}, _Defs, Vnt)    -> mk_float(32, Vnt);
mk_field_value(#field{type={enum, E}}, Defs, _Vnt) ->
    {{enum, E}, [{E1 , _V1} | _Rest]} = lists:keyfind({enum, E}, 1, Defs),
    E1;
mk_field_value(#field{type={msg, SubMsgName}}, Defs, Vnt) ->
    mk_msg(SubMsgName, Defs, Vnt).

mk_sint(32, small) -> - (1 bsl 31);
mk_sint(32, big)   -> (1 bsl 31) - 1;
mk_sint(64, small) -> - (1 bsl 63);
mk_sint(64, big)   -> (1 bsl 63) - 1;
mk_sint(_,  _)     -> 0.

mk_uint(32, big)   -> (1 bsl 32) - 1;
mk_uint(64, big)   -> (1 bsl 64) - 1;
mk_uint(_,  _)     -> 0.

mk_bool(small) -> false;
mk_bool(_)     -> true.

mk_string(short) -> "";
mk_string(big)   -> [16#10ffff];
mk_string(_)     -> "a".

mk_bytes(short) -> <<>>;
mk_bytes(_)     -> <<"b">>.

mk_float(_, _) -> 1.0.

%% --- auxiliaries -----------------

%% vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
%% begin functions that imitates the interface of the gpb module
%% needed by the common/shared tests included above from gpb_tests.erl
-ifdef(gpb_compile_common_tests).
decode_msg(Bin, MsgName, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:decode_msg(Bin, MsgName)
    after unload_code(M)
    end.

encode_msg(Msg, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:encode_msg(Msg)
    after unload_code(M)
    end.

merge_msgs(Msg1, Msg2, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:merge_msgs(Msg1, Msg2)
    after unload_code(M)
    end.

verify_msg(Msg, MsgDefs) ->
    M = compile_defs(MsgDefs),
    try M:verify_msg(Msg)
    after unload_code(M)
    end.
-endif. %% gpb_compile_common_tests
%% end of functions that imitates the interface of the gpb module
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

compile_defs(MsgDefs) ->
    compile_defs(MsgDefs, [{verify, always}]).

compile_defs(MsgDefs, ExtraOpts) ->
    Mod = find_unused_module(),
    Opts = [binary | ExtraOpts],
    {ok, Mod, Code} = gpb_compile:msg_defs(Mod, MsgDefs, Opts),
    load_code(Mod, Code),
    Mod.

compile_iolist(IoList) ->
    compile_iolist(IoList, []).

compile_iolist(IoList, ExtraOpts) ->
    Mod = find_unused_module(),
    Contents = iolist_to_binary(IoList),
    {ok, Mod, Code, []} =
        gpb_compile:file(
          f("~s.proto", [Mod]),
          [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
           {i,"."},
           binary, return_warnings | ExtraOpts]),
    load_code(Mod, Code),
    Mod.

load_code(Mod, Code) ->
    unload_code(Mod),
    {module, Mod} = code:load_binary(Mod, "<nofile>", Code),
    ok.

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

find_unused_module() -> find_unused_module(1).

find_unused_module(N) ->
    ModNameCandidate = list_to_atom(f("~s-tmp-~w", [?MODULE, N])),
    case code:is_loaded(ModNameCandidate) of
        false    -> ModNameCandidate;
        {file,_} -> find_unused_module(N+1)
    end.

id(X) -> X.

f(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).
