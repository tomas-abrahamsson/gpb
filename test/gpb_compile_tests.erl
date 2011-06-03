%%% Copyright (C) 2010  Tomas Abrahamsson
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
    Contents = iolist_to_binary(
                 ["message Msg { required uint32 field1 = 1; }\n"]),
    {ok, 'X', Code, []} =
        gpb_compile:file(
          "X.proto",
          [mk_fileop_opt([{read_file, fun(_) -> {ok, Contents} end}]),
           mk_defs_probe_sender_opt(self()),
           {i,"."},
           binary]),
    true = is_binary(Code),
    [{{msg,'Msg'},_}] = receive_filter_sort_msgs_defs().

parses_msgdefs_to_binary_test() ->
    Defs = [{{msg,'Msg'},
             [#field{name=field1, rnum=2, fnum=1, type=uint32,
                     occurrence=required, opts=[]}]}],
    M = find_unused_module(),
    {ok, M, Code, []} = gpb_compile:msg_defs(M, Defs, [binary]),
    true = is_binary(Code).

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

-record(m9,{aa, bb, cc}).

code_generation_when_submsg_size_is_known_at_compile_time_test() ->
    KnownSizeM9 =
        [{{msg,m9}, [#field{name=aa, type={enum,e}, occurrence=required,
                            fnum=1, rnum=#m9.aa, opts=[]},
                     #field{name=bb, type=fixed32, occurrence=required,
                            fnum=2, rnum=#m9.bb, opts=[]},
                     #field{name=cc, type=fixed64, occurrence=required,
                            fnum=3, rnum=#m9.cc, opts=[]}]}],
    UnknownSizeM9 =
        [{{msg,m9}, [#field{name=aa, type={enum,e}, occurrence=optional,
                            fnum=1, rnum=#m9.aa, opts=[]},
                     #field{name=bb, type=fixed32, occurrence=optional,
                            fnum=2, rnum=#m9.bb, opts=[]},
                     #field{name=cc, type=fixed64, occurrence=optional,
                            fnum=3, rnum=#m9.cc, opts=[]}]}],

    CommonDefs =
        [{{msg,m1}, [#field{name=a, type={msg,m9}, occurrence=required,
                            fnum=1, rnum=2, opts=[]}]},
         {{enum,e}, [{x1, 1}, {x2, 2}]} %% all enum values same encode size
        ],

    M1 = compile_defs(CommonDefs++KnownSizeM9),
    M2 = compile_defs(CommonDefs++UnknownSizeM9),
    Msg = #m1{a=#m9{aa=x1, bb=33, cc=44}},
    Encoded1 = M1:encode_msg(Msg),
    Encoded2 = M2:encode_msg(Msg),
    Encoded1 = Encoded2,
    unload_code(M1),
    unload_code(M2).

%% --- decoder tests ---------------

decodes_overly_long_varints_test() ->
    M = compile_defs([{{msg,m1}, [#field{name=a, type=int32, fnum=1, rnum=#m1.a,
                                         occurrence=required, opts=[]}]}]),
    #m1{a=54} = M:decode_msg(<<8, 54>>, m1), %% canonically encoded
    #m1{a=54} = M:decode_msg(<<8, (128+54), 128, 128, 0>>, m1),
    unload_code(M).

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
    {ok, Mod, Code, []} = gpb_compile:msg_defs(Mod, MsgDefs, Opts),
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
