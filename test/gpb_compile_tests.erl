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
    {ok, 'X', Code, []} = gpb_compile:msg_defs('X', Defs, [binary]),
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
