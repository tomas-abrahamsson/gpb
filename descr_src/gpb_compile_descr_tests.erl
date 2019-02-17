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

-module(gpb_compile_descr_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gpb_descriptor.hrl").

%% ------------------------------------------------------------------

individual_descriptor_test() ->
    {#'FileDescriptorSet'{
       file=[#'FileDescriptorProto'{}=B1,
             #'FileDescriptorProto'{}=B2]},
     [{"main",B1},
      {"aux",B2}]} = compile_descriptors(
                   [{"main.proto",
                     ["syntax=\"proto2\";",
                      "import \"aux.proto\";",
                      "message M { };"]},
                    {"aux.proto",
                     ["syntax=\"proto2\";",
                      "message A { optional uint32 g = 1; }"]}],
                   []).

compile_descriptors(IoLists, GpbCompileOpts) ->
    {ok, Defs, []=_Warns} = compile_files_as_iolists(IoLists, GpbCompileOpts),
    {Bin, PBins} = gpb_compile_descr:encode_defs_to_descriptors(Defs),
    {gpb_descriptor:decode_msg(Bin, 'FileDescriptorSet'),
     [{ProtoName,gpb_descriptor:decode_msg(ProtoBin, 'FileDescriptorProto')}
      || {ProtoName, ProtoBin} <- PBins]}.

compile_files_as_iolists([{FName, _IoList} | _Rest]=IoLists, GpbCompileOpts) ->
    ReadFile = fun(F) ->
                       B = filename:basename(F),
                       case lists:keyfind(B, 1, IoLists) of
                           {B, Contents} ->
                               {ok, iolist_to_binary([Contents])};
                           _ ->
                               file:read_file(F)
                       end
               end,
    ReadFileInfo = fun(F) ->
                           B = filename:basename(F),
                           case lists:keyfind(B, 1, IoLists) of
                               {B, _Contents} ->
                                   {ok, #file_info{access=read}};
                               _ ->
                                   file:read_file_info(F)
                           end
                   end,
    gpb_compile:file(
      FName,
      [{file_op, [{read_file, ReadFile},
                  {read_file_info, ReadFileInfo},
                  {write_file, fun(_,_) -> ok end}]},
       {i,"."},
       to_proto_defs, return_errors, return_warnings
       | GpbCompileOpts]).
