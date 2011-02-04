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

-module(gpb_compile).
%-compile(export_all).
-export([file/1, file/2]).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

%% @spec file(File) -> ok | {error, Reason}
%% @equiv file(File, [])
file(File) ->
    file(File, []).

%% @spec file(File, Opts) -> ok | {error, Reason}
%%            File = string()
%%            Opts = [Opt]
%%            Opt  = {i,directory()} |
%%                   {type_specs, boolean()} | type_specs |
%%                   {verify, optionally | always}
%%
%% @doc
%% Compile a .proto file to a .erl file and to a .hrl file.
%%
%% The generated .erl file will use the `gpb' module for runtime
%% support for encoding and decoding.
%%
%% The File must not include path to the .proto file. Example:
%% "SomeDefinitions.proto" is ok, while "/path/to/SomeDefinitions.proto"
%% is not ok.
%%
%% The .proto file is expected to be found in a directories specified by an
%% `{i,directory()}' option. It is possible to specify `{i,directory()}'
%% several times, they will be search in the order specified.
%%
%% The `{type_specs,boolean()}' option enables or disables `::Type()'
%% annotations in the generated .hrl file. Default is currently
%% `false'. If you set it to `true', you may get into troubles for
%% messages referencing other messages, when compiling the generated
%% files. The `type_specs' option is equivalent to `{type_specs,true}'.
%%
%% The `{verify,Value}' option concerns encoding of Erlang values to
%% binary messages. It specifies whether the Erlang value are to be
%% verified against the data types of the fields as declared in the
%% proto files.
%%
%% The `{verify,always}' option instructs the compiler to generate
%% code that unconditionally verifies Erlang values at encoding time.
%%
%% The `{verify,optionally}' option instructs the compiler to generate
%% an `encode_msg/2' function with an Opts parameter, and if you call
%% this function with the `verify' option set, the Erlang values will
%% be verified.
%%
%% Erlang value verfication either succeeds or crashes with the `error'
%% `{gpb_type_error,Reason}'.
%% Note that it is also possible to call `verify_msg/1' in the
%% generated code to verify Erlang messages if you do not want to
%% encode them.
file(File, Opts0) ->
    case parse_file(File, Opts0) of
        {ok, Defs0} ->
            {IsAcyclic, Defs} = try_topsort_defs(Defs0),
            possibly_probe_defs(Defs, Opts0),
            Opts1 = possibly_adjust_typespec_opt(IsAcyclic, Opts0),
            Ext = filename:extension(File),
            Mod = list_to_atom(filename:basename(File, Ext)),
            Erl = change_ext(File, ".erl"),
            Hrl = change_ext(File, ".hrl"),
            file_write_file(Erl, format_erl(Mod, Defs, Opts1), Opts1),
            file_write_file(Hrl, format_hrl(Mod, Defs, Opts1), Opts1);
        {error, _Reason} = Error ->
            Error
    end.

change_ext(File, NewExt) ->
    filename:join(filename:dirname(File),
                  filename:basename(File, filename:extension(File)) ++ NewExt).

parse_file(FName, Opts) ->
    case parse_file_and_imports(FName, Opts) of
        {ok, {Defs1, _AllImported}} ->
            %% io:format("processed these imports:~n  ~p~n", [_AllImported]),
            %% io:format("Defs1=~n  ~p~n", [Defs1]),
            Defs2 = gpb_parse:flatten_defs(
                      gpb_parse:absolutify_names(Defs1)),
            case gpb_parse:verify_defs(Defs2) of
                ok ->
                    {ok, gpb_parse:normalize_msg_field_options( %% Sort it?
                           gpb_parse:enumerate_msg_fields(
                             gpb_parse:extend_msgs(
                               gpb_parse:reformat_names(
                                 gpb_parse:resolve_refs(Defs2)))))};
                {error, _Reasons} = Error ->
                    Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_file_and_imports(FName, Opts) ->
    parse_file_and_imports(FName, [FName], Opts).

parse_file_and_imports(FName, AlreadyImported, Opts) ->
    case locate_import(FName, Opts) of
        {ok, FName2} ->
            {ok,B} = file_read_file(FName2, Opts),
            %% Add to AlreadyImported to prevent trying to import it again: in
            %% case we get an error we don't want to try to reprocess it later
            %% (in case it is multiply imported) and get the error again.
            AlreadyImported2 = [FName | AlreadyImported],
            case scan_and_parse_string(binary_to_list(B)) of
                {ok, Defs} ->
                    Imports = gpb_parse:fetch_imports(Defs),
                    read_and_parse_imports(Imports,AlreadyImported2,Defs,Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

scan_and_parse_string(S) ->
    case gpb_scan:string(S) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens++[{'$end', 999}]) of
                {ok, Result} ->
                    {ok, Result};
                {error, {_LNum,_Module,_EMsg}=Reason} ->
                    {error, {parse_error,S,Reason}}
            end;
        {error,Reason} ->
            {error, {scan_error,S,Reason}}
    end.


read_and_parse_imports([Import | Rest], AlreadyImported, Defs, Opts) ->
    case lists:member(Import, AlreadyImported) of
        true ->
            read_and_parse_imports(Rest, AlreadyImported, Defs, Opts);
        false ->
            case import_it(Import, AlreadyImported, Defs, Opts) of
                {ok, {Defs2, Imported2}} ->
                    read_and_parse_imports(Rest, Imported2, Defs2, Opts);
                {error, Reason} ->
                    {error, Reason}
            end
    end;
read_and_parse_imports([], Imported, Defs, _Opts) ->
    {ok, {Defs, Imported}}.

import_it(Import, AlreadyImported, Defs, Opts) ->
    %% FIXME: how do we handle scope of declarations,
    %%        e.g. options/package for imported files?
    case parse_file_and_imports(Import, AlreadyImported, Opts) of
        {ok, {MoreDefs, MoreImported}} ->
            Defs2 = Defs++MoreDefs,
            Imported2 = lists:usort(AlreadyImported++MoreImported),
            {ok, {Defs2, Imported2}};
        {error, Reason} ->
            {error, Reason}
    end.

locate_import(Import, Opts) ->
    ImportPaths = [Path || {i, Path} <- Opts],
    locate_import_aux(ImportPaths, Import, Opts).

locate_import_aux([Path | Rest], Import, Opts) ->
    File = filename:join(Path, Import),
    case file_read_file_info(File, Opts) of
        {ok, #file_info{access = A}} when A == read; A == read_write ->
            {ok, File};
        {ok, #file_info{}} ->
            locate_import_aux(Rest, Import, Opts);
        {error, _Reason} ->
            locate_import_aux(Rest, Import, Opts)
    end;
locate_import_aux([], Import, _Opts) ->
    {error, {import_not_found, Import}}.

try_topsort_defs(Defs) ->
    G = digraph:new(),
    [digraph:add_vertex(G, M) || {{msg,M}, _Fields} <- Defs],
    [[digraph:add_edge(G, From, To) || #field{type={msg,To}} <- Fields]
     || {{msg,From},Fields} <- Defs],
    case digraph_utils:topsort(G) of
        false ->
            digraph:delete(G),
            {false, Defs};
        Order ->
            digraph:delete(G),
            ROrder = lists:reverse(Order),
            OrderedMsgDefs = [lists:keyfind({msg,M},1,Defs) || M <- ROrder],
            {true, OrderedMsgDefs ++ (Defs -- OrderedMsgDefs)}
    end.

possibly_adjust_typespec_opt(true=_IsAcyclic, Opts) ->
    Opts;
possibly_adjust_typespec_opt(false=_IsAcyclic, Opts) ->
    case get_type_specs_by_opts(Opts) of
        true  ->
            io:format("Warning: omitting type specs "
                      "due to cyclic message references.~n"),
            lists:keydelete(type_specs, 1, Opts -- [type_specs]); % disable
        false ->
            Opts
    end.

format_erl(Mod, Defs, Opts) ->
    iolist_to_binary(
      [f("%% Automatically generated, do not edit~n"
         "%% Generated by ~p on ~p~n", [?MODULE, calendar:local_time()]),
       f("-module(~w).~n", [Mod]),
       "\n",
       f("-export([encode_msg/1, encode_msg/2]).~n"),
       f("-export([decode_msg/2]).~n"),
       f("-export([verify_msg/1]).~n"),
       f("-export([get_msg_defs/0]).~n"),
       "\n",
       f("-include(\"gpb.hrl\").~n"),
       "\n",
       f("encode_msg(Msg) ->~n"
         "    encode_msg(Msg, []).~n~n"),
       case proplists:get_value(verify, Opts, optionally) of
           optionally ->
               f("encode_msg(Msg, Opts) ->~n"
                 "    case proplists:get_bool(verify, Opts) of~n"
                 "        true  -> verify_msg(Msg);~n"
                 "        false -> ok~n"
                 "    end,~n"
                 "    gpb:encode_msg(Msg, get_msg_defs()).~n");
           always ->
               f("encode_msg(Msg, _Opts) ->~n"
                 "    verify_msg(Msg),~n"
                 "    gpb:encode_msg(Msg, get_msg_defs()).~n")
       end,
       "\n",
       f("decode_msg(Bin, MsgName) ->~n"
         "    gpb:decode_msg(Bin, MsgName, get_msg_defs()).~n"),
       "\n",
       f("verify_msg(Msg) ->~n"
         "    gpb:verify_msg(Msg, get_msg_defs()).~n"),
       "\n",
       f("get_msg_defs() ->~n"
         "    [~s].~n", [outdent_first(format_msgs_and_enums(5, Defs))])]).

format_msgs_and_enums(Indent, Defs) ->
    Enums = [Item || {{enum, _}, _}=Item <- Defs],
    Msgs  = [Item || {{msg, _}, _}=Item <- Defs],
    [format_enums(Indent, Enums),
     if Enums /= [], Msgs /= [] -> ",\n";
        true                    -> ""
     end,
     format_msgs(Indent, Msgs)].

format_enums(Indent, Enums) ->
    string:join([f("~s~p", [indent(Indent,""), Enum]) || Enum <- Enums],
                ",\n").

format_msgs(Indent, Msgs) ->
    string:join([indent(Indent,
                        f("{~w,~n~s[~s]}",
                          [{msg,Msg},
                           indent(Indent+1, ""),
                           outdent_first(format_efields(Indent+2, Fields))]))
                 || {{msg,Msg},Fields} <- Msgs],
                ",\n").

format_efields(Indent, Fields) ->
    string:join([format_efield(Indent, Field) || Field <- Fields],
                ",\n").

format_efield(I, #field{name=N,fnum=F,rnum=R,type=T,occurrence=O,opts=Opts}) ->
    [indent(I, f("#field{name=~w, fnum=~w, rnum=~w, type=~w,~n", [N,F,R,T])),
     indent(I, f("       occurrence=~w, opts=~p}", [O, Opts]))].


format_hrl(Mod, Defs, Opts) ->
    iolist_to_binary(
      [f("-ifndef(~p).~n", [Mod]),
       f("-define(~p, true).~n", [Mod]),
       "\n",
       string:join([format_msg_record(Msg, Fields, Opts, Defs)
                    || {{msg,Msg},Fields} <- Defs],
                   "\n"),
       "\n",
       f("-endif.~n")]).

format_msg_record(Msg, Fields, Opts, Defs) ->
    [f("-record(~p,~n", [Msg]),
     f("        {"),
     outdent_first(format_hfields(8+1, Fields, Opts, Defs)),
     "\n",
     f("        }).~n")].

format_hfields(Indent, Fields, CompileOpts, Defs) ->
    TypeSpecs = get_type_specs_by_opts(CompileOpts),
    string:join(
      lists:map(
        fun({I, #field{name=Name, fnum=FNum, opts=FOpts}=Field}) ->
                DefaultStr = case proplists:get_value(default, FOpts, '$no') of
                                 '$no'   -> "";
                                 Default -> f(" = ~p", [Default])
                             end,
                TypeStr = f("~s", [type_to_typestr(Field, Defs)]),
                CommaSep = if I < length(Fields) -> ",";
                              true               -> "" %% last entry
                           end,
                FieldTxt1 = indent(Indent, f("~w~s", [Name, DefaultStr])),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    f("~s~s:: ~s~s", [FieldTxt1, LineUp,
                                                      TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                               not TypeSpecs -> 40
                            end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                TypeComment = type_to_comment(Field, TypeSpecs),
                f("~s~s% = ~w~s~s",
                  [FieldTxt2, LineUpStr2, FNum,
                   [", " || TypeComment /= ""], TypeComment])
        end,
        index_seq(Fields)),
      "\n").

get_type_specs_by_opts(Opts) ->
    Default = false,
    proplists:get_value(type_specs, Opts, Default).

type_to_typestr(#field{type=Type, occurrence=Occurrence}, Defs) ->
    case Occurrence of
        required -> type_to_typestr_2(Type, Defs);
        repeated -> "[" ++ type_to_typestr_2(Type, Defs) ++ "]";
        optional -> type_to_typestr_2(Type, Defs) ++ " | 'undefined'"
    end.

type_to_typestr_2(sint32, _Defs)   -> "integer()";
type_to_typestr_2(sint64, _Defs)   -> "integer()";
type_to_typestr_2(int32, _Defs)    -> "integer()";
type_to_typestr_2(int64, _Defs)    -> "integer()";
type_to_typestr_2(uint32, _Defs)   -> "non_neg_integer()";
type_to_typestr_2(uint64, _Defs)   -> "non_neg_integer()";
type_to_typestr_2(bool, _Defs)     -> "boolean()";
type_to_typestr_2(fixed32, _Defs)  -> "non_neg_integer()";
type_to_typestr_2(fixed64, _Defs)  -> "non_neg_integer()";
type_to_typestr_2(sfixed32, _Defs) -> "integer()";
type_to_typestr_2(sfixed64, _Defs) -> "integer()";
type_to_typestr_2(float, _Defs)    -> "float()";
type_to_typestr_2(double, _Defs)   -> "float()";
type_to_typestr_2(string, _Defs)   -> "string()";
type_to_typestr_2(bytes, _Defs)    -> "binary()";
type_to_typestr_2({enum,E}, Defs)  -> enum_typestr(E, Defs);
type_to_typestr_2({msg,M}, _DEfs)  -> f("#~p{}", [M]).

enum_typestr(E, Defs) ->
    {value, {{enum,E}, Enumerations}} = lists:keysearch({enum,E}, 1, Defs),
    string:join(["'"++atom_to_list(EName)++"'" || {EName, _} <- Enumerations],
                " | ").

type_to_comment(#field{type=Type}, true=_TypeSpec) ->
    case Type of
        sint32   -> "32 bits";
        sint64   -> "32 bits";
        int32    -> "32 bits";
        int64    -> "32 bits";
        uint32   -> "32 bits";
        uint64   -> "32 bits";
        fixed32  -> "32 bits";
        fixed64  -> "32 bits";
        sfixed32 -> "32 bits";
        sfixed64 -> "32 bits";
        {enum,E} -> "enum "++atom_to_list(E);
        _        -> ""
    end;
type_to_comment(#field{type=Type, occurrence=Occurrence}, false=_TypeSpec) ->
    case Occurrence of
        required -> f("~w", [Type]);
        repeated -> "[" ++ f("~w", [Type]) ++ "]";
        optional -> f("~w (optional)", [Type])
    end.

lineup(CurrentCol, TargetCol) when CurrentCol < TargetCol ->
    lists:duplicate(TargetCol - CurrentCol, $\s);
lineup(_, _) ->
    " ".

indent(Indent, Str) ->
    lists:duplicate(Indent, $\s) ++ Str.

outdent_first(IoList) ->
    lists:dropwhile(fun(C) -> C == $\s end,
                    binary_to_list(iolist_to_binary(IoList))).

index_seq([]) -> [];
index_seq(L)  -> lists:zip(lists:seq(1,length(L)), L).

f(F)   -> f(F,[]).
f(F,A) -> io_lib:format(F,A).

file_read_file(FileName, Opts) ->
    file_op(read_file, [FileName], Opts).

file_read_file_info(FileName, Opts) ->
    file_op(read_file_info, [FileName], Opts).

file_write_file(FileName, Bin, Opts) ->
    file_op(write_file, [FileName, Bin], Opts).

file_op(Fn, Args, Opts) ->
    FileOp = proplists:get_value(file_op, Opts, fun use_the_file_module/2),
    FileOp(Fn, Args).

use_the_file_module(Fn, Args) ->
    apply(file, Fn, Args).

possibly_probe_defs(Defs, Opts) ->
    case proplists:get_value(probe_defs, Opts, '$no') of
        '$no' -> ok;
        Fn    -> Fn(Defs)
    end.
