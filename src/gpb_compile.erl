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
%%                   {type_specs, boolean()}
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
%% files.
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
            Defs2 = gpb_parse:reformat_names(
                      gpb_parse:flatten_defs(
                        gpb_parse:absolutify_names(Defs1))),
            case gpb_parse:verify_refs(Defs2) of
                ok ->
                    {ok, gpb_parse:normalize_msg_field_options( %% Sort it?
                           gpb_parse:enumerate_msg_fields(
                             gpb_parse:extend_msgs(
                               gpb_parse:resolve_refs(Defs2))))};
                {error, _Reason} = Error ->
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
       f("-include(\"~s.hrl\").~n", [Mod]),
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
         "    ~s.~n",
         [format_decoder_topcase(4, Defs, "Bin", "MsgName")]),
       "\n",
       f("~s~n", [format_decoders(Defs, Opts)]),
       "\n",
       f("verify_msg(Msg) ->~n"
         "    gpb:verify_msg(Msg, get_msg_defs()).~n"),
       "\n",
       f("get_msg_defs() ->~n"
         "    [~s].~n", [outdent_first(format_msgs_and_enums(5, Defs))])]).

format_decoder_topcase(Indent, Defs, BinVar, MsgNameVar) ->
    ["case ", MsgNameVar, " of\n",
     string:join([indent(Indent+4, f("~p -> ~p(~s)",
                                     [MsgName, mk_fn(d_msg_, MsgName), BinVar]))
                  || {{msg, MsgName}, _MsgDef} <- Defs],
                 ";\n"),
     "\n",
     indent(Indent, f("end"))].

format_decoders(Defs, _Opts) ->
    [format_enum_decoders(Defs),
     format_initial_msgs(Defs),
     format_msg_decoders(Defs)].

format_enum_decoders(Defs) ->
    %% FIXME: enum values can be negative, but "raw" varints are positive
    %%        insert a 2-complement in the mapping in order to move computations
    %%        from runtime to compiletime??
    [[string:join([f("~p(~w) -> ~p",
                     [mk_fn(d_enum_, EnumName), EnumValue, EnumSym])
                   || {EnumSym, EnumValue} <- EnumDef],
                  ";\n"),
      ".\n\n"]
     || {{enum, EnumName}, EnumDef} <- Defs].

format_initial_msgs(Defs) ->
    [format_initial_msg(MsgName, MsgDef, Defs)
     || {{msg, MsgName}, MsgDef} <- Defs].

format_initial_msg(MsgName, MsgDef, Defs) ->
    [f("~p() ->~n", [mk_fn(msg0_, MsgName)]),
     indent(4, format_initial_msg_record(4, MsgName, MsgDef, Defs)),
     ".\n\n"].

format_initial_msg_record(Indent, MsgName, MsgDef, Defs) ->
    MsgNameQLen = iolist_size(f("~p", [MsgName])),
    f("#~p{~s}",
      [MsgName, format_initial_msg_fields(Indent+MsgNameQLen+2, MsgDef, Defs)]).

format_initial_msg_fields(Indent, MsgDef, Defs) ->
    outdent_first(
      string:join(
        [case Field of
             #field{occurrence=repeated} ->
                 indent(Indent, f("~p = []", [FName]));
             #field{type={msg,FMsgName}} ->
                 FNameQLen = iolist_size(f("~p", [FName])),
                 {Type, FMsgDef} = lists:keyfind(Type, 1, Defs),
                 indent(Indent, f("~p = ~s",
                                  [FName,
                                   format_initial_msg_record(Indent+FNameQLen+2,
                                                             FMsgName, FMsgDef,
                                                             Defs)]))
         end
         || #field{name=FName, type=Type}=Field <- MsgDef,
            case Field of
                #field{occurrence=repeated} -> true;
                #field{occurrence=optional} -> false;
                #field{type={msg,_}}        -> true;
                _                           -> false
            end],
        ",\n")).

format_msg_decoders(Defs) ->
    [format_msg_decoder(MsgName, MsgDef) || {{msg, MsgName}, MsgDef} <- Defs].

format_msg_decoder(MsgName, MsgDef) ->
    [format_msg_decoder_read_field(MsgName, MsgDef),
     format_msg_decoder_reverse_toplevel(MsgName, MsgDef),
     format_field_decoders(MsgName, MsgDef),
     format_field_adders(MsgName, MsgDef),
     format_field_skippers(MsgName)].

format_msg_decoder_read_field(MsgName, MsgDef) ->
    [f("~p(Bin) ->~n", [mk_fn(d_msg_, MsgName)]),
     indent(4, f("Msg0 = ~s(),~n", [mk_fn(msg0_, MsgName)])),
     indent(4, f("~p(Bin, 0, 0, Msg0).~n", [mk_fn(d_read_field_def_, MsgName)])),
     "\n",
     f("~p(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n"
       "    ~p(Rest, N+1, X bsl (N*7) + Acc, Msg);~n",
       [mk_fn(d_read_field_def_, MsgName),
        mk_fn(d_read_field_def_, MsgName)]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n"
       "    Key = X bsl (N*7) + Acc,~n", [mk_fn(d_read_field_def_, MsgName)]),
     f("    case Key bsr 3 of~n"
       "~s~n"
       "        _ ->~n"
       "            case Key band 7 of  %% WireType~n"
       "                0 -> skip_varint_~s(Rest, Msg);~n"
       "                1 -> skip_64_~s(Rest, Msg);~n"
       "                2 -> skip_length_delimited_~s(Rest, 0, 0, Msg);~n"
       "                5 -> skip_32_~s(Rest, Msg)~n"
       "            end~n"
       "    end;~n",
       [format_read_field_cases(MsgName, MsgDef),
        MsgName,MsgName,MsgName,MsgName]),
     f("~p(<<>>, 0, 0, Msg) ->~n"
       "    ~p(Msg).~n~n",
       [mk_fn(d_read_field_def_, MsgName),
        mk_fn(d_reverse_toplevel_fields_, MsgName)])].

format_read_field_cases(MsgName, MsgDef) ->
    [begin
         Params = mk_field_decoder_params(FieldDef),
         indent(8, f("~p -> ~p(Rest~sMsg);~n",
                     [FNum, mk_fn(d_field_, MsgName, FName),
                      if Params == "" -> ", ";
                         true         -> [", ", Params, ", "]
                      end]))
     end
     || #field{fnum=FNum, name=FName}=FieldDef <- MsgDef].

mk_field_decoder_params(#field{is_packed=false, type=Type}) ->
    case Type of
        sint32   -> "0, 0"; %% varint-based
        sint64   -> "0, 0"; %% varint-based
        int32    -> "0, 0"; %% varint-based
        int64    -> "0, 0"; %% varint-based
        uint32   -> "0, 0"; %% varint-based
        uint64   -> "0, 0"; %% varint-based
        bool     -> "0, 0"; %% varint-based
        {enum,_} -> "0, 0"; %% varint-based
        fixed32  -> "";
        sfixed32 -> "";
        float    -> "";
        fixed64  -> "";
        sfixed64 -> "";
        double   -> "";
        string   -> "0, 0"; %% varint-based
        bytes    -> "0, 0"; %% varint-based
        {msg,_}  -> "0, 0"  %% varint-based
    end.

format_field_decoders(MsgName, MsgDef) ->
    [[format_field_decoder(MsgName, FieldDef), "\n"]
     || FieldDef <- MsgDef].

%% FIXME: is_packed=true
format_field_decoder(MsgName, #field{is_packed=false, type=Type}=FieldDef) ->
    case Type of
        sint32   -> format_vi_based_field_decoder(MsgName, FieldDef);
        sint64   -> format_vi_based_field_decoder(MsgName, FieldDef);
        int32    -> format_vi_based_field_decoder(MsgName, FieldDef);
        int64    -> format_vi_based_field_decoder(MsgName, FieldDef);
        uint32   -> format_vi_based_field_decoder(MsgName, FieldDef);
        uint64   -> format_vi_based_field_decoder(MsgName, FieldDef);
        bool     -> format_vi_based_field_decoder(MsgName, FieldDef);
        {enum,_} -> format_vi_based_field_decoder(MsgName, FieldDef);
        fixed32  -> format_uf32_field_decoder(MsgName, FieldDef);
        sfixed32 -> format_sf32_field_decoder(MsgName, FieldDef);
        float    -> format_float_field_decoder(MsgName, FieldDef);
        fixed64  -> format_uf64_field_decoder(MsgName, FieldDef);
        sfixed64 -> format_sf64_field_decoder(MsgName, FieldDef);
        double   -> format_double_field_decoder(MsgName, FieldDef);
        string   -> format_vi_based_field_decoder(MsgName, FieldDef);
        bytes    -> format_vi_based_field_decoder(MsgName, FieldDef);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, FieldDef)
    end.

format_vi_based_field_decoder(MsgName, #field{type=Type, name=FName}) ->
    BValueExpr = "X bsl (N*7) + Acc",
    {FValueCode, RestVar} =
        case Type of
            sint32 ->
                {[f("    BValue = ~s,~n", [BValueExpr]),
                  f("    FValue = ~s,~n", [fmt_decode_zigzag(13, "BValue")])],
                 "Rest"};
            sint64 ->
                {[f("    BValue = ~s,~n", [BValueExpr]),
                  f("    FValue = ~s,~n", [fmt_decode_zigzag(13, "BValue")])],
                 "Rest"};
            int32 ->
                {f("    ~s,~n", [fmt_uint_to_int(BValueExpr, "FValue", 32)]),
                 "Rest"};
            int64 ->
                {f("    ~s,~n", [fmt_uint_to_int(BValueExpr, "FValue", 64)]),
                 "Rest"};
            uint32 ->
                {f("    FValue = ~s,~n", [BValueExpr]), "Rest"};
            uint64 ->
                {f("    FValue = ~s,~n", [BValueExpr]), "Rest"};
            bool ->
                {f("    FValue = ((~s) =/= 0),~n", [BValueExpr]), "Rest"};
            {enum, EnumName} ->
                {[f("    ~s,~n", [fmt_uint_to_int(BValueExpr, "EnumValue", 32)]),
                  f("    FValue = ~p(EnumValue),~n",[mk_fn(d_enum_,EnumName)])],
                 "Rest"};
            string ->
                {[f("    Len = ~s,~n", [BValueExpr]),
                  f("    <<Utf8:Len/binary, Rest2/binary>> = Rest,~n"),
                  f("    FValue = unicode:characters_to_list(Utf8,unicode),~n")],
                 "Rest2"};
            bytes ->
                {[f("    Len = ~s,~n", [BValueExpr]),
                  f("    <<FValue:Len/binary, Rest2/binary>> = Rest,~n")],
                 "Rest2"};
            {msg,Msg2Name} ->
                {[f("    Len = ~s,~n", [BValueExpr]),
                  f("    <<MsgBytes:Len/binary, Rest2/binary>> = Rest,~n"),
                  f("    FValue = decode_msg(MsgBytes, ~p),~n", [Msg2Name])],
                 "Rest2"}
        end,
    [f("~s(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n"
       "    ~s(Rest, N+1, X bsl (N*7) + Acc, Msg);~n",
       [mk_fn(d_field_, MsgName, FName),
        mk_fn(d_field_, MsgName, FName)]),
     f("~s(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n",
       [mk_fn(d_field_, MsgName, FName)]),
     f("~s",
       [FValueCode]),
     f("    NewMsg = ~s(FValue, Msg),~n", [mk_fn(add_field_, MsgName, FName)]),
     f("    ~p(~s, 0, 0, NewMsg).~n",
       [mk_fn(d_read_field_def_, MsgName), RestVar])].

fmt_uint_to_int(SrcStr, ResultVar, NumBits) ->
    f("<<~s:~w/signed-native>> = <<(~s):~p/unsigned-native>>",
      [ResultVar, NumBits, SrcStr, NumBits]).

fmt_decode_zigzag(Indent, VarStr) ->
    [f(              "if ~s band 1 =:= 0 -> ~s bsr 1;~n", [VarStr, VarStr]),
     indent(Indent,f("   true -> -((~s + 1) bsr 1)~n", [VarStr])),
     indent(Indent,f("end"))].

format_uf32_field_decoder(MsgName, FieldDef) ->
    format_f_field_decoder(MsgName, 32, "little", FieldDef).

format_sf32_field_decoder(MsgName, FieldDef) ->
    format_f_field_decoder(MsgName, 32, "signed-little", FieldDef).

format_float_field_decoder(MsgName, FieldDef) ->
    format_f_field_decoder(MsgName, 32, "little-float", FieldDef).

format_uf64_field_decoder(MsgName, FieldDef) ->
    format_f_field_decoder(MsgName, 64, "little", FieldDef).

format_sf64_field_decoder(MsgName, FieldDef) ->
    format_f_field_decoder(MsgName, 64, "signed-little", FieldDef).

format_double_field_decoder(MsgName, FieldDef) ->
    format_f_field_decoder(MsgName, 64, "little-float", FieldDef).

format_f_field_decoder(MsgName, BitLen, BitType, #field{name=FName}) ->
    [f("~s(<<Value:~p/~s, Rest/binary>>, Msg) ->~n",
       [mk_fn(d_field_, MsgName, FName), BitLen, BitType]),
     f("    NewMsg = ~s(Value, Msg),~n", [mk_fn(add_field_, MsgName, FName)]),
     f("    ~p(Rest, 0, 0, NewMsg).~n",
       [mk_fn(d_read_field_def_, MsgName)])].


format_msg_decoder_reverse_toplevel(MsgName, MsgDef) ->
    MsgNameQLen = iolist_size(f("~p", [MsgName])),
    FieldsToReverse = [F || F <- MsgDef, F#field.occurrence == repeated],
    if FieldsToReverse /= [] ->
            FieldMatchings =
                outdent_first(
                  string:join(
                    [indent(8+2+MsgNameQLen, f("~p=F~s", [FName, FName]))
                     || #field{name=FName} <- FieldsToReverse],
                    ",\n")),
            FieldReversings =
                outdent_first(
                  string:join(
                    [indent(8+2+MsgNameQLen, f("~p=lists:reverse(F~s)",
                                               [FName, FName]))
                     || #field{name=FName} <- FieldsToReverse],
                    ",\n")),
            [f("~p(~n", [mk_fn(d_reverse_toplevel_fields_, MsgName)]),
             indent(8, f("#~p{~s}=Msg) ->~n", [MsgName, FieldMatchings])),
             indent(4, f("Msg#~p{~s}.~n~n", [MsgName, FieldReversings]))];
       true ->
            [f("~p(Msg) ->~n", [mk_fn(d_reverse_toplevel_fields_, MsgName)]),
             indent(4, f("Msg.~n~n"))]
    end.

format_field_adders(MsgName, MsgDef) ->
    %% FIXME: do cleverer things here, depending on type of the field,
    %%        and also depending on the field's occurrence
    %% for now, just make the generated code compile
    [[f("~s(_NewValue, Msg) ->~n", [mk_fn(add_field_, MsgName, FName)]),
      f("    Msg.~n~n")]
     || #field{name=FName} <- MsgDef].

format_field_skippers(MsgName) ->
    [format_varint_skipper(MsgName),
     format_length_delimited_skipper(MsgName),
     format_32bit_skipper(MsgName),
     format_64bit_skipper(MsgName)].

format_varint_skipper(MsgName) ->
    SkipFn = mk_fn(skip_varint_, MsgName),
    [f("~s(<<0:1, _:7, Rest/binary>>, Msg) ->~n", [SkipFn]),
     f("    ~s(Rest, 0, 0, Msg);~n", [mk_fn(d_read_field_def_, MsgName)]),
     f("~s(<<1:1, _:7, Rest/binary>>, Msg) ->~n", [SkipFn]),
     f("    ~s(Rest, Msg).~n~n", [SkipFn])].

format_length_delimited_skipper(MsgName) ->
    SkipFn = mk_fn(skip_length_delimited_, MsgName),
    [f("~s(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n", [SkipFn]),
     f("    ~s(Rest, N+1, X bsl (N*7) + Acc, Msg);~n", [SkipFn]),
     f("~s(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n", [SkipFn]),
     f("    Length = X bsl (N*7) + Acc,~n"),
     f("    <<_:Length/binary, Rest2/binary>> = Rest,~n"),
     f("    ~s(Rest2, 0, 0, Msg).~n~n", [mk_fn(d_read_field_def_, MsgName)])].

format_32bit_skipper(MsgName) -> format_bit_skipper(MsgName, 32).

format_64bit_skipper(MsgName) -> format_bit_skipper(MsgName, 64).

format_bit_skipper(MsgName, BitLen) ->
    SkipFn = mk_fn(skip_, BitLen, MsgName),
    [f("~s(<<_:~w, Rest/binary>>, Msg) ->~n", [SkipFn, BitLen]),
     f("    ~s(Rest, 0, 0, Msg).~n~n",  [mk_fn(d_read_field_def_, MsgName)])].

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

format_efield(I, #field{name=N, fnum=F, rnum=R, type=T,
                        occurrence=O, is_packed=IsPacked,
                        opts=Opts}) ->

    [indent(I, f("#field{name=~w, fnum=~w, rnum=~w, type=~w,~n", [N,F,R,T])),
     indent(I, f("       occurrence=~w, is_packed=~p,~n", [O, IsPacked])),
     indent(I, f("       opts=~p}", [Opts]))].


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

mk_fn(Prefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Suffix])).

mk_fn(Prefix, Middlefix, Suffix) when is_integer(Middlefix) ->
    mk_fn(Prefix, list_to_atom(integer_to_list(Middlefix)), Suffix);
mk_fn(Prefix, Middlefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Middlefix, "_", Suffix])).

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
