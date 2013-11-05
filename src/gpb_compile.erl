%%% Copyright (C) 2010-2013  Tomas Abrahamsson
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

-module(gpb_compile).
%-compile(export_all).
-export([file/1, file/2]).
-export([msg_defs/2, msg_defs/3]).
-export([format_error/1, format_warning/1]).
-export([c/0, c/1]). % Command line interface, halts vm---don't use from shell!
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").

-record(ft, {type, occurrence, is_packed}).
-record(anres, %% result of analysis
        {
          used_types          :: set(),  %% gpb_field_type()
          known_msg_size      :: dict(), %% MsgName -> Size | undefined
          msg_occurrences     :: dict(), %% MsgName -> [occurrence()]
          fixlen_types        :: set(),  %% #ft{}
          num_packed_fields   :: integer(),
          num_fields          :: dict(), %% MsgName -> integer()
          d_field_pass_method :: dict()  %% MsgName -> pass_as_record |
                                         %%            pass_as_params
        }).

-define(f(Fmt),        io_lib:format(Fmt, [])).
-define(f(Fmt, Args),  io_lib:format(Fmt, Args)).
-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

%% Varints are processed 7 bits at a time.
%% We can expect that we have processed this number of bits before
%% we expect to see the last varint byte, which must have msb==0.
%% 64 - 7 = 57.
-define(NB, 57).

%% @spec file(File) -> ok | {error, Reason}
%% @equiv file(File, [])
file(File) ->
    file(File, []).

%% @spec file(File, Opts) -> CompRet
%%            File = string()
%%            Opts = [Opt]
%%            Opt  = {type_specs, boolean()} | type_specs |
%%                   {verify, optionally | always | never} |
%%                   {copy_bytes, true | false | auto | integer() | float()} |
%%                   {strings_as_binaries, boolean()} | strings_as_binaries |
%%                   {defs_as_proplists, boolean()} | defs_as_proplists |
%%                   {descriptor,boolean()} | descriptor |
%%                   {maps,boolean()} | maps |
%%                   {nif,boolean()} | nif |
%%                   {load_nif, LoadNif} |
%%                   {i, directory()} |
%%                   {o, directory()} |
%%                   {o_erl, directory()} | {o_hrl, directory()} |
%%                   {o_nif_cc, directory()} |
%%                   binary | to_msg_defs |
%%                   return | return_warnings | return_errors |
%%                   report | report_warnings | report_errors |
%%                   include_as_lib | use_packages
%%            CompRet = ModRet | BinRet | ErrRet
%%            ModRet = ok | {ok, Warnings}
%%            BinRet = {ok, ModuleName, Code} |
%%                     {ok, ModuleName, Code, Warnings}
%%            ErrRet = {error, Reason} | {error,Reason,Warnings}
%%            ModuleName  = atom()
%%            Code = binary() | ErlAndNifCode
%%            ErlAndNifCode = [CodeType]
%%            CodeType = {erl, binary()} | {nif, NifCcText}
%%            NifCcText = binary()
%%            LoadNif = string()
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
%% The `verify' option whether or not to generate code for verifying
%% that, during encoding, values are of correct type and within range.
%% The `verify' option can have the following values:
%% <dl>
%%    <dt>`always'</dt><dd>Generate code that unconditionally
%%        verifies values.</dd>
%%    <dt>`never'</dt><dd>Generate code that never verifies
%%        values time. Encoding will fail if a value of the wrong
%%        type is supplied. This includes forgetting to set a required
%%        message field. Encoding may silently truncate values out of
%%        range for some types.</dd>
%%    <dt>`optionally'</dt><dd>Generate an `encode_msg/2' that accepts
%%        the run-time option `verify' or `{verify,boolean()}' for specifying
%%        whether or not to verify values.</dd>
%% </dl>
%%
%% Erlang value verfication either succeeds or crashes with the `error'
%% `{gpb_type_error,Reason}'. Regardless of the `verify' option,
%% a function, `verify_msg/1' is always generated.
%%
%% The `copy_bytes' option specifies whether when decoding data of
%% type `bytes' (or strings if the `strings_as_binaries' is set), the
%% decoded bytes should be copied or not.  Copying requires the
%% `binary' module, which first appeared in Erlang R14A. When not
%% copying decoded bytes, they will become sub binaries of the larger
%% input message binary. This may tie up the memory in the input
%% message binary longer than necessary after it has been
%% decoded. Copying the decoded bytes will avoid creating sub
%% binaries, which will in make it possible to free the input message
%% binary earlier. The `copy_bytes' option can have the following values:
%% <dl>
%%   <dt>`false'</dt><dd>Never copy bytes/(sub-)binaries.</dd>
%%   <dt>`true'</dt><dd>Always copy bytes/(sub-)binaries.</dd>
%%   <dt>`auto'</dt><dd>Copy bytes/(sub-)binaries if the beam vm,
%%           on which the compiler (this module) is running,
%%           has the `binary:copy/1' function. (This is the default)</dd>
%%   <dt>integer() | float()</dt><dd>Copy the bytes/(sub-)binaries if the
%%           message this many times or more larger than the size of the
%%           bytes/(sub-)binary.</dd>
%% </dl>
%%
%% The `strings_as_binaries' option specifies whether strings should
%% be returned from decoding as strings (list of unicode code points),
%% or as binaries (UTF-8 encoded). The `copy_bytes' option applies
%% to strings as well, when the `strings_as_binaries' option is set.
%% Upon encoding, both binaries and lists are accepted.
%%
%% The `defs_as_proplists' option changes the generated introspection
%% functions `find_msg_def' and `get_msg_defs' to return the description
%% of each message field as a proplist, instead of as a `#field{}' record.
%% The purpose is to make the generated code completely independent
%% of gpb, at compile-time (it is already independent at run-time).
%% The keys of the proplist are the names of the record fields in the
%% `#field{}' record.  See also {@link gpb:proplists_to_field_records()}
%% and related functions for conversion functions between these two
%% formats.
%%
%% The `descriptor' option specifies whether or not to generate a
%% function, descriptor/0, which returns a binary that describes the
%% proto file(s) contents according to the protobuf's `descriptor.proto'.
%% The default is to not generate such a description.  The generated
%% description binary is most likely not identical to what `protoc'
%% would generate, but the contents is roughly equivalent.
%%
%% The `{o,directory()}' option specifies directory to use for storing
%% the generated `.erl' and `.hrl' files. Default is the same
%% directory as for the proto `File'.
%%
%% The `{o_erl,directory()}', `{o_hrl,directory()}', `{o_nif_cc,directory()}',
%% options specify output directory for where to generate the `.erl'
%% and `.hrl' files respectively, and for the NIF C++ file,
%% if the `nif' option is specified. The `{o_erl,directory()}' option
%% overrides any `{o,directory()}' option, and similarly for the
%% other file-type specific output options.
%%
%% The `maps' option will generate a protobuf encoder/decoder that
%% uses maps instead of records. It will not generate any `.hrl' file,
%% and the functions `encode_msg', `merge_msgs' and `verify_msg' will
%% take the message name as an additional parameter. The introspection
%% will generate message field descriptions as maps instead of as
%% `#field{}' records, unless, of course `defs_as_proplists' is specified,
%% in which case they will be proplists instead. This option is not
%% compatible with the `nif' option.
%%
%% The `nif' option will cause the compiler to generate code which
%% decoding code is nif C++ code. The generated can be linked with the
%% Google protobuf C++ library.  Read the file `README.nif-cc' for
%% more info. This option is not compatible with the `maps' option;
%% the generated C++ decoding code would still create records.
%%
%% The `binary' option will cause the generated and compiled code be
%% returned as a binary. No files will be written. The return value
%% will be on the form `{ok,Mod,Code}' or `{ok,Mod,Code,Warnings}'
%% if the compilation is succesful. This option may be useful
%% e.g. when generating test cases. In case the `nif' option is set,
%% the `Code' will be a list of tuples: `{erl,binary()}' which
%% contains the erlang object byte code, and `{nif,binary()}' which
%% contains the C++ code. You will have to compile the C++ code with a
%% C++ compiler, before you can use the erlang code.
%%
%% The `to_msg_defs' option will result in `{ok,MsgDefs}' or
%% `{ok,MsgDefs,Warns}' being returned if the compilation is succesful.
%% The returned message definitions can be used with the
%% {@link msg_defs/2} or {@link msg_defs/3} functions.
%%
%% <dl>
%%   <dt>`report_errors'/`report_warnings'</dt>
%%   <dd>Causes errors/warnings to be printed as they occur.</dd>
%%   <dt>`report'</dt>
%%   <dd>This is a short form for both `report_errors' and
%%       `report_warnings'.</dd>
%%   <dt>`return_errors'</dt>
%%   <dd>If this flag is set, then  `{error,ErrorList,WarningList}' is
%%       returned when there are errors.</dd>
%%   <dt>`return_warnings'</dt>
%%   <dd>If  this  flag  is set, then an extra field containing `WarningList'
%%       is added to the tuples returned on success.</dd>
%%   <dt>`return'</dt>
%%   <dd>This is a short form for both `return_errors' and
%%       `return_warnings'.</dd>
%% </dl>
%%
%% See {@link format_error/1} for a way to turn an error <i>Reason</i> to
%% plain text.
%%
%% If the `include_as_lib' option is set, the generated code will include
%% gpb.hrl as a library, which is necessary if dependencies are managed with
%% Rebar. Otherwise, the header file is included directly and must be located
%% in the path, which is default behaviour.
%%
%% The `use_packages' option instructs gpb to prepend the name of a package
%% to every message it contains. If no package is defined, nothing will be
%% prepended. This enables the reference of messages in other packages which
%% would otherwise not be possible. However, for reasons of backward
%% compatibility, this option is disabled by default.
file(File, Opts1) ->
    Opts2 = normalize_return_report_opts(Opts1),
    case parse_file(File, Opts2) of
        {ok, Defs} ->
            Ext = filename:extension(File),
            Mod = list_to_atom(possibly_prefix_mod(filename:basename(File, Ext),
                                                   Opts2)),
            DefaultOutDir = filename:dirname(File),
            Opts3 = Opts2 ++ [{o,DefaultOutDir}],
            msg_defs(Mod, Defs, Opts3);
        {error, Reason} = Error ->
            possibly_report_error(Error, Opts2),
            case proplists:get_bool(return_warnings, Opts2) of
                true  -> {error, Reason, []};
                false -> Error
            end
    end.

normalize_return_report_opts(Opts1) ->
    Opts2 = expand_opt(return, [return_warnings, return_errors], Opts1),
    Opts3 = expand_opt(report, [report_warnings, report_errors], Opts2),
    Opts4 = unless_defined_set(return_warnings, report_warnings, Opts3),
    Opts5 = unless_defined_set(return_errors,   report_errors, Opts4),
    Opts5.

expand_opt(OptionToTestFor, OptionsToExpandTo, Opts) ->
    case proplists:get_bool(OptionToTestFor, Opts) of
        true  -> OptionsToExpandTo ++ delete_bool_opt(OptionToTestFor, Opts);
        false -> Opts
    end.

delete_bool_opt(OptToDelete, Opts) ->
    %% Boolean opts can be defined both as [opt] and as [{opt, true|false}],
    %% delete both type of occurrences.
    lists:keydelete(OptToDelete, 1, Opts -- [OptToDelete]).

unless_defined_set(OptionToTestFor, OptionToSet, Opts) ->
    case proplists:get_bool(OptionToTestFor, Opts) of
        true  -> Opts;
        false -> [OptionToSet | Opts]
    end.

possibly_prefix_mod(BaseNameNoExt, Opts) ->
    case proplists:get_value(module_name_prefix, Opts) of
        undefined ->
            BaseNameNoExt;
        Prefix ->
            lists:concat([Prefix, BaseNameNoExt])
    end.


%% @spec msg_defs(Mod, Defs) -> CompRet
%% @equiv msg_defs(Mod, Defs, [])
msg_defs(Mod, Defs) ->
    msg_defs(Mod, Defs, []).

%% @spec msg_defs(Mod, Defs, Opts) -> CompRet
%%            Mod  = atom()
%%            Defs = [Def]
%%            Def = {{enum, EnumName}, Enums} |
%%                  {{msg, MsgName}, MsgFields}
%%            EnumName = atom()
%%            Enums = [{Name, integer()}]
%%            Name = atom()
%%            MsgName = atom()
%%            MsgFields = [#field{}]
%%
%% @doc
%% Compile a list of pre-parsed definitions to file or to a binary.
%% See {@link file/2} for information on options and return values.
msg_defs(Mod, Defs0, Opts0) ->
    {IsAcyclic, Defs} = try_topsort_defs(Defs0),
    possibly_probe_defs(Defs, Opts0),
    {Warns, Opts1} = possibly_adjust_typespec_opt(IsAcyclic, Opts0),
    Opts2 = normalize_return_report_opts(Opts1),
    AnRes = analyze_defs(Defs, Opts2),
    case verify_opts(Opts2) of
        ok ->
            Res1 = do_msg_defs(Defs, clean_module_name(Mod), AnRes, Opts2),
            return_or_report_warnings_or_errors(Res1, Warns, Opts2,
                                                get_output_format(Opts2));
        {error, OptError} ->
            return_or_report_warnings_or_errors({error, OptError}, [], Opts2,
                                                get_output_format(Opts2))
    end.

do_msg_defs(Defs, Mod, AnRes, Opts) ->
    case get_output_format(Opts) of
        msg_defs ->
            {ok, Defs};
        binary ->
            ErlTxt = format_erl(Mod, Defs, AnRes, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            compile_to_binary(Mod, Defs, ErlTxt, NifTxt, Opts);
        file ->
            ErlTxt = format_erl(Mod, Defs, AnRes, Opts),
            HrlTxt = possibly_format_hrl(Mod, Defs, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            ErlOutDir = get_erl_outdir(Opts),
            HrlOutDir = get_hrl_outdir(Opts),
            NifCcOutDir = get_nif_cc_outdir(Opts),
            Erl   = filename:join(ErlOutDir, atom_to_list(Mod) ++ ".erl"),
            Hrl   = filename:join(HrlOutDir, atom_to_list(Mod) ++ ".hrl"),
            NifCc = filename:join(NifCcOutDir, atom_to_list(Mod) ++ ".nif.cc"),
            case {file_write_file(Erl, ErlTxt, Opts),
                  possibly_write_file(Hrl, HrlTxt, Opts),
                  possibly_write_file(NifCc, NifTxt, Opts)} of
                {ok, ok, ok}       -> ok;
                {{error, R}, _, _} -> {error, {write_failed, Erl, R}};
                {_, {error, R}, _} -> {error, {write_failed, Erl, R}};
                {_, _, {error, R}} -> {error, {write_failed, NifCc,  R}}
            end
    end.

verify_opts(Opts) ->
    case {get_records_or_maps_by_opts(Opts), proplists:get_bool(nif, Opts)} of
        {maps, true} ->
            {error, {option_error, {not_supported, maps_and_nif}}};
        _ ->
            ok
    end.

return_or_report_warnings_or_errors(Res, ExtraWarns, Opts, OutFormat) ->
    Res2 = merge_warns(Res, ExtraWarns, OutFormat),
    possibly_report_warnings(Res2, Opts),
    possibly_report_error(Res2, Opts),
    return_warnings_or_errors(Res2, Opts).

merge_warns(ok, Warns, _OutFmt)                  -> {ok, Warns};
merge_warns({ok, Warns1}, Warns2, file)          -> {ok, Warns2++Warns1};
merge_warns({ok, MsgDefs}, Warns, msg_defs)      -> {ok, MsgDefs, Warns};
merge_warns({ok, M, B}, Warns, binary)           -> {ok, M, B, Warns};
merge_warns({ok, M, B, Warns1}, Warns2, binary)  -> {ok, M, B, Warns2++Warns1};
merge_warns({error, R}, Warns, _OutFmt)          -> {error, R, Warns};
merge_warns({error, R, Warns1}, Warns2, _OutFmt) -> {error, R, Warns2++Warns1};
merge_warns(error, Warns, binary) ->
    erlang:error({internal_error, ?MODULE,
                  generated_code_failed_to_compile, Warns}).

possibly_report_warnings(Result, Opts) ->
    Warns = case Result of
                {error, _Reason, Ws} -> Ws;
                {ok, _M, _B, Ws}     -> Ws;
                {ok, _Defs, Ws}      -> Ws;
                {ok, Ws}             -> Ws
            end,
    case proplists:get_bool(report_warnings, Opts) of
        true  -> lists:foreach(fun report_warning/1, Warns);
        false -> ok
    end.

report_warning(Warn) ->
    io:format("~s~n", [format_warning(Warn)]).

possibly_report_error(Res, Opts) ->
    case {Res, proplists:get_bool(report_errors, Opts)} of
        {{error, _Reason, _Warns}, true} ->
            io:format("~s~n", [format_error(Res)]);
        {{error, _Reason}, true} ->
            io:format("~s~n", [format_error(Res)]);
        _ ->
            ok
    end.

return_warnings_or_errors(Res, Opts) ->
    case proplists:get_bool(return_warnings, Opts) of
        true ->
            Res;
        false ->
            case Res of
                {ok, Mod, Bin, _Warns} -> {ok, Mod, Bin};
                {ok, MsgDefs, _Warns}  -> {ok, MsgDefs};
                {ok, _Warns}           -> ok;
                {error, R, _Warns}     -> {error, R}
            end
    end.

get_output_format([binary | _])              -> binary;
get_output_format([{binary, true} | _])      -> binary;
get_output_format([to_msg_defs | _])         -> msg_defs;
get_output_format([{to_msg_defs, true} | _]) -> msg_defs;
get_output_format([_ | Rest])                -> get_output_format(Rest);
get_output_format([])                        -> file.

get_erl_outdir(Opts) ->
    proplists:get_value(o_erl, Opts, get_outdir(Opts)).

get_hrl_outdir(Opts) ->
    proplists:get_value(o_hrl, Opts, get_outdir(Opts)).

get_nif_cc_outdir(Opts) ->
    proplists:get_value(o_nif_cc, Opts, get_outdir(Opts)).

get_outdir(Opts) ->
    proplists:get_value(o, Opts, ".").

clean_module_name(Mod) ->
    Clean = re:replace(atom_to_list(Mod), "[.]", "_", [global, {return,list}]),
    list_to_atom(Clean).

%% @spec format_error({error, Reason} | Reason) -> io_list()
%%           Reason = term()
%%
%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link msg_defs/2}.
format_error({error, Reason, _Warns}) -> fmt_err(Reason);
format_error({error, Reason})         -> fmt_err(Reason);
format_error(Reason)                  -> fmt_err(Reason).

%% Note: do NOT include trailing newline (\n or ~n)
fmt_err({option_error, {not_supported, maps_and_nif}}) ->
    ?f("Options maps and nif are mutually exclusive");
fmt_err({parse_error, FileName, {Line, Module, ErrInfo}}) ->
    ?f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)]);
fmt_err({scan_error, FileName, {Line, Module, ErrInfo}}) ->
    ?f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)]);
fmt_err({import_not_found, Import}) ->
    ?f("Could not find import file ~p", [Import]);
fmt_err({read_failed, File, Reason}) ->
    ?f("failed to read ~p: ~s (~p)", [File, file:format_error(Reason), Reason]);
fmt_err({post_process, Reasons}) ->
    gpb_parse:format_post_process_error({error, Reasons});
fmt_err({write_failed, File, Reason}) ->
    ?f("failed to write ~s: ~s (~p)", [File, file:format_error(Reason),Reason]);
fmt_err(X) ->
    ?f("Unexpected error ~p", [X]).

%% @spec format_warning(Reason) -> io_list()
%%           Reason = term()
%%
%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link msg_defs/2}.
%% @end
%% Note: do NOT include trailing newline (\n or ~n)
format_warning(cyclic_message_dependencies) ->
    ?f("Warning: omitting type specs due to cyclic message references.");
format_warning(X) ->
    case io_lib:deep_char_list(X) of
        true  -> X;
        false -> ?f("Warning: Unknown warning: ~p", [X])
    end.

%% @doc Command line interface for the compiler.
%% With no proto file to compile, print a help message and exit.
-spec c() -> no_return().
c() ->
    c([undefined]).

%% @doc This function is intended as a command line interface for the compiler.
%% Call it from the command line as follows:
%% ```
%%    erl <erlargs> [gpb-opts] -s gpb_compile c File.proto ...
%%    erl <erlargs> -s gpb_compile c File.proto ... -extra [gpb-opts]
%% '''
%% The `<erlargs>' can be `-noshell -noinput +B -boot start_clean -pa SomeDir'
%%
%% The following options are supported:
%% <dl>
%%   <dt>`-IDir' `-I Dir'</dt>
%%   <dd>Specify include directory.
%%       Option may be specified more than once to specify
%%       several include directories.</dd>
%%   <dt>`-o Dir'</dt>
%%   <dd>Specify output directory for where to generate
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl</dd>
%%   <dt>`-o-erl Dir' | `-o-hrl Dir' | `-o-nif-cc Dir'</dt>
%%   <dd>Specify output directory for where to generate
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl respectively,
%%       and for the NIF C++ file, if the `-nif' option is specified.
%%       The `-o-erl Dir' option overrides any `-o Dir' option, and
%%       similarly for the other file-type specific output options.</dd>
%%   <dt>`-v optionally | always | never'</dt>
%%   <dd>Specify how the generated encoder should
%%       verify the message to be encoded.</dd>
%%   <dt>`-nif'</dt>
%%   <dd>Generate nifs for linking with the protobuf C(++) library.</dd>
%%   <dt>`-load_nif FunctionDefinition'</dt>
%%   <dd>Specify `FunctionDefinition' as the text that defines the
%%       function `load_nif/0'.  This is called as the `on_load'
%%       hook for loading the NIF.  See also the doc for the `load_nif'
%%       option in the {@link file/2} function.</dd>
%%   <dt>`-c true | false | auto | integer() | float()'</dt>
%%   <dd>Specify how or when the generated decoder should
%%       copy fields of type `bytes'. See the `copy_bytes' option
%%       for the function {@link file/2} for more info.</dd>
%%   <dt>`-strbin'</dt>
%%   <dd>Specify that decoded strings should be returend as binaries,
%%       instead of as strings (lists).</dd>
%%   <dt>`-pldefs'</dt>
%%   <dd>Specify that introspection functions shall return proplists
%%       instead of `#field{}' records, to make the generated code
%%       completely free of even compile-time dependencies to gpb.</dd>
%%   <dt>`-msgprefix Prefix'</dt>
%%   <dd>Prefix each message with `Prefix'. This can be useful to
%%       when including different sub-projects that have colliding
%%       message names.</dd>
%%   <dt>`-modprefix Prefix'</dt>
%%   <dd>Prefix each module with `Prefix'. Normally the module name of
%%       the generated code is based on the name of the `.proto' file.
%%       This option prepends a prefix to the module name, which can be
%%       useful when including different sub-projects that have
%%       colliding proto file names.</dd>
%%   <dt>`-il'</dt>
%%   <dd>Generate code that include gpb.hrl using `-include_lib'
%%       instad of `-include', which is the default.</dd>
%%   <dt>`-type'</dt>
%%   <dd>Enables `::Type()' annotations in the generated .hrl file.</dd>
%%   <dt>`-descr'</dt>
%%   <dd>Generate self-description information.</dd>
%%   <dt>`-maps'</dt>
%%   <dd>Generate code that will accept and produce maps instead of
%%       records. No .hrl file will be generated. See the `maps' option
%%       for the function {@link file/2} for more info.</dd>
%%   <dt>`--help' or `-h'</dt>
%%   <dd>Show help.</dd>
%%   <dt>`--version' or `-V'</dt>
%%   <dd>Show the version number of gpb.</dd>
%% </dl>
%% If several files are specified, each is compiled individually, no
%% checking is done for instance for multiply defined messages or
%% fields across files, such as the `protoc' does.
-spec c([string() | atom()]) -> no_return().
c([F | _]=Files) when is_atom(F); is_list(F) -> %% invoked with -s or -run
    erlang:system_flag(backtrace_depth, 32),
    FileNames = [if File == undefined -> undefined;
                    is_atom(File)     -> atom_to_list(File);
                    is_list(File)     -> File
                 end
                 || File <- Files],
    Args = init:get_arguments(),
    PlainArgs = init:get_plain_arguments(),
    Opts1 = parse_opts(Args, PlainArgs),
    Opts2 = [report_warnings, report_errors] ++ Opts1,
    Results = [case determine_cmdline_op(Opts2, FileName) of
                   error  ->
                       show_help(),
                       halt(1);
                   show_help  ->
                       show_help(),
                       halt(0);
                   show_version  ->
                       show_version(),
                       halt(0);
                   compile ->
                       file(FileName, Opts2)
               end
               || FileName <- FileNames],
    case lists:usort(Results) of
        [ok]  -> halt(0);
        _Errs -> halt(1)
    end,
    timer:sleep(infinity). %% give init:stop time to do its work

determine_cmdline_op(Opts, FileName) ->
    Help = lists:member(help, Opts) orelse
        FileName == "-h" orelse
        FileName == "--help",
    Vsn = lists:member(version, Opts) orelse
        FileName == "-V" orelse
        FileName == "--version",
    case {Help, Vsn} of
        {true, _} -> show_help;
        {_, true} -> show_version;
        _         -> if FileName == undefined -> error;
                        is_list(FileName)     -> compile
                     end
    end.

show_help() ->
    io:format(
      "gpb version ~s~n"
      "Usage: erl <erlargs> [gpb-opts] -s ~p c <ProtoFile>.proto~n"
      "   or: erl <erlargs> -s ~p c <ProtoFile>.proto -extra [gpb-opts]~n"
      "Typical erlargs = -noshell -noinput +B -boot start_clean -pa SomeDir~n"
      "~n"
      "Recognized gpb-opts: (see the edoc for ~p for further details)~n"
      "    -IDir   -I Dir~n"
      "          Specify include directory.~n"
      "          Option may be specified more than once to specify~n"
      "          several include directories.~n"
      "    -o Dir~n"
      "          Specify output directory for where to generate~n"
      "          the <ProtoFile>.erl and <ProtoFile>.hrl~n"
      "    -o-erl Dir | -o-hrl Dir | -o-nif-cc Dir~n"
      "          Specify output directory for where to generate~n"
      "          the <ProtoFile>.erl and <ProtoFile>.hrl respectively,~n"
      "          and for the NIF C++ file, if the -nif option is specified~n"
      "          The -o-erl Dir option overrides any -o Dir option, and~n"
      "          similarly for the other file-type specific output options.~n"
      "    -nif~n"
      "          Generate nifs for linking with the protobuf C(++) library.~n"
      "    -load_nif FunctionDefinition~n"
      "          Specify FunctionDefinition as the text that defines the~n"
      "          function load_nif/0.  This is called as the -on_load.~n"
      "          hook for loading the NIF.~n"
      "    -v optionally | always | never~n"
      "          Specify how the generated encoder should~n"
      "          verify the message to be encoded.~n"
      "    -c true | false | auto | integer() | float()~n"
      "          Specify how or when the generated decoder should~n"
      "          copy fields of type bytes.~n"
      "    -strbin~n"
      "          Specify that decoded strings should be returend as binaries,~n"
      "          instead of as strings (lists).~n"
      "    -pldefs~n"
      "          Specify that introspection functions shall return proplists~n"
      "          instead of #field{} records, to make the generated code~n"
      "          completely free of even compile-time dependencies to gpb.~n"
      "    -msgprefix Prefix~n"
      "          Prefix each message with Prefix.~n"
      "    -modprefix Prefix~n"
      "          Prefix the module name with Prefix.~n"
      "    -il~n"
      "          Generate code that includes gpb.hrl using -include_lib~n"
      "          instad of -include, which is the default.~n"
      "    -type~n"
      "          Enables `::Type()' annotations in the generated .hrl file.~n"
      "    -descr~n"
      "          Generate self-description information.~n"
      "    -maps~n"
      "          Generate code that will accept and produce maps instead of~n"
      "          records.~n"
      "    --help  -h~n"
      "          Show help~n"
      "    --version  -V~n"
      "          Show version~n"
      , [gpb:version_as_string(), ?MODULE, ?MODULE, ?MODULE]).

show_version() ->
    io:format("gpb version ~s~n", [gpb:version_as_string()]).

parse_opts(Args, PlainArgs) ->
    arg_zf(fun parse_opt/1, Args) ++ plain_arg_zf(fun parse_opt/1, PlainArgs).

parse_opt({"I", [Dir]})          -> {true, {i,Dir}};
parse_opt({"I"++Dir, []})        -> {true, {i,Dir}};
parse_opt({"o", [Dir]})          -> {true, {o,Dir}};
parse_opt({"o-erl", [Dir]})      -> {true, {o_erl,Dir}};
parse_opt({"o-hrl", [Dir]})      -> {true, {o_hrl,Dir}};
parse_opt({"o-nif-cc", [Dir]})   -> {true, {o_nif_cc,Dir}};
parse_opt({"nif",[]})            -> {true, nif};
parse_opt({"load_nif",[S]})      -> {true, {load_nif,S}};
parse_opt({"v", ["optionally"]}) -> {true, {verify,optionally}};
parse_opt({"v", ["always"]})     -> {true, {verify,always}};
parse_opt({"v", ["never"]})      -> {true, {verify,never}};
parse_opt({"c", ["true"]})       -> {true, {copy_bytes,true}};
parse_opt({"c", ["false"]})      -> {true, {copy_bytes,false}};
parse_opt({"c", ["auto"]})       -> {true, {copy_bytes,auto}};
parse_opt({"c", [NStr]})         -> case string_to_number(NStr) of
                                        {ok, Num} -> {true, {copy_bytes,Num}};
                                        error     -> false
                                    end;
parse_opt({"strbin", []})        -> {true, strings_as_binaries};
parse_opt({"pldefs", []})        -> {true, defs_as_proplists};
parse_opt({"msgprefix", [P]})    -> {true, {msg_name_prefix, P}};
parse_opt({"modprefix", [P]})    -> {true, {module_name_prefix, P}};
parse_opt({"il", []})            -> {true, include_as_lib};
parse_opt({"type", []})          -> {true, type_specs};
parse_opt({"descr", []})         -> {true, {descriptor,true}};
parse_opt({"maps", []})          -> {true, maps};
parse_opt({"h", _})              -> {true, help};
parse_opt({"-help", _})          -> {true, help};
parse_opt({"V", _})              -> {true, version};
parse_opt({"-version", _})       -> {true, version};
parse_opt(_X)                    -> false.

string_to_number(S) ->
    try {ok, list_to_integer(S)}
    catch error:badarg ->
            try {ok, list_to_float(S)}
            catch error:badarg -> error
            end
    end.

arg_zf(ZFFun, Args) ->
    lists:zf(ZFFun, [{atom_to_list(Opt), OptArgs} || {Opt, OptArgs} <- Args]).

plain_arg_zf(ZFFun, PlainArgs) ->
    lists:zf(ZFFun, plainargs_to_args(PlainArgs)).

plainargs_to_args(["-"++Opt1, "-"++_=Opt2 | Rest]) ->
    [{Opt1, []} | plainargs_to_args([Opt2 | Rest])];
plainargs_to_args(["-"++Opt | OptArgsAndRest]) ->
    {OptArgs, Rest} = plainoptargs_to_args(OptArgsAndRest, []),
    [{Opt, OptArgs} | plainargs_to_args(Rest)];
plainargs_to_args([]) ->
    [].

plainoptargs_to_args(["-"++_ | _]=Rest, Acc) ->
    {lists:reverse(Acc), Rest};
plainoptargs_to_args([OptArg | Rest], Acc) ->
    plainoptargs_to_args(Rest, [OptArg | Acc]);
plainoptargs_to_args([], Acc) ->
    {lists:reverse(Acc), []}.

parse_file(FName, Opts) ->
    case parse_file_and_imports(FName, Opts) of
        {ok, {Defs1, _AllImported}} ->
            case gpb_parse:post_process(Defs1, Opts) of
                {ok, Defs2} ->
                    {ok, Defs2};
                {error, Reasons} ->
                    {error, {post_process, Reasons}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_file_and_imports(FName, Opts) ->
    parse_file_and_imports(FName, [FName], Opts).

parse_file_and_imports(FName, AlreadyImported, Opts) ->
    case locate_import(FName, Opts) of
        {ok, Contents} ->
            %% Add to AlreadyImported to prevent trying to import it again: in
            %% case we get an error we don't want to try to reprocess it later
            %% (in case it is multiply imported) and get the error again.
            AlreadyImported2 = [FName | AlreadyImported],
            case scan_and_parse_string(binary_to_list(Contents), FName) of
                {ok, Defs} ->
                    Imports = gpb_parse:fetch_imports(Defs),
                    read_and_parse_imports(Imports,AlreadyImported2,Defs,Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

scan_and_parse_string(S, FName) ->
    case gpb_scan:string(S) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens++[{'$end', 999}]) of
                {ok, Result} ->
                    {ok, Result};
                {error, {_Line, _Module, _ErrInfo}=Reason} ->
                    {error, {parse_error, FName, Reason}}
            end;
        {error, {_Line0, _Module, _ErrInfo}=Reason, _Line1} ->
            {error, {scan_error, FName, Reason}}
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
            case file_read_file(File, Opts) of
                {ok,B} ->
                    {ok, B};
                {error, Reason} ->
                    {error, {read_failed, File, Reason}}
            end;
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
    {[], Opts};
possibly_adjust_typespec_opt(false=_IsAcyclic, Opts) ->
    case get_type_specs_by_opts(Opts) of
        true  ->
            %% disable `type_specs' option
            Opts1 = delete_bool_opt(type_specs, Opts),
            {[cyclic_message_dependencies], Opts1};
        false ->
            {[], Opts}
    end.

%% -- analysis -----------------------------------------------------

analyze_defs(Defs, Opts) ->
    #anres{used_types          = find_used_types(Defs),
           known_msg_size      = find_msgsizes_known_at_compile_time(Defs),
           msg_occurrences     = find_msg_occurrences(Defs),
           fixlen_types        = find_fixlen_types(Defs),
           num_packed_fields   = find_num_packed_fields(Defs),
           num_fields          = find_num_fields(Defs),
           d_field_pass_method = compute_decode_field_pass_methods(Defs, Opts)}.

find_used_types(Defs) ->
    fold_msg_fields(fun(_MsgName, #field{type=Type}, Acc) ->
                            sets:add_element(Type, Acc)
                    end,
                    sets:new(),
                    Defs).

find_msg_occurrences(Defs) ->
    fold_msg_fields(fun(_MsgName, #field{type=Type, occurrence=Occ}, Acc) ->
                            case Type of
                                {msg,SubMsgName} ->
                                    add_occurrence(SubMsgName, Occ, Acc);
                                _Other ->
                                    Acc
                            end
                    end,
                    dict:new(),
                    Defs).

add_occurrence(MsgName, Occurrence, D) ->
    case dict:find(MsgName, D) of
        {ok, Occurrences0} ->
            dict:store(MsgName, lists:usort([Occurrence | Occurrences0]), D);
        error ->
            dict:store(MsgName, [Occurrence], D)
    end.

find_fixlen_types(Defs) ->
    fold_msg_fields(
      fun(_, #field{type=Type, occurrence=Occ}=FieldDef, Acc) ->
              IsPacked = is_packed(FieldDef),
              FixlenTypeInfo = #ft{type       = Type,
                                   occurrence = Occ,
                                   is_packed  = IsPacked},
              case Type of
                  fixed32  -> sets:add_element(FixlenTypeInfo, Acc);
                  sfixed32 -> sets:add_element(FixlenTypeInfo, Acc);
                  float    -> sets:add_element(FixlenTypeInfo, Acc);
                  fixed64  -> sets:add_element(FixlenTypeInfo, Acc);
                  sfixed64 -> sets:add_element(FixlenTypeInfo, Acc);
                  double   -> sets:add_element(FixlenTypeInfo, Acc);
                  _        -> Acc
              end
      end,
      sets:new(),
      Defs).

find_num_packed_fields(Defs) ->
    fold_msg_fields(fun(_MsgName, FieldDef, Acc) ->
                            case is_packed(FieldDef) of
                                true  -> Acc + 1;
                                false -> Acc
                            end
                    end,
                    0,
                    Defs).

fold_msg_fields(Fun, InitAcc, Defs) ->
    lists:foldl(fun({{msg, MsgName}, Fields}, Acc) ->
                        lists:foldl(fun(Field, FAcc) ->
                                            Fun(MsgName, Field, FAcc)
                                    end,
                                    Acc,
                                    Fields);
                   (_Def, Acc) ->
                        Acc
                end,
                InitAcc,
                Defs).

find_num_fields(Defs) ->
    lists:foldl(fun({MsgName, MsgDef}, Acc) ->
                        dict:store(MsgName, length(MsgDef), Acc)
                end,
                dict:new(),
                [{MsgName, MsgDef} || {{msg, MsgName}, MsgDef} <- Defs]).

find_msgsizes_known_at_compile_time(Defs) ->
    T = ets:new(gpb_msg_sizes, [set, public]),
    [find_msgsize(MsgName, Defs, T) || {{msg,MsgName},_Fields} <- Defs],
    Result = dict:from_list(ets:tab2list(T)),
    ets:delete(T),
    Result.

find_msgsize(MsgName, Defs, T) ->
    case ets:lookup(T, MsgName) of
        [] ->
            {{msg,MsgName}, Fields} = lists:keyfind({msg,MsgName}, 1, Defs),
            Result = find_msgsize_2(Fields, 0, Defs, T),
            ets:insert(T, {MsgName, Result}),
            Result;
        [{MsgName, Result}] ->
            Result
    end.

find_msgsize_2([#field{occurrence=repeated} | _Rest], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#field{occurrence=optional} | _Rest], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#field{type=Type, fnum=FNum} | Rest], AccSize, Defs, T) ->
    FKey = gpb:encode_varint((FNum bsl 3) bor gpb:encode_wiretype(Type)),
    FKeySize = byte_size(FKey),
    case Type of
        sint32   -> undefined;
        sint64   -> undefined;
        int32    -> undefined;
        int64    -> undefined;
        uint32   -> undefined;
        uint64   -> undefined;
        bool     -> find_msgsize_2(Rest, AccSize+FKeySize+1, Defs, T);
        {enum,EnumName} ->
            case all_enum_values_encode_to_same_size(EnumName, Defs) of
                {yes, ESize} ->
                    find_msgsize_2(Rest, AccSize+FKeySize+ESize, Defs, T);
                no ->
                    undefined
            end;
        fixed64  -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        sfixed64 -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        double   -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        string   -> undefined;
        bytes    -> undefined;
        {msg,MsgName} ->
            case find_msgsize(MsgName, Defs, T) of
                MsgSize when is_integer(MsgSize) ->
                    SizeOfLength = byte_size(gpb:encode_varint(MsgSize)),
                    SubMsgFieldSize = FKeySize + SizeOfLength + MsgSize,
                    find_msgsize_2(Rest, AccSize + SubMsgFieldSize, Defs, T);
                undefined ->
                    undefined
            end;
        fixed32  -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T);
        sfixed32 -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T);
        float    -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T)
    end;
find_msgsize_2([], AccSize, _Defs, _T) ->
    AccSize.


all_enum_values_encode_to_same_size(EnumName, Defs) ->
    {{enum,EnumName}, EnumDef} = lists:keyfind({enum,EnumName}, 1, Defs),
    EnumSizes = [begin
                     <<N:32/unsigned-native>> = <<Value:32/signed-native>>,
                     byte_size(gpb:encode_varint(N))
                 end
                 || {_EnumSym, Value} <- EnumDef],
    case lists:usort(EnumSizes) of
        [Size] -> {yes, Size};
        _      -> no
    end.

compute_decode_field_pass_methods(Defs, Opts) ->
    lists:foldl(fun({MsgName, MsgDef}, D) ->
                        PassHow = d_field_pass_method(MsgName, MsgDef, Opts),
                        dict:store(MsgName, PassHow, D)
                end,
                dict:new(),
                [{MsgName, MsgDefs} || {{msg, MsgName}, MsgDefs} <- Defs]).


d_field_pass_method(MsgName, MsgDef, Opts) ->
    %% Allow overriding options, mainly intended for testing
    case proplists:get_value({field_pass_method,MsgName}, Opts) of
        undefined ->
            case proplists:get_value(field_pass_method, Opts) of
                undefined ->
                    d_field_pass_method(MsgDef);
                Method when Method==pass_as_record; Method==pass_as_params ->
                    Method
            end;
        Method when Method==pass_as_record; Method==pass_as_params ->
            Method
    end.

d_field_pass_method(MsgDef) ->
    %% Compute estimated costs:
    %% Either passing a message record, or pass the fields as parameters
    %% to the functions, one parameter for each field, then as the last
    %% operation, stuff all parameters into a record.
    %%
    %% There are different advantages and disadvantages:
    %% - Updating fields in a record means the vm will have to verify
    %%   that the term is a record (for each time a field is parsed/added)
    %% - Passing the fields eliminates the cost above, but for each
    %%   (non-tail-recursive) function call, the field-parameters will
    %%   be saved to the stack, then restored after the call.
    %%   Such function calls, are: call to unicode:characters_to_list
    %%   for strings, calls to parse sub messages or packed fields and
    %%   final top-level calls to lists:reverse for repeated fields.
    NF = length(MsgDef), %% num fields (awk-istic terminology)
    if NF >= 250 ->
            pass_as_record; %% Functions can take at most 255 arguments
       NF == 0 ->
            pass_as_params;
       true ->
            NumSubMsgFields = length([x || #field{type={msg,_}} <- MsgDef]),
            IsMsgDominatedBySubMsgs = NumSubMsgFields / NF > 0.5,
            if IsMsgDominatedBySubMsgs, NF >= 100 ->
                    pass_as_record;
               true ->
                    pass_as_params
            end
    end.


%% -- generating code ----------------------------------------------

format_erl(Mod, Defs, AnRes, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    AsLib = proplists:get_bool(include_as_lib, Opts),
    iolist_to_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s on ~w~n",
          [?MODULE, gpb:version_as_string(), calendar:local_time()]),
       ?f("-module(~w).~n", [Mod]),
       "\n",
       case get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([encode_msg/1, encode_msg/2]).~n");
           maps    -> ?f("-export([encode_msg/2, encode_msg/3]).~n")
       end,
       ?f("-export([decode_msg/2]).~n"),
       case get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([merge_msgs/2]).~n");
           maps    -> ?f("-export([merge_msgs/3]).~n")
       end,
       case get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([verify_msg/1]).~n");
           maps    -> ?f("-export([verify_msg/2]).~n")
       end,
       ?f("-export([get_msg_defs/0]).~n"),
       ?f("-export([get_msg_names/0]).~n"),
       ?f("-export([get_enum_names/0]).~n"),
       ?f("-export([find_msg_def/1, fetch_msg_def/1]).~n"),
       ?f("-export([find_enum_def/1, fetch_enum_def/1]).~n"),
       format_enum_value_symbol_converter_exports(Defs),
       ?f("-export([get_package_name/0]).~n"),
       [?f("-export([descriptor/0]).~n") || get_gen_descriptor_by_opts(Opts)],
       ?f("-export([gpb_version_as_string/0, gpb_version_as_list/0]).~n"),
       "\n",
       [["-on_load(load_nif/0).\n",
         "-export([load_nif/0]). %% for debugging of nif loading\n",
         "\n"]
        || DoNif],
       case get_records_or_maps_by_opts(Opts) of
           records ->
               [?f("-include(\"~s.hrl\").~n", [Mod]),
                case get_field_format_by_opts(Opts) of
                    fields_as_records ->
                        if AsLib ->
                                ?f("-include_lib(\"gpb/include/gpb.hrl\").~n");
                           not AsLib ->
                                ?f("-include(\"gpb.hrl\").~n")
                        end;
                    fields_as_proplists ->
                        ""
                end];
           maps ->
               ""
       end,
       "\n",
       [[?f("~s~n", [format_load_nif(Mod, Opts)]),
         "\n"]
        || DoNif],
       %% Enabling inlining seems to cause performance to drop drastically
       %% I've seen decoding performance go down from 76000 msgs/s
       %% to about 10000 msgs/s for a set of mixed message samples.
       %% f("-compile(inline).~n"),
       %%
       format_encoders_top_function(Defs, Opts),
       "\n",
       ?f("~s~n", [format_encoders(Defs, AnRes, Opts)]),
       "\n",
       format_decoders_top_function(Defs),
       "\n\n",
       if DoNif ->
               ?f("~s~n", [format_nif_decoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               ?f("~s~n", [format_decoders(Defs, AnRes, Opts)])
       end,
       "\n",
       ?f("~s~n", [format_msg_merge_code(Defs, AnRes, Opts)]),
       "\n",
       format_verifiers_top_function(Defs, Opts),
       "\n",
       ?f("~s~n", [format_verifiers(Defs, AnRes, Opts)]),
       "\n",
       format_introspection(Defs, Opts),
       "\n",
       ?f("gpb_version_as_string() ->~n"),
       ?f("    \"~s\".~n", [gpb:version_as_string()]),
       "\n",
       ?f("gpb_version_as_list() ->~n"),
       ?f("    ~s.~n", [gpb_version_as_list_pretty()])]).

gpb_version_as_list_pretty() ->
    %% The version "2.2-60-gb0decf3" is rendered with ~w
    %% as: [2,2,0,0,60,[103,98,48,100,101,99,102,51]]
    %% this function renders it as [2,2,0,0,60,"gb0decf3"]
    %% which is exactly the same, but easier for humans to read.
    {V, SubStrs} =
        lists:mapfoldl(fun(N, Acc) when is_integer(N) -> {N, Acc};
                          (S, Acc) when is_list(S) -> {x, Acc++[S]}
                       end,
                       [],
                       gpb:version_as_list()),
    S2 = remove_whitespaces(?ff("~p~n", [V])),
    r_strs(S2, $x, SubStrs).

remove_whitespaces(S)  -> [C || C <- S, not is_whitespace_char(C)].
is_whitespace_char($\s) -> true;
is_whitespace_char($\t) -> true;
is_whitespace_char($\n) -> true;
is_whitespace_char(_)   -> false.

r_strs([M | Tl], M, [S|Rest]) -> ?ff("~p", [S]) ++ r_strs(Tl, M, Rest);
r_strs([C | Tl], M, SubStrs)  -> [C | r_strs(Tl, M, SubStrs)];
r_strs("", _M, [])            -> "".

%% -- encoders -----------------------------------------------------

format_encoders_top_function(Defs, Opts) ->
    Verify = proplists:get_value(verify, Opts, optionally),
    Mapping = get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(MsgName)]
                  end,
    [gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>') -> encode_msg(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>', '<Opts>') ->
               '<possibly-verify-msg>',
               case '<MsgOrMsgName>' of
                   '<msg-match>' -> 'encode'(Msg)
               end
       end,
       [replace_tree('<Opts>', case Verify of
                                   optionally -> ?expr(Opts);
                                   always     -> ?expr(_Opts);
                                   never      -> ?expr(_Opts)
                               end),
        splice_trees('<possibly-verify-msg>',
                     case Verify of
                         optionally ->
                             [?expr(case proplists:get_bool(verify, Opts) of
                                        true  -> verify_msg(Msg, '<MsgName>');
                                        false -> ok
                                    end)];
                         always ->
                             [?expr(verify_msg(Msg, '<MsgName>'))];
                         never ->
                             []
                     end),
        repeat_clauses('<msg-match>',
                       [[replace_tree('<msg-match>',
                                      case Mapping of
                                          records -> record_match(MsgName, []);
                                          maps    -> erl_syntax:atom(MsgName)
                                      end),
                         replace_term('encode', mk_fn(e_msg_, MsgName))]
                        || {{msg,MsgName}, _Fields} <- Defs]),
        replace_tree('<MsgOrMsgName>', case Mapping of
                                           records -> ?expr(Msg);
                                           maps    -> ?expr(MsgName)
                                       end),
        splice_trees('<MsgName>', MsgNameVars)])].

format_encoders(Defs, AnRes, Opts) ->
    [format_enum_encoders(Defs, AnRes),
     format_msg_encoders(Defs, Opts),
     format_special_field_encoders(Defs, AnRes),
     format_type_encoders(AnRes)
    ].

format_enum_encoders(Defs, #anres{used_types=UsedTypes}) ->
    [gpb_codegen:format_fn(
       mk_fn(e_enum_, EnumName),
       fun('<EnumSym>', Bin) -> <<Bin/binary, '<varint-bytes>'>> end,
       [repeat_clauses('<EnumSym>',
                       [begin
                            ViBytes = enum_to_binary_fields(EnumValue),
                            [replace_term('<EnumSym>', EnumSym),
                             splice_trees('<varint-bytes>', ViBytes)]
                        end
                        || {EnumSym, EnumValue} <- EnumDef])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_msg_encoders(Defs, Opts) ->
    [format_msg_encoder(MsgName, MsgDef, Opts)
     || {{msg, MsgName}, MsgDef} <- Defs].

format_msg_encoder(MsgName, [], _Opts) ->
    gpb_codegen:format_fn(
      mk_fn(e_msg_, MsgName),
      fun(_Msg) ->
              <<>>
      end);
format_msg_encoder(MsgName, MsgDef, Opts) ->
    FNames = [FName || #field{name=FName} <- MsgDef],
    FVars = [var_f_n(I) || I <- lists:seq(1, length(FNames))],
    BVars = [var_b_n(I) || I <- lists:seq(1, length(FNames)-1)] ++ [last],
    {EncodeExprs, _} =
        lists:mapfoldl(
          fun({NewBVar, Field, FVar}, PrevBVar) when NewBVar /= last ->
                  EncExpr = field_encode_expr(MsgName, Field, FVar, PrevBVar),
                  E = ?expr('<NewB>' = '<encode-expr>',
                            [replace_tree('<NewB>', NewBVar),
                             replace_tree('<encode-expr>', EncExpr)]),
                  {E, NewBVar};
             ({last, Field, FVar}, PrevBVar) ->
                  EncExpr = field_encode_expr(MsgName, Field, FVar, PrevBVar),
                  {EncExpr, dummy}
          end,
          ?expr(Bin),
          lists:zip3(BVars, MsgDef, FVars)),
    FnName = mk_fn(e_msg_, MsgName),
    [gpb_codegen:format_fn(
       FnName,
       fun(Msg) ->
               call_self(Msg, <<>>)
       end),
     "\n",
     gpb_codegen:format_fn(
       mk_fn(e_msg_, MsgName),
       fun('<msg-matching>', Bin) ->
               '<encode-param-exprs>'
       end,
       [replace_tree('<msg-matching>',
                     mapping_match(MsgName, lists:zip(FNames, FVars), Opts)),
        splice_trees('<encode-param-exprs>', EncodeExprs)])].

field_encode_expr(MsgName, Field, FVar, PrevBVar) ->
    FEncoder = mk_field_encode_fn_name(MsgName, Field),
    #field{occurrence=Occurrence, type=Type, fnum=FNum}=Field,
    Transforms = [replace_tree('<F>', FVar),
                  replace_term('<enc>', FEncoder),
                  replace_tree('<Bin>', PrevBVar),
                  splice_trees('<Key>', key_to_binary_fields(FNum, Type))],
    case Occurrence of
        optional ->
            ?expr(
               if '<F>' == undefined -> '<Bin>';
                  true -> '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>)
               end,
               Transforms);
        repeated ->
            ?expr(
               if '<F>' == [] -> '<Bin>';
                  true -> '<enc>'('<F>', '<Bin>')
               end,
               Transforms);
        required ->
            ?expr(
               '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>),
               Transforms)
    end.

mk_field_encode_fn_name(MsgName, #field{occurrence=repeated, name=FName}) ->
    mk_fn(e_field_, MsgName, FName);
mk_field_encode_fn_name(MsgName, #field{type={msg,_Msg}, name=FName}) ->
    mk_fn(e_mfield_, MsgName, FName);
mk_field_encode_fn_name(_MsgName, #field{type={enum,EnumName}}) ->
    mk_fn(e_enum_, EnumName);
mk_field_encode_fn_name(_MsgName, #field{type=sint32}) ->
    mk_fn(e_type_, sint);
mk_field_encode_fn_name(_MsgName, #field{type=sint64}) ->
    mk_fn(e_type_, sint);
mk_field_encode_fn_name(_MsgName, #field{type=uint32}) ->
    e_varint;
mk_field_encode_fn_name(_MsgName, #field{type=uint64}) ->
    e_varint;
mk_field_encode_fn_name(_MsgName, #field{type=Type}) ->
    mk_fn(e_type_, Type).

format_special_field_encoders(Defs, AnRes) ->
    [[format_field_encoder(MsgName, FieldDef, AnRes)
      || #field{occurrence=Occ, type=Type}=FieldDef <- MsgDef,
         Occ == repeated orelse is_msg_type(Type)]
     || {{msg,MsgName}, MsgDef} <- Defs].

is_msg_type({msg,_}) -> true;
is_msg_type(_)       -> false.

format_field_encoder(MsgName, #field{occurrence=Occurrence}=FieldDef, AnRes) ->
    [possibly_format_mfield_encoder(MsgName,
                                    FieldDef#field{occurrence=required},
                                    AnRes),
     case {Occurrence, is_packed(FieldDef)} of
         {repeated, false} -> format_repeated_field_encoder2(MsgName, FieldDef);
         {repeated, true}  -> format_packed_field_encoder2(MsgName, FieldDef);
         {optional, false} -> [];
         {required, false} -> []
     end].

possibly_format_mfield_encoder(MsgName, #field{type={msg,SubMsg}}=FieldDef,
                               AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FieldDef),
    case is_msgsize_known_at_generationtime(SubMsg, AnRes) of
        no ->
            gpb_codegen:format_fn(
              FnName,
              fun(Msg, Bin) ->
                      SubBin = '<encode-msg>'(Msg, <<>>),
                      Bin2 = e_varint(byte_size(SubBin), Bin),
                      <<Bin2/binary, SubBin/binary>>
              end,
              [replace_term('<encode-msg>', mk_fn(e_msg_, SubMsg))]);
        {yes, MsgSize} when MsgSize > 0 ->
            MsgSizeBytes = varint_to_binary_fields(MsgSize),
            gpb_codegen:format_fn(
              FnName,
              fun(Msg, Bin) ->
                      Bin2 = <<Bin/binary, '<msg-size>'>>,
                      '<encode-msg>'(Msg, Bin2)
              end,
              [splice_trees('<msg-size>', MsgSizeBytes),
               replace_term('<encode-msg>', mk_fn(e_msg_, SubMsg))]);
        {yes, 0} ->
            %% special case, there will not be any e_msg_<MsgName>/2 function
            %% generated, so don't call it.
            gpb_codegen:format_fn(
              FnName,
              fun(_Msg, Bin) -> <<Bin/binary, 0>> end)
    end;
possibly_format_mfield_encoder(_MsgName, _FieldDef, _Defs) ->
    [].

is_msgsize_known_at_generationtime(MsgName, #anres{known_msg_size=MsgSizes}) ->
    case dict:fetch(MsgName, MsgSizes) of
        MsgSize when is_integer(MsgSize) ->
            {yes, MsgSize};
        undefined ->
            no
    end.

format_repeated_field_encoder2(MsgName, #field{fnum=FNum, type=Type}=FDef) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(MsgName,
                                            FDef#field{occurrence=required}),
    KeyBytes = key_to_binary_fields(FNum, Type),
    gpb_codegen:format_fn(
      FnName,
      fun([Elem | Rest], Bin) ->
              Bin2 = <<Bin/binary, '<KeyBytes>'>>,
              Bin3 = '<encode-elem>'(Elem, Bin2),
              call_self(Rest, Bin3);
         ([], Bin) ->
              Bin
      end,
      [splice_trees('<KeyBytes>', KeyBytes),
       replace_term('<encode-elem>', ElemEncoderFn)]).

format_packed_field_encoder2(MsgName, #field{type=Type}=FDef) ->
    case packed_byte_size_can_be_computed(Type) of
        {yes, BitLen, BitType} ->
            format_knownsize_packed_field_encoder2(MsgName, FDef,
                                                   BitLen, BitType);
        no ->
            format_unknownsize_packed_field_encoder2(MsgName, FDef)
    end.

packed_byte_size_can_be_computed(fixed32)  -> {yes, 32, [little]};
packed_byte_size_can_be_computed(sfixed32) -> {yes, 32, [little,signed]};
packed_byte_size_can_be_computed(float)    -> {yes, 32, [little,float]};
packed_byte_size_can_be_computed(fixed64)  -> {yes, 64, [little]};
packed_byte_size_can_be_computed(sfixed64) -> {yes, 64, [little,signed]};
packed_byte_size_can_be_computed(double)   -> {yes, 64, [little,float]};
packed_byte_size_can_be_computed(_)        -> no.

format_knownsize_packed_field_encoder2(MsgName, #field{name=FName,
                                                       fnum=FNum}=FDef,
                                      BitLen, BitType) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    KeyBytes = key_to_binary_fields(FNum, bytes),
    PackedFnName = mk_fn(e_pfield_, MsgName, FName),
    [gpb_codegen:format_fn(
       FnName,
       fun(Elems, Bin) when Elems =/= [] ->
               Bin2 = <<Bin/binary, '<KeyBytes>'>>,
               Bin3 = e_varint(length(Elems) * '<ElemLen>', Bin2),
               '<encode-packed>'(Elems, Bin3);
          ([], Bin) ->
               Bin
       end,
       [splice_trees('<KeyBytes>', KeyBytes),
        replace_term('<ElemLen>', BitLen div 8),
        replace_term('<encode-packed>', PackedFnName)]),
     gpb_codegen:format_fn(
       PackedFnName,
       fun([Value | Rest], Bin) ->
               Bin2 = <<Bin/binary, Value:'<Size>'/'<BitType>'>>,
               call_self(Rest, Bin2);
          ([], Bin) ->
               Bin
       end,
       [replace_term('<Size>', BitLen),
        splice_trees('<BitType>', [erl_syntax:atom(T) || T <- BitType])])].

format_unknownsize_packed_field_encoder2(MsgName, #field{name=FName,
                                                         fnum=FNum}=FDef) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(MsgName,
                                            FDef#field{occurrence=required}),
    KeyBytes = key_to_binary_fields(FNum, bytes),
    PackedFnName = mk_fn(e_pfield_, MsgName, FName),
    [gpb_codegen:format_fn(
       FnName,
       fun(Elems, Bin) when Elems =/= [] ->
               SubBin = '<encode-packed>'(Elems, <<>>),
               Bin2 = <<Bin/binary, '<KeyBytes>'>>,
               Bin3 = e_varint(byte_size(SubBin), Bin2),
               <<Bin3/binary, SubBin/binary>>;
          ([], Bin) ->
               Bin
       end,
       [splice_trees('<KeyBytes>', KeyBytes),
        replace_term('<encode-packed>', PackedFnName)]),
     gpb_codegen:format_fn(
       PackedFnName,
       fun([Value | Rest], Bin) ->
               Bin2 = '<encode-elem>'(Value, Bin),
               call_self(Rest, Bin2);
          ([], Bin) ->
               Bin
       end,
       [replace_term('<encode-elem>', ElemEncoderFn)])].

format_type_encoders(AnRes) ->
    [format_varlength_field_encoders(AnRes),
     format_fixlength_field_encoders(AnRes),
     [format_varint_encoder() || is_varint_encoder_needed(AnRes)]].

format_varlength_field_encoders(#anres{used_types=UsedTypes}) ->
    [[format_sint_encoder()         || smember_any([sint32,sint64], UsedTypes)],
     [format_int_encoder(int32, 32) || smember(int32, UsedTypes)],
     [format_int_encoder(int64, 64) || smember(int64, UsedTypes)],
     [format_bool_encoder()         || smember(bool, UsedTypes)],
     [format_string_encoder()       || smember(string, UsedTypes)],
     [format_bytes_encoder()        || smember(bytes, UsedTypes)]].

format_fixlength_field_encoders(AnRes) ->
    NeedsFixed32  = needs_f_enc(fixed32, AnRes),
    NeedsSFixed32 = needs_f_enc(sfixed32, AnRes),
    NeedsFloat    = needs_f_enc(float, AnRes),
    NeedsFixed64  = needs_f_enc(fixed64, AnRes),
    NeedsSFixed64 = needs_f_enc(sfixed64, AnRes),
    NeedsDouble   = needs_f_enc(double, AnRes),
    [[format_fixed_encoder(fixed32,  32, [little])        || NeedsFixed32],
     [format_fixed_encoder(sfixed32, 32, [little,signed]) || NeedsSFixed32],
     [format_fixed_encoder(float,    32, [little,float])  || NeedsFloat],
     [format_fixed_encoder(fixed64,  64, [little])        || NeedsFixed64],
     [format_fixed_encoder(sfixed64, 64, [little,signed]) || NeedsSFixed64],
     [format_fixed_encoder(double,   64, [little,float])  || NeedsDouble]].

needs_f_enc(FixedType, #anres{used_types=UsedTypes, fixlen_types=FTypes}) ->
    %% If a fixlength-type occurs _only_ as a packed repeated field,
    %% we need not generate a special encoder-function for it
    case [FT || FT <- sets:to_list(FTypes), FT#ft.type == FixedType] of
        [#ft{occurrence=repeated, is_packed=true}] ->
            false;
        _ ->
            smember(FixedType, UsedTypes)
    end.

is_varint_encoder_needed(#anres{used_types=UsedTypes}=AnRes) ->
    TypesNeedingAVarintEncoder = [int32, int64, uint32, uint64, sint32, sint64,
                                  string, bytes],
    smember_any(TypesNeedingAVarintEncoder, UsedTypes) orelse
        any_packed_field_exists(AnRes) orelse
        at_least_one_submsg_with_size_not_known_at_compile_time_exists(AnRes).

any_packed_field_exists(#anres{num_packed_fields=0}) -> false;
any_packed_field_exists(#anres{num_packed_fields=_}) -> true.

at_least_one_submsg_with_size_not_known_at_compile_time_exists(AnRes) ->
    #anres{used_types=UsedTypes,
           known_msg_size=KnownSize} = AnRes,
    SubMsgNames = [MsgName || {msg,MsgName} <- sets:to_list(UsedTypes)],
    lists:any(fun(SubMsgName) -> dict:fetch(SubMsgName, KnownSize) == undefined
              end,
              SubMsgNames).

format_sint_encoder() ->
    gpb_codegen:format_fn(
      e_type_sint,
      fun(Value, Bin) when Value >= 0 ->
              e_varint(Value * 2, Bin);
         (Value, Bin) ->
              e_varint(Value * -2 - 1, Bin)
      end).

format_int_encoder(Type, BitLen) ->
    gpb_codegen:format_fn(
      mk_fn(e_type_, Type),
      fun(Value, Bin) when 0 =< Value, Value =< 127 ->
              <<Bin/binary, Value>>; %% fast path
         (Value, Bin) ->
              <<N:'<Sz>'/unsigned-native>> = <<Value:'<Sz>'/signed-native>>,
              e_varint(N, Bin)
      end,
      [replace_term('<Sz>', BitLen)]).

format_bool_encoder() ->
    gpb_codegen:format_fn(
      e_type_bool,
      fun(true, Bin)  -> <<Bin/binary, 1>>;
         (false, Bin) -> <<Bin/binary, 0>>
      end).

format_fixed_encoder(Type, BitLen, BitType) ->
    gpb_codegen:format_fn(
      mk_fn(e_type_, Type),
      fun(Value, Bin) ->
              <<Bin/binary, Value:'<Sz>'/'<T>'>>
      end,
      [replace_term('<Sz>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(T) || T <- BitType])]).

format_string_encoder() ->
    gpb_codegen:format_fn(
      e_type_string,
      fun(S, Bin) ->
              Utf8 = unicode:characters_to_binary(S),
              Bin2 = e_varint(byte_size(Utf8), Bin),
              <<Bin2/binary, Utf8/binary>>
      end).

format_bytes_encoder() ->
    gpb_codegen:format_fn(
      e_type_bytes,
      fun(Bytes, Bin) ->
              Bin2 = e_varint(byte_size(Bytes), Bin),
              <<Bin2/binary, Bytes/binary>>
      end).

format_varint_encoder() ->
    gpb_codegen:format_fn(
      e_varint,
      fun(N, Bin) when N =< 127 ->
              <<Bin/binary, N>>;
         (N, Bin) ->
              Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
              call_self(N bsr 7, Bin2)
      end).

%% -- decoders -----------------------------------------------------

format_decoders_top_function(Defs) ->
    gpb_codegen:format_fn(
      decode_msg,
      fun(Bin, MsgName) ->
              case MsgName of
                  '<MsgName>' -> '<decode-call>'(Bin)
              end
      end,
      [repeat_clauses('<MsgName>',
                      [[replace_term('<MsgName>', MsgName),
                        replace_term('<decode-call>', mk_fn(d_msg_, MsgName))]
                       || {{msg,MsgName}, _Fields} <- Defs])]).

format_decoders(Defs, AnRes, Opts) ->
    [format_enum_decoders(Defs, AnRes),
     format_msg_decoders(Defs, AnRes, Opts)].

format_enum_decoders(Defs, #anres{used_types=UsedTypes}) ->
    %% FIXME: enum values can be negative, but "raw" varints are positive
    %%        insert a 2-complement in the mapping in order to move computations
    %%        from run-time to compile-time??
    [gpb_codegen:format_fn(
       mk_fn(d_enum_, EnumName),
       fun('<EnumValue>') -> '<EnumSym>' end,
       [repeat_clauses('<EnumValue>',
                       [[replace_term('<EnumValue>', EnumValue),
                         replace_term('<EnumSym>', EnumSym)]
                        || {EnumSym, EnumValue} <- EnumDef])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_msg_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoder(MsgName, MsgDef, AnRes, Opts)
     || {{msg, MsgName}, MsgDef} <- Defs].

format_msg_decoder(MsgName, MsgDef, AnRes, Opts) ->
    [format_msg_decoder_read_field(MsgName, MsgDef, AnRes, Opts),
     format_field_decoders(MsgName, MsgDef, AnRes, Opts),
     format_field_skippers(MsgName, AnRes)].

format_msg_decoder_read_field(MsgName, MsgDef, AnRes, Opts) ->
    Key = ?expr(Key),
    Rest = ?expr(Rest),
    Params = decoder_params(MsgName, AnRes),
    Bindings = new_bindings([{'<Params>', Params},
                             {'<Key>', Key},
                             {'<Rest>', Rest}]),
    [format_msg_init_decoder(MsgName, MsgDef, AnRes, Opts),
     format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts),
     format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts)].

format_msg_init_decoder(MsgName, MsgDef, AnRes, Opts) ->
    gpb_codegen:format_fn(
      mk_fn(d_msg_, MsgName),
      fun(Bin) -> '<decode-field-fp>'(Bin, 0, 0, '<initial-params>') end,
      [replace_term('<decode-field-fp>', mk_fn(dfp_read_field_def_, MsgName)),
       splice_trees('<initial-params>',
                    msg_decoder_initial_params(MsgName, MsgDef, AnRes, Opts))]).

format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts) ->
    %% The fast-path decoder directly matches the minimal varint form
    %% of the field-number combined with the wiretype.
    %% Unrecognized fields fall back to the more generic decoder-loop
    Params = fetch_binding('<Params>', Bindings),
    gpb_codegen:format_fn(
      mk_fn(dfp_read_field_def_, MsgName),
      fun('<precomputed-binary-match>', Z1, Z2, '<Params>') ->
              '<calls-to-field-decoding>';
         (<<>>, 0, 0, '<Params>') ->
              '<finalize-result>';
         (Other, Z1, Z2, '<Params>') ->
              '<decode-general>'(Other, Z1, Z2, '<Params>')
      end,
      [splice_trees('<Params>', Params),
       repeat_clauses(
         '<precomputed-binary-match>',
         [[replace_tree('<precomputed-binary-match>', BinMatch),
           replace_tree('<calls-to-field-decoding>', FnCall)]
          || {BinMatch, FnCall} <- decoder_fp(Bindings, MsgName, MsgDef)]),
       replace_tree('<finalize-result>',
                    decoder_finalize_result(Params, MsgName, MsgDef, AnRes,
                                            Opts)),
       replace_term('<decode-general>', mk_fn(dg_read_field_def_, MsgName))]).

format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts) ->
    %% The more general field selecting decoder
    %% Stuff that ends up here: non-minimal varint forms and field to skip
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    gpb_codegen:format_fn(
      mk_fn(dg_read_field_def_, MsgName),
      fun(<<1:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>') when N < (32-7) ->
              call_self('<Rest>', N+7, X bsl N + Acc, '<Params>');
         (<<0:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>') ->
              '<Key>' = X bsl N + Acc,
              '<calls-to-field-decoding-or-skip>';
         (<<>>, 0, 0, '<Params>') ->
              '<finalize-result>'
      end,
      [replace_tree('<Key>', Key),
       replace_tree('<Rest>', Rest),
       splice_trees('<Params>', Params),
       replace_tree('<calls-to-field-decoding-or-skip>',
                    decoder_field_calls(Bindings, MsgName, MsgDef, AnRes)),
       replace_tree('<finalize-result>',
                    decoder_finalize_result(Params, MsgName, MsgDef, AnRes,
                                            Opts))]).

msg_decoder_initial_params(MsgName, MsgDef, AnRes, Opts) ->
    FNVExprs = [case Occurrence of
                    repeated -> {FName, [],        ?expr([])};
                    required -> {FName, undefined, ?expr(undefined)};
                    optional -> {FName, undefined, ?expr(undefined)}
                end
                || #field{name=FName, occurrence=Occurrence} <- MsgDef],
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            [Expr || {_FName, _Value, Expr} <- FNVExprs];
        pass_as_record ->
            case get_records_or_maps_by_opts(Opts) of
                records ->
                    [record_create(
                       MsgName,
                       [{FName, Expr} || {FName, Value, Expr} <- FNVExprs,
                                         Value /= undefined])];
                maps ->
                    [map_create(
                       [{FName, Expr} || {FName, _Value, Expr} <- FNVExprs])]
            end
    end.

decoder_params(MsgName, AnRes) ->
    NumFields = get_num_fields(MsgName, AnRes),
    case get_field_pass(MsgName, AnRes) of
        pass_as_params -> [var_f_n(I) || I <- lists:seq(1, NumFields)];
        pass_as_record -> [?expr(Msg)]
    end.

%% compute info for the fast-path field recognition/decoding-call
decoder_fp(Bindings, MsgName, MsgDef) ->
    Rest = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    [begin
         BMatch = ?expr(<<'<field-and-wiretype-bytes>', '<Rest>'/binary>>,
                        [splice_trees('<field-and-wiretype-bytes>',
                                      varint_to_binary_fields(Selector)),
                         replace_tree('<Rest>', Rest)]),
         FnCall = ?expr('decode_field'('<Rest>', Z1, Z2, '<Params>'),
                        [replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         splice_trees('<Params>', Params)]),
         {BMatch, FnCall}
     end
     || {Selector, DecodeFn} <- decoder_field_selectors(MsgName, MsgDef)].

decoder_field_calls(Bindings, MsgName, []=_MsgDef, _AnRes) ->
    Key = fetch_binding('<Key>', Bindings),
    WiretypeExpr = ?expr('<Key>' band 7, [replace_tree('<Key>', Key)]),
    Bindings1 = add_binding({'<wiretype-expr>', WiretypeExpr}, Bindings),
    decoder_skip_calls(Bindings1, MsgName);
decoder_field_calls(Bindings, MsgName, MsgDef, AnRes) ->
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    SkipCalls = decoder_field_calls(Bindings, MsgName, [], AnRes),
    FieldSelects = decoder_field_selectors(MsgName, MsgDef),
    ?expr(case '<Key>' of
              '<selector>' -> 'decode_field'('<Rest>', 0, 0, '<Params>');
              _            -> '<skip-calls>'
       end,
       [replace_tree('<Key>', Key),
        repeat_clauses('<selector>',
                       [[replace_term('<selector>', Selector),
                         replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         splice_trees('<Params>', Params)]
                        || {Selector, DecodeFn} <- FieldSelects]),
        replace_tree('<skip-calls>', SkipCalls)]).

decoder_skip_calls(Bindings, MsgName) ->
    WiretypeExpr = fetch_binding('<wiretype-expr>', Bindings),
    RestExpr = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    ?expr(case '<wiretype-expr>' of
              0 -> skip_vi('<Rest>', 0, 0, '<Params>');
              1 -> skip_64('<Rest>', 0, 0, '<Params>');
              2 -> skip_ld('<Rest>', 0, 0, '<Params>');
              5 -> skip_32('<Rest>', 0, 0, '<Params>')
          end,
          [replace_tree('<wiretype-expr>', WiretypeExpr),
           replace_tree('<Rest>', RestExpr),
           splice_trees('<Params>', Params),
           replace_term(skip_vi, mk_fn(skip_varint_, MsgName)),
           replace_term(skip_64, mk_fn(skip_64_, MsgName)),
           replace_term(skip_ld, mk_fn(skip_length_delimited_, MsgName)),
           replace_term(skip_32, mk_fn(skip_32_, MsgName))]).

decoder_field_selectors(MsgName, MsgDef) ->
    [begin
         Wiretype = case is_packed(FieldDef) of
                        true  -> gpb:encode_wiretype(bytes);
                        false -> gpb:encode_wiretype(Type)
                    end,
         Selector = (FNum bsl 3) bor Wiretype,
         DecodeFn = mk_fn(d_field_, MsgName, FName),
         {Selector, DecodeFn}
     end
     || #field{fnum=FNum, type=Type, name=FName}=FieldDef <- MsgDef].

decoder_finalize_result(Params, MsgName, MsgDef, AnRes, Opts) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            mapping_create(
              MsgName,
              [begin
                   #field{name=FName, occurrence=Occurrence}=Field,
                   FValueExpr =
                       case Occurrence of
                           required -> Param;
                           optional -> Param;
                           repeated -> ?expr(lists:reverse('<Param>'),
                                             [replace_tree('<Param>', Param)])
                       end,
                   {FName, FValueExpr}
               end
               || {Field, Param} <- lists:zip(MsgDef, Params)],
              Opts);
        pass_as_record ->
            MsgVar = hd(Params),
            mapping_update(
              MsgVar,
              MsgName,
              [begin
                   FieldAccess = mapping_access(MsgVar, MsgName, FName, Opts),
                   FValueExpr = ?expr(lists:reverse('<Param>'),
                                      [replace_tree('<Param>', FieldAccess)]),
                   {FName, FValueExpr}
               end
               || #field{name=FName, occurrence=repeated} <- MsgDef],
             Opts)
    end.

format_field_decoders(MsgName, MsgDef, AnRes, Opts) ->
    [[format_field_decoder(MsgName, Field, AnRes, Opts), "\n"]
     || Field <- MsgDef].

format_field_decoder(MsgName, Field, AnRes, Opts) ->
    case is_packed(Field) of
        false -> format_non_packed_field_decoder(MsgName, Field, AnRes, Opts);
        true  -> format_packed_field_decoder(MsgName, Field, AnRes, Opts)
    end.

format_non_packed_field_decoder(MsgName, #field{type=Type}=Field, AnRes, Opts)->
    case Type of
        sint32   -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        sint64   -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        int32    -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        int64    -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        uint32   -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        uint64   -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        bool     -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        {enum,_} -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        fixed32  -> format_fixlen_field_decoder(MsgName, Field, AnRes, Opts);
        sfixed32 -> format_fixlen_field_decoder(MsgName, Field, AnRes, Opts);
        float    -> format_fixlen_field_decoder(MsgName, Field, AnRes, Opts);
        fixed64  -> format_fixlen_field_decoder(MsgName, Field, AnRes, Opts);
        sfixed64 -> format_fixlen_field_decoder(MsgName, Field, AnRes, Opts);
        double   -> format_fixlen_field_decoder(MsgName, Field, AnRes, Opts);
        string   -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts)
    end.

format_packed_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #field{name=FName, rnum=RNum} = FieldDef,
    Params = decoder_params(MsgName, AnRes),
    Param = case get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    lists:nth(RNum - 1, Params);
                pass_as_record ->
                    MsgVar = hd(Params),
                    mapping_access(MsgVar, MsgName, FName, Opts)
            end,
    OutParams = case get_field_pass(MsgName, AnRes) of
                    pass_as_params ->
                        lists_setelement(RNum - 1, Params, ?expr(NewSeq));
                    pass_as_record ->
                        [mapping_update(hd(Params), MsgName,
                                           [{FName, ?expr(NewSeq)}],
                                           Opts)]
                end,
    [gpb_codegen:format_fn(
       mk_fn(d_field_, MsgName, FName),
       fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>') when N < ?NB ->
               call_self(Rest, N + 7, X bsl N + Acc, '<Params>');
          (<<0:1, X:7, Rest/binary>>, N, Acc, '<InParams>') ->
               Len = X bsl N + Acc,
               <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
               NewSeq = decode_packed(PackedBytes, 0, 0, '<Param>'),
               '<call-read-field>'(Rest2, 0, 0, '<OutParams>')
       end,
       [splice_trees('<Params>', Params),
        splice_trees('<InParams>', Params),
        replace_term(decode_packed, mk_fn(d_packed_field_, MsgName, FName)),
        replace_tree('<Param>', Param),
        replace_term('<call-read-field>', mk_fn(dfp_read_field_def_, MsgName)),
        splice_trees('<OutParams>', OutParams)]),
     "\n",
     format_packed_field_seq_decoder(MsgName, FieldDef, Opts)].

format_packed_field_seq_decoder(MsgName, #field{type=Type}=Field, Opts) ->
    case Type of
        fixed32  -> format_dpacked_nonvi(MsgName, Field, 32, [little]);
        sfixed32 -> format_dpacked_nonvi(MsgName, Field, 32, [little,signed]);
        float    -> format_dpacked_nonvi(MsgName, Field, 32, [little,float]);
        fixed64  -> format_dpacked_nonvi(MsgName, Field, 64, [little]);
        sfixed64 -> format_dpacked_nonvi(MsgName, Field, 64, [little,signed]);
        double   -> format_dpacked_nonvi(MsgName, Field, 64, [little,float]);
        _        -> format_dpacked_vi(MsgName, Field, Opts)
    end.

format_dpacked_nonvi(MsgName, #field{name=FName}, BitLen, BitTypes)     ->
    gpb_codegen:format_fn(
      mk_fn(d_packed_field_, MsgName, FName),
      fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [Value | AccSeq]);
         (<<>>, _, _, AccSeq) ->
              AccSeq
      end,
      [replace_term('<N>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes])]).

format_dpacked_vi(MsgName, #field{name=FName}=FieldDef, Opts) ->
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest}]),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                C = ?exprs(call_self('<Rest2>', 0, 0, ['<Res>' | AccSeq]),
                           [replace_tree('<Rest2>', Rest2Var),
                            replace_tree('<Res>', FVar)]),
                DecodeExprs ++ C
        end,
    Body = decode_int_value(FVar, Bindings, FieldDef, Opts, BodyTailFn),
    gpb_codegen:format_fn(
      mk_fn(d_packed_field_, MsgName, FName),
      fun(<<1:1, X:7, Rest/binary>>, N, Acc, AccSeq) when N < ?NB ->
              call_self(Rest, N + 7, X bsl N + Acc, AccSeq);
         (<<0:1, X:7, Rest/binary>>, N, Acc, AccSeq) ->
              '<body>';
         (<<>>, 0, 0, AccSeq) ->
              AccSeq
      end,
      [splice_trees('<body>', Body)]).

format_vi_based_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #field{name=FName}=FieldDef,
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest}]),
    Params = decoder_params(MsgName, AnRes),
    InParams = decoder_in_params(Params, MsgName, FieldDef, AnRes),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                ReadFieldDefFn = mk_fn(dfp_read_field_def_, MsgName),
                Params2 = updated_merged_params(MsgName, FieldDef, AnRes,
                                                FVar, Params, Opts),
                C = ?exprs('<call-read-field>'('<Rest2>', 0, 0, '<Params2>'),
                           [replace_term('<call-read-field>', ReadFieldDefFn),
                            replace_tree('<Rest2>', Rest2Var),
                            splice_trees('<Params2>', Params2)]),
                DecodeExprs ++ C
        end,
    Body = decode_int_value(FVar, Bindings, FieldDef, Opts, BodyTailFn),
    gpb_codegen:format_fn(
      mk_fn(d_field_, MsgName, FName),
      fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>') when N < ?NB ->
              call_self(Rest, N + 7, X bsl N + Acc, '<Params>');
         (<<0:1, X:7, Rest/binary>>, N, Acc, '<InParams>') ->
              '<body>'
      end,
      [splice_trees('<Params>', Params),
       splice_trees('<InParams>', InParams),
       splice_trees('<body>', Body)]).

%% -> {[Expr], Rest2VarExpr}
%% where [Expr] is a list of exprs to calculate the resulting decoded value
decode_int_value(ResVar, Bindings, #field{type=Type}, Opts, TailFn) ->
    Value = fetch_binding('<Value>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    StringsAsBinaries = get_strings_as_binaries_by_opts(Opts),
    case Type of
        sint32 ->
            TailFn(decode_zigzag_to_var(ResVar, Value), Rest);
        sint64 ->
            TailFn(decode_zigzag_to_var(ResVar, Value), Rest);
        int32 ->
            TailFn([uint_to_int_to_var(ResVar, Value, 32)], Rest);
        int64 ->
            TailFn([uint_to_int_to_var(ResVar, Value, 64)], Rest);
        uint32 ->
            TailFn([assign_to_var(ResVar, Value)], Rest);
        uint64 ->
            TailFn([assign_to_var(ResVar, Value)], Rest);
        bool ->
            Bool = ?expr('<Res>' = ('<Value>') =/= 0,
                         [replace_tree('<Res>', ResVar),
                          replace_tree('<Value>', Value)]),
            TailFn([Bool], Rest);
        {enum, EnumName} ->
            Tmp = ?expr(Tmp),
            ToSym = [uint_to_int_to_var(Tmp, Value, 32),
                     ?expr('<Res>' = decode_enum('<Int>'),
                           [replace_tree('<Res>', ResVar),
                            replace_term(decode_enum, mk_fn(d_enum_, EnumName)),
                            replace_tree('<Int>', Tmp)])],
            TailFn(ToSym, Rest);
        string when StringsAsBinaries ->
            Rest2 = ?expr(Rest2),
            TailFn(unpack_bytes(ResVar, Value, Rest, Rest2, Opts),
                   Rest2);
        string when not StringsAsBinaries ->
            Rest2 = ?expr(Rest2),
            TailFn(?exprs(Len = '<Value>',
                          <<Utf8:Len/binary, Rest2/binary>> = '<Rest>',
                          '<Res>' = unicode:characters_to_list(Utf8, unicode),
                          [replace_tree('<Value>', Value),
                           replace_tree('<Rest>', Rest),
                           replace_tree('<Res>', ResVar)]),
                   Rest2);
        bytes ->
            Rest2 = ?expr(Rest2),
            TailFn(unpack_bytes(ResVar, Value, Rest, Rest2, Opts),
                   Rest2);
        {msg, Msg2Name} ->
            Rest2 = ?expr(Rest2),
            TailFn(?exprs(Len = '<Value>',
                          <<Bs:Len/binary, Rest2/binary>> = '<Rest>',
                          '<Res>' = decode_msg(Bs, '<sub-msg-name>'),
                          [replace_tree('<Value>', Value),
                           replace_tree('<Rest>', Rest),
                           replace_tree('<Res>', ResVar),
                           replace_term('<sub-msg-name>', Msg2Name)]),
                   Rest2)
    end.

unpack_bytes(ResVar, Value, Rest, Rest2, Opts) ->
    CompilerHasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    Copy = case proplists:get_value(copy_bytes, Opts, auto) of
               auto when not CompilerHasBinary -> false;
               auto when CompilerHasBinary     -> true;
               true                            -> true;
               false                           -> false;
               N when is_integer(N)            -> N;
               N when is_float(N)              -> N
           end,
    Transforms = [replace_tree('<Value>', Value),
                  replace_tree('<Res>', ResVar),
                  replace_tree('<Rest>', Rest),
                  replace_tree('<Rest2>', Rest2),
                  replace_term('<Copy>', Copy)],
    if Copy == false ->
            ?exprs(Len = '<Value>',
                   <<'<Res>':Len/binary, '<Rest2>'/binary>> = '<Rest>',
                   Transforms);
       Copy == true ->
            ?exprs(Len = '<Value>',
                   <<Bytes:Len/binary, '<Rest2>'/binary>> = '<Rest>',
                   '<Res>' = binary:copy(Bytes),
                   Transforms);
       is_integer(Copy); is_float(Copy) ->
            ?exprs(Len = '<Value>',
                   <<Bytes:Len/binary, '<Rest2>'/binary>> = '<Rest>',
                   '<Res>' = case binary:referenced_byte_size(Bytes) of
                                 LB when LB >= byte_size(Bytes) * '<Copy>' ->
                                     binary:copy(Bytes);
                                 _ ->
                                     Bytes
                             end,
                   Transforms)
    end.

updated_merged_params(MsgName, FieldDef, AnRes, NewValue, Params, Opts) ->
    #field{name=FName, rnum=RNum} = FieldDef,
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            PrevValue = lists:nth(RNum - 1, Params),
            MergedValue = merge_field_expr(FieldDef, PrevValue, NewValue),
            lists_setelement(RNum - 1, Params, MergedValue);
        pass_as_record ->
            MsgVar = hd(Params),
            PrevValue = mapping_access(MsgVar, MsgName, FName, Opts),
            MergedValue = merge_field_expr(FieldDef, PrevValue, NewValue),
            [mapping_update(MsgVar, MsgName, [{FName, MergedValue}], Opts)]
    end.

merge_field_expr(FieldDef, PrevValue, NewValue) ->
    case classify_field_merge_action(FieldDef) of
        overwrite ->
            NewValue;
        seqadd ->
            ?expr(['<New>' | '<Acc>'],
                  [replace_tree('<New>', NewValue),
                   replace_tree('<Acc>', PrevValue)]);
        msgmerge ->
            #field{type={msg,FMsgName}} = FieldDef,
            ?expr(if '<Prev>' == undefined -> '<New>';
                     true -> '<merge-msgs>'('<Prev>', '<New>')
                  end,
                  [replace_term('<merge-msgs>', mk_fn(merge_msg_, FMsgName)),
                   replace_tree('<Prev>', PrevValue),
                   replace_tree('<New>', NewValue)])
    end.

decoder_in_params(Params, MsgName, FieldDef, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            #field{rnum=RNum} = FieldDef,
            case classify_field_merge_action(FieldDef) of
                overwrite -> lists_setelement(RNum-1, Params, ?expr(_));
                seqadd    -> Params;
                msgmerge  -> Params
            end;
        pass_as_record ->
            Params
    end.

format_fixlen_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #field{name=FName, type=Type}=FieldDef,
    {BitLen, BitTypes} = case Type of
                             fixed32  -> {32, [little]};
                             sfixed32 -> {32, [little,signed]};
                             float    -> {32, [little,float]};
                             fixed64  -> {64, [little]};
                             sfixed64 -> {64, [little,signed]};
                             double   -> {64, [little,float]}
                         end,
    Params = decoder_params(MsgName, AnRes),
    InParams = decoder_in_params(Params, MsgName, FieldDef, AnRes),
    Value = ?expr(Value),
    Params2 = updated_merged_params(MsgName, FieldDef, AnRes, Value, Params,
                                    Opts),
    ReadFieldDefFnName = mk_fn(dfp_read_field_def_, MsgName),
    gpb_codegen:format_fn(
      mk_fn(d_field_, MsgName, FName),
      fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, '<InParams>') ->
              '<call-read-field>'(Rest, Z1, Z2, '<OutParams>')
      end,
      [replace_term('<N>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes]),
       splice_trees('<InParams>', InParams),
       replace_term('<call-read-field>', ReadFieldDefFnName),
       splice_trees('<OutParams>', Params2)]).

assign_to_var(Var, Expr) ->
    ?expr('<Var>' = '<Expr>',
          [replace_tree('<Var>', Var),
           replace_tree('<Expr>', Expr)]).

decode_zigzag_to_var(ResVar, ValueExpr) ->
    ?exprs(ZValue = '<Value>',
           '<Res>' = if ZValue band 1 =:= 0 -> ZValue bsr 1;
                        true                -> -((ZValue + 1) bsr 1)
                     end,
           [replace_tree('<Value>', ValueExpr),
            replace_tree('<Res>', ResVar)]).

uint_to_int_to_var(ResVar, ValueExpr, NumBits) ->
    ?expr(
       <<'<Res>':'<N>'/signed-native>> = <<('<Value>'):'<N>'/unsigned-native>>,
       [replace_term('<N>', NumBits),
        replace_tree('<Res>', ResVar),
        replace_tree('<Value>', ValueExpr)]).

classify_field_merge_action(FieldDef) ->
    case FieldDef of
        #field{occurrence=required, type={msg, _}} -> msgmerge;
        #field{occurrence=optional, type={msg, _}} -> msgmerge;
        #field{occurrence=required}                -> overwrite;
        #field{occurrence=optional}                -> overwrite;
        #field{occurrence=repeated}                -> seqadd
    end.

format_msg_merge_code(Defs, AnRes, Opts) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgDef} <- Defs],
    [format_merge_msgs_top_level(MsgNames, Opts),
     [format_msg_merger(MsgName, MsgDef, AnRes, Opts)
      || {{msg, MsgName}, MsgDef} <- Defs]].

format_merge_msgs_top_level([], _Opts) ->
    gpb_codegen:format_fn(
      merge_msgs,
      fun(_Prev, New, _MsgName) -> New end);
format_merge_msgs_top_level(MsgNames, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            gpb_codegen:format_fn(
              merge_msgs,
              fun(Prev, New) when element(1, Prev) =:= element(1, New) ->
                      case Prev of
                          '<msg-type>' -> '<merge-msg>'(Prev, New)
                      end
              end,
              [repeat_clauses(
                 '<msg-type>',
                 [[replace_tree('<msg-type>', record_match(MsgName, [])),
                   replace_term('<merge-msg>', mk_fn(merge_msg_, MsgName))]
                  || MsgName <- MsgNames])]);
        maps ->
            gpb_codegen:format_fn(
              merge_msgs,
              fun(Prev, New, MsgName) ->
                      case MsgName of
                          '<msg-type>' -> '<merge-msg>'(Prev, New)
                      end
              end,
              [repeat_clauses(
                 '<msg-type>',
                 [[replace_tree('<msg-type>', erl_syntax:atom(MsgName)),
                   replace_term('<merge-msg>', mk_fn(merge_msg_, MsgName))]
                  || MsgName <- MsgNames])])
    end.

format_msg_merger(MsgName, [], _AnRes, _Opts) ->
    gpb_codegen:format_fn(
      mk_fn(merge_msg_, MsgName),
      fun(_Prev, New) -> New end);
format_msg_merger(MsgName, MsgDef, AnRes, Opts) ->
    {PFields, NFields, Mergings} = compute_msg_field_merge_exprs(MsgDef),
    Transforms = [replace_tree('<Prev>', mapping_match(MsgName, PFields, Opts)),
                  replace_tree('<New>', mapping_match(MsgName, NFields, Opts)),
                  replace_tree('<merge>',
                               mapping_create(MsgName, Mergings, Opts))],
    MsgUndefFnClauses =
        case occurs_as_optional_submsg(MsgName, AnRes) of
            true ->
                [?fn_clause(fun(Prev, undefined) -> Prev end),
                 ?fn_clause(fun(undefined, New)  -> New end)];
            false ->
                []
        end,

    gpb_codegen:format_fn(
      mk_fn(merge_msg_, MsgName),
      fun('<msg-undefined-handling>', _) -> '<return-the-defined-msg>';
         ('<Prev>', '<New>')             -> '<merge>'
      end,
      Transforms ++ [splice_clauses('<msg-undefined-handling>',
                                    MsgUndefFnClauses)]).

compute_msg_field_merge_exprs(MsgDef) ->
    PFields = [{FName, erl_syntax:variable(?ff("PF~s", [FName]))}
               || #field{name=FName} <- MsgDef],
    NFields = [{FName, erl_syntax:variable(?ff("NF~s", [FName]))}
               || #field{name=FName} <- MsgDef],
    Mergings =
    [begin
         {value, {FName, PF}} = lists:keysearch(FName, 1, PFields),
         {value, {FName, NF}} = lists:keysearch(FName, 1, NFields),
         Transforms = [replace_tree('<PF>', PF),
                       replace_tree('<NF>', NF)],
         Expr = case classify_field_merge_action(Field) of
                    overwrite ->
                        ?expr(if '<NF>' =:= undefined -> '<PF>';
                                 true                 -> '<NF>'
                              end,
                              Transforms);
                    seqadd ->
                        ?expr('<PF>' ++ '<NF>', Transforms);
                    msgmerge ->
                        #field{type={msg,SubMsgName}}=Field,
                        MergeFn = mk_fn(merge_msg_, SubMsgName),
                        NewTranform = replace_term('<merge>', MergeFn),
                        MTransforms = [NewTranform | Transforms],
                        ?expr('<merge>'('<PF>', '<NF>'), MTransforms)
                end,
         {FName, Expr}
     end
     || #field{name=FName}=Field <- MsgDef],
    {PFields, NFields, Mergings}.

occurs_as_optional_submsg(MsgName, #anres{msg_occurrences=Occurrences}=AnRes) ->
    %% Note: order of evaluation below is important (the exprs of `andalso'):
    %% Messages are present in Occurrences only if they are sub-messages
    can_occur_as_sub_msg(MsgName, AnRes) andalso
        lists:member(optional, dict:fetch(MsgName, Occurrences)).

format_field_skippers(MsgName, AnRes) ->
    SkipVarintFnName = mk_fn(skip_varint_, MsgName),
    SkipLenDelimFnName = mk_fn(skip_length_delimited_, MsgName),
    ReadFieldFnName = mk_fn(dfp_read_field_def_, MsgName),
    Params = decoder_params(MsgName, AnRes),
    [%% skip_varint_<MsgName>/2,4
     gpb_codegen:format_fn(
       SkipVarintFnName,
       fun(<<1:1, _:7, Rest/binary>>, Z1, Z2, '<Params>') ->
               '<call-recursively>'(Rest, Z1, Z2, '<Params>');
          (<<0:1, _:7, Rest/binary>>, Z1, Z2, '<Params>') ->
               '<call-read-field>'(Rest, Z1,Z2, '<Params>')
       end,
       [replace_term('<call-recursively>', SkipVarintFnName),
        replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_length_delimited_<MsgName>/4
     gpb_codegen:format_fn(
       SkipLenDelimFnName,
       fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>') when N < ?NB ->
               '<call-recursively>'(Rest, N+7, X bsl N + Acc, '<Params>');
          (<<0:1, X:7, Rest/binary>>, N, Acc, '<Params>') ->
               Length = X bsl N + Acc,
               <<_:Length/binary, Rest2/binary>> = Rest,
               '<call-read-field>'(Rest2, 0, 0, '<Params>')
       end,
       [replace_term('<call-recursively>', SkipLenDelimFnName),
        replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_32_<MsgName>/2,4
     %% skip_64_<MsgName>/2,4
     [[gpb_codegen:format_fn(
         mk_fn(skip_, NumBits, MsgName),
         fun(<<_:'<NumBits>', Rest/binary>>, Z1, Z2, '<Params>') ->
                 '<call-read-field>'(Rest, Z1, Z2, '<Params>')
         end,
         [replace_term('<call-read-field>', ReadFieldFnName),
          replace_term('<NumBits>', NumBits),
          splice_trees('<Params>', Params)]),
       "\n"]
      || NumBits <- [32, 64]]].

format_nif_decoder_error_wrappers(Defs, _AnRes, _Opts) ->
    [format_msg_nif_error_wrapper(MsgName)
     || {{msg, MsgName}, _MsgDef} <- Defs].

format_msg_nif_error_wrapper(MsgName) ->
    gpb_codegen:format_fn(
      mk_fn(d_msg_, MsgName),
      fun(Bin) ->
              erlang:nif_error({error,{nif_not_loaded,'<msg-name>'}}, [Bin])
      end,
      [replace_term('<msg-name>', MsgName)]).

%% -- verifiers -----------------------------------------------------

format_verifiers_top_function(Defs, Opts) ->
    Mapping = get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(MsgName)]
                  end,
    gpb_codegen:format_fn(
      verify_msg,
      fun(Msg, '<MsgName>') ->
              case '<MsgOrMsgName>' of
                  '<msg-match>' -> '<verify-msg>'(Msg, ['<MsgName>']);
                  _ -> mk_type_error(not_a_known_message, Msg, [])
              end
      end,
      [repeat_clauses(
         '<msg-match>',
         [[replace_tree('<msg-match>',
                        case Mapping of
                            records -> record_match(MsgName, []);
                            maps    -> erl_syntax:atom(MsgName)
                        end),
           replace_term('<verify-msg>', mk_fn(v_msg_, MsgName)),
           replace_term('<MsgName>', MsgName)]
          || {{msg, MsgName}, _MsgDef} <- Defs]),
       replace_tree('<MsgOrMsgName>', case Mapping of
                                          records -> ?expr(Msg);
                                          maps    -> ?expr(MsgName)
                                      end),
       splice_trees('<MsgName>', MsgNameVars)]).

format_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifiers(Defs, AnRes, Opts),
     format_enum_verifiers(Defs, AnRes),
     format_type_verifiers(AnRes),
     format_verifier_auxiliaries()
    ].

format_msg_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifier(MsgName, MsgDef, AnRes, Opts)
     || {{msg,MsgName}, MsgDef} <- Defs].

format_msg_verifier(MsgName, MsgDef, AnRes, Opts) ->
    FVars = [{var_f_n(I), Field} || {I, Field} <- index_seq(MsgDef)],
    RFields = [{FName, Var} || {Var, #field{name=FName}} <- FVars],
    NeedsMatchOther = case get_records_or_maps_by_opts(Opts) of
                          records -> can_occur_as_sub_msg(MsgName, AnRes);
                          maps    -> true
                      end,
    gpb_codegen:format_fn(
      mk_fn(v_msg_, MsgName),
      fun('<msg-match>', '<Path>') ->
              '<verify-fields>',
              ok;
         ('<X>', Path) ->
              mk_type_error({expected_msg,'<MsgName>'}, X, Path)
      end,
      [replace_tree('<msg-match>', mapping_match(MsgName, RFields, Opts)),
       replace_tree('<Path>', if MsgDef == [] -> ?expr(_Path);
                                 MsgDef /= [] -> ?expr(Path)
                              end),
       splice_trees('<verify-fields>', field_verifiers(FVars)),
       repeat_clauses('<X>', case NeedsMatchOther of
                                 true  -> [[replace_tree('<X>', ?expr(X))]];
                                 false -> [] %% omit the else clause
                             end),
       replace_term('<MsgName>', MsgName)]).

field_verifiers(FVars) ->
    [begin
         FVerifierFn = case Type of
                           {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                           {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                           Type            -> mk_fn(v_type_, Type)
                       end,
         Replacements = [replace_term('<verify-fn>', FVerifierFn),
                         replace_tree('<F>', FVar),
                         replace_term('<FName>', FName),
                         replace_term('<Type>', Type)],
         case Occurrence of
             required ->
                 %% FIXME: check especially for `undefined'
                 %% and if found, error out with required_field_not_set
                 %% specifying expected type
                 ?expr('<verify-fn>'('<F>', ['<FName>' | Path]),
                       Replacements);
             repeated ->
                 ?expr(if is_list('<F>') ->
                               ['<verify-fn>'(Elem, ['<FName>' | Path])
                                || Elem <- '<F>'];
                          true ->
                               mk_type_error(
                                 {invalid_list_of, '<Type>'}, '<F>', Path)
                       end,
                       Replacements);
             optional ->
                 ?expr(if '<F>' == undefined -> ok;
                          true -> '<verify-fn>'('<F>', ['<FName>', Path])
                       end,
                       Replacements)
         end
     end
     || {FVar, #field{name=FName, type=Type, occurrence=Occurrence}} <- FVars].

can_occur_as_sub_msg(MsgName, #anres{used_types=UsedTypes}) ->
    sets:is_element({msg,MsgName}, UsedTypes).

format_enum_verifiers(Defs, #anres{used_types=UsedTypes}) ->
    [format_enum_verifier(EnumName, Def)
     || {{enum,EnumName}, Def} <- Defs,
        smember({enum, EnumName}, UsedTypes)].

format_enum_verifier(EnumName, EnumMembers) ->
    gpb_codegen:format_fn(
      mk_fn(v_enum_, EnumName),
      fun('<sym>', _Path) -> ok;
         (X, Path) -> mk_type_error({invalid_enum, '<EnumName>'}, X, Path)
      end,
      [repeat_clauses('<sym>', [[replace_term('<sym>', EnumSym)]
                                || {EnumSym, _Value} <- EnumMembers]),
       replace_term('<EnumName>', EnumName)]).

format_type_verifiers(#anres{used_types=UsedTypes}) ->
    [[format_int_verifier(sint32, signed, 32)   || smember(sint32, UsedTypes)],
     [format_int_verifier(sint64, signed, 64)   || smember(sint64, UsedTypes)],
     [format_int_verifier(int32,  signed, 32)   || smember(int32, UsedTypes)],
     [format_int_verifier(int64,  signed, 64)   || smember(int64, UsedTypes)],
     [format_int_verifier(uint32, unsigned, 32) || smember(uint32, UsedTypes)],
     [format_int_verifier(uint64, unsigned, 64) || smember(uint64, UsedTypes)],
     [format_bool_verifier()                    || smember(bool, UsedTypes)],
     [format_int_verifier(fixed32, unsigned, 32)|| smember(fixed32, UsedTypes)],
     [format_int_verifier(fixed64, unsigned, 64)|| smember(fixed64, UsedTypes)],
     [format_int_verifier(sfixed32,signed, 32)  || smember(sfixed32,UsedTypes)],
     [format_int_verifier(sfixed64,signed, 64)  || smember(sfixed64,UsedTypes)],
     [format_float_verifier(float)              || smember(float, UsedTypes)],
     [format_float_verifier(double)             || smember(double, UsedTypes)],
     [format_string_verifier()                  || smember(string, UsedTypes)],
     [format_bytes_verifier()                   || smember(bytes, UsedTypes)]].

format_int_verifier(IntType, Signedness, NumBits) ->
    Min = case Signedness of
              unsigned -> 0;
              signed   -> -(1 bsl (NumBits-1))
          end,
    Max = case Signedness of
              unsigned -> 1 bsl NumBits - 1;
              signed   -> 1 bsl (NumBits-1) - 1
          end,
    gpb_codegen:format_fn(
      mk_fn(v_type_, IntType),
      fun(N, _Path) when '<Min>' =< N, N =< '<Max>' ->
              ok;
         (N, Path) when is_integer(N) ->
              mk_type_error({value_out_of_range, '<details>'}, N, Path);
         (X, Path) ->
              mk_type_error({bad_integer, '<details>'}, X, Path)
      end,
      [replace_term('<Min>', Min),
       replace_term('<Max>', Max),
       splice_trees('<details>', [erl_syntax:atom(IntType),
                                  erl_syntax:atom(Signedness),
                                  erl_syntax:integer(NumBits)])]).

format_bool_verifier() ->
    gpb_codegen:format_fn(
      mk_fn(v_type_, bool),
      fun(false, _Path) -> ok;
         (true, _Path)  -> ok;
         (X, Path) -> mk_type_error(bad_boolean_value, X, Path)
      end).

format_float_verifier(FlType) ->
    BadTypeOfValue = list_to_atom(lists:concat(["bad_", FlType, "_value"])),
    gpb_codegen:format_fn(
      mk_fn(v_type_, FlType),
      fun(N, _Path) when is_float(N) -> ok;
         %% It seems a float for the corresponding integer value is
         %% indeed packed when doing <<Integer:32/little-float>>.
         %% So let verify accept integers too.
         %% When such a value is unpacked, we get a float.
         (N, _Path) when is_integer(N) -> ok;
         (X, Path) -> mk_type_error('<bad_x_value>', X, Path)
      end,
      [replace_term('<bad_x_value>', BadTypeOfValue)]).

format_string_verifier() ->
    gpb_codegen:format_fn(
      mk_fn(v_type_, string),
      fun(S, Path) when is_list(S) ->
              try
                  unicode:characters_to_binary(S),
                  ok
              catch error:badarg ->
                      mk_type_error(bad_unicode_string, S, Path)
              end;
         (X, Path) ->
              mk_type_error(bad_unicode_string, X, Path)
      end).

format_bytes_verifier() ->
    gpb_codegen:format_fn(
      mk_fn(v_type_, bytes),
      fun(B, _Path) when is_binary(B) ->
              ok;
         (X, Path) ->
              mk_type_error(bad_binary_value, X, Path)
      end).

format_verifier_auxiliaries() ->
    [gpb_codegen:format_fn(
       mk_type_error,
       fun(Error, ValueSeen, Path) ->
               Path2 = prettify_path(Path),
               erlang:error({gpb_type_error,
                             {Error, [{value, ValueSeen},{path, Path2}]}})
       end),
     "\n",
     gpb_codegen:format_fn(
       prettify_path,
       fun([]) ->
               top_level;
          (PathR) ->
               list_to_atom(
                 string:join(
                   lists:map(fun atom_to_list/1, lists:reverse(PathR)),
                   "."))
       end)].

%% -- message defs -----------------------------------------------------

format_introspection(Defs, Opts) ->
    MsgDefs  = [Item || {{msg, _}, _}=Item <- Defs],
    EnumDefs = [Item || {{enum, _}, _}=Item <- Defs],
    [gpb_codegen:format_fn(
       get_msg_defs, fun() -> '<Defs>' end,
       [replace_tree('<Defs>', def_trees(EnumDefs, MsgDefs, Opts))]),
     "\n",
     gpb_codegen:format_fn(
       get_msg_names, fun() -> '<Names>' end,
       [replace_term('<Names>', [MsgName || {{msg,MsgName}, _} <- Defs])]),
     "\n",
     gpb_codegen:format_fn(
       get_enum_names, fun() -> '<Names>' end,
       [replace_term('<Names>', [EnumName || {{enum,EnumName}, _} <- Defs])]),
     "\n",
     format_fetch_msg_defs(MsgDefs),
     ?f("~n"),
     format_fetch_enum_defs(EnumDefs),
     ?f("~n"),
     format_find_msg_defs(MsgDefs, Opts),
     ?f("~n"),
     format_find_enum_defs(EnumDefs),
     ?f("~n"),
     format_enum_value_symbol_converters(EnumDefs),
     ?f("~n"),
     format_get_package_name(Defs),
     ?f("~n"),
     format_descriptor(Defs, Opts)
    ].

def_trees(EnumDefs, MsgDefs, Opts) ->
    EnumDefTrees = [erl_parse:abstract(EnumDef) || EnumDef <- EnumDefs],
    MsgDefTrees = [msg_def_tree(MsgDef, Opts) || MsgDef <- MsgDefs],
    erl_syntax:list(EnumDefTrees ++ MsgDefTrees).

msg_def_tree({{msg, MsgName}, Fields}, Opts) ->
    erl_syntax:tuple(
      [erl_syntax:tuple([erl_syntax:atom(msg), erl_syntax:atom(MsgName)]),
       fields_tree(Fields, Opts)]).

fields_tree(Fields, Opts) ->
    case get_field_format_by_opts(Opts) of
        fields_as_records   ->
            erl_syntax:list([field_tree(Field, Opts) || Field <- Fields]);
        fields_as_proplists ->
            erl_parse:abstract(gpb:field_records_to_proplists(Fields))
    end.

get_field_format_by_opts(Opts) ->
    case proplists:get_bool(defs_as_proplists, proplists:unfold(Opts)) of
        false -> fields_as_records; %% default
        true  -> fields_as_proplists
    end.

field_tree(#field{}=F, Opts) ->
    [field | FValues] = tuple_to_list(F),
    FNames = record_info(fields, field),
    mapping_create(
      field,
      lists:zip(FNames,
                [erl_parse:abstract(FValue) || FValue <- FValues]),
      Opts).

format_fetch_msg_defs([]) ->
    gpb_codegen:format_fn(
      fetch_msg_def,
      fun(MsgName) -> erlang:error({no_such_msg, MsgName}) end);
format_fetch_msg_defs(_MsgDefs) ->
    gpb_codegen:format_fn(
      fetch_msg_def,
      fun(MsgName) ->
              case find_msg_def(MsgName) of
                  Fs when is_list(Fs) -> Fs;
                  error               -> erlang:error({no_such_msg, MsgName})
              end
      end).

format_fetch_enum_defs([]) ->
    gpb_codegen:format_fn(
      fetch_enum_def,
      fun(EnumName) -> erlang:error({no_such_enum, EnumName}) end);
format_fetch_enum_defs(_EnumDefs) ->
    gpb_codegen:format_fn(
      fetch_enum_def,
      fun(EnumName) ->
              case find_enum_def(EnumName) of
                  Es when is_list(Es) -> Es;
                  error               -> erlang:error({no_such_enum, EnumName})
              end
      end).

format_find_msg_defs(Msgs, Opts) ->
    gpb_codegen:format_fn(
      find_msg_def,
      fun('<MsgName>') -> '<Fields>';
         (_) -> error
      end,
      [repeat_clauses('<MsgName>',
                      [[replace_term('<MsgName>', MsgName),
                        replace_tree('<Fields>', fields_tree(Fields, Opts))]
                       || {{msg, MsgName}, Fields} <- Msgs])]).

format_find_enum_defs(Enums) ->
    gpb_codegen:format_fn(
      find_enum_def,
      fun('<EnumName>') -> '<Values>';
         (_) -> error
      end,
      [repeat_clauses('<EnumName>',
                      [[replace_term('<EnumName>', EnumName),
                        replace_term('<Values>', Values)]
                       || {{enum, EnumName}, Values} <- Enums])]).


format_enum_value_symbol_converter_exports(Defs) ->
    [?f("-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).~n"),
     [begin
         ToSymFnName = mk_fn(enum_symbol_by_value_, EnumName),
         ToValFnName = mk_fn(enum_value_by_symbol_, EnumName),
         ?f("-export([~p/1, ~p/1]).~n", [ToSymFnName, ToValFnName])
     end
     || {{enum, EnumName}, _EnumDef} <- Defs]].

format_enum_value_symbol_converters(EnumDefs) when EnumDefs /= [] ->
    %% A difference between this function and `d_enum_X' as generated
    %% by `format_enum_decoders' is that this function generates
    %% value/symbol converters for all enums, not only for the ones
    %% that are used in messags.
    [gpb_codegen:format_fn(
       enum_symbol_by_value,
       fun('<EnumName>', Value) -> 'cvt'(Value) end,
       [repeat_clauses(
          '<EnumName>',
          [[replace_term('<EnumName>', EnumName),
            replace_term('cvt', mk_fn(enum_symbol_by_value_, EnumName))]
           || {{enum, EnumName}, _EnumDef} <- EnumDefs])]),
     "\n",
     gpb_codegen:format_fn(
       enum_value_by_symbol,
       fun('<EnumName>', Sym) -> 'cvt'(Sym) end,
       [repeat_clauses(
          '<EnumName>',
          [[replace_term('<EnumName>', EnumName),
            replace_term('cvt', mk_fn(enum_value_by_symbol_, EnumName))]
           || {{enum, EnumName}, _EnumDef} <- EnumDefs])]),
     "\n",
     [[gpb_codegen:format_fn(
         mk_fn(enum_symbol_by_value_, EnumName),
         fun('<Value>') -> '<Sym>' end,
         [repeat_clauses('<Value>',
                         [[replace_term('<Value>', EnumValue),
                           replace_term('<Sym>', EnumSym)]
                          || {EnumSym, EnumValue} <- EnumDef])]),
       "\n",
       gpb_codegen:format_fn(
         mk_fn(enum_value_by_symbol_, EnumName),
         fun('<Sym>') -> '<Value>' end,
         [repeat_clauses('<Sym>',
                         [[replace_term('<Value>', EnumValue),
                           replace_term('<Sym>', EnumSym)]
                          || {EnumSym, EnumValue} <- EnumDef])])]
      || {{enum, EnumName}, EnumDef} <- EnumDefs]];
format_enum_value_symbol_converters([]=_EnumDefs) ->
    [gpb_codegen:format_fn(
       enum_symbol_by_value,
       fun(E, V) -> erlang:error({no_enum_defs, E, V}) end),
     "\n",
     gpb_codegen:format_fn(
       enum_value_by_symbol,
       fun(E, V) -> erlang:error({no_enum_defs, E, V}) end),
     "\n"].

format_get_package_name(Defs) ->
    case lists:keyfind(package, 1, Defs) of
        false ->
            gpb_codegen:format_fn(
              get_package_name, fun() -> undefined end);
        {package, Package} ->
            gpb_codegen:format_fn(
              get_package_name, fun() -> '<Package>' end,
              [replace_term('<Package>', Package)])
    end.

format_descriptor(Defs, Opts) ->
    case get_gen_descriptor_by_opts(Opts) of
        true ->
            try gpb_compile_descr:encode_defs_to_descriptor(Defs) of
                Bin when is_binary(Bin) ->
                    gpb_codegen:format_fn(
                      descriptor, fun() -> 'bin' end,
                      [replace_term(bin, Bin)])
            catch error:undef ->
                    ST = erlang:get_stacktrace(),
                    case {element(1,hd(ST)), element(2,hd(ST))} of
                        {gpb_compile_descr, encode_defs_to_descriptor} ->
                            gpb_codegen:format_fn(
                              descriptor,
                              fun() -> erlang:error(descr_not_avail) end);
                        _ ->
                            %% other error
                            erlang:raise(error, undef, ST)
                    end
            end;
        false ->
            ""
    end.

get_gen_descriptor_by_opts(Opts) ->
    proplists:get_bool(descriptor, Opts).

%% -- hrl -----------------------------------------------------

possibly_format_hrl(Mod, Defs, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> format_hrl(Mod, Defs, Opts);
        maps    -> '$not_generated'
    end.

format_hrl(Mod, Defs, Opts) ->
    ModVsn = list_to_atom(atom_to_list(Mod) ++ "_gpb_version"),
    iolist_to_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s on ~w~n",
          [?MODULE, gpb:version_as_string(), calendar:local_time()]),
       "\n",
       ?f("-ifndef(~p).~n", [Mod]),
       ?f("-define(~p, true).~n", [Mod]),
       "\n",
       ?f("-define(~p, \"~s\").~n", [ModVsn, gpb:version_as_string()]),
       "\n",
       string:join([format_msg_record(Msg, Fields, Opts, Defs)
                    || {{msg,Msg},Fields} <- Defs],
                   "\n"),
       "\n",
       ?f("-endif.~n")]).

format_msg_record(Msg, Fields, Opts, Defs) ->
    [?f("-record(~p,~n", [Msg]),
     ?f("        {"),
     outdent_first(format_hfields(8+1, Fields, Opts, Defs)),
     "\n",
     ?f("        }).~n")].

format_hfields(Indent, Fields, CompileOpts, Defs) ->
    TypeSpecs = get_type_specs_by_opts(CompileOpts),
    string:join(
      lists:map(
        fun({I, #field{name=Name, fnum=FNum, opts=FOpts, occurrence=Occur}=Field}) ->
                DefaultStr = case proplists:get_value(default, FOpts, '$no') of
                                 '$no'   -> case Occur of
                                                repeated -> ?f(" = []");
                                                _        -> ""
                                            end;
                                 Default -> ?f(" = ~p", [Default])
                             end,
                TypeStr = ?f("~s", [type_to_typestr(Field, Defs)]),
                CommaSep = if I < length(Fields) -> ",";
                              true               -> "" %% last entry
                           end,
                FieldTxt1 = indent(Indent, ?f("~w~s", [Name, DefaultStr])),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s:: ~s~s", [FieldTxt1, LineUp,
                                                       TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    ?f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                                not TypeSpecs -> 40
                             end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                TypeComment = type_to_comment(Field, TypeSpecs),
                ?f("~s~s% = ~w~s~s",
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
type_to_typestr_2({msg,M}, _DEfs)  -> ?f("#~p{}", [M]).

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
        required -> ?f("~w", [Type]);
        repeated -> "[" ++ ?f("~w", [Type]) ++ "]";
        optional -> ?f("~w (optional)", [Type])
    end.

lineup(CurrentCol, TargetCol) when CurrentCol < TargetCol ->
    lists:duplicate(TargetCol - CurrentCol, $\s);
lineup(_, _) ->
    " ".

indent(Indent, Str) ->
    lists:duplicate(Indent, $\s) ++ Str.

indent_lines(Indent, Lines) ->
    [indent(Indent, Line) || Line <- Lines].

outdent_first(IoList) ->
    lists:dropwhile(fun(C) -> C == $\s end,
                    binary_to_list(iolist_to_binary(IoList))).

mk_fn(Prefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Suffix])).

mk_fn(Prefix, Middlefix, Suffix) when is_integer(Middlefix) ->
    mk_fn(Prefix, list_to_atom(integer_to_list(Middlefix)), Suffix);
mk_fn(Prefix, Middlefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Middlefix, "_", Suffix])).

mk_c_fn(Prefix, Suffix) ->
    dot_to_underscore(lists:concat([Prefix, Suffix])).

mk_c_var(Prefix, Suffix) ->
    dot_to_underscore(lists:concat([Prefix, Suffix])).

dot_to_underscore(X) when is_list(X) -> dot_replace_s(X, "_").

dot_replace_s(S, New) when is_list(S) -> d_r(S, New);
dot_replace_s(S, New) when is_atom(S) -> d_r(atom_to_list(S), New).

d_r("."++Rest, New) -> New ++ d_r(Rest, New);
d_r([C|Rest], New)  -> [C | d_r(Rest, New)];
d_r("", _New)       -> "".

%% -- nif c++ code -----------------------------------------------------

possibly_format_nif_cc(Mod, Defs, AnRes, Opts) ->
    case proplists:get_bool(nif, Opts) of
        true  -> format_nif_cc(Mod, Defs, AnRes, Opts);
        false -> '$not_generated'
    end.

format_nif_cc(Mod, Defs, AnRes, Opts) ->
    iolist_to_binary(
      [format_nif_cc_includes(Mod, Defs, Opts),
       format_nif_cc_local_function_decls(Mod, Defs, Opts),
       format_nif_cc_mk_atoms(Mod, Defs, AnRes, Opts),
       format_nif_cc_utf8_conversion(Mod, Defs, AnRes, Opts),
       format_nif_cc_decoders(Mod, Defs, Opts),
       format_nif_cc_unpackers(Mod, Defs, Opts),
       format_nif_cc_foot(Mod, Defs, Opts)]).

get_cc_pkg(Defs) ->
    case lists:keyfind(package, 1, Defs) of
        false              -> "";
        {package, Package} -> "::"++dot_replace_s(Package, "::")
    end.

is_lite_rt(Defs) ->
    OptimizeOpts = [Opt || {option,{optimize_for,Opt}} <- Defs],
    lists:any(fun(OptOpt) -> OptOpt == 'LITE_RUNTIME' end,
              OptimizeOpts).

format_nif_cc_includes(Mod, Defs, _Opts) ->
    IsLiteRT = is_lite_rt(Defs),
    ["#include <string.h>\n",
     "#include <string>\n",
     "\n",
     "#include <erl_driver.h>\n",
     "#include <erl_nif.h>\n",
     "\n",
     ?f("#include \"~s.pb.h\"\n", [Mod]),
     ["#include <google/protobuf/message_lite.h>\n" || IsLiteRT],
     "\n"].

format_nif_cc_local_function_decls(_Mod, Defs, _Opts) ->
    CPkg = get_cc_pkg(Defs),
    [[begin
          UnpackFnName = mk_c_fn(u_msg_, MsgName),
          CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
          ["static ERL_NIF_TERM ",UnpackFnName,["(ErlNifEnv *env, ",
                                                "const ",CMsgType," *m);\n"]]
      end
      || {{msg, MsgName}, _Fields} <- Defs],
     "\n"].

format_nif_cc_mk_atoms(_Mod, Defs, AnRes, _Opts) ->
    EnumAtoms = lists:flatten([[Sym || {Sym, _V} <- EnumDef]
                               || {{enum, _}, EnumDef} <- Defs]),
    RecordAtoms = [MsgName || {{msg, MsgName}, _Fields} <- Defs],
    MiscAtoms0 = [undefined],
    MiscAtoms1 = case is_any_field_of_type_bool(AnRes) of
                     true  -> MiscAtoms0 ++ [true, false];
                     false -> MiscAtoms0
                 end,
    Atoms = lists:usort(EnumAtoms ++ RecordAtoms ++ MiscAtoms1),
    AtomVars = [{mk_c_var(aa_, A), A} || A <- Atoms],

    [[?f("static ERL_NIF_TERM ~s;\n", [Var]) || {Var,_Atom} <- AtomVars],
     "\n",
     ["static void install_atoms(ErlNifEnv *env)\n"
      "{\n",
      [?f("    ~s = enif_make_atom(env, \"~s\");\n", [AtomVar, Atom])
       || {AtomVar, Atom} <- AtomVars],
      "}\n",
      "\n"]].

format_nif_cc_utf8_conversion(_Mod, _Defs, AnRes, Opts) ->
    case is_any_field_of_type_string(AnRes) of
        true  -> format_nif_cc_utf8_conversion_code(Opts);
        false -> ""
    end.

is_any_field_of_type_string(#anres{used_types=UsedTypes}) ->
    sets:is_element(string, UsedTypes).

is_any_field_of_type_bool(#anres{used_types=UsedTypes}) ->
    sets:is_element(bool, UsedTypes).

format_nif_cc_utf8_conversion_code(Opts) ->
    ["/* Source for https://www.ietf.org/rfc/rfc2279.txt */\n",
     "\n",
     "static int\n",
     "utf8_count_codepoints(const char *sinit, int len)\n",
     "{\n",
     "    int n = 0;\n",
     "    const unsigned char *s0 = (unsigned char *)sinit;\n",
     "    const unsigned char *s  = s0;\n",
     "\n",
     "    while ((s - s0) < len)\n",
     "    {\n",
     "        if (*s <= 0x7f) { n++; s++; } /* code point fits 1 octet */\n",
     "        else if (*s <= 0xdf) { n++; s += 2; } /* 2 octets */\n",
     "        else if (*s <= 0xef) { n++; s += 3; } /* 3 octets */\n",
     "        else if (*s <= 0xf7) { n++; s += 4; }\n",
     "        else if (*s <= 0xfb) { n++; s += 5; }\n",
     "        else if (*s <= 0xfd) { n++; s += 6; }\n",
     "        else return -1;\n",
     "\n",
     "        if ((s - s0) > len)\n",
     "            return -1;\n",
     "    }\n",
     "    return n;\n",
     "}\n",
     "\n",
     "static int\n",
     "utf8_to_uint32(unsigned int *dest, const char *src, int numCodePoints)\n",
     "{\n",
     "    int i;\n",
     "    const unsigned char *s = (unsigned char *)src;\n",
     "\n",
     "\n",
     "    /* Should perhaps check for illegal chars in d800-dfff and\n",
     "     * a other illegal chars\n",
     "     */\n",
     "\n",
     "    for (i = 0; i < numCodePoints; i++)\n",
     "    {\n",
     "        if (*s <= 0x7f)\n",
     "            *dest++ = *s++;\n",
     "        else if (*s <= 0xdf) /* code point is 2 octets long */\n",
     "        {\n",
     "            *dest   =  *s++ & 0x1f; *dest <<= 6;\n",
     "            *dest++ |= *s++ & 0x3f;\n",
     "        }\n",
     "        else if (*s <= 0xef) /* code point is 3 octets long */\n",
     "        {\n",
     "            *dest   =  *s++ & 0x0f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest++ |= *s++ & 0x3f;\n",
     "        }\n",
     "        else if (*s <= 0xf7) /* code point is 4 octets long */\n",
     "        {\n",
     "            *dest   =  *s++ & 0x07; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest++ |= *s++ & 0x3f;\n",
     "        }\n",
     "        else if (*s <= 0xfb) /* code point is 5 octets long */\n",
     "        {\n",
     "            *dest   =  *s++ & 0x03; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest++ |= *s++ & 0x3f;\n",
     "        }\n",
     "        else if (*s <= 0xfd) /* code point is 6 octets long */\n",
     "        {\n",
     "            *dest   =  *s++ & 0x01; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest   |= *s++ & 0x3f; *dest <<= 6;\n",
     "            *dest++ |= *s++ & 0x3f;\n",
     "        }\n",
     "        else\n",
     "            return 0;\n",
     "    }\n",
     "    return 1;\n",
     "}\n",
     "\n"
     "static ERL_NIF_TERM\n",
     "utf8_to_erl_string(ErlNifEnv *env,\n",
     "                   const char *utf8data,\n",
     "                   unsigned int numOctets)\n",
     "{\n",
     "    int numcp = utf8_count_codepoints(utf8data, numOctets);\n",
     "\n",
     "    if (numcp < 0)\n",
     "    {\n",
     "        return enif_make_string(env,\n",
     "                                \"<invalid UTF-8>\",\n",
     "                                ERL_NIF_LATIN1);\n",
     "    }\n",
     "    else\n",
     case get_strings_as_binaries_by_opts(Opts) of
         true ->
             ["    {\n",
              "        ERL_NIF_TERM   b;\n",
              "        unsigned char *data;\n",
              "\n",
              "        data = enif_make_new_binary(env, numOctets, &b);\n",
              "        memmove(data, utf8data, numOctets);\n",
              "        return b;\n",
              "    }\n"];
         false ->
             ["    {\n",
              "        unsigned int  cp[numcp];\n",
              "        ERL_NIF_TERM  es[numcp];\n",
              "        int i;\n",
              "\n",
              "        utf8_to_uint32(cp, utf8data, numcp);\n",
              "        for (i = 0; i < numcp; i++)\n",
              "            es[i] = enif_make_uint(env, cp[i]);\n",
              "        return enif_make_list_from_array(env, es, numcp);\n"
              "    }\n"]
     end,
     "}\n",
     "\n"].

format_nif_cc_foot(Mod, Defs, _Opts) ->
    ["static int\n",
     "load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)\n",
     "{\n",
     "    install_atoms(env);\n"
     "    return 0;\n",
     "}\n",
     "\n",
     "static int\n",
     "reload(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)\n",
     "{\n",
     "    return 0;\n",
     "}\n",
     "\n",
     "void\n",
     "unload(ErlNifEnv *env, void *priv_data)\n",
     "{\n",
     "}\n",
     "\n",
     "static int\n",
     "upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,\n",
     "        ERL_NIF_TERM load_info)\n",
     "{\n",
     "    return 0;\n",
     "}\n",
     "\n",
     "static ErlNifFunc nif_funcs[] =\n",
     "{\n",
     [begin
          FnName = mk_fn(d_msg_, MsgName),
          CFnName = mk_c_fn(d_msg_, MsgName),
          IsLast = I == length(Defs),
          Comma = ["," || not IsLast],
          ?f("    {\"~s\", 1, ~s}~s\n", [FnName, CFnName, Comma])
      end
      || {I, {{msg, MsgName}, _MsgFields}} <- index_seq(Defs)],
     "\n",
     "};\n",
     "\n",
     ?f("ERL_NIF_INIT(~s, nif_funcs, load, reload, upgrade, unload)\n",
        [Mod])].

format_nif_cc_decoders(Mod, Defs, Opts) ->
    CPkg = get_cc_pkg(Defs),
    [format_nif_cc_decoder(Mod, CPkg, MsgName, Fields, Opts)
     || {{msg, MsgName}, Fields} <- Defs].

format_nif_cc_decoder(_Mod, CPkg, MsgName, _Fields, _Opts) ->
    FnName = mk_c_fn(d_msg_, MsgName),
    UnpackFnName = mk_c_fn(u_msg_, MsgName),
    CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
    ["static ERL_NIF_TERM\n",
     FnName,"(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])\n",
     "{\n",
     "    ErlNifBinary data;\n",
     "    ERL_NIF_TERM res;\n",
     "    ",CMsgType," *m = new ",CMsgType,"();\n",
     "\n"
     "    if (argc != 1)\n",
     "        return enif_make_badarg(env);\n",
     "\n",
     "    if (m == NULL)\n",
     "        return enif_make_badarg(env);\n", %% FIXME: enomem?
     "\n",
     "    if (!enif_inspect_binary(env, argv[0], &data))\n"
     "        return enif_make_badarg(env);\n",
     "\n",
     "    if (!m->ParseFromArray(data.data, data.size))\n",
     "    {\n",
     "        delete m;\n",
     "        return enif_make_badarg(env);\n",
     "    }\n",
     "\n",
     "    res = ",UnpackFnName,"(env, m);\n",
     "    delete m;\n",
     "    return res;\n",
     "}\n"
     "\n"].

format_nif_cc_unpackers(_Mod, Defs, _Opts) ->
    CPkg = get_cc_pkg(Defs),
    [format_nif_cc_unpacker(CPkg, MsgName, Fields, Defs)
     || {{msg, MsgName}, Fields} <- Defs].

format_nif_cc_unpacker(CPkg, MsgName, Fields, Defs) ->
    UnpackFnName = mk_c_fn(u_msg_, MsgName),
    CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
    Is = [I || {I,_} <- index_seq(Fields)],
    ["static ERL_NIF_TERM\n",
     UnpackFnName,"(ErlNifEnv *env, const ",CMsgType," *m)\n",
     "{\n",
     "    ERL_NIF_TERM res;\n",
     ?f("    ERL_NIF_TERM rname = ~s;\n", [mk_c_var(aa_, MsgName)]),
     [?f("    ERL_NIF_TERM elem~w;\n", [I]) || I <- Is],
     "\n",
     [begin
          DestVar = ?f("elem~w",[I]),
          format_nif_cc_field_unpacker(DestVar, "m", Field, Defs)
      end
      || {I, Field} <- index_seq(Fields)],
     "\n",
     ?f("    res = enif_make_tuple(env, ~w, rname~s);\n",
        [length(Fields) + 1, [?f(", elem~w",[I]) || I <- Is]]),
     "    return res;\n"
     "}\n",
     "\n"].

format_nif_cc_field_unpacker(DestVar, MsgVar, Field, Defs) ->
    #field{occurrence=Occurrence}=Field,
    case Occurrence of
        required ->
            format_nif_cc_field_unpacker_single(DestVar, MsgVar, Field, Defs);
        optional ->
            format_nif_cc_field_unpacker_single(DestVar, MsgVar, Field, Defs);
        repeated ->
            format_nif_cc_field_unpacker_repeated(DestVar, MsgVar, Field, Defs)
    end.


format_nif_cc_field_unpacker_single(DestVar, MsgVar, Field, Defs) ->
    #field{name=FName, type=FType} = Field,
    LCFName = to_lower(FName),
    [?f("    if (!~s->has_~s())\n", [MsgVar, LCFName]),
     ?f("        ~s = aa_undefined;\n", [DestVar]),
     ?f("    else\n"),
     indent_lines(
       8,
       case FType of
           float ->
               [?f("~s = enif_make_double(env, (double)~s->~s());\n",
                   [DestVar, MsgVar, LCFName])];
           double ->
               [?f("~s = enif_make_double(env, ~s->~s());\n",
                   [DestVar, MsgVar, LCFName])];
           _S32 when FType == sint32;
                     FType == int32;
                     FType == sfixed32 ->
               [?f("~s = enif_make_int(env, ~s->~s());\n",
                   [DestVar, MsgVar, LCFName])];
           _S64 when FType == sint64;
                     FType == int64;
                     FType == sfixed64 ->
               [?f("~s = enif_make_int64(env, (ErlNifSInt64)~s->~s());\n",
                   [DestVar, MsgVar, LCFName])];
           _U32 when FType == uint32;
                     FType == fixed32 ->
               [?f("~s = enif_make_uint(env, ~s->~s());\n",
                   [DestVar, MsgVar, LCFName])];
           _U64 when FType == uint64;
                     FType == fixed64 ->
               [?f("~s = enif_make_uint64(env, (ErlNifUInt64)~s->~s());\n",
                   [DestVar, MsgVar, LCFName])];
           bool ->
               [?f("if (~s->~s())\n", [MsgVar, LCFName]),
                ?f("    ~s = aa_true;\n", [DestVar]),
                ?f("else\n"),
                ?f("    ~s = aa_false;\n", [DestVar])];
           {enum, EnumName} ->
               {value, {{enum,EnumName}, Enumerations}} =
                   lists:keysearch({enum,EnumName}, 1, Defs),
               [] ++
                   [?f("switch (~s->~s()) {\n", [MsgVar, LCFName])] ++
                   [?f("    case ~w: ~s = ~s; break;\n",
                       [Value, DestVar, mk_c_var(aa_, Sym)])
                    || {Sym, Value} <- Enumerations] ++
                   [?f("    default: ~s = aa_undefined;\n", [DestVar])] ++
                   [?f("}\n")];
           string ->
               [?f("{\n"),
                ?f("    const char    *sData = ~s->~s().data();\n",
                   [    MsgVar, LCFName]),
                ?f("    unsigned int   sSize = ~s->~s().size();\n",
                   [    MsgVar, LCFName]),
                ?f("    ~s = utf8_to_erl_string(env, sData, sSize);\n",
                   [    DestVar]),
                ?f("}\n")];
           bytes ->
               [?f("{\n"),
                ?f("    unsigned char *data;\n"),
                ?f("    unsigned int   bSize = ~s->~s().size();\n",
                   [    MsgVar, LCFName]),
                ?f("    const char    *bData = ~s->~s().data();\n",
                   [    MsgVar, LCFName]),
                ?f("    data = enif_make_new_binary(\n"), %% can data be NULL??
                ?f("               env,\n"),
                ?f("               bSize,\n"),
                ?f("               &~s);\n", [DestVar]),
                ?f("    memmove(data, bData, bSize);\n"),
                ?f("}\n")];
           {msg, Msg2Name} ->
               UnpackFnName = mk_c_fn(u_msg_, Msg2Name),
               [?f("~s = ~s(env, &~s->~s());\n",
                   [DestVar, UnpackFnName, MsgVar, LCFName])]
       end),
     "\n"].

format_nif_cc_field_unpacker_repeated(DestVar, MsgVar, Field, Defs) ->
    #field{name=FName, type=FType} = Field,
    LCFName = to_lower(FName),
    [?f("    {\n"),
     ?f("        unsigned int numElems = ~s->~s_size();\n", [MsgVar, LCFName]),
     ?f("        ERL_NIF_TERM relem[numElems];\n"),
     ?f("        unsigned int i;\n"),
     "\n",
     ?f("        for (i = 0; i < numElems; i++)\n"),
     indent_lines(
       12,
       case FType of
           float ->
               [?f("relem[i] = enif_make_double(env, (double)~s->~s(i));\n",
                   [MsgVar, LCFName])];
           double ->
               [?f("relem[i] = enif_make_double(env, ~s->~s(i));\n",
                   [MsgVar, LCFName])];
           _S32 when FType == sint32;
                     FType == int32;
                     FType == sfixed32 ->
               [?f("relem[i] = enif_make_int(env, ~s->~s(i));\n",
                   [MsgVar, LCFName])];
           _S64 when FType == sint64;
                     FType == int64;
                     FType == sfixed64 ->
               [?f("relem[i] = enif_make_int64(env, (ErlNifSInt64)~s->~s(i));\n",
                   [MsgVar, LCFName])];
           _U32 when FType == uint32;
                     FType == fixed32 ->
               [?f("relem[i] = enif_make_uint(env, ~s->~s(i));\n",
                   [MsgVar, LCFName])];
           _U64 when FType == uint64;
                     FType == fixed64 ->
               [?f("relem[i] = enif_make_uint64(env,\n"
                   "                            (ErlNifUInt64)~s->~s(i));\n",
                   [MsgVar, LCFName])];
           bool ->
               [?f("if (~s->~s(i))\n", [MsgVar, LCFName]),
                ?f("    relem[i] = aa_true;\n"),
                ?f("else\n"),
                ?f("    relem[i] = aa_false;\n")];
           {enum, EnumName} ->
               {value, {{enum,EnumName}, Enumerations}} =
                   lists:keysearch({enum,EnumName}, 1, Defs),
               [] ++
                   [?f("switch (~s->~s(i)) {\n", [MsgVar, LCFName])] ++
                   [?f("    case ~w: relem[i] = ~s; break;\n",
                       [Value, mk_c_var(aa_, Sym)])
                    || {Sym, Value} <- Enumerations] ++
                   [?f("    default: relem[i] = aa_undefined;\n")] ++
                   [?f("}\n")];
           string ->
               [?f("{\n"),
                ?f("    const char    *sData = ~s->~s(i).data();\n",
                   [    MsgVar, LCFName]),
                ?f("    unsigned int   sSize = ~s->~s(i).size();\n",
                   [    MsgVar, LCFName]),
                ?f("    relem[i] = utf8_to_erl_string(env, sData, sSize);\n"),
                ?f("}\n")];
           bytes ->
               [?f("{\n"),
                ?f("    unsigned char *data;\n"),
                ?f("    unsigned int   bSize = ~s->~s(i).size();\n",
                   [    MsgVar, LCFName]),
                ?f("    const char    *bData = ~s->~s(i).data();\n",
                   [    MsgVar, LCFName]),
                ?f("    data = enif_make_new_binary(\n"), %% can data be NULL??
                ?f("               env,\n"),
                ?f("               bSize,\n"),
                ?f("               &relem[i]);\n"),
                ?f("    memmove(data, bData, bSize);\n"),
                ?f("}\n")];
           {msg, Msg2Name} ->
               UnpackFnName = mk_c_fn(u_msg_, Msg2Name),
               [?f("relem[i] = ~s(env, &~s->~s(i));\n",
                   [UnpackFnName, MsgVar, LCFName])]
       end),
     ?f("        ~s = enif_make_list_from_array(env, relem, numElems);\n",
        [DestVar]),
     "    }\n",
     "\n"].

format_load_nif(Mod, Opts) ->
    VsnAsList = gpb:version_as_list(),
    case proplists:get_value(load_nif, Opts, '$undefined') of
        '$undefined' ->
            ["load_nif() ->\n",
             %% Note: using ?MODULE here has impacts on compiling to
             %% binary, because we don't pass it through the preprocessor
             %% maybe we should?
             "    SelfDir = filename:dirname(code:which(?MODULE)),\n",
             "    NifDir = case lists:reverse(filename:split(SelfDir)) of\n"
             "                 [\"ebin\" | PDR] ->\n"
             "                      PD = filename:join(lists:reverse(PDR)),\n",
             "                      filename:join(PD, \"priv\");\n",
             "                 _ ->\n",
             "                      SelfDir\n",
             "             end,\n",
             "    NifBase = \"", atom_to_list(Mod) ++ ".nif", "\",\n",
             "    Nif = filename:join(NifDir, NifBase),\n",
             ?f("    erlang:load_nif(Nif, ~w).\n", [VsnAsList])];
        LoadNifFnText when is_list(LoadNifFnText); is_binary(LoadNifFnText) ->
            [replace_tilde_s(iolist_to_binary(LoadNifFnText),
                             iolist_to_binary(?f("\"~s.nif\"", [Mod])),
                             iolist_to_binary(?f("~w", [VsnAsList])))]
    end.

replace_tilde_s(<<"{{nifbase}}", Rest/binary>>, ModBin, VsnBin) ->
    <<ModBin/binary, (replace_tilde_s(Rest, ModBin, VsnBin))/binary>>;
replace_tilde_s(<<"{{loadinfo}}", Rest/binary>>, ModBin, VsnBin) ->
    <<VsnBin/binary, (replace_tilde_s(Rest, ModBin, VsnBin))/binary>>;
replace_tilde_s(<<C, Rest/binary>>, ModBin, VsnBin) ->
    <<C, (replace_tilde_s(Rest, ModBin, VsnBin))/binary>>;
replace_tilde_s(<<>>, _ModBin, _VsnBin) ->
    <<>>.

to_lower(A) when is_atom(A) ->
    list_to_atom(string:to_lower(atom_to_list(A))).

%% -- compile to memory -----------------------------------------------------

compile_to_binary(Mod, MsgDefs, ErlCode, PossibleNifCode, Opts) ->
    ModAsStr = flatten_iolist(?f("~p", [Mod])),
    ErlCode2 = replace_module_macro(ErlCode, ModAsStr),
    {ok, Toks, _EndLine} = erl_scan:string(ErlCode2),
    FormToks = split_toks_at_dot(Toks),
    Forms = [case erl_parse:parse_form(Ts) of
                 {ok, Form} ->
                     Form;
                 {error, Reason} ->
                     erlang:error(
                       {internal_error,?MODULE,Mod,ErlCode2,MsgDefs,
                        PossibleNifCode,Opts,Reason})
             end
             || Ts <- FormToks],
    {AttrForms, CodeForms} = split_forms_at_first_code(Forms),
    FieldDef = field_record_to_attr_form(),
    MsgRecordForms = msgdefs_to_record_attrs(MsgDefs),
    AllForms = AttrForms ++ [FieldDef] ++ MsgRecordForms ++ CodeForms,
    combine_erl_and_possible_nif(compile:forms(AllForms, Opts),
                                 PossibleNifCode).

replace_module_macro(Code, ModAsStr) ->
    rmm_aux(Code, ModAsStr, []).

rmm_aux(<<"?MODULE", Rest/binary>>, ModAsStr, Acc) ->
    rmm_aux(Rest, ModAsStr, lists:reverse(ModAsStr, Acc));
rmm_aux(<<C, Rest/binary>>, ModAsStr, Acc) ->
    rmm_aux(Rest, ModAsStr, [C | Acc]);
rmm_aux(<<>>, _ModAsStr, Acc) ->
    lists:reverse(Acc).

split_toks_at_dot(AllToks) ->
    case lists:splitwith(fun is_no_dot/1, AllToks) of
        {Toks, [{dot,_}=Dot]}      -> [Toks ++ [Dot]];
        {Toks, [{dot,_}=Dot | Tl]} -> [Toks ++ [Dot] | split_toks_at_dot(Tl)]
    end.

is_no_dot({dot,_}) -> false;
is_no_dot(_)       -> true.

split_forms_at_first_code(Forms) -> split_forms_at_first_code_2(Forms, []).

split_forms_at_first_code_2([{attribute,_,_,_}=Attr | Rest], Acc) ->
    split_forms_at_first_code_2(Rest, [Attr | Acc]);
split_forms_at_first_code_2([{function, _, _Name, _, _Clauses}|_]=Code, Acc) ->
    {lists:reverse(Acc), Code}.

field_record_to_attr_form() ->
    record_to_attr(field, record_info(fields, field)).

msgdefs_to_record_attrs(Defs) ->
    [record_to_attr(MsgName, lists:map(fun gpb_field_to_record_field/1, Fields))
     || {{msg, MsgName}, Fields} <- Defs].

record_to_attr(RecordName, Fields) ->
    {attribute, 0, record,
     {RecordName,
      [case F of
           {FName, Default} ->
               {record_field, 0, {atom, 0, FName}, erl_parse:abstract(Default)};
           {FName} ->
               {record_field, 0, {atom, 0, FName}};
           FName when is_atom(FName) ->
               {record_field, 0, {atom, 0, FName}}
       end
       || F <- Fields]}}.

gpb_field_to_record_field(#field{name=FName, opts=Opts}) ->
    case proplists:get_value(default, Opts) of
        undefined -> {FName};
        Default   -> {FName, Default}
    end.

combine_erl_and_possible_nif(ErlCompilationResult, '$not_generated'=_Nif) ->
    ErlCompilationResult;
combine_erl_and_possible_nif({ok, ModuleName, ErlCode}, NifTxt) ->
    {ok, ModuleName, combine_erlcode_with_niftxt(ErlCode, NifTxt)};
combine_erl_and_possible_nif({ok, ModuleName, ErlCode, Warnings}, NifTxt) ->
    {ok, ModuleName, combine_erlcode_with_niftxt(ErlCode, NifTxt), Warnings};
combine_erl_and_possible_nif(Error, _NifTxt) ->
    Error.

combine_erlcode_with_niftxt(ErlCode, NifTxt) ->
    [{erl, ErlCode},
     {nif, NifTxt}].

%% -- internal utilities -----------------------------------------------------

new_bindings(Tuples) ->
    lists:foldl(fun add_binding/2, new_bindings(), Tuples).

new_bindings() ->
    dict:new().

add_binding({Key, Value}, Bindings) ->
    dict:store(Key, Value, Bindings).

fetch_binding(Key, Bindings) ->
    dict:fetch(Key, Bindings).

%% a mapping is either a record or a map
%%
%%
mapping_match(RName, Fields, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> record_match(RName, Fields);
        maps    -> map_match(Fields)
    end.

mapping_create(RName, Fields, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> record_create(RName, Fields);
        maps    -> map_create(Fields)
    end.

mapping_update(Var, RName, FieldsValues, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> record_update(Var, RName, FieldsValues);
        maps    -> map_update(Var, FieldsValues)
    end.

mapping_access(Var, RName, FieldName, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> record_access(Var, RName, FieldName);
        maps    -> map_access(Var, FieldName)
    end.

get_records_or_maps_by_opts(Opts) ->
    Default = false,
    case proplists:get_value(maps, Opts, Default) of
        false -> records;
        true  -> maps
    end.


%% records
record_match(RName, Fields) -> record_create_or_match(RName, Fields).
record_create(RName, Fields) -> record_create_or_match(RName, Fields).

record_create_or_match(RecordName, FieldsValueTrees) ->
    record_update(none, RecordName, FieldsValueTrees).

record_update(Var, _RecordName, []) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
record_update(Var, RecordName, FieldsValueTrees) ->
    erl_syntax:record_expr(
      Var,
      erl_syntax:atom(RecordName),
      [erl_syntax:record_field(erl_syntax:atom(FName), ValueSyntaxTree)
       || {FName, ValueSyntaxTree} <- FieldsValueTrees]).

record_access(Var, RecordName, FieldName) ->
    erl_syntax:record_access(Var,
                             erl_syntax:atom(RecordName),
                             erl_syntax:atom(FieldName)).

%% maps
map_match(Fields) ->
    erl_syntax:text(
      ?ff("#{~s}", [string:join([?ff("~p := ~s", [FName, Var])
                                 || {FName, Var} <- map_kvars(Fields)],
                                ", ")])).

map_create(Fields) ->
    erl_syntax:text(
      ?ff("#{~s}", [string:join([?ff("~p => ~s", [FName, Val])
                                 || {FName, Val} <- map_kvalues(Fields)],
                                ", ")])).

map_update(Var, []) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_update(Var, FieldsValueTrees) ->
    erl_syntax:text(
      ?ff("~s#{~s}",
          [var_literal(Var),
           string:join([?ff("~p := ~s", [FName, Val])
                        || {FName, Val} <- map_kvalues(FieldsValueTrees)],
                       ", ")])).

map_access(Var, FieldName) ->
    erl_syntax:text(?ff("~s#{~p}", [var_literal(Var), FieldName])).

%% -> [{atom(), string()}]
map_kvars(KVars) ->
    [{Key, var_literal(Var)} || {Key, Var} <- KVars].

var_literal(Var) ->
    variable = erl_syntax:type(Var),
    erl_syntax:variable_literal(Var).

%% -> [{atom(), string()}]
map_kvalues(KVars) ->
    [begin
         ExprAsStr = erl_prettypr:format(Expr),
         {Key, ExprAsStr}
     end
     || {Key, Expr} <- KVars].


var_f_n(N) -> var_n("F", N).
var_b_n(N) -> var_n("B", N).

var_n(S, N) ->
    erl_syntax:variable(?ff("~s~w", [S, N])).

enum_to_binary_fields(Value) ->
    <<N:32/unsigned-native>> = <<Value:32/signed-native>>,
    varint_to_binary_fields(N).

key_to_binary_fields(FNum, Type) ->
    Key = (FNum bsl 3) bor gpb:encode_wiretype(Type),
    varint_to_binary_fields(Key).

varint_to_binary_fields(IntValue) ->
    [erl_syntax:binary_field(?expr('<n>', [replace_term('<n>', N)]), [])
     || N <- binary_to_list(gpb:encode_varint(IntValue))].

is_packed(#field{opts=Opts}) ->
    lists:member(packed, Opts).

get_field_pass(MsgName, #anres{d_field_pass_method=D}) ->
    dict:fetch(MsgName, D).

get_num_fields(MsgName, #anres{num_fields=D}) ->
    dict:fetch(MsgName, D).

smember(Elem, Set) -> %% set-member
    sets:is_element(Elem, Set).

smember_any(Elems, Set) -> %% is any elem a member in the set
    lists:any(fun(Elem) -> smember(Elem, Set) end,
              Elems).

index_seq([]) -> [];
index_seq(L)  -> lists:zip(lists:seq(1,length(L)), L).

%% lists_replace(N, List, New) -> NewList
%% Like erlang:setelement, but for a list:
%% Replace the Nth element in List with a New value.
lists_setelement(1, [_ | Rest], New) ->
    [New | Rest];
lists_setelement(N, [X | Rest], New) when N > 1 ->
    [X | lists_setelement(N - 1, Rest, New)].

%% Parse tree transform instructions
replace_term(Marker, NewTerm) when is_atom(Marker) ->
    {replace_term, Marker, NewTerm}.

replace_tree(Marker, NewTree) when is_atom(Marker) ->
    {replace_tree, Marker, NewTree}.

splice_trees(Marker, Trees) when is_atom(Marker) ->
    {splice_trees, Marker, Trees}.

splice_clauses(Marker, Clauses) when is_atom(Marker) ->
    {splice_clauses, Marker, Clauses}.

repeat_clauses(Marker, RepetitionReplacements) ->
    {repeat_clauses, Marker, RepetitionReplacements}.

get_strings_as_binaries_by_opts(Opts) ->
    proplists:get_bool(strings_as_binaries, Opts).

flatten_iolist(IoList) ->
    binary_to_list(iolist_to_binary(IoList)).

file_read_file(FileName, Opts) ->
    file_op(read_file, [FileName], Opts).

file_read_file_info(FileName, Opts) ->
    file_op(read_file_info, [FileName], Opts).

file_write_file(FileName, Bin, Opts) ->
    file_op(write_file, [FileName, Bin], Opts).

possibly_write_file(FileName, Bin, Opts) when is_binary(Bin) ->
    file_op(write_file, [FileName, Bin], Opts);
possibly_write_file(_FileName, '$not_generated', _Opts) ->
    ok.

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
