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

-module(gpb_compile).
%-compile(export_all).
-export([file/1, file/2]).
-export([msg_defs/2, msg_defs/3]).
-export([format_error/1, format_warning/1]).
-export([c/0, c/1]). % Command line interface, halts vm---don't use from shell!
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

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
%%                   {nif,boolean()} | nif |
%%                   {load_nif, LoadNif} |
%%                   {i, directory()} |
%%                   {o, directory()} |
%%                   {o_erl, directory()} | {o_hrl, directory()} |
%%                   {o_nif_cc, directory()} |
%%                   binary | to_msg_defs |
%%                   return | return_warnings | return_errors |
%%                   report | report_warnings | report_errors |
%%                   include_as_lib
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
%% The `nif' option will cause the compiler to generate nif C++ code
%% for that can be linked with the Google protobuf C++ library. The
%% purpose for this is speed: the Google protobuf C++ is faster than
%% the interpreted Erlang code, but will also lock up an Erlang
%% scheduler for as long as it takes for the Google protobuf library
%% to complete. This is normally a small amount of time, but can be a
%% longer time for huge messages.  The generated nif code can be
%% compiled with Erlang R14B or later. The `gpb_compile' will only
%% generate C++ code, it will <i>not</i> compile the generated C++
%% code for you. Below is an example of how to generate and compile
%% the C++ nif code under Linux. Details may differ from system to
%% system.
%%
%% <pre>
%%      # Use protoc to generate C++ code, and compile it.
%%      # This will generate x.pb.cc and x.pb.h from x.proto.
%%      protoc --cpp_out=$PWD x.proto
%%      g++ -g -fPIC -O3 -I/path/to/protobuf-include -o x.pb.o -c x.pb.cc
%%      # Generate Erlang code and C++ nif glue code, and compile it.
%%      # This will generate x.erl, x.hrl and x.nif.cc from x.proto.
%%      erl -boot start_clean -pa /path/to/gpb/ebin -noshell -noinput +B \
%%            -I$PWD -nif -s gpb_compile c x.proto
%%      erlc -Wall x.erl
%%      g++ -g -fPIC -Wall -O3 -I/path/to/protobuf -o x.nif.o -c x.nif.cc
%%      # Link all the C++ code together to a shared library.
%%      g++ -g -fPIC -shared -O3 -Wall -o x.nif.so x.nif.o x.pb.o \
%%            -L/path/to/protobuf-libs -lprotobuf \
%%            -Wl,-rpath=/path/to/protobuf-libs
%%      # Now, if you load x.beam into an Erlang VM, it will
%%      # automatically load x.nif.so which contains the nif glue C++
%%      code as well as the protobuf code.
%% </pre>
%%
%% NB: Caveats: Reloading or upgrading code compiled with the `nif'
%% option, might have unexpected behaviour.  The Google protobuf
%% library maintains a database of .proto descriptors and will
%% complain -- and halt the entire Erlang VM -- if you try to load nif
%% code in two different modules that defines a message with the same
%% name.  I have also seen cases where protobuf descriptors from old
%% versions of the nif code appears to be used even if new code has
%% been loaded.  The descriptor database appears to be kept as long as
%% the Google C++ protobuf library is loaded, and it is not possible
%% to control unloading of a sharerd library in Erlang, at least as of
%% R15B01.  Even if the code has been purged and deleted, the shared
%% object for the nif, and thus also for the Google protobuf library's
%% database, may still be kept in memory.
%%
%% The `load_nif' option lets you specify the code to use to load the nif.
%% The value to the `load_nif' must be a text that defines the function
%% `load_nif/0', that in the end calls `erlang:load_nif/2'.
%% Two special substrings are recognized and substituted in the text:
%% <dl>
%%   <dt>`{{nifbase}}'</dt>
%%   <dd>The basename of the nif file to be loaded (a string).
%%       Example: `"MyModule.nif"' if we are compiling `MyModule.proto'.
%%       This is intended to be (part of) the first argument in
%%       the call to `erlang:load_nif/2'.</dd>
%%   <dt>`{{loadinfo}}'</dt>
%%   <dd>This is a term that is intended to be the second argument in
%%       the call to `erlang:load_nif/2'.</dd>
%% </dl>
%% The default for the `load_nif' is as follows: If the module's
%% directory, as returned by`code:which/1', is on the form
%% `/path/to/ebin/Mod.beam', then the nif object code is loaded from
%% `/path/to/priv/Mod.nif'. Otherwise: if `code:which/1' returns
%% `/some/path/Mod.beam', then the nif is loaded from
%% `/some/path/Mod.nif'.
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
file(File, Opts1) ->
    Opts2 = normalize_return_report_opts(Opts1),
    case parse_file(File, Opts2) of
        {ok, Defs} ->
            Ext = filename:extension(File),
            Mod = list_to_atom(filename:basename(File, Ext)),
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
    Res1 = do_msg_defs(Defs, Mod, AnRes, Opts2),
    return_or_report_warnings_or_errors(Res1, Warns, Opts2,
                                        get_output_format(Opts2)).

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
            HrlTxt = format_hrl(Mod, Defs, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            ErlOutDir = get_erl_outdir(Opts),
            HrlOutDir = get_hrl_outdir(Opts),
            NifCcOutDir = get_nif_cc_outdir(Opts),
            Erl   = filename:join(ErlOutDir, atom_to_list(Mod) ++ ".erl"),
            Hrl   = filename:join(HrlOutDir, atom_to_list(Mod) ++ ".hrl"),
            NifCc = filename:join(NifCcOutDir, atom_to_list(Mod) ++ ".nif.cc"),
            case {file_write_file(Erl, ErlTxt, Opts),
                  file_write_file(Hrl, HrlTxt, Opts),
                  possibly_write_file(NifCc, NifTxt, Opts)} of
                {ok, ok, ok}       -> ok;
                {{error, R}, _, _} -> {error, {write_failed, Erl, R}};
                {_, {error, R}, _} -> {error, {write_failed, Erl, R}};
                {_, _, {error, R}} -> {error, {write_failed, NifCc,  R}}
            end
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
merge_warns({error, R, Warns1}, Warns2, _OutFmt) -> {error, R, Warns2++Warns1}.

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


%% @spec format_error({error, Reason} | Reason) -> io_list()
%%           Reason = term()
%%
%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link msg_defs/2}.
format_error({error, Reason, _Warns}) -> fmt_err(Reason);
format_error({error, Reason})         -> fmt_err(Reason);
format_error(Reason)                  -> fmt_err(Reason).

%% Note: do NOT include trailing newline (\n or ~n)
fmt_err({parse_error, FileName, {Line, Module, ErrInfo}}) ->
    f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)]);
fmt_err({scan_error, FileName, {Line, Module, ErrInfo}}) ->
    f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)]);
fmt_err({import_not_found, Import}) ->
    f("Could not find import file ~p", [Import]);
fmt_err({read_failed, File, Reason}) ->
    f("failed to read ~p: ~s (~p)", [File, file:format_error(Reason), Reason]);
fmt_err({verification, Reasons}) ->
    gpb_parse:format_verification_error({error, Reasons});
fmt_err({write_failed, File, Reason}) ->
    f("failed to write ~s: ~s (~p)", [File, file:format_error(Reason),Reason]);
fmt_err(X) ->
    f("Unexpected error ~p", [X]).

%% @spec format_warning(Reason) -> io_list()
%%           Reason = term()
%%
%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link msg_defs/2}.
%% @end
%% Note: do NOT include trailing newline (\n or ~n)
format_warning(cyclic_message_dependencies) ->
  f("Warning: omitting type specs due to cyclic message references.");
format_warning(X) ->
    case io_lib:deep_char_list(X) of
        true  -> X;
        false -> f("Warning: Unknown warning: ~p", [X])
    end.

%% @doc Command line interface for the compiler.
%% With no proto file to compile, print a help message and exit.
-spec c() -> no_return().
c() ->
    c([undefined]).

%% @doc This function is intended as a command line interface for the compiler.
%% Call it from the command line as follows:
%% ```
%%    erl <erlargs> [gpb-opts] -s gpb_compile c ProtoFile.proto
%%    erl <erlargs> -s gpb_compile c ProtoFile.proto -extra [gpb-opts]
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
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl</dd> respectively,
%%       and for the NIF C++ file, if the `-nif' option is specified.
%%       The `-o-erl Dir' option overrides any `-o Dir' option, and
%%       similarly for the other file-type specific output options.
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
%%   <dt>`-il'</dt>
%%   <dd>Generate code that include gpb.hrl using `-include_lib'
%%       instad of `-include', which is the default.</dd>
%%   <dt>`--help' or `-h'</dt>
%%   <dd>Show help.</dd>
%%   <dt>`--version' or `-V'</dt>
%%   <dd>Show the version number of gpb.</dd>
%% </dl>
-spec c([string() | atom()]) -> no_return().
c([File]) when is_atom(File); is_list(File) -> %% invoked with -s or -run
    FileName = if File == undefined -> undefined;
                  is_atom(File)     -> atom_to_list(File);
                  is_list(File)     -> File
               end,
    Args = init:get_arguments(),
    PlainArgs = init:get_plain_arguments(),
    Opts1 = parse_opts(Args, PlainArgs),
    Opts2 = [report_warnings, report_errors] ++ Opts1,
    case determine_cmdline_op(Opts2, FileName) of
        error  ->
            show_help(),
            init:stop(1);
        show_help  ->
            show_help(),
            init:stop(0);
        show_version  ->
            show_version(),
            init:stop(0);
        compile ->
            case file(FileName, Opts2) of
                ok ->
                    init:stop(0);
                {error, _Reason} ->
                    init:stop(1)
            end
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
      "    -il~n"
      "          Generate code that includes gpb.hrl using -include_lib~n"
      "          instad of -include, which is the default.~n"
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
parse_opt({"il", []})            -> {true, include_as_lib};
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
            %% io:format("processed these imports:~n  ~p~n", [_AllImported]),
            %% io:format("Defs1=~n  ~p~n", [Defs1]),
            Defs2 = gpb_parse:flatten_defs(
                      gpb_parse:absolutify_names(Defs1)),
            case gpb_parse:verify_defs(Defs2) of
                ok ->
                    {ok, gpb_parse:normalize_msg_field_options( %% Sort it?
                           gpb_parse:enumerate_msg_fields(
                             gpb_parse:extend_msgs(
                               gpb_parse:resolve_refs(
                                 gpb_parse:reformat_names(Defs2)))))};
                {error, Reasons} ->
                    {error, {verification, Reasons}}
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
      [f("%% Automatically generated, do not edit~n"
         "%% Generated by ~p version ~s on ~p~n",
         [?MODULE, gpb:version_as_string(), calendar:local_time()]),
       f("-module(~w).~n", [Mod]),
       "\n",
       f("-export([encode_msg/1, encode_msg/2]).~n"),
       f("-export([decode_msg/2]).~n"),
       f("-export([merge_msgs/2]).~n"),
       f("-export([verify_msg/1]).~n"),
       f("-export([get_msg_defs/0]).~n"),
       f("-export([get_msg_names/0]).~n"),
       f("-export([get_enum_names/0]).~n"),
       f("-export([find_msg_def/1, fetch_msg_def/1]).~n"),
       f("-export([find_enum_def/1, fetch_enum_def/1]).~n"),
       format_enum_value_symbol_converter_exports(Defs),
       f("-export([get_package_name/0]).~n"),
       f("-export([gpb_version_as_string/0, gpb_version_as_list/0]).~n"),
       "\n",
       [["-on_load(load_nif/0).\n",
         "-export([load_nif/0]). %% for debugging of nif loading\n",
         "\n"]
        || DoNif],
       f("-include(\"~s.hrl\").~n", [Mod]),
       if AsLib ->
               f("-include_lib(\"gpb/include/gpb.hrl\").~n");
          not AsLib ->
               f("-include(\"gpb.hrl\").~n")
       end,
       "\n",
       [[f("~s~n", [format_load_nif(Mod, Opts)]),
         "\n"]
        || DoNif],
       %% Enabling inlining seems to cause performance to drop drastically
       %% I've seen decoding performance go down from 76000 msgs/s
       %% to about 10000 msgs/s for a set of mixed message samples.
       %% f("-compile(inline).~n"),
       %%
       f("encode_msg(Msg) ->~n"
         "    encode_msg(Msg, []).~n~n"),
       case proplists:get_value(verify, Opts, optionally) of
           optionally ->
               f("encode_msg(Msg, Opts) ->~n"
                 "    case proplists:get_bool(verify, Opts) of~n"
                 "        true  -> verify_msg(Msg);~n"
                 "        false -> ok~n"
                 "    end,~n"
                 "    ~s.~n", [format_encoder_topcase(4, Defs, "Msg")]);
           always ->
               f("encode_msg(Msg, _Opts) ->~n"
                 "    verify_msg(Msg),~n"
                 "    ~s.~n", [format_encoder_topcase(4, Defs, "Msg")]);
           never ->
               f("encode_msg(Msg, _Opts) ->~n"
                 "    ~s.~n", [format_encoder_topcase(4, Defs, "Msg")])
       end,
       "\n",
       f("~s~n", [format_encoders(Defs, AnRes, Opts)]),
       "\n",
       f("decode_msg(Bin, MsgName) ->~n"
         "    ~s.~n",
         [format_decoder_topcase(4, Defs, "Bin", "MsgName")]),
       "\n",
       if DoNif ->
               f("~s~n", [format_nif_decoder_error_wrappers(
                            Defs, AnRes, Opts)]);
          not DoNif ->
               f("~s~n", [format_decoders(Defs, AnRes, Opts)])
       end,
       "\n",
       f("~s~n", [format_msg_merge_code(Defs, AnRes)]),
       "\n",
       f("verify_msg(Msg) ->~n" %% Option to use gpb:verify_msg??
         "    ~s.~n", [format_verifier_topcase(4, Defs, "Msg")]),
       "\n",
       f("~s~n", [format_verifiers(Defs, AnRes, Opts)]),
       "\n",
       format_introspection(Defs),
       "\n",
       f("gpb_version_as_string() ->~n"),
       f("    \"~s\".~n", [gpb:version_as_string()]),
       "\n",
       f("gpb_version_as_list() ->~n"),
       f("    ~w.~n", [gpb:version_as_list()])]).

%% -- encoders -----------------------------------------------------

format_encoder_topcase(Indent, Defs, MsgVar) ->
    IndStr = indent(Indent+4, ""),
    ["case ", MsgVar, " of\n",
     IndStr,
     string:join([f("#~p{} -> ~p(~s)",
                    [MsgName, mk_fn(e_msg_, MsgName), MsgVar])
                  || {{msg, MsgName}, _MsgDef} <- Defs],
                 ";\n"++IndStr),
     "\n",
     indent(Indent, "end")].

format_encoders(Defs, AnRes, _Opts) ->
    [format_enum_encoders(Defs, AnRes),
     format_msg_encoders(Defs),
     format_special_field_encoders(Defs, AnRes),
     format_type_encoders(AnRes)
    ].

format_enum_encoders(Defs, #anres{used_types=UsedTypes}) ->
    [format_enum_encoder(EnumName, EnumDef)
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_enum_encoder(EnumName, EnumDef) ->
    FnName = mk_fn(e_enum_, EnumName),
    [string:join([f("~p(~p, Bin) -> <<Bin/binary, ~s>>",
                    [FnName, EnumSym, encode_format_enum_value(EnumValue)])
                  || {EnumSym, EnumValue} <- EnumDef],
                 ";\n"),
     ".\n\n"].

encode_format_enum_value(Value) ->
    <<N:32/unsigned-native>> = <<Value:32/signed-native>>,
    varint_to_byte_text(N).

varint_to_byte_text(N) ->
    Bin = gpb:encode_varint(N),
    string:join([integer_to_list(B) || <<B:8>> <= Bin], ",").

format_msg_encoders(Defs) ->
    [format_msg_encoder(MsgName, MsgDef) || {{msg, MsgName}, MsgDef} <- Defs].

format_msg_encoder(MsgName, []) ->
    FnName = mk_fn(e_msg_, MsgName),
    [f("~p(_Msg) ->~n", [FnName]),
     f("    <<>>.~n~n")];
format_msg_encoder(MsgName, MsgDef) ->
    FnName = mk_fn(e_msg_, MsgName),
    MatchIndent = flength("~p(#~p{", [FnName, MsgName]),
    MatchCommaSep = f(",~n~s", [indent(MatchIndent, "")]),
    FieldMatchings = string:join([f("~p=F~s", [FName, FName])
                                  || #field{name=FName} <- MsgDef],
                                 MatchCommaSep),
    [f("~p(Msg) ->~n", [FnName]),
     f("    ~p(Msg, <<>>).~n", [FnName]),
     f("~n"),
     f("~p(#~p{~s}, Bin0) ->~n", [FnName, MsgName, FieldMatchings]),
     string:join(
       [begin
            FieldEncoderFn = mk_field_encode_fn_name(MsgName, Field),
            ResultVar = if I == length(MsgDef) -> "";
                           true                -> f("Bin~w = ", [I])
                        end,
            RIndent = lists:flatlength(ResultVar),
            KeyTxt = mk_key_txt(FNum, Type),
            if Occurrence == optional ->
                    f("    ~sif F~s == undefined -> Bin~w;~n"
                      "    ~*s  true -> ~p(F~s, <<Bin~w/binary, ~s>>)~n"
                      "    ~*send",
                      [ResultVar, FName, I-1,
                       RIndent, "", FieldEncoderFn, FName, I-1, KeyTxt,
                       RIndent, ""]);
               Occurrence == repeated ->
                    f("    ~sif F~s == [] -> Bin~w;~n"
                      "    ~*s  true -> ~p(F~s, Bin~w)~n"
                      "    ~*send",
                      [ResultVar, FName, I-1,
                       RIndent, "", FieldEncoderFn, FName, I-1,
                       RIndent, ""]);
               Occurrence == required ->
                    f("    ~s~p(F~s, <<Bin~w/binary, ~s>>)",
                      [ResultVar, FieldEncoderFn, FName, I-1, KeyTxt])
            end
        end
        || {I, #field{name=FName, occurrence=Occurrence, type=Type,
                      fnum=FNum}=Field} <- index_seq(MsgDef)],
       ",\n"),
     ".\n",
     "\n"].

mk_key_txt(FNum, Type) ->
    Key = (FNum bsl 3) bor gpb:encode_wiretype(Type),
    varint_to_byte_text(Key).

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
    case is_msgsize_known_at_compiletime(SubMsg, AnRes) of
        no ->
            [f("~p(Msg, Bin) ->~n", [FnName]),
             f("    SubBin = ~p(Msg, <<>>),~n", [mk_fn(e_msg_, SubMsg)]),
             f("    Bin2 = e_varint(byte_size(SubBin), Bin),~n"),
             f("    <<Bin2/binary, SubBin/binary>>.~n~n")];
        {yes, MsgSize} when MsgSize > 0 ->
            MsgSizeTxt = varint_to_byte_text(MsgSize),
            [f("~p(Msg, Bin) ->~n", [FnName]),
             f("    Bin2 = <<Bin/binary, ~s>>,~n", [MsgSizeTxt]),
             f("    ~p(Msg, Bin2).~n~n", [mk_fn(e_msg_, SubMsg)])];
        {yes, 0} ->
            %% special case, there will not be any e_msg_<MsgName>/2 function
            %% generated, so don't call it.
            [f("~p(_Msg, Bin) ->~n", [FnName]),
             f("    <<Bin/binary, 0>>.~n~n")]
    end;
possibly_format_mfield_encoder(_MsgName, _FieldDef, _Defs) ->
    [].

is_msgsize_known_at_compiletime(MsgName, #anres{known_msg_size=MsgSizes}) ->
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
    KeyTxt = mk_key_txt(FNum, Type),
    [f("~p([Elem | Rest], Bin) ->~n", [FnName]),
     f("    Bin2 = <<Bin/binary, ~s>>,~n", [KeyTxt]),
     f("    Bin3 = ~p(Elem, Bin2),~n", [ElemEncoderFn]),
     f("    ~p(Rest, Bin3);~n", [FnName]),
     f("~p([], Bin) ->~n", [FnName]),
     f("    Bin.~n~n")].

format_packed_field_encoder2(MsgName, #field{type=Type}=FDef) ->
    case packed_byte_size_can_be_computed(Type) of
        {yes, BitLen, BitType} ->
            format_knownsize_packed_field_encoder2(MsgName, FDef,
                                                  BitLen, BitType);
        no ->
            format_unknownsize_packed_field_encoder2(MsgName, FDef)
    end.

packed_byte_size_can_be_computed(fixed32)  -> {yes, 32, 'little'};
packed_byte_size_can_be_computed(sfixed32) -> {yes, 32, 'little-signed'};
packed_byte_size_can_be_computed(float)    -> {yes, 32, 'little-float'};
packed_byte_size_can_be_computed(fixed64)  -> {yes, 64, 'little'};
packed_byte_size_can_be_computed(sfixed64) -> {yes, 64, 'little-signed'};
packed_byte_size_can_be_computed(double)   -> {yes, 64, 'little-float'};
packed_byte_size_can_be_computed(_)        -> no.

format_knownsize_packed_field_encoder2(MsgName, #field{name=FName,
                                                       fnum=FNum}=FDef,
                                      BitLen, BitType) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    KeyTxt = mk_key_txt(FNum, bytes),
    PackedFnName = mk_fn(e_pfield_, MsgName, FName),
    [f("~p(Elems, Bin) when Elems =/= [] ->~n", [FnName]),
     f("    Bin2 = <<Bin/binary, ~s>>,~n", [KeyTxt]),
     f("    Bin3 = e_varint(length(Elems) * ~w, Bin2),~n", [BitLen div 8]),
     f("    ~p(Elems, Bin3);~n", [PackedFnName]),
     f("~p([], Bin) ->~n", [FnName]),
     f("    Bin.~n"),
     f("~n"),
     f("~p([Value | Rest], Bin) ->~n", [PackedFnName]),
     f("    Bin2 = <<Bin/binary, Value:~w/~s>>,~n", [BitLen, BitType]),
     f("    ~p(Rest, Bin2);~n", [PackedFnName]),
     f("~p([], Bin) ->~n", [PackedFnName]),
     f("    Bin.~n~n")].

format_unknownsize_packed_field_encoder2(MsgName, #field{name=FName,
                                                         fnum=FNum}=FDef) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(MsgName,
                                            FDef#field{occurrence=required}),
    KeyTxt = mk_key_txt(FNum, bytes),
    PackedFnName = mk_fn(e_pfield_, MsgName, FName),
    [f("~p(Elems, Bin) when Elems =/= [] ->~n", [FnName]),
     f("    SubBin = ~p(Elems, <<>>),~n", [PackedFnName]),
     f("    Bin2 = <<Bin/binary, ~s>>,~n", [KeyTxt]),
     f("    Bin3 = e_varint(byte_size(SubBin), Bin2),~n"),
     f("    <<Bin3/binary, SubBin/binary>>;~n"),
     f("~p([], Bin) ->~n", [FnName]),
     f("    Bin.~n"),
     f("~n"),
     f("~p([Value | Rest], Bin) ->~n", [PackedFnName]),
     f("    Bin2 = ~p(Value, Bin),~n", [ElemEncoderFn]),
     f("    ~p(Rest, Bin2);~n", [PackedFnName]),
     f("~p([], Bin) ->~n", [PackedFnName]),
     f("    Bin.~n~n")].

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
    [[format_fixed_encoder(fixed32,  32, 'little')        || NeedsFixed32],
     [format_fixed_encoder(sfixed32, 32, 'little-signed') || NeedsSFixed32],
     [format_fixed_encoder(float,    32, 'little-float')  || NeedsFloat],
     [format_fixed_encoder(fixed64,  64, 'little')        || NeedsFixed64],
     [format_fixed_encoder(sfixed64, 64, 'little-signed') || NeedsSFixed64],
     [format_fixed_encoder(double,   64, 'little-float')  || NeedsDouble]].

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
    [f("e_type_sint(Value, Bin) when Value >= 0 ->~n"),
     f("    e_varint(Value * 2, Bin);~n"),
     f("e_type_sint(Value, Bin) ->~n"),
     f("    e_varint(Value * -2 - 1, Bin).~n~n")].

format_int_encoder(Type, BitLen) ->
    FnName = mk_fn(e_type_, Type),
    [f("~p(Value, Bin) when 0 =< Value, Value =< 127 ->~n", [FnName]),
     f("    <<Bin/binary, Value>>; %% fast path~n"),
     f("~p(Value, Bin) ->~n", [FnName]),
     f("    <<N:~w/unsigned-native>> = <<Value:~w/signed-native>>,~n",
       [BitLen, BitLen]),
     f("    e_varint(N, Bin).~n~n")].

format_bool_encoder() ->
    [f("e_type_bool(true, Bin)  -> <<Bin/binary, 1>>;~n"),
     f("e_type_bool(false, Bin) -> <<Bin/binary, 0>>.~n~n")].

format_fixed_encoder(Type, BitLen, BitType) ->
    FnName = mk_fn(e_type_, Type),
    [f("~p(Value, Bin) ->~n", [FnName]),
     f("    <<Bin/binary, Value:~p/~s>>.~n~n", [BitLen, BitType])].

format_string_encoder() ->
    [f("e_type_string(S, Bin) ->~n"),
     f("    Utf8 = unicode:characters_to_binary(S),~n"),
     f("    Bin2 = e_varint(byte_size(Utf8), Bin),~n"),
     f("    <<Bin2/binary, Utf8/binary>>.~n~n")].

format_bytes_encoder() ->
    [f("e_type_bytes(Bytes, Bin) ->~n"),
     f("    Bin2 = e_varint(byte_size(Bytes), Bin),~n"),
     f("    <<Bin2/binary, Bytes/binary>>.~n~n")].

format_varint_encoder() ->
    [f("e_varint(N, Bin) when N =< 127 ->~n"),
     f("    <<Bin/binary, N>>;~n"),
     f("e_varint(N, Bin) ->~n"),
     f("    Bin2 = <<Bin/binary, (N band 127 bor 128)>>,~n"),
     f("    e_varint(N bsr 7, Bin2).~n~n")].

%% -- decoders -----------------------------------------------------

format_decoder_topcase(Indent, Defs, BinVar, MsgNameVar) ->
    ["case ", MsgNameVar, " of\n",
     string:join([indent(Indent+4, f("~p -> ~p(~s)",
                                     [MsgName, mk_fn(d_msg_, MsgName), BinVar]))
                  || {{msg, MsgName}, _MsgDef} <- Defs],
                 ";\n"),
     "\n",
     indent(Indent, f("end"))].

format_decoders(Defs, AnRes, Opts) ->
    [format_enum_decoders(Defs, AnRes),
     format_initial_msgs(Defs, AnRes),
     format_msg_decoders(Defs, AnRes, Opts)].

format_enum_decoders(Defs, #anres{used_types=UsedTypes}) ->
    %% FIXME: enum values can be negative, but "raw" varints are positive
    %%        insert a 2-complement in the mapping in order to move computations
    %%        from run-time to compile-time??
    [[string:join([f("~p(~w) -> ~p",
                     [mk_fn(d_enum_, EnumName), EnumValue, EnumSym])
                   || {EnumSym, EnumValue} <- EnumDef],
                  ";\n"),
      ".\n\n"]
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_initial_msgs(Defs, AnRes) ->
    [format_initial_msg(MsgName, MsgDef, Defs, AnRes)
     || {{msg, MsgName}, MsgDef} <- Defs].

format_initial_msg(MsgName, MsgDef, Defs, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params -> ""; %% handled in d_read_field_def_<MsgName>/1
        pass_as_record -> format_initial_msg_par(MsgName, MsgDef, Defs)
    end.

format_initial_msg_par(MsgName, MsgDef, Defs) ->
    [f("~p() ->~n", [mk_fn(msg0_, MsgName)]),
     indent(4, format_initial_msg_record(4, MsgName, MsgDef, Defs)),
     ".\n\n"].

format_initial_msg_record(Indent, MsgName, MsgDef, Defs) ->
    MsgNameQLen = flength("~p", [MsgName]),
    f("#~p{~s}",
      [MsgName, format_initial_msg_fields(Indent+MsgNameQLen+2, MsgDef, Defs)]).

format_initial_msg_fields(Indent, MsgDef, Defs) ->
    outdent_first(
      string:join(
        [case Field of
             #field{occurrence=repeated} ->
                 indent(Indent, f("~p = []", [FName]));
             #field{type={msg,FMsgName}} ->
                 FNameQLen = flength("~p", [FName]),
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

format_initial_field_values(MsgDef) ->
    [[", ", case Occurrence of
                repeated -> "[]";
                required -> "undefined"; %% Use default value? (if available)
                optional -> "undefined"
            end] || #field{occurrence = Occurrence} <- MsgDef].

format_msg_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoder(MsgName, MsgDef, AnRes, Opts)
     || {{msg, MsgName}, MsgDef} <- Defs].

format_msg_decoder(MsgName, MsgDef, AnRes, Opts) ->
    [format_msg_decoder_read_field(MsgName, MsgDef, AnRes),
     format_msg_decoder_reverse_toplevel(MsgName, MsgDef, AnRes),
     format_field_decoders(MsgName, MsgDef, AnRes, Opts),
     format_field_adders(MsgName, MsgDef, AnRes),
     format_field_skippers(MsgName, AnRes)].

format_msg_decoder_read_field(MsgName, MsgDef, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params -> format_msg_decoder_read_field_pap(MsgName, MsgDef);
        pass_as_record -> format_msg_decoder_read_field_par(MsgName, MsgDef)
    end.

format_msg_decoder_read_field_pap(MsgName, MsgDef) ->
    FieldVars = [f(", F~w", [I]) || I <- lists:seq(1,length(MsgDef))],
    ReadFieldCases = format_read_field_cases(MsgName, MsgDef, pass_as_params,
                                             FieldVars),
    MsgIndent = flength("    #~p{", [MsgName]),
    FieldSets = string:join([case Occurrence of
                                 required -> f("~p = F~w", [FName, I]);
                                 optional -> f("~p = F~w", [FName, I]);
                                 repeated -> f("~p = lists:reverse(F~w)",
                                               [FName, I])
                             end
                             || {I, #field{occurrence=Occurrence,
                                           name=FName}} <- index_seq(MsgDef)],
                            f(",~n~s",[indent(MsgIndent, "")])),
    [f("~p(Bin) ->~n", [mk_fn(d_msg_, MsgName)]),
     f("    ~p(Bin, 0, 0~s).~n",
       [mk_fn(d_read_field_def_, MsgName),
        format_initial_field_values(MsgDef)]),
     "\n",
     f("~p(<<1:1, X:7, Rest/binary>>, N, Acc~s) ->~n"
       "    ~p(Rest, N+7, X bsl N + Acc~s);~n",
       [mk_fn(d_read_field_def_, MsgName), FieldVars,
        mk_fn(d_read_field_def_, MsgName), FieldVars]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc~s) ->~n"
       "    Key = X bsl N + Acc,~n",
       [mk_fn(d_read_field_def_, MsgName), FieldVars]),
     if ReadFieldCases == [] ->
             f("    case Key band 7 of  %% WireType~n"
               "        0 -> 'skip_varint_~s'(Rest, 1, 1~s);~n"
               "        1 -> 'skip_64_~s'(Rest, 1, 1~s);~n"
               "        2 -> 'skip_length_delimited_~s'(Rest, 0, 0~s);~n"
               "        5 -> 'skip_32_~s'(Rest, 1, 1~s)~n"
               "    end;~n",
               [MsgName, FieldVars,
                MsgName, FieldVars,
                MsgName, FieldVars,
                MsgName, FieldVars]);
        true ->
             f("    case Key of~n"
               "~s"
               "        _ ->~n"
               "            case Key band 7 of %% wiretype~n"
               "                0 -> 'skip_varint_~s'(Rest, 1, 1~s);~n"
               "                1 -> 'skip_64_~s'(Rest, 1, 1~s);~n"
               "                2 -> 'skip_length_delimited_~s'(Rest,0,0~s);~n"
               "                5 -> 'skip_32_~s'(Rest, 1, 1~s)~n"
               "            end~n"
               "    end;~n",
               [ReadFieldCases,
                MsgName, FieldVars,
                MsgName, FieldVars,
                MsgName, FieldVars,
                MsgName, FieldVars])
     end,
     f("~p(<<>>, 0, 0~s) ->~n", [mk_fn(d_read_field_def_, MsgName), FieldVars]),
     f("    #~p{~s}.~n~n", [MsgName, FieldSets])].

format_msg_decoder_read_field_par(MsgName, MsgDef) ->
    ReadFieldCases = format_read_field_cases(MsgName, MsgDef, pass_as_record, x),
    [f("~p(Bin) ->~n", [mk_fn(d_msg_, MsgName)]),
     indent(4, f("Msg0 = ~s(),~n", [mk_fn(msg0_, MsgName)])),
     indent(4, f("~p(Bin, 0, 0, Msg0).~n", [mk_fn(d_read_field_def_, MsgName)])),
     "\n",
     f("~p(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n"
       "    ~p(Rest, N+7, X bsl N + Acc, Msg);~n",
       [mk_fn(d_read_field_def_, MsgName),
        mk_fn(d_read_field_def_, MsgName)]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n"
       "    Key = X bsl N + Acc,~n", [mk_fn(d_read_field_def_, MsgName)]),
     if ReadFieldCases == [] ->
             f("    case Key band 7 of  %% WireType~n"
               "        0 -> 'skip_varint_~s'(Rest, Msg);~n"
               "        1 -> 'skip_64_~s'(Rest, Msg);~n"
               "        2 -> 'skip_length_delimited_~s'(Rest, 0, 0, Msg);~n"
               "        5 -> 'skip_32_~s'(Rest, Msg)~n"
               "    end;~n",
               [MsgName,MsgName,MsgName,MsgName]);
        true ->
             f("    case Key of~n"
               "~s"
               "        _ ->~n"
               "            case Key band 7 of %% wiretype~n"
               "                0 -> 'skip_varint_~s'(Rest, Msg);~n"
               "                1 -> 'skip_64_~s'(Rest, Msg);~n"
               "                2 -> 'skip_length_delimited_~s'(Rest,0,0,Msg);~n"
               "                5 -> 'skip_32_~s'(Rest, Msg)~n"
               "            end~n"
               "    end;~n",
               [ReadFieldCases,
                MsgName,MsgName,MsgName,MsgName])
     end,
     f("~p(<<>>, 0, 0, Msg) ->~n"
       "    ~p(Msg).~n~n",
       [mk_fn(d_read_field_def_, MsgName),
        mk_fn(d_reverse_toplevel_fields_, MsgName)])].

format_read_field_cases(MsgName, MsgDef, FieldPass, FieldVars) ->
    [begin
         Wiretype = case is_packed(FieldDef) of
                        true  -> gpb:encode_wiretype(bytes);
                        false -> gpb:encode_wiretype(Type)
                    end,
         Key = (FNum bsl 3) bor Wiretype,
         Fill = if FieldPass == pass_as_params -> fill;
                   FieldPass == pass_as_record -> no_fill
                end,
         indent(8, f("~w -> ~p(Rest~s~s);~n",
                 [Key, mk_fn(d_field_, MsgName, FName),
                  case mk_field_decoder_vi_params(FieldDef, Fill) of
                      ""     -> [];
                      Params -> [", ", Params]
                  end,
                  if FieldPass == pass_as_record -> ", Msg";
                     FieldPass == pass_as_params -> FieldVars
                  end]))
     end
     || #field{fnum=FNum, type=Type, name=FName}=FieldDef <- MsgDef].


mk_field_decoder_vi_params(#field{type=Type}=FieldDef, Fill) ->
    case is_packed(FieldDef) of
        false ->
            case Type of
                sint32   -> "0, 0"; %% varint-based
                sint64   -> "0, 0"; %% varint-based
                int32    -> "0, 0"; %% varint-based
                int64    -> "0, 0"; %% varint-based
                uint32   -> "0, 0"; %% varint-based
                uint64   -> "0, 0"; %% varint-based
                bool     -> "0, 0"; %% varint-based
                {enum,_} -> "0, 0"; %% varint-based
                fixed32  -> if Fill == fill -> "0, 0"; true -> "" end;
                sfixed32 -> if Fill == fill -> "0, 0"; true -> "" end;
                float    -> if Fill == fill -> "0, 0"; true -> "" end;
                fixed64  -> if Fill == fill -> "0, 0"; true -> "" end;
                sfixed64 -> if Fill == fill -> "0, 0"; true -> "" end;
                double   -> if Fill == fill -> "0, 0"; true -> "" end;
                string   -> "0, 0"; %% varint-based
                bytes    -> "0, 0"; %% varint-based
                {msg,_}  -> "0, 0"  %% varint-based
            end;
        true ->
            "0, 0" %% length of packed bytes is varint-based
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
        fixed32  -> format_uf32_field_decoder(MsgName, Field, AnRes);
        sfixed32 -> format_sf32_field_decoder(MsgName, Field, AnRes);
        float    -> format_float_field_decoder(MsgName, Field, AnRes);
        fixed64  -> format_uf64_field_decoder(MsgName, Field, AnRes);
        sfixed64 -> format_sf64_field_decoder(MsgName, Field, AnRes);
        double   -> format_double_field_decoder(MsgName, Field, AnRes);
        string   -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, Field, AnRes, Opts)
    end.

format_packed_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            format_packed_field_decoder_pap(MsgName, FieldDef, AnRes, Opts);
        pass_as_record ->
            format_packed_field_decoder_par(MsgName, FieldDef, Opts)
    end.

format_packed_field_decoder_pap(MsgName, FieldDef, AnRes, Opts) ->
    NF = get_num_fields(MsgName, AnRes),
    #field{name=FName, rnum=RNum, opts=FOpts}=FieldDef,
    DecodePackWrapFn = mk_fn(d_field_, MsgName, FName),
    FieldDefAsNonpacked = FieldDef#field{opts = FOpts -- [packed]},
    FieldVars = [[", ", f("F~w", [I])] || I <- lists:seq(1, NF)],
    InFieldVars = [[", ", if I == RNum-1 -> "AccSeq";
                             I /= RNum-1 -> f("F~w", [I])
                          end]
                   || I <- lists:seq(1, NF)],
    OutFieldVars = [[", ", if I == RNum-1 -> "NewSeq";
                              I /= RNum-1 -> f("F~w", [I])
                           end]
                    || I <- lists:seq(1, NF)],
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc~s) ->~n",
       [DecodePackWrapFn, FieldVars]),
     f("    ~p(Rest, N+7, X bsl N + Acc~s);~n", [DecodePackWrapFn, FieldVars]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc~s) ->~n",
       [DecodePackWrapFn, InFieldVars]),
     f("    Len = X bsl N + Acc,~n"),
     f("    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,~n"),
     f("    NewSeq = ~p(PackedBytes~sAccSeq),~n",
       [mk_fn(d_packed_field_, MsgName, FName),
        case mk_field_decoder_vi_params(FieldDefAsNonpacked, no_fill) of
            ""     -> [", "];
            Params -> [", ", Params, ", "]
        end]),
     f("    ~p(Rest2, 0, 0~s).~n~n",
       [mk_fn(d_read_field_def_, MsgName), OutFieldVars]),
     format_packed_field_seq_decoder(MsgName, FieldDef, Opts)].

format_packed_field_decoder_par(MsgName, FieldDef, Opts) ->
    #field{name=FName, opts=FOpts}=FieldDef,
    DecodePackWrapFn = mk_fn(d_field_, MsgName, FName),
    FieldDefAsNonpacked = FieldDef#field{opts = FOpts -- [packed]},
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n", [DecodePackWrapFn]),
     f("    ~p(Rest, N+7, X bsl N + Acc, Msg);~n", [DecodePackWrapFn]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc, #~p{~p=AccSeq}=Msg) ->~n",
       [DecodePackWrapFn, MsgName, FName]),
     f("    Len = X bsl N + Acc,~n"),
     f("    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,~n"),
     f("    NewSeq = ~p(PackedBytes~sAccSeq),~n",
       [mk_fn(d_packed_field_, MsgName, FName),
        case mk_field_decoder_vi_params(FieldDefAsNonpacked, no_fill) of
            ""     -> [", "];
            Params -> [", ", Params, ", "]
        end]),
     f("    NewMsg = Msg#~p{~p=NewSeq},~n", [MsgName, FName]),
     f("    ~p(Rest2, 0, 0, NewMsg).~n~n", [mk_fn(d_read_field_def_, MsgName)]),
     format_packed_field_seq_decoder(MsgName, FieldDef, Opts)].

format_vi_based_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            format_vi_based_field_decoder_pap(MsgName, FieldDef, AnRes, Opts);
        pass_as_record ->
            format_vi_based_field_decoder_par(MsgName, FieldDef, Opts)
    end.

format_vi_based_field_decoder_pap(MsgName, FieldDef, AnRes, Opts) ->
    NF = get_num_fields(MsgName, AnRes),
    #field{type=Type, rnum=RNum, name=FName}=FieldDef,
    Merge = classify_field_merge_action(FieldDef),
    PassFieldVars = [[", ", f("F~w", [I])] || I <- lists:seq(1, NF)],
    InFieldVars = [[", ", if I == RNum-1, Merge == overwrite -> "_";
                             I == RNum-1, Merge == seqadd    -> f("F~w", [I]);
                             I == RNum-1, Merge == msgmerge  -> f("F~w", [I]);
                             I /= RNum-1                     -> f("F~w", [I])
                          end]
                   || I <- lists:seq(1, NF)],
    MergeCode = case Merge of
                    overwrite -> "";
                    seqadd    -> f("    FValue2 = [FValue | F~w],~n", [RNum-1]);
                    msgmerge  -> {msg,FMsgName} = Type,
                                 f("    FValue2 = if F~w == undefined ->~n"
                                   "                     FValue;~n"
                                   "                 true ->~n"
                                   "                     ~p(F~w, FValue)~n"
                                   "              end,~n",
                                   [RNum-1,
                                    mk_fn(merge_msg_, FMsgName), RNum-1])
                end,
    OutFieldVars = [[", ", if I == RNum-1, Merge == overwrite -> "FValue";
                              I == RNum-1, Merge == seqadd    -> "FValue2";
                              I == RNum-1, Merge == msgmerge  -> "FValue2";
                              I /= RNum-1                     -> f("F~w", [I])
                           end]
                    || I <- lists:seq(1, NF)],
    BValueExpr = "X bsl N + Acc",
    {FValueCode, RestVar} = mk_unpack_vi(4, "FValue", BValueExpr, Type, "Rest",
                                         Opts),
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc~s) ->~n"
       "    ~p(Rest, N+7, X bsl N + Acc~s);~n",
       [mk_fn(d_field_, MsgName, FName), PassFieldVars,
        mk_fn(d_field_, MsgName, FName), PassFieldVars]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc~s) ->~n",
       [mk_fn(d_field_, MsgName, FName), InFieldVars]),
     f("~s",
       [FValueCode]),
     f("~s",
       [MergeCode]),
     f("    ~p(~s, 0, 0~s).~n",
       [mk_fn(d_read_field_def_, MsgName), RestVar, OutFieldVars])].

format_vi_based_field_decoder_par(MsgName, FieldDef, Opts) ->
    #field{type=Type, name=FName}=FieldDef,
    BValueExpr = "X bsl N + Acc",
    {FValueCode, RestVar} = mk_unpack_vi(4, "FValue", BValueExpr, Type, "Rest",
                                         Opts),
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n"
       "    ~p(Rest, N+7, X bsl N + Acc, Msg);~n",
       [mk_fn(d_field_, MsgName, FName),
        mk_fn(d_field_, MsgName, FName)]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n",
       [mk_fn(d_field_, MsgName, FName)]),
     f("~s",
       [FValueCode]),
     f("    NewMsg = ~p(FValue, Msg),~n", [mk_fn(add_field_, MsgName, FName)]),
     f("    ~p(~s, 0, 0, NewMsg).~n",
       [mk_fn(d_read_field_def_, MsgName), RestVar])].

%% -> {FValueCode, Rest2Var}
%% Produce code for setting a variable, FValueVar, depending on the
%% varint type Type, for the raw unpacked expr, BValueExpr.
%%
%% The initial RestVar is the name of the variable holding the rest
%% of the bytes. An updated -- or the same -- such variable is returned.
%%
%% Indent the code to Indent spaces.
%% The resulting code contains one or more statements that ends with comma.
mk_unpack_vi(Indent, FValueVar, BValueExpr, Type, RestVar, Opts) ->
    Rest2Var = RestVar ++ "2",
    case Type of
        sint32 ->
            {[indent(Indent, f("BValue = ~s,~n", [BValueExpr])),
              indent(Indent, f("~s = ~s,~n",
                               [FValueVar,
                                fmt_decode_zigzag(Indent+7, "BValue")]))],
             RestVar};
        sint64 ->
            {[indent(Indent, f("BValue = ~s,~n", [BValueExpr])),
              indent(Indent, f("~s = ~s,~n",
                               [FValueVar,
                                fmt_decode_zigzag(Indent+7, "BValue")]))],
             RestVar};
        int32 ->
            {indent(Indent, f("~s,~n",
                              [fmt_uint_to_int(BValueExpr, FValueVar, 32)])),
             RestVar};
        int64 ->
            {indent(Indent, f("~s,~n",
                              [fmt_uint_to_int(BValueExpr, FValueVar, 64)])),
             RestVar};
        uint32 ->
            {indent(Indent, f("~s = ~s,~n", [FValueVar, BValueExpr])),
             RestVar};
        uint64 ->
            {indent(Indent, f("~s = ~s,~n", [FValueVar, BValueExpr])),
             RestVar};
        bool ->
            {indent(Indent, f("~s = ((~s) =/= 0),~n", [FValueVar, BValueExpr])),
             RestVar};
        {enum, EnumName} ->
            {[indent(Indent, f("~s,~n",
                               [fmt_uint_to_int(BValueExpr, "EnumValue", 32)])),
              indent(Indent, f("~s = ~p(EnumValue),~n",
                               [FValueVar, mk_fn(d_enum_, EnumName)]))],
             RestVar};
        string ->
            {[indent(Indent, f("Len = ~s,~n", [BValueExpr])),
              case proplists:get_bool(strings_as_binaries, Opts) of
                  false ->
                      [indent(Indent, f("<<Utf8:Len/binary, ~s/binary>> = ~s,~n",
                                        [Rest2Var, RestVar])),
                       indent(Indent, f("~s = unicode:characters_to_list("
                                        "Utf8,unicode),~n",
                                        [FValueVar]))];
                  true ->
                      mk_unpack_bytes(Indent, FValueVar, RestVar, Rest2Var,
                                      Opts)
              end],
             Rest2Var};
        bytes ->
            {[indent(Indent, f("Len = ~s,~n", [BValueExpr])),
              mk_unpack_bytes(Indent, FValueVar, RestVar, Rest2Var, Opts)],
             Rest2Var};
        {msg,Msg2Name} ->
            {[indent(Indent, f("Len = ~s,~n", [BValueExpr])),
              indent(Indent, f("<<MsgBytes:Len/binary, ~s/binary>> = ~s,~n",
                               [Rest2Var, RestVar])),
              indent(Indent, f("~s = decode_msg(MsgBytes, ~p),~n",
                               [FValueVar, Msg2Name]))],
             Rest2Var}
    end.

mk_unpack_bytes(I, FValueVar, RestVar, Rest2Var, Opts) ->
    CompilerHasBinary = (catch binary:copy(<<1>>)) == <<1>>,
    Copy = case proplists:get_value(copy_bytes, Opts, auto) of
               auto when not CompilerHasBinary -> false;
               auto when CompilerHasBinary     -> true;
               true                            -> true;
               false                           -> false;
               N when is_integer(N)            -> N;
               N when is_float(N)              -> N
           end,
    if Copy == false ->
            indent(I, f("<<~s:Len/binary, ~s/binary>> = ~s,~n",
                        [FValueVar, Rest2Var, RestVar]));
       Copy == true ->
            [indent(I, f("<<Bytes:Len/binary, ~s/binary>> = ~s,~n",
                         [Rest2Var, RestVar])),
             indent(I, f("~s = binary:copy(Bytes),~n", [FValueVar]))];
       is_integer(Copy); is_float(Copy) ->
            I2 = I + length(FValueVar) + length(" = "),
            [indent(I, f("<<Bytes:Len/binary, ~s/binary>> = ~s,~n",
                         [Rest2Var, RestVar])),
             indent(I, f("~s = case binary:referenced_byte_size(Bytes) of~n",
                            [FValueVar])),
             indent(I2+4, f("LB when LB >= byte_size(Bytes) * ~w ->~n", [Copy])),
             indent(I2+4+4, f("binary:copy(Bytes);~n")),
             indent(I2+4, f("_ ->~n")),
             indent(I2+4+4, f("Bytes~n")),
             indent(I2, f("end,~n"))]
    end.

fmt_uint_to_int(SrcStr, ResultVar, NumBits) ->
    f("<<~s:~w/signed-native>> = <<(~s):~p/unsigned-native>>",
      [ResultVar, NumBits, SrcStr, NumBits]).

fmt_decode_zigzag(Indent, VarStr) ->
    [f(              "if ~s band 1 =:= 0 -> ~s bsr 1;~n", [VarStr, VarStr]),
     indent(Indent,f("   true -> -((~s + 1) bsr 1)~n", [VarStr])),
     indent(Indent,f("end"))].

format_uf32_field_decoder(MsgName, FieldDef, AnRes) ->
    format_f_field_decoder(MsgName, 32, 'little', FieldDef, AnRes).

format_sf32_field_decoder(MsgName, FieldDef, AnRes) ->
    format_f_field_decoder(MsgName, 32, 'little-signed', FieldDef, AnRes).

format_float_field_decoder(MsgName, FieldDef, AnRes) ->
    format_f_field_decoder(MsgName, 32, 'little-float', FieldDef, AnRes).

format_uf64_field_decoder(MsgName, FieldDef, AnRes) ->
    format_f_field_decoder(MsgName, 64, 'little', FieldDef, AnRes).

format_sf64_field_decoder(MsgName, FieldDef, AnRes) ->
    format_f_field_decoder(MsgName, 64, 'little-signed', FieldDef, AnRes).

format_double_field_decoder(MsgName, FieldDef, AnRes) ->
    format_f_field_decoder(MsgName, 64, 'little-float', FieldDef, AnRes).

format_f_field_decoder(MsgName, BitLen, BitType, FieldDef, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            format_f_field_decoder_pap(MsgName, BitLen, BitType, FieldDef,
                                       AnRes);
        pass_as_record ->
            format_f_field_decoder_par(MsgName, BitLen, BitType, FieldDef)
    end.

format_f_field_decoder_pap(MsgName, BitLen, BitType, FieldDef, AnRes) ->
    NF = get_num_fields(MsgName, AnRes),
    #field{rnum=RNum, name=FName}=FieldDef,
    Merge = classify_field_merge_action(FieldDef),
    InFieldVars = [[", ", if I == RNum-1, Merge == overwrite -> "_";
                             I == RNum-1, Merge == seqadd    -> f("F~w", [I]);
                             I /= RNum-1                     -> f("F~w", [I])
                          end]
                   || I <- lists:seq(1, NF)],
    MergeCode = case Merge of
                    overwrite -> "";
                    seqadd    -> f("    Value2 = [Value | F~w],~n", [RNum-1])
                end,
    OutFieldVars = [[", ", if I == RNum-1, Merge == overwrite -> "Value";
                              I == RNum-1, Merge == seqadd    -> "Value2";
                              I /= RNum-1                     -> f("F~w", [I])
                           end]
                    || I <- lists:seq(1, NF)],

    [f("~p(<<Value:~p/~s, Rest/binary>>, _, _~s) ->~n",
       [mk_fn(d_field_, MsgName, FName), BitLen, BitType, InFieldVars]),
     f("~s", [MergeCode]),
     f("    ~p(Rest, 0, 0~s).~n",
       [mk_fn(d_read_field_def_, MsgName), OutFieldVars])].

format_f_field_decoder_par(MsgName, BitLen, BitType, FieldDef)  ->
    #field{name=FName}=FieldDef,
    [f("~p(<<Value:~p/~s, Rest/binary>>, Msg) ->~n",
       [mk_fn(d_field_, MsgName, FName), BitLen, BitType]),
     f("    NewMsg = ~p(Value, Msg),~n", [mk_fn(add_field_, MsgName, FName)]),
     f("    ~p(Rest, 0, 0, NewMsg).~n",
       [mk_fn(d_read_field_def_, MsgName)])].

format_packed_field_seq_decoder(MsgName, #field{type=Type}=FieldDef, Opts) ->
    case Type of
        fixed32  -> format_dpacked_nonvi(MsgName, FieldDef, 32, 'little');
        sfixed32 -> format_dpacked_nonvi(MsgName, FieldDef, 32, 'little-signed');
        float    -> format_dpacked_nonvi(MsgName, FieldDef, 32, 'little-float');
        fixed64  -> format_dpacked_nonvi(MsgName, FieldDef, 64, 'little');
        sfixed64 -> format_dpacked_nonvi(MsgName, FieldDef, 64, 'little-signed');
        double   -> format_dpacked_nonvi(MsgName, FieldDef, 64, 'little-float');
        _        -> format_dpacked_vi(MsgName, FieldDef, Opts)
    end.

format_dpacked_nonvi(MsgName, #field{name=FName}, BitLen, BitType) ->
    FnName = mk_fn(d_packed_field_, MsgName, FName),
    [f("~p(<<Value:~p/~s, Rest/binary>>, AccSeq) ->~n",
       [FnName, BitLen, BitType]),
     f("    ~p(Rest, [Value | AccSeq]);~n", [FnName]),
     f("~p(<<>>, AccSeq) ->~n", [FnName]),
     f("    AccSeq.~n")].

format_dpacked_vi(MsgName, #field{name=FName, type=Type}, Opts) ->
    FnName = mk_fn(d_packed_field_, MsgName, FName),
    BValueExpr = "X bsl N + Acc",
    {FValueCode, RestVar} = mk_unpack_vi(4, "FValue", BValueExpr, Type, "Rest",
                                         Opts),
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc, AccSeq) ->~n", [FnName]),
     f("    ~p(Rest, N+7, X bsl N + Acc, AccSeq);~n", [FnName]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc, AccSeq) ->~n", [FnName]),
     f("~s", [FValueCode]),
     f("    ~p(~s, 0, 0, [FValue | AccSeq]);~n", [FnName, RestVar]),
     f("~p(<<>>, 0, 0, AccSeq) ->~n", [FnName]),
     f("    AccSeq.~n~n")].

format_msg_decoder_reverse_toplevel(MsgName, MsgDef, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params -> ""; %% handled in d_read_field_def_<Msg>
        pass_as_record -> format_msg_decoder_reverse_toplevel_par(MsgName,MsgDef)
    end.

format_msg_decoder_reverse_toplevel_par(MsgName, MsgDef) ->
    MsgNameQLen = flength("~p", [MsgName]),
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

format_field_adders(MsgName, MsgDef, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params -> ""; %% handled by passing other field params
        pass_as_record -> format_field_adders(MsgName, MsgDef)
    end.

format_field_adders(MsgName, MsgDef) ->
    [case classify_field_merge_action(FieldDef) of
         msgmerge  ->
             format_field_msgmerge_adder(MsgName, FieldDef);
         overwrite ->
             format_field_overwrite_adder(MsgName, FieldDef);
         seqadd ->
             case is_packed(FieldDef) of
                 false -> format_field_seqappend_adder(MsgName, FieldDef);
                 true  -> ""
             end
     end
     || FieldDef <- MsgDef].

format_field_overwrite_adder(MsgName, #field{name=FName}) ->
    FAdderFn = mk_fn(add_field_, MsgName, FName),
    [f("~p(NewValue, Msg) ->~n", [FAdderFn]),
     f("    Msg#~p{~p = NewValue}.~n~n", [MsgName, FName])].

format_field_seqappend_adder(MsgName, #field{name=FName}) ->
    FAdderFn = mk_fn(add_field_, MsgName, FName),
    [f("~p(NewValue, #~p{~p=PrevElems}=Msg) ->~n", [FAdderFn, MsgName, FName]),
     f("    Msg#~p{~p = [NewValue | PrevElems]}.~n~n", [MsgName, FName])].

format_field_msgmerge_adder(MsgName, #field{name=FName, type={msg,FMsgName}}) ->
    FAdderFn = mk_fn(add_field_, MsgName, FName),
    MergeFn = mk_fn(merge_msg_, FMsgName),
    [f("~p(NewValue, #~p{~p=undefined}=Msg) ->~n", [FAdderFn, MsgName, FName]),
     f("    Msg#~p{~p = NewValue};~n", [MsgName, FName]),
     f("~p(NewValue, #~p{~p=PrevValue}=Msg) ->~n", [FAdderFn, MsgName, FName]),
     f("    Msg#~p{~p = ~p(PrevValue, NewValue)}.~n~n",
       [MsgName, FName, MergeFn])].

classify_field_merge_action(FieldDef) ->
    case FieldDef of
        #field{occurrence=required, type={msg, _}} -> msgmerge;
        #field{occurrence=optional, type={msg, _}} -> msgmerge;
        #field{occurrence=required}                -> overwrite;
        #field{occurrence=optional}                -> overwrite;
        #field{occurrence=repeated}                -> seqadd
    end.


format_msg_merge_code(Defs, AnRes) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgDef} <- Defs],
    [format_merge_msgs_top_level(MsgNames),
     [format_msg_merger(MsgName, MsgDef, AnRes)
      || {{msg, MsgName}, MsgDef} <- Defs]].

format_merge_msgs_top_level([]) ->
    [f("merge_msgs(_Prev, New) ->~n"),
     f("    New.~n"),
     f("~n")];
format_merge_msgs_top_level(MsgNames) ->
    [f("merge_msgs(Prev, New) when element(1, Prev) == element(1, New) ->~n"),
     f("   case Prev of~n"),
     f("       ~s~n", [format_merger_top_level_cases(MsgNames)]),
     f("   end.~n"),
     f("~n")].

format_merger_top_level_cases(MsgNames) ->
    string:join([f("#~p{} -> ~p(Prev, New)", [MName, mk_fn(merge_msg_, MName)])
                 || MName <- MsgNames],
                ";\n        ").


format_msg_merger(MsgName, [], _AnRes) ->
    MergeFn = mk_fn(merge_msg_, MsgName),
    [f("~p(_Prev, New) ->~n", [MergeFn]),
     f("    New.~n~n")];
format_msg_merger(MsgName, MsgDef, AnRes) ->
    MergeFn = mk_fn(merge_msg_, MsgName),
    FInfos = [{classify_field_merge_action(Field), Field} || Field <- MsgDef],
    ToOverwrite = [Field || {overwrite, Field} <- FInfos],
    ToMsgMerge  = [Field || {msgmerge, Field} <- FInfos],
    ToSeqAdd    = [Field || {seqadd, Field} <- FInfos],

    MatchIndent = flength("~p(#~p{", [MergeFn, MsgName]),
    MatchCommaSep = f(",~n~s", [indent(MatchIndent, "")]),

    PFieldMatchings = string:join([f("~p=PF~s", [FName, FName])
                                   || #field{name=FName} <- MsgDef],
                                  MatchCommaSep),
    NFieldMatchings = string:join([f("~p=NF~s", [FName, FName])
                                   || #field{name=FName} <- MsgDef],
                                  MatchCommaSep),

    %% FIXME: reverse order?? ie: do NF ++ PF??
    %%        we'll reverse _top-level_ seq fields lastly when decoding
    UpdateIndent = flength("    #~p(", [MsgName]),
    Overwritings = [begin
                        FUpdateIndent = UpdateIndent + flength("~p = ",[FName]),
                        [f("~p = if NF~s == undefined -> PF~s;~n",
                           [FName, FName, FName]),
                         indent(FUpdateIndent + 3, f("true -> NF~s~n", [FName])),
                         indent(FUpdateIndent, f("end"))]
                    end
                    || #field{name=FName} <- ToOverwrite],
    SeqAddings = [f("~p = PF~s ++ NF~s", [FName, FName, FName])
                  || #field{name=FName} <- ToSeqAdd],
    MsgMergings = [f("~p = ~p(PF~s, NF~s)", [FName, mk_fn(merge_msg_, FMsgName),
                                             FName, FName])
                   || #field{name=FName, type={msg,FMsgName}} <- ToMsgMerge],
    UpdateCommaSep = f(",~n~s", [indent(UpdateIndent, "")]),
    FieldUpdatings = string:join(Overwritings ++ SeqAddings ++ MsgMergings,
                                 UpdateCommaSep),
    FnIndent = flength("~p(", [MergeFn]),
    [[[f("~p(Prev, undefined) -> Prev;~n", [MergeFn]),
       f("~p(undefined, New) -> New;~n", [MergeFn])]
      || occurs_as_optional_submsg(MsgName, AnRes)],
     f("~p(#~p{~s},~n", [MergeFn, MsgName, PFieldMatchings]),
     indent(FnIndent, f("#~p{~s}) ->~n", [MsgName, NFieldMatchings])),
     f("    #~p{~s}.~n~n", [MsgName, FieldUpdatings])].

occurs_as_optional_submsg(MsgName, #anres{msg_occurrences=Occurrences}=AnRes) ->
    %% Note: order of evaluation below is important (the exprs of `andalso'):
    %% Messages are present in Occurrences only if they are sub-messages
    can_occur_as_sub_msg(MsgName, AnRes) andalso
        lists:member(optional, dict:fetch(MsgName, Occurrences)).

format_field_skippers(MsgName, AnRes) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            NF = get_num_fields(MsgName, AnRes),
            [format_varint_skipper_pap(MsgName, NF),
             format_length_delimited_skipper_pap(MsgName, NF),
             format_bit_skipper_pap(MsgName, 32, NF),
             format_bit_skipper_pap(MsgName, 64, NF)];
        pass_as_record ->
            [format_varint_skipper_par(MsgName),
             format_length_delimited_skipper_par(MsgName),
             format_bit_skipper_par(MsgName, 32),
             format_bit_skipper_par(MsgName, 64)]
    end.

format_varint_skipper_pap(MsgName, NF) ->
    SkipFn = mk_fn(skip_varint_, MsgName),
    ReadFieldFn = mk_fn(d_read_field_def_, MsgName),
    PassFieldVars = [f(", F~w", [I]) || I <- lists:seq(1,NF)],
    [f("~p(<<0:1, _:7, Rest/binary>>, _, _~s) ->~n", [SkipFn, PassFieldVars]),
     f("    ~p(Rest, 0, 0~s);~n", [ReadFieldFn, PassFieldVars]),
     f("~p(<<1:1, _:7, Rest/binary>>, X1, X2~s) ->~n", [SkipFn, PassFieldVars]),
     f("    ~p(Rest, X1, X2~s).~n~n", [SkipFn, PassFieldVars])].

format_varint_skipper_par(MsgName) ->
    SkipFn = mk_fn(skip_varint_, MsgName),
    [f("~p(<<0:1, _:7, Rest/binary>>, Msg) ->~n", [SkipFn]),
     f("    ~p(Rest, 0, 0, Msg);~n", [mk_fn(d_read_field_def_, MsgName)]),
     f("~p(<<1:1, _:7, Rest/binary>>, Msg) ->~n", [SkipFn]),
     f("    ~p(Rest, Msg).~n~n", [SkipFn])].

format_length_delimited_skipper_pap(MsgName, NF) ->
    SkipFn = mk_fn(skip_length_delimited_, MsgName),
    ReadFieldFn = mk_fn(d_read_field_def_, MsgName),
    PassFieldVars = [f(", F~w", [I]) || I <- lists:seq(1,NF)],
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc~s) ->~n", [SkipFn, PassFieldVars]),
     f("    ~p(Rest, N+7, X bsl N + Acc~s);~n", [SkipFn, PassFieldVars]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc~s) ->~n", [SkipFn, PassFieldVars]),
     f("    Length = X bsl N + Acc,~n"),
     f("    <<_:Length/binary, Rest2/binary>> = Rest,~n"),
     f("    ~p(Rest2, 0, 0~s).~n~n", [ReadFieldFn, PassFieldVars])].

format_length_delimited_skipper_par(MsgName) ->
    SkipFn = mk_fn(skip_length_delimited_, MsgName),
    [f("~p(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n", [SkipFn]),
     f("    ~p(Rest, N+7, X bsl N + Acc, Msg);~n", [SkipFn]),
     f("~p(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->~n", [SkipFn]),
     f("    Length = X bsl N + Acc,~n"),
     f("    <<_:Length/binary, Rest2/binary>> = Rest,~n"),
     f("    ~p(Rest2, 0, 0, Msg).~n~n", [mk_fn(d_read_field_def_, MsgName)])].

format_bit_skipper_pap(MsgName, BitLen, NF) ->
    SkipFn = mk_fn(skip_, BitLen, MsgName),
    ReadFieldFn = mk_fn(d_read_field_def_, MsgName),
    PassFieldVars = [f(", F~w", [I]) || I <- lists:seq(1,NF)],
    [f("~p(<<_:~w, Rest/binary>>, _, _~s) ->~n",
       [SkipFn, BitLen, PassFieldVars]),
     f("    ~p(Rest, 0, 0~s).~n~n",  [ReadFieldFn, PassFieldVars])].

format_bit_skipper_par(MsgName, BitLen) ->
    SkipFn = mk_fn(skip_, BitLen, MsgName),
    [f("~p(<<_:~w, Rest/binary>>, Msg) ->~n", [SkipFn, BitLen]),
     f("    ~p(Rest, 0, 0, Msg).~n~n",  [mk_fn(d_read_field_def_, MsgName)])].


format_nif_decoder_error_wrappers(Defs, _AnRes, _Opts) ->
    [format_msg_nif_error_wrapper(MsgName)
     || {{msg, MsgName}, _MsgDef} <- Defs].

format_msg_nif_error_wrapper(MsgName) ->
    FnName = mk_fn(d_msg_, MsgName),
    f("~p(Bin) ->~n"
      "    erlang:nif_error({error,{nif_not_loaded,~p}}, [Bin]).~n~n",
     [FnName, MsgName]).

%% -- verifiers -----------------------------------------------------

format_verifier_topcase(Indent, Defs, MsgVar) ->
    IndStr = indent(Indent+4, ""),
    ElseCase = "_ -> mk_type_error(not_a_known_message, Msg, [])",
    ["case ", MsgVar, " of\n",
     IndStr,
     string:join([f("#~p{} -> ~p(~s, [~p])",
                    [MsgName, mk_fn(v_msg_, MsgName), MsgVar, MsgName])
                  || {{msg, MsgName}, _MsgDef} <- Defs] ++ [ElseCase],
                 ";\n"++IndStr),
     "\n",
     indent(Indent, "end")].

format_verifiers(Defs, AnRes, _Opts) ->
    [format_msg_verifiers(Defs, AnRes),
     format_enum_verifiers(Defs, AnRes),
     format_type_verifiers(AnRes),
     format_verifier_auxiliaries()
    ].

format_msg_verifiers(Defs, AnRes) ->
    [format_msg_verifier(MsgName, MsgDef, AnRes)
     || {{msg,MsgName}, MsgDef} <- Defs].

format_msg_verifier(MsgName, [], AnRes) ->
    FnName = mk_fn(v_msg_, MsgName),
    [f("~p(#~p{}, _Path) ->~n", [FnName, MsgName]),
     f("    ok"),
     [[f(     ";~n"),
       f("~p(X, Path) ->~n", [FnName]),
       f("    mk_type_error({expected_msg,~p}, X, Path)", [MsgName])]
      || can_occur_as_sub_msg(MsgName, AnRes)],
     f(".~n~n")];
format_msg_verifier(MsgName, MsgDef, AnRes) ->
    FnName = mk_fn(v_msg_, MsgName),
    MatchIndent = flength("~p(#~p{", [FnName, MsgName]),
    MatchCommaSep = f(",~n~s", [indent(MatchIndent, "")]),
    FieldMatchings = string:join([f("~p=F~s", [FName, FName])
                                  || #field{name=FName} <- MsgDef],
                                 MatchCommaSep),
    [f("~p(#~p{~s}, Path) ->~n", [FnName, MsgName, FieldMatchings]),
     [begin
          FVerifierFn = case Type of
                            {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                            {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                            Type            -> mk_fn(v_type_, Type)
                        end,
          [indent_lines(
             4,
             case Occurrence of
                 required ->
                     %% FIXME: check especially for `undefined'
                     %% and if found, error out with required_field_not_set
                     %% specifying expected type
                     [f("~p(F~s, [~p | Path])", [FVerifierFn, FName, FName])];
                 repeated ->
                     [f("if is_list(F~s) ->~n", [FName]),
                      f("       [~p(Elem, [~p | Path]) || Elem <- F~s];~n",
                        [FVerifierFn, FName, FName]),
                      f("   true ->~n"),
                      f("       mk_type_error({invalid_list_of,~p},F~s,Path)~n",
                        [Type, FName]),
                      f("end")];
                 optional ->
                     [f("[~p(F~s, [~p | Path]) || F~s /= undefined]",
                        [FVerifierFn, FName, FName, FName])]
             end),
           f(",~n")]
      end
      || #field{name=FName, type=Type, occurrence=Occurrence} <- MsgDef],
     f("    ok"),
     [[f(     ";~n"),
       f("~p(X, Path) ->~n", [FnName]),
       f("    mk_type_error({expected_msg,~p}, X, Path)", [MsgName])]
      || can_occur_as_sub_msg(MsgName, AnRes)],
     f(".~n~n")].

can_occur_as_sub_msg(MsgName, #anres{used_types=UsedTypes}) ->
    sets:is_element({msg,MsgName}, UsedTypes).

format_enum_verifiers(Defs, #anres{used_types=UsedTypes}) ->
    [format_enum_verifier(EnumName, Def)
     || {{enum,EnumName}, Def} <- Defs,
        smember({enum, EnumName}, UsedTypes)].

format_enum_verifier(EnumName, EnumMembers) ->
    FnName = mk_fn(v_enum_, EnumName),
    [[f("~p(~p, _) -> ok;~n", [FnName, EnumSym]) || {EnumSym,_} <- EnumMembers],
     f("~p(X, Path) ->~n", [FnName]),
     f("    mk_type_error({invalid_enum,~p}, X, Path).~n", [EnumName]),
     f("~n")].

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
     [format_int_verifier(sfixed32,signed, 32)  || smember(sfixed32, UsedTypes)],
     [format_int_verifier(sfixed64,signed, 64)  || smember(sfixed64, UsedTypes)],
     [format_float_verifier(float)              || smember(float, UsedTypes)],
     [format_float_verifier(double)             || smember(double, UsedTypes)],
     [format_string_verifier()                  || smember(string, UsedTypes)],
     [format_bytes_verifier()                   || smember(bytes, UsedTypes)]].

format_int_verifier(IntType, Signedness, NumBits) ->
    FnName = mk_fn(v_type_, IntType),
    Min = case Signedness of
              unsigned -> 0;
              signed   -> -(1 bsl (NumBits-1))
          end,
    Max = case Signedness of
              unsigned -> 1 bsl NumBits - 1;
              signed   -> 1 bsl (NumBits-1) - 1
          end,
    [f("~p(N, _P) when ~w =< N, N =< ~w ->~n", [FnName, Min, Max]),
     f("    ok;~n"),
     f("~p(N, Path) when is_integer(N) ->~n", [FnName]),
     f("    mk_type_error({value_out_of_range, ~p, ~p, ~w}, N, Path);~n",
       [IntType, Signedness, NumBits]),
     f("~p(X, Path) ->~n", [FnName]),
     f("    mk_type_error({bad_integer, ~p, ~p, ~w}, X, Path).~n",
       [IntType, Signedness, NumBits]),
     f("~n")].

format_bool_verifier() ->
    FnName = mk_fn(v_type_, bool),
    [f("~p(false, _Path) -> ok;~n", [FnName]),
     f("~p(true, _Path)  -> ok;~n", [FnName]),
     f("~p(X, Path)  -> mk_type_error(bad_boolean_value, X, Path).~n",
       [FnName]),
     f("~n")].

format_float_verifier(FlType) ->
    FnName = mk_fn(v_type_, FlType),
    [f("~p(N, _Path) when is_float(N) -> ok;~n", [FnName]),
     %% It seems a float for the corresponding integer value is
     %% indeed packed when doing <<Integer:32/little-float>>.
     %% So let verify accept integers too.
     %% When such a value is unpacked, we get a float.
     f("~p(N, _Path) when is_integer(N) -> ok;~n", [FnName]),
     f("~p(X, Path)  -> mk_type_error(bad_~w_value, X, Path).~n",
       [FnName, FlType]),
     f("~n")].

format_string_verifier() ->
    FnName = mk_fn(v_type_, string),
    [f("~p(S, Path) when is_list(S) ->~n", [FnName]),
     f("    try unicode:characters_to_binary(S),~n"),
     f("        ok~n"),
     f("    catch error:badarg ->~n"),
     f("        mk_type_error(bad_unicode_string, S, Path)~n"),
     f("    end;~n"),
     f("~p(X, Path) ->~n", [FnName]),
     f("    mk_type_error(bad_unicode_string, X, Path).~n"),
     f("~n")].

format_bytes_verifier() ->
    FnName = mk_fn(v_type_, bytes),
    [f("~p(B, _Path) when is_binary(B) ->~n", [FnName]),
     f("    ok;~n"),
     f("~p(X, Path) ->~n", [FnName]),
     f("    mk_type_error(bad_binary_value, X, Path).~n"),
     f("~n")].

format_verifier_auxiliaries() ->
    [f("mk_type_error(Error, ValueSeen, Path) ->~n"),
     f("    Path2 = prettify_path(Path),~n"),
     f("    erlang:error({gpb_type_error,~n"),
     f("                  {Error, [{value, ValueSeen},{path, Path2}]}}).~n"),
     f("~n"),
     f("prettify_path([]) ->~n"),
     f("    top_level;~n"),
     f("prettify_path(PathR) ->~n"),
     f("    list_to_atom(~n"),
     f("      string:join(~n"),
     f("        lists:map(fun atom_to_list/1, lists:reverse(PathR)),~n"),
     f("        \".\")).~n"),
     f("~n")].

%% -- message defs -----------------------------------------------------

format_introspection(Defs) ->
    MsgDefs  = [Item || {{msg, _}, _}=Item <- Defs],
    EnumDefs = [Item || {{enum, _}, _}=Item <- Defs],
    [f("get_msg_defs() ->~n"
       "    [~s].~n", [outdent_first(format_msgs_and_enums(5, Defs))]),
     f("~n"),
     f("get_msg_names() ->~n"
       "    ~p.~n", [[MsgName || {{msg,MsgName},_Fields} <- Defs]]),
     f("~n"),
     f("get_enum_names() ->~n"
       "    ~p.~n", [[EnumName || {{enum,EnumName},_Enums} <- Defs]]),
     f("~n"),
     format_fetch_msg_defs(MsgDefs),
     f("~n"),
     format_fetch_enum_defs(EnumDefs),
     f("~n"),
     format_find_msg_defs(MsgDefs),
     f("~n"),
     format_find_enum_defs(EnumDefs),
     f("~n"),
     format_enum_value_symbol_converters(EnumDefs),
     f("~n"),
     format_get_package_name(Defs)
    ].


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
                        occurrence=Occurrence, opts=Opts}) ->

    [indent(I, f("#field{name=~w, fnum=~w, rnum=~w, type=~w,~n", [N,F,R,T])),
     indent(I, f("       occurrence=~w,~n", [Occurrence])),
     indent(I, f("       opts=~p}", [Opts]))].

format_fetch_msg_defs([]) ->
    f("fetch_msg_def(MsgName) ->~n"
      "    erlang:error({no_such_msg, MsgName}).~n");
format_fetch_msg_defs(_MsgDefs) ->
    f("fetch_msg_def(MsgName) ->~n"
      "    case find_msg_def(MsgName) of~n"
      "        Fs when is_list(Fs) -> Fs;~n"
      "        error               -> erlang:error({no_such_msg, MsgName})~n"
      "    end.~n").

format_fetch_enum_defs([]) ->
    f("fetch_enum_def(EnumName) ->~n"
      "    erlang:error({no_such_enum,EnumName}).~n");
format_fetch_enum_defs(_EnumDefs) ->
    f("fetch_enum_def(EnumName) ->~n"
      "    case find_enum_def(EnumName) of~n"
      "        Es when is_list(Es) -> Es;~n"
      "        error               -> erlang:error({no_such_enum,EnumName})~n"
      "    end.~n").

format_find_msg_defs([]) ->
    f("find_msg_def(_) ->~n"
      "    error.~n");
format_find_msg_defs(Msgs) ->
    [[[f("find_msg_def(~p) ->~n", [MsgName]),
       f("    [~s];~n", [outdent_first(format_efields(4+1, Fields))])]
      || {{msg, MsgName}, Fields} <- Msgs],
     f("find_msg_def(_) ->~n"
       "    error.~n")].

format_find_enum_defs([]) ->
    f("find_enum_def(_) ->~n"
      "    error.~n");
format_find_enum_defs(Enums) ->
    [[[f("find_enum_def(~p) ->~n", [EnumName]),
       f("    ~p;~n", [EnumValues])]
      || {{enum, EnumName}, EnumValues} <- Enums],
     f("find_enum_def(_) ->~n"
       "    error.~n")].

format_enum_value_symbol_converter_exports(Defs) ->
    [f("-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).~n"),
     [begin
         ToSymFnName = mk_fn(enum_symbol_by_value_, EnumName),
         ToValFnName = mk_fn(enum_value_by_symbol_, EnumName),
         f("-export([~p/1, ~p/1]).~n", [ToSymFnName, ToValFnName])
     end
     || {{enum, EnumName}, _EnumDef} <- Defs]].

format_enum_value_symbol_converters(EnumDefs) when EnumDefs /= [] ->
    %% A difference between this function and `d_enum_X' as generated
    %% by `format_enum_decoders' is that this function generates
    %% value/symbol converters for all enums, not only for the ones
    %% that are used in messags.
    [string:join([begin
                      ToSymFnName = mk_fn(enum_symbol_by_value_, EnumName),
                      f("enum_symbol_by_value(~p, V) -> ~p(V)",
                        [EnumName, ToSymFnName])
                  end
                  || {{enum, EnumName}, _EnumDef} <- EnumDefs],
                 ";\n"),
     ".\n\n",
     string:join([begin
                      ToValFnName = mk_fn(enum_value_by_symbol_, EnumName),
                      f("enum_value_by_symbol(~p, S) -> ~p(S)",
                        [EnumName, ToValFnName])
                  end
                  || {{enum, EnumName}, _EnumDef} <- EnumDefs],
                 ";\n"),
     ".\n\n",
     [[string:join([begin
                        FnName = mk_fn(enum_symbol_by_value_, EnumName),
                        f("~p(~w) -> ~p", [FnName, EnumValue, EnumSym])
                    end
                    || {EnumSym, EnumValue} <- EnumDef],
                   ";\n"),
       ".\n\n",
       string:join([begin
                        FnName = mk_fn(enum_value_by_symbol_, EnumName),
                        f("~p(~w) -> ~p", [FnName, EnumSym, EnumValue])
                    end
                    || {EnumSym, EnumValue} <- EnumDef],
                   ";\n"),
       ".\n\n"]
      || {{enum, EnumName}, EnumDef} <- EnumDefs]];
format_enum_value_symbol_converters([]=_EnumDefs) ->
    [f("enum_symbol_by_value(E, V) -> erlang:error({no_enum_defs,E,V}).~n"),
     f("enum_value_by_symbol(E, V) -> erlang:error({no_enum_defs,E,V}).~n")].

format_get_package_name(Defs) ->
    case lists:keyfind(package, 1, Defs) of
        false ->
            f("get_package_name() ->~n"
              "    undefined.~n");
        {package, Package} ->
            f("get_package_name() ->~n"
              "    ~p.~n", [Package])
    end.

%% -- hrl -----------------------------------------------------

format_hrl(Mod, Defs, Opts) ->
    ModVsn = list_to_atom(atom_to_list(Mod) ++ "_gpb_version"),
    iolist_to_binary(
      [f("%% Automatically generated, do not edit~n"
         "%% Generated by ~p version ~s on ~p~n",
         [?MODULE, gpb:version_as_string(), calendar:local_time()]),
       "\n",
       f("-ifndef(~p).~n", [Mod]),
       f("-define(~p, true).~n", [Mod]),
       "\n",
       f("-define(~p, \"~s\").~n", [ModVsn, gpb:version_as_string()]),
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

dot_to_underscore(X) when is_atom(X) -> dot_to_underscore(atom_to_list(X));
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
     f("#include \"~s.pb.h\"\n", [Mod]),
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

    [[f("static ERL_NIF_TERM ~s;\n", [Var]) || {Var,_Atom} <- AtomVars],
     "\n",
     ["static void install_atoms(ErlNifEnv *env)\n"
      "{\n",
      [f("    ~s = enif_make_atom(env, \"~s\");\n", [AtomVar, Atom])
       || {AtomVar, Atom} <- AtomVars],
      "}\n",
      "\n"]].

format_nif_cc_utf8_conversion(_Mod, _Defs, AnRes, _Opts) ->
    case is_any_field_of_type_string(AnRes) of
        true  -> format_nif_cc_utf8_conversion_code();
        false -> ""
    end.

is_any_field_of_type_string(#anres{used_types=UsedTypes}) ->
    sets:is_element(string, UsedTypes).

is_any_field_of_type_bool(#anres{used_types=UsedTypes}) ->
    sets:is_element(bool, UsedTypes).

format_nif_cc_utf8_conversion_code() ->
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
     "    {\n",
     "        unsigned int  cp[numcp];\n",
     "        ERL_NIF_TERM  es[numcp];\n",
     "        int i;\n",
     "\n",
     "        utf8_to_uint32(cp, utf8data, numcp);\n",
     "        for (i = 0; i < numcp; i++)\n",
     "            es[i] = enif_make_uint(env, cp[i]);\n",
     "        return enif_make_list_from_array(env, es, numcp);\n",
     "    }\n",
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
          f("    {\"~s\", 1, ~s}~s\n", [FnName, CFnName, Comma])
      end
      || {I, {{msg, MsgName}, _MsgFields}} <- index_seq(Defs)],
     "\n",
     "};\n",
     "\n",
     f("ERL_NIF_INIT(~s, nif_funcs, load, reload, upgrade, unload)\n",
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
     f("    ERL_NIF_TERM rname = ~s;\n", [mk_c_var(aa_, MsgName)]),
     [f("    ERL_NIF_TERM elem~w;\n", [I]) || I <- Is],
     "\n",
     [begin
          DestVar = f("elem~w",[I]),
          format_nif_cc_field_unpacker(DestVar, "m", Field, Defs)
      end
      || {I, Field} <- index_seq(Fields)],
     "\n",
     f("    res = enif_make_tuple(env, ~w, rname~s);\n",
       [length(Fields) + 1, [f(", elem~w",[I]) || I <- Is]]),
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
    [f("    if (!~s->has_~s())\n", [MsgVar, LCFName]),
     f("        ~s = aa_undefined;\n", [DestVar]),
     f("    else\n"),
     indent_lines(
       8,
       case FType of
           float ->
               [f("~s = enif_make_double(env, (double)~s->~s());\n",
                  [DestVar, MsgVar, LCFName])];
           double ->
               [f("~s = enif_make_double(env, ~s->~s());\n",
                  [DestVar, MsgVar, LCFName])];
           _S32 when FType == sint32;
                     FType == int32;
                     FType == sfixed32 ->
               [f("~s = enif_make_int(env, ~s->~s());\n",
                  [DestVar, MsgVar, LCFName])];
           _S64 when FType == sint64;
                     FType == int64;
                     FType == sfixed64 ->
               [f("~s = enif_make_int64(env, (ErlNifSInt64)~s->~s());\n",
                  [DestVar, MsgVar, LCFName])];
           _U32 when FType == uint32;
                     FType == fixed32 ->
               [f("~s = enif_make_uint(env, ~s->~s());\n",
                  [DestVar, MsgVar, LCFName])];
           _U64 when FType == uint64;
                     FType == fixed64 ->
               [f("~s = enif_make_uint64(env, (ErlNifUInt64)~s->~s());\n",
                  [DestVar, MsgVar, LCFName])];
           bool ->
               [f("if (~s->~s())\n", [MsgVar, LCFName]),
                f("    ~s = aa_true;\n", [DestVar]),
                f("else\n"),
                f("    ~s = aa_false;\n", [DestVar])];
           {enum, EnumName} ->
               {value, {{enum,EnumName}, Enumerations}} =
                   lists:keysearch({enum,EnumName}, 1, Defs),
               [] ++
                   [f("switch (~s->~s()) {\n", [MsgVar, LCFName])] ++
                   [f("    case ~w: ~s = ~s; break;\n",
                      [Value, DestVar, mk_c_var(aa_, Sym)])
                    || {Sym, Value} <- Enumerations] ++
                   [f("    default: ~s = aa_undefined;\n", [DestVar])] ++
                   [f("}\n")];
           string ->
               [f("{\n"),
                f("    const char    *sData = ~s->~s().data();\n",
                  [    MsgVar, LCFName]),
                f("    unsigned int   sSize = ~s->~s().size();\n",
                  [    MsgVar, LCFName]),
                f("    ~s = utf8_to_erl_string(env, sData, sSize);\n",
                  [    DestVar]),
                f("}\n")];
           bytes ->
               [f("{\n"),
                f("    unsigned char *data;\n"),
                f("    unsigned int   bSize = ~s->~s().size();\n",
                  [    MsgVar, LCFName]),
                f("    const char    *bData = ~s->~s().data();\n",
                  [    MsgVar, LCFName]),
                f("    data = enif_make_new_binary(\n"), %% can data be NULL??
                f("               env,\n"),
                f("               bSize,\n"),
                f("               &~s);\n", [DestVar]),
                f("    memmove(data, bData, bSize);\n"),
                f("}\n")];
           {msg, Msg2Name} ->
               UnpackFnName = mk_c_fn(u_msg_, Msg2Name),
               [f("~s = ~s(env, &~s->~s());\n",
                  [DestVar, UnpackFnName, MsgVar, LCFName])]
       end),
     "\n"].

format_nif_cc_field_unpacker_repeated(DestVar, MsgVar, Field, Defs) ->
    #field{name=FName, type=FType} = Field,
    LCFName = to_lower(FName),
    [f("    {\n"),
     f("        unsigned int numElems = ~s->~s_size();\n", [MsgVar, LCFName]),
     f("        ERL_NIF_TERM relem[numElems];\n"),
     f("        unsigned int i;\n"),
     "\n",
     f("        for (i = 0; i < numElems; i++)\n"),
     indent_lines(
       12,
       case FType of
           float ->
               [f("relem[i] = enif_make_double(env, (double)~s->~s(i));\n",
                  [MsgVar, LCFName])];
           double ->
               [f("relem[i] = enif_make_double(env, ~s->~s(i));\n",
                  [MsgVar, LCFName])];
           _S32 when FType == sint32;
                     FType == int32;
                     FType == sfixed32 ->
               [f("relem[i] = enif_make_int(env, ~s->~s(i));\n",
                  [MsgVar, LCFName])];
           _S64 when FType == sint64;
                     FType == int64;
                     FType == sfixed64 ->
               [f("relem[i] = enif_make_int64(env, (ErlNifSInt64)~s->~s(i));\n",
                  [MsgVar, LCFName])];
           _U32 when FType == uint32;
                     FType == fixed32 ->
               [f("relem[i] = enif_make_uint(env, ~s->~s(i));\n",
                  [MsgVar, LCFName])];
           _U64 when FType == uint64;
                     FType == fixed64 ->
               [f("relem[i] = enif_make_uint64(env,\n"
                  "                            (ErlNifUInt64)~s->~s(i));\n",
                  [MsgVar, LCFName])];
           bool ->
               [f("if (~s->~s(i))\n", [MsgVar, LCFName]),
                f("    relem[i] = aa_true;\n"),
                f("else\n"),
                f("    relem[i] = aa_false;\n")];
           {enum, EnumName} ->
               {value, {{enum,EnumName}, Enumerations}} =
                   lists:keysearch({enum,EnumName}, 1, Defs),
               [] ++
                   [f("switch (~s->~s(i)) {\n", [MsgVar, LCFName])] ++
                   [f("    case ~w: relem[i] = ~s; break;\n",
                      [Value, mk_c_var(aa_, Sym)])
                    || {Sym, Value} <- Enumerations] ++
                   [f("    default: relem[i] = aa_undefined;\n")] ++
                   [f("}\n")];
           string ->
               [f("{\n"),
                f("    const char    *sData = ~s->~s(i).data();\n",
                  [    MsgVar, LCFName]),
                f("    unsigned int   sSize = ~s->~s(i).size();\n",
                  [    MsgVar, LCFName]),
                f("    relem[i] = utf8_to_erl_string(env, sData, sSize);\n"),
                f("}\n")];
           bytes ->
               [f("{\n"),
                f("    unsigned char *data;\n"),
                f("    unsigned int   bSize = ~s->~s(i).size();\n",
                  [    MsgVar, LCFName]),
                f("    const char    *bData = ~s->~s(i).data();\n",
                  [    MsgVar, LCFName]),
                f("    data = enif_make_new_binary(\n"), %% can data be NULL??
                f("               env,\n"),
                f("               bSize,\n"),
                f("               &relem[i]);\n"),
                f("    memmove(data, bData, bSize);\n"),
                f("}\n")];
           {msg, Msg2Name} ->
               UnpackFnName = mk_c_fn(u_msg_, Msg2Name),
               [f("relem[i] = ~s(env, &~s->~s(i));\n",
                  [UnpackFnName, MsgVar, LCFName])]
       end),
     f("        ~s = enif_make_list_from_array(env, relem, numElems);\n",
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
             f("    erlang:load_nif(Nif, ~w).\n", [VsnAsList])];
        LoadNifFnText when is_list(LoadNifFnText); is_binary(LoadNifFnText) ->
            [replace_tilde_s(iolist_to_binary(LoadNifFnText),
                             iolist_to_binary(f("\"~s.nif\"", [Mod])),
                             iolist_to_binary(f("~w", [VsnAsList])))]
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
    ModAsBin = list_to_binary(atom_to_list(Mod)),
    ErlCode2 = replace_module_macro(ErlCode, ModAsBin),
    {ok, Toks, _EndLine} = erl_scan:string(flatten_iolist(ErlCode2)),
    FormToks = split_toks_at_dot(Toks),
    Forms = lists:map(fun(Ts) ->
                              {ok, Form} = erl_parse:parse_form(Ts),
                              Form
                      end,
                      FormToks),
    {AttrForms, CodeForms} = split_forms_at_first_code(Forms),
    FieldDef = field_record_to_attr_form(),
    MsgRecordForms = msgdefs_to_record_attrs(MsgDefs),
    AllForms = AttrForms ++ [FieldDef] ++ MsgRecordForms ++ CodeForms,
    combine_erl_and_possible_nif(compile:forms(AllForms, Opts),
                                 PossibleNifCode).

replace_module_macro(<<$?, "MODULE", Rest/binary>>, ModBin) ->
    <<ModBin/binary, (replace_module_macro(Rest, ModBin))/binary>>;
replace_module_macro(<<C, Rest/binary>>, ModBin) ->
    <<C, (replace_module_macro(Rest, ModBin))/binary>>;
replace_module_macro(<<>>, _ModBin) ->
    <<>>.

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

f(F)   -> f(F,[]).
f(F,A) -> io_lib:format(F,A).

%flength(F) -> iolist_size(f(F)).
flength(F, A) -> iolist_size(f(F, A)).

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
