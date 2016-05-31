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
-export([proto_defs/2, proto_defs/3]).
-export([msg_defs/2, msg_defs/3]).
-export([format_error/1, format_warning/1]).
-export([c/0, c/1, c/2]). % Cmd line interface, halts vm---don't use from shell!
-export([parse_opts_and_args/1]).
-export([show_args/0]).
-export([show_version/0]).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").

-record(ft, {type, occurrence, is_packed}).
-record(anres, %% result of analysis
        {
          used_types,         % :: sets:set(gpb_field_type()),
          known_msg_size,     % :: dict:dict(), %% MsgName -> Size | undefined
          msg_occurrences,    % :: dict:dict(), %% MsgName -> [occurrence()]
          fixlen_types,       % :: sets:set(#ft{}),
          num_packed_fields,  % :: integer(),
          num_fields,         % :: dict:dict(), %% MsgName -> integer()
          d_field_pass_method,% :: dict:dict()  %% MsgName -> pass_as_record |
                              %                 %%            pass_as_params
          maps_as_msgs,       % :: list() % same format as `Defs'
          map_translations,   % :: [translation()]
          map_types           % :: sets:set({map,_,_})
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
%%                   {maps_unset_optional, omitted | present_undefined} |
%%                   {nif,boolean()} | nif |
%%                   {load_nif, LoadNif} |
%%                   {i, directory()} |
%%                   {o, directory()} |
%%                   {o_erl, directory()} | {o_hrl, directory()} |
%%                   {o_nif_cc, directory()} |
%%                   binary | to_proto_defs | to_msg_defs |
%%                   return |
%%                   return_warnings | return_errors |
%%                   {return_warnings, boolean()} | {return_errors, boolean()} |
%%                   report |
%%                   report_warnings | report_errors |
%%                   {report_warnings, boolean()} | {report_errors, boolean()} |
%%                   warnings_as_errors |
%%                   include_as_lib | use_packages |
%%                   {erlc_compile_options,string()} |
%%                   {msg_name_prefix, string() | atom()} |
%%                   {msg_name_suffix, string() | atom()} |
%%                   {msg_name_to_lower, boolean()} |
%%                   {module_name_prefix, string() | atom()} |
%%                   {module_name_suffix, string() | atom()}
%%            CompRet = ModRet | BinRet | ErrRet
%%            ModRet = ok | {ok, Warnings}
%%            BinRet = {ok, ModuleName, Code} |
%%                     {ok, ModuleName, Code, Warnings}
%%            ErrRet = error | {error, Reason} | {error,Reason,Warnings}
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
%% The File must not include path to the .proto file. Example:
%% "SomeDefinitions.proto" is ok, while "/path/to/SomeDefinitions.proto"
%% is not ok.
%%
%% The .proto file is expected to be found in a directories specified by an
%% `{i,directory()}' option. It is possible to specify `{i,directory()}'
%% several times, they will be searched in the order specified.
%%
%% The `{type_specs,boolean()}' option enables or disables `::Type()'
%% annotations in the generated .hrl file. Default is currently
%% `false'. If you set it to `true', you may get into troubles for
%% messages referencing other messages, when compiling the generated
%% files. The `type_specs' option is equivalent to `{type_specs,true}'.
%%
%% The `verify' option specifies whether or not to generate code
%% that verifies, during encoding, that values are of correct type and
%% within range.  The `verify' option can have the following values:
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
%% Erlang value verification either succeeds or crashes with the `error'
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
%% binaries, which will in turn make it possible to free the input message
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
%% be returned from decoding as strings (list of Unicode code points),
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
%% options specify output directories for where to generate the `.erl'
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
%% For maps, for optional fields, if not set, the
%% `maps_unset_optional' option specifies the Erlang-internal
%% representation; both how it is expected to be found at encoding,
%% and how decoding will return it:
%% <dl>
%%   <dt>`omitted'</dt>
%%   <dd>This means it is not included in the map</dd>
%%   <dt>`present_undefined'</dt>
%%   <dd>This means it is present and has the value `undefined'.
%%       This is the default, for historical reasons mostly, due to
%%       the way records works, but there are some caveats, although
%%       apparently rare, but still.  imagine an enum with a symbol
%%       `undefined', and an optional field of that enum type. It will
%%       not be possible to tell the difference between it being unset
%%       and being present and set.  Encoding will assume it is unset.
%%   </dd>
%% </dl>
%%
%%
%% The `nif' option will cause the compiler to generate nif C++ code
%% for encoding and decoding. The generated nif C++ code can be linked
%% with the Google protobuf C++ library.  Read the file
%% `README.nif-cc' for more info. This option is not compatible with
%% the `maps' option; the generated C++ decoding code would still
%% create records.
%%
%% The `binary' option will cause the generated and compiled code to be
%% returned as a binary. No files will be written. The return value
%% will be on the form `{ok,Mod,Code}' or `{ok,Mod,Code,Warnings}'
%% if the compilation is successful. This option may be useful
%% e.g. when generating test cases. In case the `nif' option is set,
%% the `Code' will be a list of tuples: `{erl,binary()}' which
%% contains the Erlang object byte code, and `{nif,binary()}' which
%% contains the C++ code. You will have to compile the C++ code with a
%% C++ compiler, before you can use the Erlang code.
%%
%% The `to_proto_defs' option will result in `{ok,Defs}' or
%% `{ok,Defs,Warns}' being returned if the compilation is successful.
%% The returned message definitions can be used with the
%% {@link proto_defs/2} or {@link proto_defs/3} functions.
%%
%% The `to_msg_defs' option is a deprecated alias for `to_proto_defs'.
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
%% Setting the `warnings_as_errors' option will cause warnings to be
%% treated as errors.  If there are warnings but no errors, and
%% `return_warnings' is not specified, then `error' will be returned.
%%
%% See {@link format_error/1} for a way to turn an error <i>Reason</i> to
%% plain text.
%%
%% If the `include_as_lib' option is set, the generated code will include
%% gpb.hrl as a library, which is necessary if dependencies are managed with
%% Rebar. Otherwise, the header file is included directly and must be located
%% in the path, which is default behavior.
%%
%% The `use_packages' option instructs gpb to prepend the name of a package
%% to every message it contains. If no package is defined, nothing will be
%% prepended. This enables the reference of messages in other packages which
%% would otherwise not be possible. However, for reasons of backward
%% compatibility, this option is disabled by default.
%%
%% If the the `{erlc_compile_options,string()}' option is set,
%% then the genereted code will contain a directive `-compile([String]).'
%%
%% The `{msg_name_prefix,Prefix}' will add `Prefix' (a string or an atom)
%% to each message. This might be useful for resolving colliding names,
%% when incorporating several protocol buffer definitions into the same
%% project. The `{msg_name_suffix,Suffix}' works correspondingly.
%%
%% The `{module_name_prefix,Prefix}' will add `Prefix' (a string or an atom)
%% to the generated code and definition files. The `{module_name_suffix,Suffix}'
%% works correspondingly.
file(File, Opts0) ->
    Opts1 = normalize_alias_opts(Opts0),
    Opts2 = normalize_return_report_opts(Opts1),
    case parse_file(File, Opts2) of
        {ok, Defs} ->
            Ext = filename:extension(File),
            Mod = list_to_atom(
                    possibly_suffix_mod(
                      possibly_prefix_mod(filename:basename(File, Ext), Opts2),
                      Opts2)),
            DefaultOutDir = filename:dirname(File),
            Opts3 = Opts2 ++ [{o,DefaultOutDir}],
            proto_defs(Mod, Defs, Opts3);
        {error, Reason} = Error ->
            possibly_report_error(Error, Opts2),
            case proplists:get_bool(return_warnings, Opts2) of
                true  -> {error, Reason, []};
                false -> Error
            end
    end.

normalize_alias_opts(Opts) ->
    lists:map(fun(to_msg_defs)         -> to_proto_defs;
                 ({to_msg_defs, Bool}) -> {to_proto_defs, Bool};
                 (Opt)                 -> Opt
              end,
              Opts).

normalize_return_report_opts(Opts1) ->
    Opts2 = expand_opt(return, [return_warnings, return_errors], Opts1),
    Opts3 = expand_opt(report, [report_warnings, report_errors], Opts2),
    Opts4 = unless_defined_set(return_warnings, report_warnings, Opts3),
    Opts5 = unless_defined_set(return_errors,   report_errors, Opts4),
    Opts5.

expand_opt(OptionToTestFor, OptionsToExpandTo, Opts) ->
    lists:append(
      lists:map(fun(Opt) when Opt == OptionToTestFor -> OptionsToExpandTo;
                   (Opt) -> [Opt]
                end,
                Opts)).

delete_bool_opt(OptToDelete, Opts) ->
    %% Boolean opts can be defined both as [opt] and as [{opt, true|false}],
    %% delete both type of occurrences.
    lists:keydelete(OptToDelete, 1, Opts -- [OptToDelete]).

unless_defined_set(OptionToTestFor, Default, Opts) ->
    case is_option_defined(OptionToTestFor, Opts) of
        true  -> Opts;
        false -> Opts ++ [Default]
    end.

is_option_defined(Key, Opts) ->
    lists:any(fun({K, _V}) -> K =:= Key;
                 (K)       -> K =:= Key
              end,
              Opts).

possibly_prefix_mod(BaseNameNoExt, Opts) ->
    case proplists:get_value(module_name_prefix, Opts) of
        undefined ->
            BaseNameNoExt;
        Prefix ->
            lists:concat([Prefix, BaseNameNoExt])
    end.

possibly_suffix_mod(BaseNameNoExt, Opts) ->
    case proplists:get_value(module_name_suffix, Opts) of
        undefined ->
            BaseNameNoExt;
        Suffix ->
            lists:concat([BaseNameNoExt, Suffix])
    end.


%% @spec proto_defs(Mod, Defs) -> CompRet
%% @equiv proto_defs(Mod, Defs, [])
proto_defs(Mod, Defs) ->
    proto_defs(Mod, Defs, []).

%% @spec proto_defs(Mod, Defs, Opts) -> CompRet
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
proto_defs(Mod, Defs0, Opts0) ->
    {IsAcyclic, Defs} = try_topsort_defs(Defs0),
    possibly_probe_defs(Defs, Opts0),
    {Warns, Opts1} = possibly_adjust_typespec_opt(IsAcyclic, Opts0),
    Opts2 = normalize_return_report_opts(Opts1),
    AnRes = analyze_defs(Defs, Opts2),
    case verify_opts(Opts2) of
        ok ->
            Res1 = do_proto_defs(Defs, clean_module_name(Mod), AnRes, Opts2),
            return_or_report_warnings_or_errors(Res1, Warns, Opts2,
                                                get_output_format(Opts2));
        {error, OptError} ->
            return_or_report_warnings_or_errors({error, OptError}, [], Opts2,
                                                get_output_format(Opts2))
    end.


%% @spec msg_defs(Mod, Defs) -> CompRet
%% @equiv msg_defs(Mod, Defs, [])
%% @doc Deprecated, use proto_defs/2 instead.
msg_defs(Mod, Defs) ->
    msg_defs(Mod, Defs, []).

%% @spec msg_defs(Mod, Defs, Opts) -> CompRet
%% @equiv proto_defs(Mod, Defs, Opts)
%% @doc Deprecated, use proto_defs/2 instead.
msg_defs(Mod, Defs, Opts) ->
    proto_defs(Mod, Defs, Opts).

do_proto_defs(Defs, Mod, AnRes, Opts) ->
    case get_output_format(Opts) of
        proto_defs ->
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

verify_opts(_Opts) ->
    %% placeholder for verifications
    ok.

return_or_report_warnings_or_errors(Res, ExtraWarns, Opts, OutFormat) ->
    Res2 = merge_warns(Res, ExtraWarns, OutFormat),
    possibly_report_warnings(Res2, Opts),
    possibly_report_error(Res2, Opts),
    return_warnings_or_errors(Res2, Opts).

merge_warns(ok, Warns, _OutFmt)                  -> {ok, Warns};
merge_warns({ok, Warns1}, Warns2, file)          -> {ok, Warns2++Warns1};
merge_warns({ok, Defs}, Warns, proto_defs)       -> {ok, Defs, Warns};
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
            case proplists:get_bool(warnings_as_errors, Opts) of
                true  -> turn_warnings_to_errors_keep(Res);
                false -> Res
            end;
        false ->
            case proplists:get_bool(warnings_as_errors, Opts) of
                true  -> turn_warnings_to_errors_remove(Res);
                false -> remove_warnings_from_res(Res)
            end
    end.

turn_warnings_to_errors_keep({ok, _Mod, _Bin, []}=Res) -> Res;
turn_warnings_to_errors_keep({ok, _MsgDefs, []}=Res)   -> Res;
turn_warnings_to_errors_keep({ok, []}=Res)             -> Res;
turn_warnings_to_errors_keep({ok, _Mod, _Bin, Warns})  -> {error, [], Warns};
turn_warnings_to_errors_keep({ok, _MsgDefs, Warns})    -> {error, [], Warns};
turn_warnings_to_errors_keep({ok, Warns})              -> {error, [], Warns};
turn_warnings_to_errors_keep({error, R, Warns})        -> {error, R, Warns}.

turn_warnings_to_errors_remove({ok, Mod, Bin, []})       -> {ok, Mod, Bin};
turn_warnings_to_errors_remove({ok, MsgDefs, []})        -> {ok, MsgDefs};
turn_warnings_to_errors_remove({ok, []})                 -> ok;
turn_warnings_to_errors_remove({ok, _Mod, _Bin, _Warns}) -> error;
turn_warnings_to_errors_remove({ok, _MsgDefs, _Warns})   -> error;
turn_warnings_to_errors_remove({ok, _Warns})             -> error;
turn_warnings_to_errors_remove({error, R, _Warns})       -> {error, R}.

remove_warnings_from_res({ok, Mod, Bin, _Warns}) -> {ok, Mod, Bin};
remove_warnings_from_res({ok, MsgDefs, _Warns})  -> {ok, MsgDefs};
remove_warnings_from_res({ok, _Warns})           -> ok;
remove_warnings_from_res({error, R, _Warns})     -> {error, R}.

get_output_format([binary | _])                -> binary;
get_output_format([{binary, true} | _])        -> binary;
get_output_format([to_proto_defs | _])         -> proto_defs;
get_output_format([{to_proto_defs, true} | _]) -> proto_defs;
get_output_format([_ | Rest])                  -> get_output_format(Rest);
get_output_format([])                          -> file.

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
%% for instance {@link file/2} or {@link proto_defs/2}.
format_error({error, Reason, _Warns}) -> fmt_err(Reason);
format_error({error, Reason})         -> fmt_err(Reason);
format_error(Reason)                  -> fmt_err(Reason).

%% Note: do NOT include trailing newline (\n or ~n)
fmt_err({option_error, {not_supported, maps_omitted_nif}}) ->
    ?f("Options maps, maps_unset_optional=omitted and nif is not supported");
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
%% for instance {@link file/2} or {@link proto_defs/2}.
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
    io:format("No proto files specified.~n"),
    show_help(),
    halt(0).

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
%%   <dd>Specify that decoded strings should be returned as binaries,
%%       instead of as strings (lists).</dd>
%%   <dt>`-pldefs'</dt>
%%   <dd>Specify that introspection functions shall return proplists
%%       instead of `#field{}' records, to make the generated code
%%       completely free of even compile-time dependencies to gpb.</dd>
%%   <dt>`-pkgs'</dt>
%%   <dd>Prepend the name of a package to every message it contains.
%%       If no package is defined, nothing will be prepended.
%%       Default is to not prepend package names for backwards
%%       compatibility, but it is needed for some proto files.</dd>
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
%%   <dt>`-msgsuffix Suffix'</dt>
%%   <dd>Suffix each message name with `Suffix'.</dd>
%%   <dt>`-modsuffix Suffix'</dt>
%%   <dd>Suffix each module name with `Suffix'.</dd>
%%   <dt>`-msgtolower'</dt>
%%   <dd>ToLower each message. Any prefixes/suffixes are added
%%       after case modification.</dd>
%%   <dt>`-il'</dt>
%%   <dd>Generate code that include gpb.hrl using `-include_lib'
%%       instead of `-include', which is the default.</dd>
%%   <dt>`-type'</dt>
%%   <dd>Enables `::Type()' annotations in the generated .hrl file.</dd>
%%   <dt>`-descr'</dt>
%%   <dd>Generate self-description information.</dd>
%%   <dt>`-maps'</dt>
%%   <dd>Generate code that will accept and produce maps instead of
%%       records. No .hrl file will be generated. See the `maps' option
%%       for the function {@link file/2} for more info.</dd>
%%   <dt>`-maps_unset_optional omitted | present_undefined'</dt>
%%   <dd>Specifies the internal format for optional fields that are unset.</dd>
%%   <dt>`-erlc_compile_options Options'</dt>
%%   <dd>Specifies compilation options, in a comma separated string, to pass
%%       along to the \-compile\(\) directive on the generated code.</dd>>
%%   <dt>`-Werror', `-W1', `-W0', `-W', `-Wall'</dt>
%%   <dd>`-Werror' means treat warnings as errors<br></br>
%%       `-W1' enables warnings, `-W0' disables warnings.<br></br>
%%       `-W' and `-Wall' are the same as `-W1'</dd>
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
    FileNames = [if is_atom(File)     -> atom_to_list(File);
                    is_list(File)     -> File
                 end
                 || File <- Files],
    InitArgs = init_args_to_argv(init:get_arguments()),
    PlainArgs = init:get_plain_arguments(),
    Argv = InitArgs ++ PlainArgs ++ FileNames,
    case parse_opts_and_args(Argv) of
        {ok, {Opts, Args}} ->
            c(Opts, Args);
        {error, Reason} ->
            io:format("Error: ~s.~n", [Reason]),
            show_args(),
            halt(1)
    end.

init_args_to_argv(InitArgs) ->
    lists:append([["-"++atom_to_list(OptName) | OptArgs]
                  || {OptName, OptArgs} <- InitArgs,
                     is_gpb_opt(OptName)]).

%% Opts are expected to be on same format as accepted by file/2.
%% passed by parse_opts_and_args/2.
c(Opts, Args) ->
    case determine_cmdline_op(Opts, Args) of
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
            Opts2 = Opts ++ [report_warnings, report_errors],
            Results = [file(FileName, Opts2) || FileName <- Args],
            case lists:usort(Results) of
                [ok]  -> halt(0);
                _Errs -> halt(1)
            end
    end.

parse_opts_and_args(Argv) ->
    do_parse_argv(Argv, [], []).

do_parse_argv(["-"++OptName=Opt | Rest], Opts, Files) ->
    case find_opt_spec(OptName) of
        {ok, OptSpec} ->
            case parse_opt(OptName, OptSpec, Rest) of
                {ok, {ParsedOpt, Rest2}} ->
                    do_parse_argv(Rest2, [ParsedOpt | Opts], Files);
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, "Unknown option " ++ Opt}
    end;
do_parse_argv([File | Rest], Opts, Files) ->
    do_parse_argv(Rest, Opts, [File | Files]);
do_parse_argv([], Opts, Files) ->
    {ok, {lists:reverse(Opts), lists:reverse(Files)}}.

is_gpb_opt(InitArgOptAtom) ->
    find_opt_spec(atom_to_list(InitArgOptAtom)) /= error.

find_opt_spec(OptName) ->
    case [OptSpec || OptSpec <- opt_specs(), opt_matches(OptName, OptSpec)] of
        [] ->
            error;
        [OptSpec] ->
            {ok, OptSpec}
    end.

opt_matches(Opt, {OptName, 'string_maybe_appended()', _OptTag, _Descr}) ->
    lists:prefix(OptName, Opt);
opt_matches(Opt, {OptName, _Type, _OptTag, _Descr}) ->
    Opt == OptName.

parse_opt(Opt, {OptName, 'string_maybe_appended()', OptTag, _Descr}, Rest) ->
    case {Opt, Rest} of
        {OptName, [H | Rest2]} ->
            {ok, {{OptTag, H}, Rest2}};
        {OptName, []} ->
            {error, "Missing argument for option -" ++ OptName};
        _ ->
            true = lists:prefix(OptName, Opt),
            OptArg = string:substr(Opt, length(OptName)+1),
            {ok, {{OptTag, OptArg}, Rest}}
    end;
parse_opt(OptName, {OptName, undefined, OptTag, _Descr}, Rest) ->
    {ok, {OptTag, Rest}};
parse_opt(OptName, {OptName, 'string()', OptTag, _Descr}, [OptArg | Rest]) ->
    {ok, {{OptTag, OptArg}, Rest}};
parse_opt(OptName, {OptName, Alternatives, OptTag, _Descr}, [OptArg | Rest]) ->
    case parse_opt_alts(tuple_to_list(Alternatives), OptArg, OptTag) of
        {ok, Opt} -> {ok, {Opt, Rest}};
        error     -> {error, "Invalid argument for -" ++ OptName}
    end;
parse_opt(OptName, _OptSpec, []) ->
    {error, "Missing argument for option -" ++ OptName}.

parse_opt_alts(['number()' | Rest], OptArg, OptTag) ->
    case string_to_number(OptArg) of
        {ok, Value} -> {ok, {OptTag, Value}};
        error       -> parse_opt_alts(Rest, OptArg, OptTag)
    end;
parse_opt_alts([Value | Rest], OptArg, OptTag) ->
    case atom_to_list(Value) of
        OptArg -> {ok, {OptTag, Value}};
        _      -> parse_opt_alts(Rest, OptArg, OptTag)
    end;
parse_opt_alts([], _OptArg, _OptTag) ->
    error.

opt_specs() ->
    [
     {"I", 'string_maybe_appended()', i, "\n"
      "       Specify include directory.\n"
      "       Option may be specified more than once to specify\n"
      "       several include directories.\n"},
     {"o", 'string()', o, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the <ProtoFile>.erl and <ProtoFile>.hrl\n"},
     {"o-erl", 'string()', o_erl, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the <ProtoFile>.erl.\n"
      "       The -o-erl Dir option overrides any -o Dir option, and\n"
      "       similarly for the other file-type specific output options.\n"},
     {"o-hrl", 'string()', o_hrl, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the <ProtoFile>.hrl\n"},
     {"o-nif-cc", 'string()', o_nif_cc, "Dir\n"
      "       Specify output directory for where to generate\n"
      "       the NIF C++ file, if the -nif option is specified\n"},
     {"nif", undefined, nif, "\n"
      "       Generate nifs for linking with the protobuf C(++) library.\n"},
     {"load_nif", 'string()', load_nif, "FunctionDefinition\n"
      "       Specify FunctionDefinition as the text that defines the\n"
      "       function load_nif/0.  This is called as the -on_load.\n"
      "       hook for loading the NIF.\n"},
     {"v", {optionally, always, never}, verify, " optionally | always | never\n"
      "       Specify how the generated encoder should\n"
      "       verify the message to be encoded.\n"},
     {"c", {true, false, auto, 'number()'}, copy_bytes,
      " true | false | auto | number() \n"
      "       Specify how or when the generated decoder should\n"
      "       copy fields of type bytes.\n"},
     {"strbin", undefined, strings_as_binaries, "\n"
      "       Specify that decoded strings should be returned as binaries,\n"
      "       instead of as strings (lists).\n"},
     {"pldefs", undefined, defs_as_proplists, "\n"
      "       Specify that introspection functions shall return proplists\n"
      "       instead of #field{} records, to make the generated code\n"
      "       completely free of even compile-time dependencies to gpb.\n"},
     {"pkgs", undefined, use_packages, "\n"
      "       Prepend the name of a package to every message it contains.\n"
      "       If no package is defined, nothing will be prepended.\n"
      "       Default is to not prepend package names for backwards\n"
      "       compatibility, but it is needed for some proto files.\n"},
     {"msgprefix", 'string()', msg_name_prefix, "Prefix\n"
      "       Prefix each message with Prefix.\n"},
     {"modprefix", 'string()', module_name_prefix, "Prefix\n"
      "       Prefix the module name with Prefix.\n"},
     {"msgsuffix", 'string()', msg_name_suffix, "Suffix\n"
      "       Suffix each message with Suffix.\n"},
     {"msgtolower", undefined, msg_name_to_lower, "ToLower\n"
      "       ToLower each message.  Any prefixes/suffixes are added\n"
      "       after case modification.\n"},
     {"modsuffix", 'string()', module_name_suffix, "Suffix\n"
      "       Suffix the module name with Suffix.\n"},
     {"il", undefined, include_as_lib, "\n"
      "       Generate code that includes gpb.hrl using -include_lib\n"
      "       instead of -include, which is the default.\n"},
     {"type", undefined, type_specs, "\n"
      "       Enables `::Type()' annotations in the generated .hrl file.\n"},
     {"descr", undefined, descriptor, "\n"
      "       Generate self-description information.\n"},
     {"maps", undefined, maps, "\n"
      "       Generate code that will accept and produce maps instead of\n"
      "       records.\n"},
     {"maps_unset_optional", {omitted, present_undefined}, maps_unset_optional,
      "\n"
      "       Specifies the internal format for optional fields\n"
      "       that are unset.\n"},
     {"erlc_compile_options", 'string()', erlc_compile_options,
      "\n"
      "       Specifies compilation options, in a comma separated string, to\n"
      "       pass along to the -compile() directive on the generated code.\n"},
     {"Werror",undefined, warnings_as_errors, "\n"
      "       Treat warnings as errors\n"},
     {"W1", undefined, report_warnings, "\n"
      "       Report warnings\n"},
     {"W0", undefined, {report_warnings,false}, "\n"
      "       Do not report warnings\n"},
     {"Wall", undefined, report_warnings, "\n"
      "       Same as -W1\n"},
     {"W", undefined, report_warnings, "\n"
      "       Same as -W1\n"},
     {"h", undefined, help, "\n"
      "       Show help\n"},
     {"-help", undefined, help, "\n"
      "       Show help\n"},
     {"V", undefined, version, "\n"
      "       Show version\n"},
     {"-version", undefined, version, "\n"
      "       Show version\n"}
    ].


determine_cmdline_op(Opts, FileNames) ->
    case {lists:member(help, Opts), lists:member(version, Opts)} of
        {true, _} -> show_help;
        {_, true} -> show_version;
        _         -> if FileNames == [] -> error;
                        FileNames /= [] -> compile
                     end
    end.

show_help() ->
    io:format(
      "gpb version ~s~n"
      "Usage: erl <erlargs> [gpb-opts] -s ~p c <ProtoFile>.proto~n"
      "   or: erl <erlargs> -s ~p c <ProtoFile>.proto -extra [gpb-opts]~n"
      "Typical erlargs = -noshell -noinput +B -boot start_clean -pa SomeDir~n"
      "~n",
      [gpb:version_as_string(), ?MODULE, ?MODULE]),
    show_args().

show_arg({OptDef, 'string_maybe_appended()', _, OptDoc}) ->
    io:format("   -~s   -~sOption ~s", [OptDef, OptDef, OptDoc]);
show_arg({OptDef, _, _, OptDoc}) ->
    io:format("   -~s ~s", [OptDef, OptDoc]).

show_args() ->
    io:format(
      "Recognized gpb-opts: (see the edoc for ~p for further details)~n",
      [?MODULE]),
    lists:foreach(fun show_arg/1, opt_specs()).

show_version() ->
    io:format("gpb version ~s~n", [gpb:version_as_string()]).

string_to_number(S) ->
    try {ok, list_to_integer(S)}
    catch error:badarg ->
            try {ok, list_to_float(S)}
            catch error:badarg -> error
            end
    end.

parse_file(FName, Opts) ->
    case parse_file_and_imports(FName, Opts) of
        {ok, {Defs1, _AllImported}} ->
            case gpb_parse:post_process_all_files(Defs1, Opts) of
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
            case scan_and_parse_string(binary_to_list(Contents), FName, Opts) of
                {ok, Defs} ->
                    Imports = gpb_parse:fetch_imports(Defs),
                    read_and_parse_imports(Imports,AlreadyImported2,Defs,Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

scan_and_parse_string(S, FName, Opts) ->
    case gpb_scan:string(S) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens++[{'$end', 999}]) of
                {ok, ParseTree} ->
                    case gpb_parse:post_process_one_file(ParseTree, Opts) of
                        {ok, Result} ->
                            {ok, Result};
                        {error, Reason} ->
                            {error, {parse_error, FName, Reason}}
                    end;
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
    fold_msg_fields(fun(From, #?gpb_field{type={msg,To}}, _) ->
                            digraph:add_edge(G, From, To);
                       (_MsgName, _Feild, _Acc) ->
                            ok
                    end,
                    ok,
                    Defs),
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
    MapTypes = find_map_types(Defs),
    MapsAsMsgs = map_types_to_msgs(sets:to_list(MapTypes)),
    #anres{used_types          = find_used_types(Defs),
           known_msg_size      = find_msgsizes_known_at_compile_time(
                                   MapsAsMsgs ++ Defs),
           msg_occurrences     = find_msg_occurrences(MapsAsMsgs ++ Defs),
           fixlen_types        = find_fixlen_types(MapsAsMsgs ++ Defs),
           num_packed_fields   = find_num_packed_fields(MapsAsMsgs ++ Defs),
           num_fields          = find_num_fields(MapsAsMsgs ++ Defs),
           d_field_pass_method = compute_decode_field_pass_methods(
                                   MapsAsMsgs ++ Defs, Opts),
           maps_as_msgs        = MapsAsMsgs,
           map_translations    = compute_map_translations(Defs, Opts),
           map_types           = MapTypes}.

find_map_types(Defs) ->
    fold_msg_fields(
      fun(_MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              sets:add_element({KeyType,ValueType}, Acc);
         (_MsgName, _Field, Acc) ->
              Acc
      end,
      sets:new(),
      Defs).

map_types_to_msgs(MapTypes) ->
    [{{msg, map_type_to_msg_name(KeyType,ValueType)},
      gpb:map_item_pseudo_fields(KeyType, ValueType)}
     || {KeyType,ValueType} <- MapTypes].

map_type_to_msg_name(KeyType, {msg,MsgName}) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, MsgName]));
map_type_to_msg_name(KeyType, {enum,EnumName}) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, EnumName]));
map_type_to_msg_name(KeyType, ValueType) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, ValueType])).

find_used_types(Defs) ->
    fold_msg_fields(
      fun(_MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              Acc1 = sets:add_element(KeyType, Acc),
              sets:add_element(ValueType, Acc1);
         (_MsgName, #?gpb_field{type=Type}, Acc) ->
              sets:add_element(Type, Acc)
      end,
      sets:new(),
      Defs).

find_msg_occurrences(Defs) ->
    fold_msg_fields(fun(_MsgName, #?gpb_field{type=Type, occurrence=Occ}, Acc) ->
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
      fun(_, #?gpb_field{type=Type, occurrence=Occ}=FieldDef, Acc) ->
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

%% Loop over all message fields, including oneof-fields
%% Call Fun for all #?gpb_fields{}, skip over non-msg defs
fold_msg_fields(Fun, InitAcc, Defs) ->
    lists:foldl(
      fun({{msg, MsgName}, Fields}, Acc) ->
              FFun = fun(Field, FAcc) -> Fun(MsgName, Field, FAcc) end,
              fold_msgdef_fields(FFun, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).

fold_msgdef_fields(Fun, InitAcc, Fields) ->
    lists:foldl(
      fun(#?gpb_field{}=Field, Acc) ->
              Fun(Field, Acc);
         (#gpb_oneof{fields=OFields}, Acc) ->
              lists:foldl(fun(OField, OAcc) -> Fun(OField, OAcc) end,
                          Acc,
                          OFields)
      end,
      InitAcc,
      Fields).

%% The fun takes two args: Fun(#?gpb_field{}, IsOneofField) -> term()
map_msgdef_fields_o(Fun, Fields) ->
    lists:reverse(
      lists:foldl(
        fun(#?gpb_field{}=Field, Acc) ->
                [Fun(Field, false) | Acc];
           (#gpb_oneof{name=CFName, fields=OFields}, Acc) ->
                IsOneOf = {true, CFName},
                lists:foldl(fun(OField, OAcc) -> [Fun(OField, IsOneOf) | OAcc]
                            end,
                            Acc,
                            OFields)
        end,
        [],
        Fields)).

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

find_msgsize_2([#gpb_oneof{} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=repeated} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=optional} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{type=Type, fnum=FNum} | Rest], AccSize, Defs, T) ->
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
            NumSubMsgFields = count_submsg_fields(MsgDef),
            NumMapFields = count_map_fields(MsgDef),
            IsMsgDominatedBySubMsgsOrMaps =
                (NumSubMsgFields + NumMapFields) / NF > 0.5,
            if IsMsgDominatedBySubMsgsOrMaps, NF >= 100 ->
                    pass_as_record;
               true ->
                    pass_as_params
            end
    end.

count_submsg_fields(MsgDef) ->
    fold_msgdef_fields(fun(#?gpb_field{type={msg,_}}, N) -> N+1;
                          (#?gpb_field{}, N)             -> N
                       end,
                       0,
                       MsgDef).

count_map_fields(MsgDef) ->
    fold_msgdef_fields(fun(#?gpb_field{type={map,_,_}}, N) -> N+1;
                          (#?gpb_field{}, N)               -> N
                       end,
                       0,
                       MsgDef).

compute_map_translations(Defs, Opts) ->
    MapInfos =
        fold_msg_fields(
          fun(MsgName, #?gpb_field{name=FName, type={map,KType,VType}}, Acc) ->
                  [{{MsgName, FName}, {KType, VType}} | Acc];
             (_MsgName, _Field, Acc) ->
                  Acc
          end,
          [],
          Defs),
    MapsOrRecords = get_records_or_maps_by_opts(Opts),
    dict:from_list(
      lists:append(
        [begin
             MapAsMsgName = map_type_to_msg_name(KeyType, ValueType),
             case MapsOrRecords of
                 records ->
                     [{[MsgName,FName,elem],
                       [{encode, {mt_maptuple_to_pseudomsg_r,
                                  ['$1',MapAsMsgName]}}]},
                      {[MsgName,FName],
                       [{decode_init_default, {mt_empty_map_r,[]}},
                        {decode_repeated_add_elem,{mt_add_item_r,['$1','$2']}},
                        {decode_repeated_finalize,{mt_finalize_items_r,['$1']}},
                        {merge, {mt_merge_maptuples_r,['$1','$2']}}]}];
                 maps ->
                     [{[MsgName,FName,elem],
                       [{encode, {mt_maptuple_to_pseudomsg_m, ['$1']}}]},
                      {[MsgName,FName],
                       [{encode, {mt_map_to_list_m,['$1']}},
                        {decode_init_default, {mt_empty_map_m,[]}},
                        {decode_repeated_add_elem,{mt_add_item_m,['$1','$2']}},
                        {decode_repeated_finalize,{id,['$1']}},
                        {merge, {mt_merge_maps_m,['$1','$2']}}]}]
             end
         end
         || {{MsgName, FName}, {KeyType, ValueType}} <- MapInfos])).

%% -- generating code ----------------------------------------------

format_erl(Mod, Defs, #anres{maps_as_msgs=MapsAsMsgs}=AnRes, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    AsLib = proplists:get_bool(include_as_lib, Opts),
    CompileOptsStr = get_erlc_compile_options_str(Opts),
    iolist_to_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s on ~w~n",
          [?MODULE, gpb:version_as_string(), calendar:local_time()]),
       ?f("-module(~w).~n", [Mod]),
       case CompileOptsStr of
           ""    -> "";
           [_|_] -> ?f("-compile([~ts]).~n", [CompileOptsStr])
       end,
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
       ?f("-export([get_service_names/0]).~n"),
       ?f("-export([get_service_def/1]).~n"),
       ?f("-export([get_rpc_names/1]).~n"),
       ?f("-export([find_rpc_def/2, fetch_rpc_def/2]).~n"),
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
       format_export_types(Defs, Opts),
       "\n",
       if not DoNif ->
               case get_records_or_maps_by_opts(Opts) of
                   records ->
                       ?f("~s~n", [fmt_maps_as_msgs_record_defs(AnRes)]);
                   maps ->
                       ""
               end;
          DoNif ->
               ""
       end,
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
       if DoNif ->
               ?f("~s~n", [format_nif_encoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [?f("~s~n", [format_msg_encoders(Defs, AnRes, Opts, true)]),
                ?f("~s~n", [format_msg_encoders(MapsAsMsgs,AnRes,Opts,false)]),
                ?f("~s~n", [format_aux_encoders(Defs, AnRes, Opts)])]
       end,
       "\n",
       format_decoders_top_function(Defs),
       "\n\n",
       if DoNif ->
               ?f("~s~n", [format_nif_decoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [?f("~s~n", [format_msg_decoders(Defs, AnRes, Opts)]),
                ?f("~s~n", [format_msg_decoders(MapsAsMsgs, AnRes, Opts)]),
                ?f("~s~n", [format_aux_decoders(Defs, AnRes, Opts)])]
       end,
       "\n",
       ?f("~s~n", [format_msg_merge_code(Defs, AnRes, Opts)]),
       "\n",
       format_verifiers_top_function(Defs, Opts),
       "\n",
       ?f("~s~n", [format_verifiers(Defs, AnRes, Opts)]),
       "\n",
       if not DoNif ->
               [?f("~s~n", [format_aux_transl_helpers()]),
                ?f("~s~n", [format_aux_transl_helpers_used_also_with_nifs()]),
                ?f("~s~n", [format_translators(Defs, AnRes, Opts)])];
          DoNif ->
               [?f("~s~n", [format_aux_transl_helpers_used_also_with_nifs()]),
                ?f("~s~n", [format_merge_translators(Defs, AnRes, Opts)])]
       end,
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

get_erlc_compile_options_str(Opts) ->
    proplists:get_value(erlc_compile_options, Opts, "").

%% -- encoders -----------------------------------------------------

format_encoders_top_function(Defs, Opts) ->
    case contains_messages(Defs) of
        true  -> format_encoders_top_function_msgs(Defs, Opts);
        false -> format_encoders_top_function_no_msgs(Opts)
    end.

format_encoders_top_function_no_msgs(Opts) ->
    Mapping = get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(_MsgName)]
                  end,
    [gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>') -> encode_msg(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     gpb_codegen:format_fn(
       encode_msg,
       fun(_Msg, '<MsgName>', _Opts) ->
               erlang:error({gpb_error, no_messages})
       end,
       [splice_trees('<MsgName>', MsgNameVars)])].

format_encoders_top_function_msgs(Defs, Opts) ->
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

format_aux_encoders(Defs, AnRes, _Opts) ->
    [format_enum_encoders(Defs, AnRes),
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

format_msg_encoders(Defs, AnRes, Opts, IncludeStarter) ->
    [[format_msg_encoder(MsgName, MsgDef, Defs, AnRes, Opts, IncludeStarter)
      || {{msg, MsgName}, MsgDef} <- Defs],
     format_special_field_encoders(Defs, AnRes)].

format_msg_encoder(MsgName, [], _Defs, _AnRes, _Opts, _IncludeStarter) ->
    gpb_codegen:format_fn(
      mk_fn(e_msg_, MsgName),
      fun(_Msg) ->
              <<>>
      end);
format_msg_encoder(MsgName, MsgDef, Defs, AnRes, Opts, IncludeStarter) ->
    FNames = get_field_names(MsgDef),
    FVars = [var_f_n(I) || I <- lists:seq(1, length(FNames))],
    BVars = [var_b_n(I) || I <- lists:seq(1, length(FNames)-1)] ++ [last],
    MsgVar = ?expr(M),
    {EncodeExprs, _} =
        lists:mapfoldl(
          fun({NewBVar, Field, FVar}, PrevBVar) when NewBVar /= last ->
                  EncExpr = field_encode_expr(MsgName, MsgVar, Field, FVar,
                                              PrevBVar, Defs, AnRes, Opts,
                                              p3_check_typedefaults),
                  E = ?expr('<NewB>' = '<encode-expr>',
                            [replace_tree('<NewB>', NewBVar),
                             replace_tree('<encode-expr>', EncExpr)]),
                  {E, NewBVar};
             ({last, Field, FVar}, PrevBVar) ->
                  EncExpr = field_encode_expr(MsgName, MsgVar, Field, FVar,
                                              PrevBVar, Defs, AnRes, Opts,
                                              p3_check_typedefaults),
                  {EncExpr, dummy}
          end,
          ?expr(Bin),
          lists:zip3(BVars, MsgDef, FVars)),
    FnName = mk_fn(e_msg_, MsgName),
    FieldMatching =
        case get_mapping_and_unset_by_opts(Opts) of
            X when X == records;
                   X == {maps, present_undefined} ->
                mapping_match(MsgName, lists:zip(FNames, FVars), Opts);
            {maps, omitted} ->
                FMap = zip_for_non_opt_fields(MsgDef, FVars),
                if length(FMap) == length(FNames) ->
                        map_match(FMap);
                   length(FMap) < length(FNames) ->
                        ?expr('mapmatch' = 'M',
                              [replace_tree('mapmatch', map_match(FMap)),
                               replace_tree('M', MsgVar)])
                end
        end,
    [[[gpb_codegen:format_fn(
         FnName,
         fun(Msg) ->
                 call_self(Msg, <<>>)
         end),
       "\n"] || IncludeStarter],
     gpb_codegen:format_fn(
       FnName,
       fun('<msg-matching>', Bin) ->
               '<encode-param-exprs>'
       end,
       [replace_tree('<msg-matching>', FieldMatching),
        splice_trees('<encode-param-exprs>', EncodeExprs)])].

get_field_names(MsgDef) ->
    [case Field of
         #?gpb_field{name=FName} -> FName;
         #gpb_oneof{name=FName}  -> FName
     end
     || Field <- MsgDef].

zip_for_non_opt_fields([#?gpb_field{name=FName, occurrence=Occurrence} | FRest],
                       [Elem | ERest]) ->
    case Occurrence of
        optional -> zip_for_non_opt_fields(FRest, ERest);
        required -> [{FName, Elem} | zip_for_non_opt_fields(FRest, ERest)];
        repeated -> [{FName, Elem} | zip_for_non_opt_fields(FRest, ERest)]
    end;
zip_for_non_opt_fields([#gpb_oneof{} | FRest], [_Elem | ERest]) ->
    zip_for_non_opt_fields(FRest, ERest);
zip_for_non_opt_fields([], []) ->
    [].

field_encode_expr(MsgName, MsgVar, #?gpb_field{name=FName}=Field,
                  FVar, PrevBVar, Defs, AnRes, Opts, P3TypeDefaultHandling)->
    FEncoder = mk_field_encode_fn_name(MsgName, Field),
    #?gpb_field{occurrence=Occurrence, type=Type, fnum=FNum, name=FName}=Field,
    TrFVar = prefix_var("Tr", FVar),
    ElemPath = [MsgName, FName],
    TranslFn = find_translation(ElemPath, encode, AnRes),
    Transforms = [replace_term('fieldname', FName),
                  replace_tree('<F>', FVar),
                  replace_tree('TrF', TrFVar),
                  replace_term('Tr', TranslFn),
                  replace_term('<enc>', FEncoder),
                  replace_tree('<Bin>', PrevBVar),
                  splice_trees('<Key>', key_to_binary_fields(FNum, Type))],
    case Occurrence of
        optional ->
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(
                       if '<F>' == undefined -> '<Bin>';
                          true -> '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>)
                       end,
                       Transforms);
                {maps, omitted} ->
                    ?expr(
                       case maps:find('fieldname', 'M') of
                           error ->
                               '<Bin>';
                           {ok, '<F>'} ->
                               '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>)
                       end,
                       [replace_tree('M', MsgVar) | Transforms])
            end;
        repeated ->
            ?expr(
               begin
                   'TrF' = 'Tr'('<F>'),
                   if 'TrF' == [] -> '<Bin>';
                      true -> '<enc>'('TrF', '<Bin>')
                   end
               end,
               Transforms);
        required ->
            case gpb:is_msg_proto3(MsgName, Defs) of
                true when P3TypeDefaultHandling == p3_check_typedefaults,
                          Type /= string ->
                    TypeDefault = gpb:proto3_type_default(Type, Defs),
                    ?expr(
                       if '<F>' =:= '<TypeDefault>' ->
                               '<Bin>';
                          true ->
                               '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>)
                       end,
                       [replace_term('<TypeDefault>', TypeDefault)
                        | Transforms]);
                true when P3TypeDefaultHandling == p3_check_typedefaults,
                          Type == string ->
                    ?expr(case iolist_size('<F>') of
                              0 ->
                                  '<Bin>';
                              _ ->
                                  '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>)
                          end,
                          Transforms);
                _not_proto3_or_no_check_for_typedefaults ->
                    ?expr(
                       '<enc>'('<F>', <<'<Bin>'/binary, '<Key>'>>),
                       Transforms)
            end
    end;
field_encode_expr(MsgName, MsgVar, #gpb_oneof{name=FName, fields=OFields},
                  FVar, PrevBVar, Defs, AnRes, Opts, _P3TypeDefaultHandling) ->
    OFVar = prefix_var("O", FVar),
    OneofClauseTransform =
        repeat_clauses(
          '<oneof...>',
          [begin
               MatchPattern =
                   case get_mapping_and_unset_by_opts(Opts) of
                       X when X == records;
                              X == {maps, present_undefined} ->
                           ?expr({'<oneof-name>', '<OF>'},
                                 [replace_term('<oneof-name>', Name),
                                  replace_tree('<OF>', OFVar)]);
                       {maps, omitted} ->
                           ?expr({ok, {'<oneof-name>', '<OF>'}},
                                 [replace_term('<oneof-name>', Name),
                                  replace_tree('<OF>', OFVar)])
                   end,
               %% undefined is already handled, we have a match,
               %% the field occurs, as if it had been required
               OField2 = OField#?gpb_field{occurrence=required},
               EncExpr = field_encode_expr(MsgName, MsgVar, OField2, OFVar,
                                           PrevBVar, Defs, AnRes, Opts,
                                           no_typedefault_checking),
               [replace_tree('<oneof...>', MatchPattern),
                replace_tree('<expr>', EncExpr)]
           end
           || #?gpb_field{name=Name}=OField <- OFields]),
    case get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            ?expr(case '<F>' of
                      undefined    -> '<Bin>';
                      '<oneof...>' -> '<expr>'
                  end,
                  [replace_tree('<F>', FVar),
                   replace_tree('<Bin>', PrevBVar),
                   OneofClauseTransform]);
        {maps, omitted} ->
            ?expr(case maps:find('fieldname', 'M') of
                      error        -> '<Bin>';
                      '<oneof...>' -> '<expr>'
                  end,
                  [replace_term('fieldname', FName),
                   replace_tree('M', MsgVar),
                   replace_tree('<Bin>', PrevBVar),
                   OneofClauseTransform])
    end.

mk_field_encode_fn_name(MsgName, #?gpb_field{occurrence=repeated, name=FName})->
    mk_fn(e_field_, MsgName, FName);
mk_field_encode_fn_name(MsgName, #?gpb_field{type={msg,_Msg}, name=FName}) ->
    mk_fn(e_mfield_, MsgName, FName);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type={enum,EnumName}}) ->
    mk_fn(e_enum_, EnumName);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=sint32}) ->
    mk_fn(e_type_, sint);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=sint64}) ->
    mk_fn(e_type_, sint);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=uint32}) ->
    e_varint;
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=uint64}) ->
    e_varint;
mk_field_encode_fn_name(MsgName,  #?gpb_field{type=Type}=F) ->
    case Type of
        {map,KeyType,ValueType} ->
            MapAsMsgMame = map_type_to_msg_name(KeyType, ValueType),
            F2 = F#?gpb_field{type = {msg,MapAsMsgMame}},
            mk_field_encode_fn_name(MsgName, F2);
        _ ->
            mk_fn(e_type_, Type)
    end.

format_special_field_encoders(Defs, AnRes) ->
    lists:reverse( %% so generated auxiliary functions come in logical order
      fold_msg_fields(
        fun(MsgName, #?gpb_field{occurrence=repeated}=FieldDef, Acc) ->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (MsgName, #?gpb_field{type={msg,_}}=FieldDef, Acc)->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_MsgName, #?gpb_field{}, Acc) ->
                Acc
        end,
        [],
        Defs)).

format_field_encoder(MsgName, FieldDef, AnRes) ->
    #?gpb_field{occurrence=Occurrence} = FieldDef,
    RFieldDef = FieldDef#?gpb_field{occurrence=required},
    [possibly_format_mfield_encoder(MsgName, RFieldDef, AnRes),
     case {Occurrence, is_packed(FieldDef)} of
         {repeated, false} ->
             format_repeated_field_encoder2(MsgName, FieldDef, AnRes);
         {repeated, true} ->
             format_packed_field_encoder2(MsgName, FieldDef);
         {optional, false} ->
             [];
         {required, false} ->
             []
     end].

possibly_format_mfield_encoder(MsgName, #?gpb_field{type={msg,SubMsg}}=FieldDef,
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
possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={map,KType,VType}}=FieldDef,
                               AnRes) ->
    MapAsMsgName = map_type_to_msg_name(KType, VType),
    FieldDef2 = FieldDef#?gpb_field{type = {msg,MapAsMsgName}},
    possibly_format_mfield_encoder(MsgName, FieldDef2, AnRes);
possibly_format_mfield_encoder(_MsgName, _FieldDef, _Defs) ->
    [].

is_msgsize_known_at_generationtime(MsgName, #anres{known_msg_size=MsgSizes}) ->
    case dict:fetch(MsgName, MsgSizes) of
        MsgSize when is_integer(MsgSize) ->
            {yes, MsgSize};
        undefined ->
            no
    end.

format_repeated_field_encoder2(MsgName, FDef, AnRes) ->
    #?gpb_field{fnum=FNum, type=Type, name=FName} = FDef,
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(
                      MsgName, FDef#?gpb_field{occurrence=required}),
    KeyBytes = key_to_binary_fields(FNum, Type),
    ElemPath = [MsgName,FName,elem],
    Transl = find_translation(ElemPath, encode, AnRes),
    gpb_codegen:format_fn(
      FnName,
      fun([Elem | Rest], Bin) ->
              Bin2 = <<Bin/binary, '<KeyBytes>'>>,
              Bin3 = '<encode-elem>'('Tr'(Elem), Bin2),
              call_self(Rest, Bin3);
         ([], Bin) ->
              Bin
      end,
      [splice_trees('<KeyBytes>', KeyBytes),
       replace_term('<encode-elem>', ElemEncoderFn),
       replace_term('Tr', Transl)]).

format_packed_field_encoder2(MsgName, #?gpb_field{type=Type}=FDef) ->
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

format_knownsize_packed_field_encoder2(MsgName, #?gpb_field{name=FName,
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

format_unknownsize_packed_field_encoder2(MsgName, #?gpb_field{name=FName,
                                                              fnum=FNum}=FDef) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(
                      MsgName, FDef#?gpb_field{occurrence=required}),
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

format_nif_encoder_error_wrappers(Defs, _AnRes, _Opts) ->
    [format_msg_nif_encode_error_wrapper(MsgName)
     || {{msg, MsgName}, _MsgDef} <- Defs].

format_msg_nif_encode_error_wrapper(MsgName) ->
    gpb_codegen:format_fn(
      mk_fn(e_msg_, MsgName),
      fun(Msg) ->
              erlang:nif_error({error,{nif_not_loaded,'<msg-name>'}}, [Msg])
      end,
      [replace_term('<msg-name>', MsgName)]).

%% -- decoders -----------------------------------------------------

format_decoders_top_function(Defs) ->
    case contains_messages(Defs) of
        true  -> format_decoders_top_function_msgs(Defs);
        false -> format_decoders_top_function_no_msgs()
    end.

format_decoders_top_function_no_msgs() ->
    gpb_codegen:format_fn(
      decode_msg,
      fun(Bin, _MsgName) when is_binary(Bin) ->
              erlang:error({gpb_error, no_messages})
      end).

format_decoders_top_function_msgs(Defs) ->
    gpb_codegen:format_fn(
      decode_msg,
      fun(Bin, MsgName) when is_binary(Bin) ->
              case MsgName of
                  '<MsgName>' -> '<decode-call>'(Bin)
              end
      end,
      [repeat_clauses('<MsgName>',
                      [[replace_term('<MsgName>', MsgName),
                        replace_term('<decode-call>', mk_fn(d_msg_, MsgName))]
                       || {{msg,MsgName}, _Fields} <- Defs])]).

format_aux_decoders(Defs, AnRes, _Opts) ->
    format_enum_decoders(Defs, AnRes).

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
                        || {EnumSym, EnumValue} <- unalias_enum(EnumDef)])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_msg_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts)
     || {{msg, MsgName}, MsgDef} <- Defs].

format_msg_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    [format_msg_decoder_read_field(MsgName, MsgDef, Defs, AnRes, Opts),
     format_field_decoders(MsgName, MsgDef, AnRes, Opts),
     format_field_skippers(MsgName, AnRes)].

format_msg_decoder_read_field(MsgName, MsgDef, Defs, AnRes, Opts) ->
    Key = ?expr(Key),
    Rest = ?expr(Rest),
    {Params, FParams, FParamBinds} =
        decoder_read_field_params(MsgName, MsgDef, AnRes, Opts),
    Bindings = new_bindings([{'<Params>', Params},
                             {'<FParams>', FParams},
                             {'<FFields>', FParamBinds},
                             {'<Key>', Key},
                             {'<Rest>', Rest}]),
    [format_msg_init_decoder(MsgName, MsgDef, Defs, AnRes, Opts),
     format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts),
     format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts)].

format_msg_init_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    gpb_codegen:format_fn(
      mk_fn(d_msg_, MsgName),
      fun(Bin) -> '<decode-field-fp>'(Bin, 0, 0, '<initial-params>') end,
      [replace_term('<decode-field-fp>', mk_fn(dfp_read_field_def_, MsgName)),
       splice_trees('<initial-params>',
                    msg_decoder_initial_params(MsgName, MsgDef, Defs,
                                               AnRes, Opts))]).

format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts) ->
    %% The fast-path decoder directly matches the minimal varint form
    %% of the field-number combined with the wiretype.
    %% Unrecognized fields fall back to the more generic decoder-loop
    Params = fetch_binding('<Params>', Bindings),
    FParams = fetch_binding('<FParams>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    gpb_codegen:format_fn(
      mk_fn(dfp_read_field_def_, MsgName),
      fun('<precomputed-binary-match>', Z1, Z2, '<Params>') ->
              '<calls-to-field-decoding>';
         (<<>>, 0, 0, '<FParams>') ->
              '<finalize-result>';
         (Other, Z1, Z2, '<Params>') ->
              '<decode-general>'(Other, Z1, Z2, '<Params>')
      end,
      [splice_trees('<Params>', Params),
       splice_trees('<FParams>', FParams),
       repeat_clauses(
         '<precomputed-binary-match>',
         [[replace_tree('<precomputed-binary-match>', BinMatch),
           replace_tree('<calls-to-field-decoding>', FnCall)]
          || {BinMatch, FnCall} <- decoder_fp(Bindings, MsgName, MsgDef)]),
       splice_trees('<finalize-result>',
                    decoder_finalize_result(Params, FFields,
                                            MsgName, MsgDef, AnRes, Opts)),
       replace_term('<decode-general>', mk_fn(dg_read_field_def_, MsgName))]).

format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts) ->
    %% The more general field selecting decoder
    %% Stuff that ends up here: non-minimal varint forms and field to skip
    Key = fetch_binding('<Key>', Bindings),
    Rest = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    FParams = fetch_binding('<FParams>', Bindings),
    FFields = fetch_binding('<FFields>', Bindings),
    gpb_codegen:format_fn(
      mk_fn(dg_read_field_def_, MsgName),
      fun(<<1:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>') when N < (32-7) ->
              call_self('<Rest>', N+7, X bsl N + Acc, '<Params>');
         (<<0:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>') ->
              '<Key>' = X bsl N + Acc,
              '<calls-to-field-decoding-or-skip>';
         (<<>>, 0, 0, '<FParams>') ->
              '<finalize-result>'
      end,
      [replace_tree('<Key>', Key),
       replace_tree('<Rest>', Rest),
       splice_trees('<Params>', Params),
       splice_trees('<FParams>', FParams),
       replace_tree('<calls-to-field-decoding-or-skip>',
                    decoder_field_calls(Bindings, MsgName, MsgDef, AnRes)),
       splice_trees('<finalize-result>',
                    decoder_finalize_result(Params, FFields,
                                            MsgName, MsgDef, AnRes, Opts))]).

msg_decoder_initial_params(MsgName, MsgDef, Defs, AnRes, Opts) ->
    ExprInfos1 =
        [case Field of
             #?gpb_field{name=FName, occurrence=Occurrence, type=Type} ->
                 {Undefined, Undef} =
                     case gpb:is_msg_proto3(MsgName, Defs) of
                         true ->
                             TD = proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD};
                         false ->
                             {?expr(undefined), ?expr('$undef')}
                     end,
                 case Occurrence of
                     repeated -> {FName, m, ?expr([]),        ?expr([])};
                     required -> {FName, o, Undefined,        Undef};
                     optional -> {FName, o, ?expr(undefined), ?expr('$undef')}
                 end;
             #gpb_oneof{name=FName} ->
                 {FName, o, ?expr(undefined), ?expr('$undef')}
         end
         || Field <- MsgDef],
    ExprInfos2 =
        [begin
             ElemPath = [MsgName, FName],
             TranslFn = find_translation(ElemPath, decode_init_default, AnRes),
             TrInitExpr = ?expr('Tr'('InitExpr'),
                                [replace_tree('InitExpr', InitExpr),
                                 replace_term('Tr', TranslFn)]),
             TrMOExpr = ?expr('Tr'('MOExpr'),
                              [replace_tree('MOExpr', MOExpr),
                               replace_term('Tr', TranslFn)]),
             {FName, Presence, TrInitExpr, TrMOExpr}
         end
         || {FName, Presence, InitExpr, MOExpr} <- ExprInfos1],
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    [Expr || {_FName, _, Expr, _MOExpr} <- ExprInfos2];
                {maps, omitted} ->
                    [MapsOmittedExpr
                     || {_FName, _, _Expr, MapsOmittedExpr} <- ExprInfos2]
            end;
        pass_as_record ->
            case get_mapping_and_unset_by_opts(Opts) of
                records ->
                    [record_create(
                       MsgName,
                       [{FName, Expr} || {FName, m, Expr, _} <- ExprInfos2])];
                {maps, present_undefined} ->
                    [map_create(
                       [{FName, Expr} || {FName, _, Expr, _} <- ExprInfos2])];
                {maps, omitted} ->
                    [map_create(
                       [{FName, Expr} || {FName, m, Expr, _} <- ExprInfos2])]
            end
    end.

decoder_read_field_params(MsgName, MsgDef, AnRes, Opts) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            Params = decoder_params(MsgName, AnRes),
            {Params, Params, []};
        pass_as_record ->
            %% Maps currently don't support single value access, ie: M#{f},
            %% so when passing as records/maps, in the end, we must reverse
            %% repeated fields to get a linear amortized cost of
            %% reading/adding elements)
            %%
            %% So instead of generating code that looks
            %% like below for the maps case (similar for records):
            %%
            %%    d_read_field_m_f(<<>>, _, _, M) ->
            %%      M#{f1 = lists:reverse(M#{f1})
            %%
            %% we generate code like this:
            %%
            %%    d_read_field_m_f(<<>>, _, _, #{f1 := F1}=M) ->
            %%      M#{f1 := lists:reverse(F1)
            %%
            %% Here we must provide enough info to generate
            %% the finalizing code (ie: the function body in the example above)
            %%
            Params = decoder_params(MsgName, AnRes),
            MappingVar = hd(Params),
            FFields = [{FName, var_n("R", I)}
                       || {I,FName} <- index_seq(repeated_field_names(MsgDef))],
            FMatch = mapping_match(MsgName, FFields, Opts),
            FParam = ?expr(matching = '<Var>',
                           [replace_tree(matching, FMatch),
                            replace_tree('<Var>', MappingVar)]),
            {Params, [FParam], FFields}
    end.

repeated_field_names(MsgDef) ->
    [FName || #?gpb_field{name=FName, occurrence=repeated} <- MsgDef].

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
    map_msgdef_fields_o(
      fun(#?gpb_field{name=FName, fnum=FNum, type=Type}=FieldDef, _IsOneof) ->
              Wiretype = case is_packed(FieldDef) of
                             true  -> gpb:encode_wiretype(bytes);
                             false -> gpb:encode_wiretype(Type)
                         end,
              Selector = (FNum bsl 3) bor Wiretype,
              DecodeFn = mk_fn(d_field_, MsgName, FName),
              {Selector, DecodeFn}
      end,
      MsgDef).

decoder_finalize_result(Params, FFields, MsgName, MsgDef, AnRes, Opts) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    decoder_finalize_params_all_present(Params, MsgName, MsgDef,
                                                        AnRes, Opts);
                {maps, omitted} ->
                    decoder_finalize_params_opt_omitted(Params, MsgName, MsgDef,
                                                        AnRes, Opts)
            end;
        pass_as_record ->
            MsgVar = hd(Params),
            [mapping_update(
               MsgVar,
               MsgName,
               [begin
                    ElemPath = [MsgName, FName],
                    Finalizer = find_translation(ElemPath,
                                                 decode_repeated_finalize,
                                                 AnRes),
                    FValueExpr = ?expr('lists:reverse'('<FVar>'),
                                       [replace_term('lists:reverse',Finalizer),
                                        replace_tree('<FVar>', FVar)]),
                    {FName, FValueExpr}
                end
                || {FName, FVar} <- FFields],
               Opts)]
    end.

decoder_finalize_params_all_present(Params, MsgName, MsgDef, AnRes, Opts) ->
    [mapping_create(
       MsgName,
       [decoder_finalize_param_for_mapping(Field, Param, MsgName, AnRes)
        || {Field, Param} <- lists:zip(MsgDef, Params)],
       Opts)].

decoder_finalize_params_opt_omitted(Params, MsgName, MsgDef, AnRes, _Opts) ->
    {Optionals, NonOptionals} = key_partition_on_optionality(
                                  1, lists:zip(MsgDef, Params)),
    NonOptionalsMap = map_create(
                        [decoder_finalize_param_for_mapping(
                           Field, Param, MsgName, AnRes)
                         || {Field, Param} <- NonOptionals]),
    do_exprs(fun({Field, Param}, Var) ->
                     FV = decoder_finalize_param_for_mapping(
                            Field, Param, MsgName, AnRes),
                     ?expr(if 'Param' == '$undef' -> 'Var';
                              true -> 'Var#{field => Param}'
                           end,
                           [replace_tree('Param', Param),
                            replace_tree('Var', Var),
                            replace_tree('Var#{field => Param}',
                                         map_set(Var, [FV]))])
             end,
             NonOptionalsMap,
             Optionals).

decoder_finalize_param_for_mapping(Field, Param, MsgName, AnRes) ->
    FName = get_field_name(Field),
    ElemPath = [MsgName, FName],
    Finalizer = find_translation(ElemPath, decode_repeated_finalize, AnRes),
    FValueExpr = case get_field_occurrence(Field) of
                     required -> Param;
                     optional -> Param;
                     repeated -> ?expr('lists:reverse'('Param'),
                                       [replace_term('lists:reverse',Finalizer),
                                        replace_tree('Param', Param)])
                 end,
    {FName, FValueExpr}.

format_field_decoders(MsgName, MsgDef, AnRes, Opts) ->
    map_msgdef_fields_o(
      fun(Field, IsOneof) ->
              [format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts), "\n"]
      end,
      MsgDef).

format_field_decoder(MsgName, Field, IsOneof, AnRes, Opts) ->
    case is_packed(Field) of
        false ->
            XField = {Field, IsOneof},
            format_non_packed_field_decoder(MsgName, XField, AnRes, Opts);
        true ->
            %% a packed field can never be one of a `oneof' fields
            format_packed_field_decoder(MsgName, Field, AnRes, Opts)
    end.

format_non_packed_field_decoder(MsgName, XField, AnRes, Opts) ->
    {#?gpb_field{type=Type}, _IsOneof} = XField,
    case Type of
        sint32   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        sint64   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        int32    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        int64    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        uint32   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        uint64   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bool     -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {enum,_} -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        fixed32  -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        sfixed32 -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        float    -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        fixed64  -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        sfixed64 -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        double   -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        string   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {map,_,_}-> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts)
    end.

format_packed_field_decoder(MsgName, FieldDef, AnRes, Opts) ->
    #?gpb_field{name=FName, rnum=RNum} = FieldDef,
    Params = decoder_params(MsgName, AnRes),
    InParams = case get_field_pass(MsgName, AnRes) of
                   pass_as_params ->
                       Params;
                   pass_as_record ->
                       MMatch = mapping_match(MsgName, [{FName, ?expr(E)}],
                                              Opts),
                       [?expr(matching = '<Var>',
                              [replace_tree(matching, MMatch),
                               replace_tree('<Var>', hd(Params))])]
               end,
    Param = case get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    lists:nth(RNum - 1, Params);
                pass_as_record ->
                    ?expr(E)
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
        splice_trees('<InParams>', InParams),
        replace_term(decode_packed, mk_fn(d_packed_field_, MsgName, FName)),
        replace_tree('<Param>', Param),
        replace_term('<call-read-field>', mk_fn(dfp_read_field_def_, MsgName)),
        splice_trees('<OutParams>', OutParams)]),
     "\n",
     format_packed_field_seq_decoder(MsgName, FieldDef, Opts)].

format_packed_field_seq_decoder(MsgName, #?gpb_field{type=Type}=Field, Opts) ->
    case Type of
        fixed32  -> format_dpacked_nonvi(MsgName, Field, 32, [little]);
        sfixed32 -> format_dpacked_nonvi(MsgName, Field, 32, [little,signed]);
        float    -> format_dpacked_nonvi(MsgName, Field, 32, [little,float]);
        fixed64  -> format_dpacked_nonvi(MsgName, Field, 64, [little]);
        sfixed64 -> format_dpacked_nonvi(MsgName, Field, 64, [little,signed]);
        double   -> format_dpacked_nonvi(MsgName, Field, 64, [little,float]);
        _        -> format_dpacked_vi(MsgName, Field, Opts)
    end.

format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, BitLen, BitTypes) ->
    gpb_codegen:format_fn(
      mk_fn(d_packed_field_, MsgName, FName),
      fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [Value | AccSeq]);
         (<<>>, _, _, AccSeq) ->
              AccSeq
      end,
      [replace_term('<N>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes])]).

format_dpacked_vi(MsgName, #?gpb_field{name=FName}=FieldDef, Opts) ->
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

format_vi_based_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName}=FieldDef, _IsOneof}=XFieldDef,
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest}]),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef, AnRes,
                                              Opts),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                ReadFieldDefFn = mk_fn(dfp_read_field_def_, MsgName),
                Params2 = updated_merged_params(MsgName, XFieldDef, AnRes,
                                                FVar, PrevValue, Params, Opts),
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
decode_int_value(ResVar, Bindings, #?gpb_field{type=Type}=F, Opts, TailFn) ->
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
                          '<Res>' = 'd_msg_X'(Bs),
                          [replace_tree('<Value>', Value),
                           replace_tree('<Rest>', Rest),
                           replace_tree('<Res>', ResVar),
                           replace_term('d_msg_X', mk_fn(d_msg_, Msg2Name))]),
                   Rest2);
        {map, KeyType, ValueType} ->
            MapAsMsgMame = map_type_to_msg_name(KeyType, ValueType),
            F2 = F#?gpb_field{type={msg,MapAsMsgMame}},
            decode_int_value(ResVar, Bindings, F2, Opts, TailFn)
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

updated_merged_params(MsgName, XFieldDef, AnRes, NewValue, PrevValue,
                      Params, Opts) ->
    case {get_field_pass(MsgName, AnRes), XFieldDef} of
        {pass_as_params, {#?gpb_field{rnum=RNum}, _IsOneof}} ->
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, AnRes, Opts),
            lists_setelement(RNum - 1, Params, MergedValue);
        {pass_as_record, {#?gpb_field{name=FName}, false}} ->
            MsgVar = hd(Params),
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, AnRes, Opts),
            [mapping_update(MsgVar, MsgName, [{FName, MergedValue}], Opts)];
        {pass_as_record, {_OField, {true, CFName}}} ->
            MsgVar = hd(Params),
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, AnRes, Opts),
            [mapping_update(MsgVar, MsgName, [{CFName, MergedValue}], Opts)]
    end.

merge_field_expr({FieldDef, false}, PrevValue, NewValue, MsgName, AnRes, Opts) ->
    case classify_field_merge_action(FieldDef) of
        overwrite ->
            NewValue;
        seqadd ->
            ElemPath = [MsgName, get_field_name(FieldDef)],
            Cons = find_translation(ElemPath, decode_repeated_add_elem, AnRes),
            ?expr('[New|Acc]'('<New>', '<Acc>'),
                  [replace_term('[New|Acc]', Cons),
                   replace_tree('<New>', NewValue),
                   replace_tree('<Acc>', PrevValue)]);
        msgmerge ->
            #?gpb_field{type={msg,FMsgName}} = FieldDef,
            MergeFn = mk_fn(merge_msg_, FMsgName),
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if 'Prev' == undefined -> 'New';
                             true -> 'merge_msg_X'('Prev', 'New')
                          end,
                          [replace_term('merge_msg_X', MergeFn),
                           replace_tree('Prev', PrevValue),
                           replace_tree('New', NewValue)]);
                {maps, omitted} ->
                    case get_field_pass(MsgName, AnRes) of
                        pass_as_params ->
                            ?expr(if 'Prev' =:= '$undef' -> 'New';
                                     true -> 'merge_msg_X'('Prev', 'New')
                                  end,
                                  [replace_tree('Prev', PrevValue),
                                   replace_term('merge_msg_X', MergeFn),
                                   replace_tree('New', NewValue)]);
                        pass_as_record ->
                            ?expr(case maps:find('fieldname', 'Msg') of
                                      error -> 'New';
                                      {ok, Prev} -> 'merge_msg_X'(Prev, 'New')
                                  end,
                                  [replace_term('fieldname',
                                                get_field_name(FieldDef)),
                                   replace_tree('Msg', PrevValue),
                                   replace_term('merge_msg_X', MergeFn),
                                   replace_tree('New', NewValue)])
                    end
            end
    end;
merge_field_expr({FieldDef, {true, CFName}}, PrevValue, NewValue,
                 MsgName, AnRes, Opts)->
    #?gpb_field{name=FName, type=Type} = FieldDef,
    case Type of
        {msg, FMsgName} ->
            MergeFn = mk_fn(merge_msg_, FMsgName),
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    MVPrev = prefix_var("MV", PrevValue),
                    ?expr(case 'Prev' of
                              undefined ->
                                  {'tag', 'New'};
                              {'tag', 'MVPrev'} ->
                                  {'tag', 'merge_msg_X'('MVPrev', 'New')};
                              _ ->
                                  {'tag', 'New'}
                          end,
                          [replace_tree('Prev', PrevValue),
                           replace_term('tag', FName),
                           replace_tree('New', NewValue),
                           replace_term('merge_msg_X', MergeFn),
                           replace_tree('MVPrev', MVPrev)]);
                {maps, omitted} ->
                    MsgVar = PrevValue,
                    case get_field_pass(MsgName, AnRes) of
                        pass_as_params ->
                            ?expr(case 'Prev' of
                                      '$undef' ->
                                          {'tag', 'New'};
                                      {'tag', MVPrev} ->
                                          {'tag', 'merge_msg_X'(MVPrev, 'New')};
                                      _ ->
                                          {'tag', 'New'}
                                  end,
                                  [replace_term('tag', FName),
                                   replace_tree('Prev', PrevValue),
                                   replace_term('merge_msg_X', MergeFn),
                                   replace_tree('New', NewValue)]);
                        pass_as_record ->
                            ?expr(case maps:find('fieldname', 'Msg') of
                                      error ->
                                          {'tag', 'New'};
                                      {ok, {'tag', MVPrev}} ->
                                          {'tag', 'merge_msg_X'(MVPrev, 'New')};
                                      _ ->
                                          {'tag', 'New'}
                                  end,
                                  [replace_term('fieldname', CFName),
                                   replace_term('tag', FName),
                                   replace_tree('Msg', MsgVar),
                                   replace_term('merge_msg_X', MergeFn),
                                   replace_tree('New', NewValue)])
                    end
            end;
        _ ->
            %% Replace
            ?expr({'fieldname', '<expr>'},
                  [replace_term('fieldname', FName),
                   replace_tree('<expr>', NewValue)])
    end.

decoder_in_params(Params, MsgName, {FieldDef, false}, AnRes, Opts) ->
    #?gpb_field{name=FName}=FieldDef,
    Any = ?expr(_),
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            #?gpb_field{rnum=RNum} = FieldDef,
            Prev = lists:nth(RNum-1, Params),
            case classify_field_merge_action(FieldDef) of
                overwrite -> {lists_setelement(RNum-1, Params, Any), Any};
                seqadd    -> {Params, Prev};
                msgmerge  -> {Params, Prev}
            end;
        pass_as_record ->
            Prev = ?expr(Prev),
            InParams = [match_bind_var(
                          mapping_match(MsgName, [{FName, Prev}], Opts),
                          hd(Params))],
            case classify_field_merge_action(FieldDef) of
                overwrite -> {Params, Any};
                seqadd    -> {InParams, Prev};
                msgmerge  ->
                    case get_mapping_and_unset_by_opts(Opts) of
                        X when X == records;
                               X == {maps, present_undefined} ->
                            {InParams, Prev};
                        {maps, omitted} ->
                            MsgVar = hd(Params),
                            {[MsgVar], MsgVar}
                    end
            end
    end;
decoder_in_params(Params, MsgName, {FieldDef, {true, CFName}}, AnRes, Opts) ->
    #?gpb_field{type=Type, rnum=RNum} = FieldDef,
    case Type of
        {msg, _} ->
            %% oneof fields that of message type may need merging
            case get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    Prev = lists:nth(RNum-1, Params),
                    {Params, Prev};
                pass_as_record ->
                    case get_mapping_and_unset_by_opts(Opts) of
                        X when X == records;
                               X == {maps, present_undefined} ->
                            Prev = ?expr(Prev),
                            InParams = [match_bind_var(
                                          mapping_match(MsgName,
                                                        [{CFName, Prev}],
                                                        Opts),
                                          hd(Params))],
                            {InParams, Prev};
                        {maps, omitted} ->
                            MsgVar = hd(Params),
                            {[MsgVar], MsgVar}
                    end
            end;
        _ ->
            %% Non-messages, treat as an optional field
            Any = ?expr(_),
            case get_field_pass(MsgName, AnRes) of
                pass_as_params ->
                    {lists_setelement(RNum-1, Params, Any), Any};
                pass_as_record ->
                    {Params, Any}
            end
    end.

format_fixlen_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName, type=Type}, _IsOneof} = XFieldDef,
    {BitLen, BitTypes} = case Type of
                             fixed32  -> {32, [little]};
                             sfixed32 -> {32, [little,signed]};
                             float    -> {32, [little,float]};
                             fixed64  -> {64, [little]};
                             sfixed64 -> {64, [little,signed]};
                             double   -> {64, [little,float]}
                         end,
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef, AnRes,
                                              Opts),
    Value = ?expr(Value),
    Params2 = updated_merged_params(MsgName, XFieldDef, AnRes,
                                    Value, PrevValue, Params,
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
        #?gpb_field{occurrence=required, type={msg, _}} -> msgmerge;
        #?gpb_field{occurrence=optional, type={msg, _}} -> msgmerge;
        #?gpb_field{occurrence=required}                -> overwrite;
        #?gpb_field{occurrence=optional}                -> overwrite;
        #?gpb_field{occurrence=repeated}                -> seqadd
    end.

format_msg_merge_code(Defs, AnRes, Opts) ->
    case contains_messages(Defs) of
        true  -> format_msg_merge_code_msgs(Defs, AnRes, Opts);
        false -> format_msg_merge_code_no_msgs(Opts)
    end.

format_msg_merge_code_no_msgs(Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            gpb_codegen:format_fn(
              merge_msgs,
              fun(_Prev, _New) ->
                      erlang:error({gpb_error, no_messages})
              end);
        maps ->
            gpb_codegen:format_fn(
              merge_msgs,
              fun(_Prev, _New, _MsgName) ->
                      erlang:error({gpb_error, no_messages})
              end)
    end.

format_msg_merge_code_msgs(Defs, AnRes, Opts) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgDef} <- Defs],
    [format_merge_msgs_top_level(MsgNames, Opts),
     [format_msg_merger(MsgName, MsgDef, AnRes, Opts)
      || {{msg, MsgName}, MsgDef} <- Defs]].

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
    MsgUndefFnClauses = compute_msg_merger_undef_clauses(MsgName, AnRes, Opts),
    {PrevMatch, NewMatch, ExtraInfo} =
        format_msg_merger_fnclause_match(MsgName, MsgDef, Opts),
    {MandatoryMergings, OptMergings} = compute_msg_field_mergers(
                                         ExtraInfo, MsgName, AnRes),
    gpb_codegen:format_fn(
      mk_fn(merge_msg_, MsgName),
      fun('<msg-undefined-handling>', _) -> '<return-the-defined-msg>';
         ('Prev', 'New')                 -> '<merge-it>'
      end,
      [replace_tree('Prev',  PrevMatch),
       replace_tree('New',   NewMatch),
       splice_trees(
         '<merge-it>',
         do_exprs(fun render_omissible_merger/2,
                  render_field_mergers(MsgName, MandatoryMergings, Opts),
                  OptMergings)),
       splice_clauses('<msg-undefined-handling>', MsgUndefFnClauses)]).

compute_msg_merger_undef_clauses(MsgName, AnRes, Opts) ->
    case occurs_as_optional_submsg(MsgName, AnRes) of
        false ->
            %% If it cannot occur as optional sub message,
            %% then it cannot be called with either Prev or New being
            %% undefined. Don't layout code for it. Eg. Dialyzer will
            %% find it and complain.
            [];
        true ->
            case get_mapping_and_unset_by_opts(Opts) of
                Y when Y == records;
                       Y == {maps, present_undefined} ->
                    [?fn_clause(fun(Prev, undefined) -> Prev end),
                     ?fn_clause(fun(undefined, New)  -> New end)];
                {maps, omitted} ->
                    %% Similar to above: The entire map is always
                    %% matched to a variable.
                    %% FIXME: also if all field are non-optionals??
                    []
            end
    end.

format_msg_merger_fnclause_match(_MsgName, [], _Opts) ->
    {?expr(PF), ?expr(_), no_fields};
format_msg_merger_fnclause_match(MsgName, MsgDef, Opts) ->
    FNames  = [get_field_name(Field) || Field <- MsgDef],
    PFVars  = [var("PF~s", [FName]) || FName <- FNames],
    NFVars  = [var("NF~s", [FName]) || FName <- FNames],
    PFields = lists:zip(FNames, PFVars),
    NFields = lists:zip(FNames, NFVars),
    Infos = zip4(FNames, PFVars, NFVars, MsgDef),
    case get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            P = mapping_match(MsgName, PFields, Opts),
            N = mapping_match(MsgName, NFields, Opts),
            {P, N, {pr, Infos}};
        {maps, omitted} ->
            {OptInfos, MandInfos} = key_partition_on_optionality(4, Infos),
            PMsg = ?expr(PMsg),
            NMsg = ?expr(NMsg),
            P = map_match([{FName, PFVar} || {FName,PFVar,_,_} <- MandInfos]),
            N = map_match([{FName, NFVar} || {FName,_,NFVar,_} <- MandInfos]),
            PB = match_bind_var(P, PMsg),
            NB = match_bind_var(N, NMsg),
            XInfo = {om, {MandInfos, OptInfos, PMsg, NMsg}},
            case {MandInfos, OptInfos} of
                {_, []} -> {P, N, XInfo};
                {[], _} -> {PMsg, NMsg, XInfo};
                {_,  _} -> {PB, NB, XInfo}
            end
    end.

compute_msg_field_mergers({pr, XInfo}, MsgName, AnRes) ->
    Merges =
        [{FName, format_field_merge_expr(Field, PFVar, NFVar, MsgName, AnRes)}
         || {FName, PFVar, NFVar, Field} <- XInfo],
    {Merges, []};
compute_msg_field_mergers({om, {MandXInfo, OptXInfo, PMsg, NMsg}},
                          MsgName, AnRes) ->
    {MandMergs, []} = compute_msg_field_mergers({pr, MandXInfo},
                                                MsgName, AnRes),
    {OptMergs, []} = compute_msg_field_mergers({pr, OptXInfo},
                                               MsgName, AnRes),
    {MandMergs, reshape_cases_for_maps_find(OptMergs, PMsg, NMsg)}.

format_field_merge_expr(#?gpb_field{name=FName}=Field, PF, NF, MsgName, AnRes)->
    Transforms = [replace_tree('PF', PF),
                  replace_tree('NF', NF)],
    case classify_field_merge_action(Field) of
        overwrite ->
            {overwrite, {PF, NF}};
        seqadd ->
            ElemPath = [MsgName, FName],
            Append = find_translation(ElemPath, merge, AnRes, 'erlang_++'),
            {expr, ?expr('PF++NF'('PF', 'NF'),
                         Transforms ++ [replace_term('PF++NF',Append)])};
        msgmerge ->
            #?gpb_field{type={msg,SubMsgName}}=Field,
            {merge, {{PF, NF}, mk_fn(merge_msg_, SubMsgName)}}
    end;
format_field_merge_expr(#gpb_oneof{fields=OFields}, PF, NF, _MsgName, _AnRes) ->
    case [OField || #?gpb_field{type={msg,_}}=OField <- OFields] of
        [] ->
            {overwrite, {PF, NF}};
        MOFields ->
            {oneof,
             {{PF, NF},
              [{OFName, mk_fn(merge_msg_, M2Name)}
               || #?gpb_field{name=OFName, type={msg,M2Name}} <- MOFields]}}
    end.

reshape_cases_for_maps_find(Merges, PMsg, NMsg) ->
    [{FName, case Merge of
                 {overwrite, {_, _}} ->
                     {overwrite, {PMsg, NMsg}};
                 {merge, {{_, _}, MergeFn}} ->
                     {merge, {{PMsg, NMsg}, MergeFn}};
                 {oneof, {{_, _}, OFMerges}} ->
                     {oneof, {{PMsg, NMsg}, OFMerges}}
             end}
     || {FName, Merge} <- Merges].

render_field_mergers(MsgName, Mergings, Opts) ->
    Fields = [{FName, render_field_merger(Merge)}
              || {FName, Merge} <- Mergings],
    mapping_create(MsgName, Fields, Opts).

render_field_merger({overwrite, {PF, NF}}) ->
    ?expr(if 'NF' =:= undefined -> 'PF';
             true               -> 'NF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF)]);
render_field_merger({expr, Expr}) ->
    Expr;
render_field_merger({merge, {{PF, NF}, MergeFn}}) ->
    ?expr('merge'('PF', 'NF'),
          [replace_tree('PF', PF),
           replace_tree('NF', NF),
           replace_term('merge', MergeFn)]);
render_field_merger({oneof, {{PF, NF}, OFMerges}}) ->
    Transforms = [replace_tree('PF', PF),
                  replace_tree('NF', NF),
                  replace_tree('OPF', prefix_var("O", PF)),
                  replace_tree('ONF', prefix_var("O", NF))],
    ?expr(case {'PF', 'NF'} of
              '{{tag,OPF},{tag,ONF}}' -> {'tag', 'merge'('OPF','ONF')};
              {_, undefined}          -> 'PF';
              _                       -> 'NF'
          end,
          [repeat_clauses(
             '{{tag,OPF},{tag,ONF}}',
             [[replace_tree('{{tag,OPF},{tag,ONF}}',
                            ?expr({{'tag','OPF'},{'tag','ONF'}})),
               replace_term('tag', OFName),
               replace_term('merge', OFMergeFn) | Transforms]
              || {OFName, OFMergeFn} <- OFMerges])
           | Transforms]).

render_omissible_merger({FName, {overwrite, {PMsg, NMsg}}}, Var) ->
    ?expr(case {maps:find('fname', 'PMsg'), maps:find('fname', 'NMsg')} of
              {error, error}  -> 'Var';
              {_, {ok, 'NF'}} -> 'Var#{fname=>NF}';
              {{ok, 'PF'}, _} -> 'Var#{fname=>PF}'
          end,
          std_omitable_merge_transforms(PMsg, NMsg, FName, Var));
render_omissible_merger({FName, {merge, {{PMsg, NMsg}, MergeFn}}}, Var) ->
    Trs = std_omitable_merge_transforms(PMsg, NMsg, FName, Var),
    MergeCall = ?expr('merge'('PF','NF'), [replace_term(merge,MergeFn) | Trs]),
    ?expr(case {maps:find('fname', 'PMsg'), maps:find('fname', 'NMsg')} of
              {error, error}           -> 'Var';
              {error, {ok, 'NF'}}      -> 'Var#{fname=>NF}';
              {{ok, 'PF'}, error}      -> 'Var#{fname=>PF}';
              {{ok, 'PF'}, {ok, 'NF'}} -> 'Var#{fname=>merge(PF,NF)}'
          end,
          [replace_term('merge', MergeFn),
           replace_tree('Var#{fname=>merge(PF,NF)}',
                        map_set(Var, [{FName,MergeCall}]))
           | Trs]);
render_omissible_merger({FName, {oneof, {{PMsg, NMsg}, OFMerges}}}, Var) ->
    OneofTransforms = [replace_tree('OPF', var("OPF~s", [FName])),
                       replace_tree('ONF', var("ONF~s", [FName]))],
    ?expr(case {maps:find('fname', 'PMsg'), maps:find('fname', 'NMsg')} of
              {error, error} ->
                  'Var';
              '{{ok,{tag,OPF}},{ok,{tag,ONF}}}' ->
                  'Var#{fname=>{tag,merge(OPF,ONF)}}';
              {_, {ok, 'NF'}} ->
                  'Var#{fname=>NF}';
              {{ok, 'PF'}, _} ->
                  'Var#{fname=>PF}'
          end,
          [repeat_clauses(
             '{{ok,{tag,OPF}},{ok,{tag,ONF}}}',
             [begin
                  Trs2 = [replace_term('tag', OFName),
                          replace_term('merge', OFMergeFn)] ++ OneofTransforms,
                  MergeCall = ?expr({'tag','merge'('OPF','ONF')}, Trs2),
                  [replace_tree('{{ok,{tag,OPF}},{ok,{tag,ONF}}}',
                                ?expr({{ok,{'tag','OPF'}},{ok,{'tag','ONF'}}})),
                   replace_tree('Var#{fname=>{tag,merge(OPF,ONF)}}',
                                map_set(Var, [{FName,MergeCall}]))
                   | Trs2]
              end
              || {OFName, OFMergeFn} <- OFMerges])
           | std_omitable_merge_transforms(PMsg, NMsg, FName, Var)]).

std_omitable_merge_transforms(PMsg, NMsg, FName, Var) ->
    PF = var("PF~s", [FName]),
    NF = var("NF~s", [FName]),
    [replace_term('fname', FName),
     replace_tree('PMsg', PMsg),
     replace_tree('NMsg', NMsg),
     replace_tree('PF', PF),
     replace_tree('NF', NF),
     replace_tree('Var', Var),
     replace_tree('Var#{fname=>NF}', map_set(Var, [{FName, NF}])),
     replace_tree('Var#{fname=>PF}', map_set(Var, [{FName, PF}]))].

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
    [format_msg_nif_decode_error_wrapper(MsgName)
     || {{msg, MsgName}, _MsgDef} <- Defs].

format_msg_nif_decode_error_wrapper(MsgName) ->
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
     format_map_verifiers(AnRes, Opts),
     format_verifier_auxiliaries()
    ].

format_msg_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifier(MsgName, MsgDef, AnRes, Opts)
     || {{msg,MsgName}, MsgDef} <- Defs].

format_msg_verifier(MsgName, MsgDef, AnRes, Opts) ->
    FNames = get_field_names(MsgDef),
    FVars = [var_f_n(I) || I <- lists:seq(1, length(FNames))],
    MsgVar = ?expr(M),
    FieldMatching =
        case get_mapping_and_unset_by_opts(Opts) of
            X when X == records;
                   X == {maps, present_undefined} ->
                mapping_match(MsgName, lists:zip(FNames, FVars), Opts);
            {maps, omitted} ->
                FMap = zip_for_non_opt_fields(MsgDef, FVars),
                if length(FMap) == length(FNames) ->
                        map_match(FMap);
                   length(FMap) < length(FNames) ->
                        ?expr('mapmatch' = 'M',
                              [replace_tree('mapmatch', map_match(FMap)),
                               replace_tree('M', MsgVar)])
                end
        end,
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
      [replace_tree('<msg-match>', FieldMatching),
       replace_tree('<Path>', if MsgDef == [] -> ?expr(_Path);
                                 MsgDef /= [] -> ?expr(Path)
                              end),
       splice_trees('<verify-fields>',
                    field_verifiers(MsgDef, FVars, MsgVar, Opts)),
       repeat_clauses('<X>', case NeedsMatchOther of
                                 true  -> [[replace_tree('<X>', ?expr(X))]];
                                 false -> [] %% omit the else clause
                             end),
       replace_term('<MsgName>', MsgName)]).

field_verifiers(Fields, FVars, MsgVar, Opts) ->
    [field_verifier(Field, FVar, MsgVar, Opts)
     || {Field, FVar} <- lists:zip(Fields, FVars)].

field_verifier(#?gpb_field{name=FName, type=Type, occurrence=Occurrence},
               FVar, MsgVar, Opts) ->
    FVerifierFn = case Type of
                      {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                      {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                      {map,KT,VT}     -> mk_fn(v_, map_type_to_msg_name(KT,VT));
                      Type            -> mk_fn(v_type_, Type)
                  end,
    Replacements = [replace_term('<verify-fn>', FVerifierFn),
                    replace_tree('<F>', FVar),
                    replace_term('<FName>', FName),
                    replace_term('<Type>', Type)],
    IsMapField = case Type of
                     {map,_,_} -> true;
                     _ -> false
                 end,
    case Occurrence of
        required ->
            %% FIXME: check especially for `undefined'
            %% and if found, error out with required_field_not_set
            %% specifying expected type
            ?expr('<verify-fn>'('<F>', ['<FName>' | Path]),
                  Replacements);
        repeated when not IsMapField ->
            ?expr(if is_list('<F>') ->
                          ['<verify-fn>'(Elem, ['<FName>' | Path])
                           || Elem <- '<F>'];
                     true ->
                          mk_type_error(
                            {invalid_list_of, '<Type>'}, '<F>', Path)
                  end,
                  Replacements);
        repeated when IsMapField ->
            ?expr('<verify-fn>'('<F>', ['<FName>' | Path]),
                  Replacements);
        optional ->
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path])
                          end,
                          Replacements);
                {maps, omitted} ->
                    ?expr(case maps:find('<FName>', 'M') of
                              error ->
                                  ok;
                              {ok, '<F>'} ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path])
                          end,
                          [replace_tree('M', MsgVar) | Replacements])
            end
    end;
field_verifier(#gpb_oneof{name=FName, fields=OFields}, FVar, MsgVar, Opts) ->
    case get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            ?expr(
               case '<F>' of
                   undefined ->
                       ok;
                   '<oneof-pattern>' ->
                       '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path]);
                   _ ->
                       mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path])
               end,
               [replace_tree('<F>', FVar),
                replace_term('<FName>', FName),
                repeat_clauses(
                  '<oneof-pattern>',
                  [begin
                       FVerifierFn =
                           case Type of
                               {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                               {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                               Type            -> mk_fn(v_type_, Type)
                           end,
                       OFVar = prefix_var("O", FVar),
                       [replace_tree('M', MsgVar),
                        replace_tree('<oneof-pattern>',
                                     ?expr({'<OFName>','<OFVar>'})),
                        replace_term('<verify-fn>', FVerifierFn),
                        replace_tree('<OFVar>', OFVar),
                        replace_term('<OFName>', OFName)]
                   end
                   || #?gpb_field{name=OFName, type=Type} <- OFields])]);
        {maps, omitted} ->
            ?expr(
               case maps:find('<FName>', 'M') of
                   error ->
                       ok;
                   '<oneof-pattern>' ->
                       '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path]);
                   {ok, '<F>'} ->
                       mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path])
               end,
               [replace_tree('<F>', FVar),
                replace_term('<FName>', FName),
                replace_tree('M', MsgVar),
                repeat_clauses(
                  '<oneof-pattern>',
                  [begin
                       FVerifierFn =
                           case Type of
                               {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                               {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                               Type            -> mk_fn(v_type_, Type)
                           end,
                       OFVar = prefix_var("O", FVar),
                       [replace_tree('<oneof-pattern>',
                                     ?expr({ok, {'<OFName>','<OFVar>'}})),
                        replace_term('<verify-fn>', FVerifierFn),
                        replace_tree('<OFVar>', OFVar),
                        replace_term('<OFName>', OFName)]
                   end
                   || #?gpb_field{name=OFName, type=Type} <- OFields])])
    end.


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
      fun(S, Path) when is_list(S); is_binary(S) ->
              try unicode:characters_to_binary(S) of
                  B when is_binary(B) ->
                      ok;
                  {error, _, _} -> %% a non-UTF-8 binary
                      mk_type_error(bad_unicode_string, S, Path)
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

format_map_verifiers(#anres{map_types=MapTypes}, Opts) ->
    RecordsOrMaps = get_records_or_maps_by_opts(Opts),
    [format_map_verifier(KeyType, ValueType, RecordsOrMaps)
     || {KeyType,ValueType} <- sets:to_list(MapTypes)].

format_map_verifier(KeyType, ValueType, RecordsOrMaps) ->
    FnName = mk_fn(v_, map_type_to_msg_name(KeyType, ValueType)),
    KeyVerifierFn = mk_fn(v_type_, KeyType),
    ValueVerifierFn = case ValueType of
                          {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                          {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                          Type            -> mk_fn(v_type_, Type)
                      end,
    case RecordsOrMaps of
        records ->
            gpb_codegen:format_fn(
              FnName,
              fun(KVs, Path) when is_list(KVs) ->
                      [case X of
                           {Key, Value} ->
                               'VerifyKey'(Key, ['key' | Path]),
                               'VerifyValue'(Value, ['value' | Path]);
                           _ ->
                               mk_type_error(invalid_key_value_tuple, X, Path)
                       end
                       || X <- KVs],
                      ok;
                 (X, Path) ->
                      mk_type_error(invalid_list_of_key_value_tuples, X, Path)
              end,
              [replace_term('VerifyKey', KeyVerifierFn),
               replace_term('VerifyValue', ValueVerifierFn)]);
        maps ->
            gpb_codegen:format_fn(
              FnName,
              fun(M, Path) when is_map(M) ->
                      [begin
                           'VerifyKey'(Key, ['key' | Path]),
                           'VerifyValue'(Value, ['value' | Path])
                       end
                       || {Key, Value} <- maps:to_list(M)],
                      ok;
                 (X, Path) ->
                      mk_type_error(invalid_map, X, Path)
              end,
              [replace_term('VerifyKey', KeyVerifierFn),
               replace_term('VerifyValue', ValueVerifierFn)])
    end.

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

%% -- translators ------------------------------------------------------

format_translators(_Defs, #anres{map_translations=Ts}=AnRes, Opts) ->
    [[[format_field_op_translator(ElemPath, Op, Fn, ArgTemplate)
       || {Op, {Fn, ArgTemplate}} <- OpTransls]
      || {ElemPath, OpTransls} <- dict:to_list(Ts)],
     format_default_translators(AnRes, Opts)].

format_merge_translators(_Defs, #anres{map_translations=Ts}=AnRes, Opts) ->
    [[[format_field_op_translator(ElemPath, Op, Fn, ArgTemplate)
       || {Op, {Fn, ArgTemplate}} <- OpTransls,
          Op == merge]
      || {ElemPath, OpTransls} <- dict:to_list(Ts)],
     format_default_merge_translators(AnRes, Opts)].

format_field_op_translator(ElemPath, Op, Fn, ArgTemplate) ->
    FnName = mk_tr_fn_name(ElemPath, Op),
    InArgs = underscore_unused_args(args_by_op(Op), ArgTemplate),
    TrArgs = [case Term of
                  '$1' -> lists:nth(1, InArgs);
                  '$2' -> lists:nth(2, InArgs);
                  _ -> erl_parse:abstract(Term)
              end
              || Term <- ArgTemplate],
    [inline_attr(FnName,length(InArgs)),
     gpb_codegen:format_fn(
       FnName,
       fun('InArgs') ->
               'Fn'('TrArgs')
       end,
       [replace_term('Fn', Fn),
        splice_trees('InArgs', InArgs),
        splice_trees('TrArgs', TrArgs)])].

underscore_unused_args(Args, Templ) ->
    [case is_arg_used(I, Templ) of
         true  -> Arg;
         false -> ?expr(_)
     end
     || {I, Arg} <- index_seq(Args)].

is_arg_used(I, Templ) ->
    lists:member(dollar_i(I), Templ).

dollar_i(I) ->
    list_to_atom(?ff("$~w", [I])).

args_by_op(encode)                   -> [?expr(X)];
args_by_op(decode)                   -> [?expr(X)];
args_by_op(decode_init_default)      -> [?expr(InitialValue)];
args_by_op(decode_repeated_add_elem) -> [?expr(Elem), ?expr(L)];
args_by_op(decode_repeated_finalize) -> [?expr(L)];
args_by_op(merge)                    -> [?expr(X1), ?expr(X2)].

format_aux_transl_helpers() ->
    [nowarn_attrs(id,1),
     inline_attr(id,1),
     "id(X) -> X.\n",
     "\n",
     nowarn_attrs(cons,2),
     inline_attr(cons,2),
     "cons(Elem, Acc) -> [Elem | Acc].\n",
     "\n",
     nowarn_attrs('lists_reverse',1),
     inline_attr('lists_reverse',1),
     "'lists_reverse'(L) -> lists:reverse(L).\n"].

format_aux_transl_helpers_used_also_with_nifs() ->
    [nowarn_attrs('erlang_++',2),
     inline_attr('erlang_++',2),
     "'erlang_++'(A, B) -> A ++ B.\n"].

format_default_translators(#anres{map_types=MapTypes}=AnRes, Opts) ->
    HaveMaps = sets:size(MapTypes) > 0,
    [%% Auxiliary helpers in case of fields of type map<_,_>
     [case get_records_or_maps_by_opts(Opts) of
          records ->
              [inline_attr(mt_maptuple_to_pseudomsg_r,2),
               gpb_codegen:format_fn(
                 mt_maptuple_to_pseudomsg_r,
                 fun({K,V},RName) -> {RName,K,V} end),
               "\n",
               inline_attr(mt_empty_map_r,0),
               gpb_codegen:format_fn(
                 mt_empty_map_r,
                 fun() -> dict:new() end),
               inline_attr(mt_add_item_r,2),
               gpb_codegen:format_fn(
                 mt_add_item_r,
                 fun({_RName,K,V}, D) -> dict:store(K,V,D) end),
               "\n",
               inline_attr(mt_finalize_items_r,1),
               gpb_codegen:format_fn(
                 mt_finalize_items_r,
                 fun(D) -> dict:to_list(D) end),
               "\n"];
          maps ->
              {M,K,V} = {?expr(M), ?expr(K), ?expr(V)},
              [inline_attr(mt_maptuple_to_pseudomsg_m,1),
               gpb_codegen:format_fn(
                 mt_maptuple_to_pseudomsg_m,
                 fun({K,V}) -> '#{key => K, value => V}' end,
                 [replace_tree('#{key => K, value => V}',
                               map_create([{key,K}, {value,V}]))]),
               "\n",
               inline_attr(mt_map_to_list_m,1),
               gpb_codegen:format_fn(
                 mt_map_to_list_m,
                 fun(M) -> maps:to_list(M) end),
               "\n",
               inline_attr(mt_empty_map_m,0),
               gpb_codegen:format_fn(
                 mt_empty_map_m,
                 fun() -> '#{}' end,
                 [replace_tree('#{}', map_create([]))]),
               "\n",
               inline_attr(mt_add_item_m,2),
               gpb_codegen:format_fn(
                 mt_add_item_m,
                 fun('#{key := K,value := V}', M) -> 'M#{K => V}' end,
                 [replace_tree('#{key := K,value := V}',
                               map_match([{key,K}, {value,V}])),
                  replace_tree('M#{K => V}',
                               map_set(M, [{K,V}]))]),
               "\n"]
      end,
      format_default_merge_translators(AnRes, Opts)]
     || HaveMaps].

format_default_merge_translators(#anres{map_types=MapTypes}, Opts) ->
    HaveMaps = sets:size(MapTypes) > 0,
    [%% Auxiliary helpers in case of fields of type map<_,_>
     case get_records_or_maps_by_opts(Opts) of
         records ->
             [inline_attr(mt_merge_maptuples_r,2),
              gpb_codegen:format_fn(
                mt_merge_maptuples_r,
                fun(L1, L2) ->
                        dict:to_list(dict:merge(fun(_Key, _V1, V2) -> V2 end,
                                                dict:from_list(L1),
                                                dict:from_list(L2)))
                end),
              "\n"];
         maps ->
             [inline_attr(mt_merge_maps_m,2),
              gpb_codegen:format_fn(
                mt_merge_maps_m,
                fun(M1, M2) -> maps:merge(M1,M2) end),
              "\n"]
     end
     || HaveMaps].

nowarn_attrs(FnName,Arity) ->
    ?f("-compile({nowarn_unused_function,~p/~w}).~n", [FnName,Arity]).

inline_attr(FnName,Arity) ->
    ?f("-compile({inline,~p/~w}).~n", [FnName,Arity]).

%% -- message defs -----------------------------------------------------

format_introspection(Defs, Opts) ->
    MsgDefs  = [Item || {{msg, _}, _}=Item <- Defs],
    EnumDefs = [Item || {{enum, _}, _}=Item <- Defs],
    ServiceDefs = [Item || {{service, _}, _}=Item <- Defs],
    [gpb_codegen:format_fn(
       get_msg_defs, fun() -> '<Defs>' end,
       [replace_tree('<Defs>', msg_def_trees(EnumDefs, MsgDefs, Opts))]),
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
     format_get_service_names(ServiceDefs),
     ?f("~n"),
     format_get_service_defs(ServiceDefs, Opts),
     ?f("~n"),
     format_get_rpc_names(ServiceDefs),
     ?f("~n"),
     format_find_rpc_defs(ServiceDefs),
     ?f("~n"),
     [format_find_service_rpc_defs(ServiceName, Rpcs, Opts) || {{service, ServiceName}, Rpcs} <- ServiceDefs],
     ?f("~n"),
     format_fetch_rpc_defs(ServiceDefs),
     ?f("~n"),
     format_get_package_name(Defs),
     ?f("~n"),
     format_descriptor(Defs, Opts)
    ].

msg_def_trees(EnumDefs, MsgDefs, Opts) ->
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

field_tree(#?gpb_field{}=F, Opts) ->
    [?gpb_field | FValues] = tuple_to_list(F),
    FNames = record_info(fields, ?gpb_field),
    mapping_create(
      ?gpb_field,
      lists:zip(FNames,
                [erl_parse:abstract(FValue) || FValue <- FValues]),
      Opts);
field_tree(#gpb_oneof{fields=OFields}=F, Opts) ->
    [gpb_oneof | FValues] = tuple_to_list(F),
    FNames = record_info(fields, gpb_oneof),
    mapping_create(
      gpb_oneof,
      [if FName == fields -> {FName, fields_tree(OFields, Opts)};
          FName /= fields -> {FName, erl_parse:abstract(FValue)}
       end
       || {FName, FValue} <- lists:zip(FNames, FValues)],
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
    %% that are used in messages.
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
                          || {EnumSym, EnumValue} <- unalias_enum(EnumDef)])]),
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

% --- service introspection methods

format_get_service_names(ServiceDefs) ->
    gpb_codegen:format_fn(
      get_service_names,
      fun() -> '<ServiceNames>' end,
      [replace_term(
         '<ServiceNames>',
         [ServiceName || {{service, ServiceName}, _Rpcs} <- ServiceDefs])]).

format_get_service_defs(ServiceDefs, Opts) ->
    gpb_codegen:format_fn(
      get_service_def,
      fun('<ServiceName>') -> '<ServiceDef>';
         (_) -> error
      end,
      [repeat_clauses(
         '<ServiceName>',
         [[replace_term('<ServiceName>', ServiceName),
           replace_tree('<ServiceDef>', service_def_tree(ServiceDef, Opts))]
          || {{service, ServiceName}, _Rpcs} = ServiceDef <- ServiceDefs])]).

format_get_rpc_names(ServiceDefs) ->
    gpb_codegen:format_fn(
      get_rpc_names,
      fun('<ServiceName>') -> '<ServiceRpcNames>';
         (_) -> error
      end,
      [repeat_clauses('<ServiceName>',
                      [[replace_term('<ServiceName>', ServiceName),
                        replace_term('<ServiceRpcNames>',
                                     [RpcName
                                      || #?gpb_rpc{name=RpcName} <- Rpcs])]
                       || {{service, ServiceName}, Rpcs} <- ServiceDefs])]).

format_find_rpc_defs(ServiceDefs) ->
    gpb_codegen:format_fn(
      find_rpc_def,
      fun('<ServiceName>', RpcName) -> '<ServiceFindRpcDef>'(RpcName);
         (_, _) -> error
      end,
      [repeat_clauses(
         '<ServiceName>',
         [[replace_term('<ServiceName>', ServiceName),
           replace_term('<ServiceFindRpcDef>',
                        mk_fn(find_rpc_def_, ServiceName))]
          || {{service, ServiceName}, _} <- ServiceDefs])]).

format_find_service_rpc_defs(ServiceName, Rpcs, Opts) ->
    gpb_codegen:format_fn(
      mk_fn(find_rpc_def_, ServiceName),
      fun('<RpcName>') -> '<RpcDef>';
         (_) -> error
      end,
      [repeat_clauses('<RpcName>',
                      [[replace_term('<RpcName>', RpcName),
                        replace_tree('<RpcDef>', rpc_def_tree(Rpc, Opts))]
                       || #?gpb_rpc{name=RpcName} = Rpc <- Rpcs])]).

format_fetch_rpc_defs([]) ->
    gpb_codegen:format_fn(
      fetch_rpc_def,
      fun(ServiceName, RpcName) ->
              erlang:error({no_such_rpc, ServiceName, RpcName})
      end);
format_fetch_rpc_defs(_ServiceDefs) ->
    gpb_codegen:format_fn(
      fetch_rpc_def,
      fun(ServiceName, RpcName) ->
              case find_rpc_def(ServiceName, RpcName) of
                  Def when is_tuple(Def) -> Def;
                  error -> erlang:error({no_such_rpc, ServiceName, RpcName})
              end
      end).

service_def_tree({{service, ServiceName}, Rpcs}, Opts) ->
    erl_syntax:tuple(
      [erl_syntax:tuple([erl_syntax:atom(service),
                         erl_syntax:atom(ServiceName)]),
       rpcs_def_tree(Rpcs, Opts)]);
service_def_tree(undefined, _) ->
    erl_syntax:list([]).

get_rpc_format_by_opts(Opts) ->
    case proplists:get_bool(defs_as_proplists, proplists:unfold(Opts)) of
        false -> rpcs_as_records; %% default
        true  -> rpcs_as_proplists
    end.

rpc_record_def_tree(#?gpb_rpc{}=Rpc, Opts) ->
    [?gpb_rpc | RValues] = tuple_to_list(Rpc),
    RNames = record_info(fields, ?gpb_rpc),
    mapping_create(
      ?gpb_rpc,
      lists:zip(RNames,
                [erl_parse:abstract(RValue) || RValue <- RValues]),
      Opts).

rpcs_def_tree(Rpcs, Opts) ->
    case get_rpc_format_by_opts(Opts) of
        rpcs_as_records   ->
            erl_syntax:list([rpc_record_def_tree(Rpc, Opts) || Rpc <- Rpcs]);
        rpcs_as_proplists ->
            erl_parse:abstract(gpb:rpc_records_to_proplists(Rpcs))
    end.

rpc_def_tree(#?gpb_rpc{}=Rpc, Opts) ->
    case get_rpc_format_by_opts(Opts) of
        rpcs_as_records   ->
            rpc_record_def_tree(Rpc, Opts);
        rpcs_as_proplists ->
            erl_parse:abstract(gpb:rpc_record_to_proplist(Rpc))
    end.

% --- exported types -----------------------------------------------------
format_enum_typespec(Enum, Enumeration) ->
  ?f("-type '~s'() :: ~s.", [Enum,
    string:join(["'"++atom_to_list(EName)++"'" || {EName, _} <- Enumeration],
                " | ")]).


format_record_typespec(Msg, Fields, Defs, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            ?f("-type ~p() :: #~p{}.", [Msg, Msg]);
        maps ->
            ?f("-type ~p() ::~n"
               "      #{~s~n"
               "       }.",
               [Msg, outdent_first(format_hfields(7 + 1, Fields, Opts, Defs))])
    end.

format_export_types(Defs, Opts) ->
    case get_type_specs_by_opts(Opts) of
        false ->
            "";
        true ->
            iolist_to_binary(
              ["%% enumerated types\n",
               string:join([format_enum_typespec(Enum, Enumeration)
                            || {{enum, Enum}, Enumeration} <- Defs],
                           "\n"),
               "\n",
               ?f("-export_type([~s]).",
                  [string:join(["'"++atom_to_list(Enum)++"'/0"
                                || {{enum, Enum}, _} <- Defs], ", ")]),
               "\n\n",
               "%% message types\n",
               string:join([format_record_typespec(Msg, Fields, Defs, Opts)
                            || {{msg, Msg}, Fields} <- Defs],
                           "\n"),
               "\n",
               ?f("-export_type([~s]).",
                  [string:join(["'"++atom_to_list(Msg)++"'/0"
                                || {{msg, Msg}, _} <- Defs], ", ")]),
               "\n"])
    end.

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
    Def = list_to_atom(string:to_upper(lists:concat([Msg, "_PB_H"]))),
    [?f("-ifndef(~p).~n", [Def]),
     ?f("-define(~p, true).~n", [Def]),
     ?f("-record(~p,~n", [Msg]),
     ?f("        {"),
     outdent_first(format_hfields(8+1, Fields, Opts, Defs)),
     "\n",
     ?f("        }).~n"),
     ?f("-endif.~n")].

format_hfields(Indent, Fields, Opts, Defs) ->
    TypeSpecs = get_type_specs_by_opts(Opts),
    MapsOrRecords = get_records_or_maps_by_opts(Opts),
    TypeSpecifierSep = case MapsOrRecords of
                           records -> "::";
                           maps -> "=>"
                       end,
    MappingAndUnset = get_mapping_and_unset_by_opts(Opts),
    LastIndex = case MappingAndUnset of
                    records -> length(Fields);
                    {maps, present_undefined} -> length(Fields);
                    {maps, omitted} -> find_last_nonopt_field_index(Fields)
                end,
    string:join(
      lists:map(
        fun({I, #?gpb_field{name=Name, fnum=FNum, opts=FOpts,
                            occurrence=Occur}=Field}) ->
                LineLead = if MappingAndUnset == {maps, omitted},
                              Occur == optional ->
                                   "%% ";
                              true ->
                                   ""
                           end,
                DefaultStr = case proplists:get_value(default, FOpts, '$no') of
                                 '$no' ->
                                     case {Occur, MapsOrRecords} of
                                         {repeated, records} -> ?f(" = []");
                                         _        -> ""
                                     end;
                                 Default ->
                                     ?f(" = ~p", [Default])
                             end,
                TypeStr = ?f("~s", [type_to_typestr(Field, Defs, Opts)]),
                CommaSep = if I < LastIndex -> ",";
                              true          -> "" %% last entry
                           end,
                FieldTxt0 = ?f("~s~w~s", [LineLead, Name, DefaultStr]),
                FieldTxt1 = indent(Indent, FieldTxt0),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                       TypeSpecifierSep,
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
                    [", " || TypeComment /= ""], TypeComment]);
           ({I, #gpb_oneof{name=Name}=Field}) ->
                LineLead = if MappingAndUnset == {maps, omitted} -> "%% ";
                              true -> ""
                           end,
                TypeStr = ?f("~s", [type_to_typestr(Field, Defs, Opts)]),
                CommaSep = if I < LastIndex -> ",";
                              true          -> "" %% last entry
                           end,
                FieldTxt0 = ?f("~s~w", [LineLead, Name]),
                FieldTxt1 = indent(Indent, FieldTxt0),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                       TypeSpecifierSep,
                                                       TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    ?f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                                not TypeSpecs -> 40
                             end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                TypeComment = type_to_comment(Field, TypeSpecs),
                ?f("~s~s% ~s",
                   [FieldTxt2, LineUpStr2, TypeComment])
        end,
        index_seq(Fields)),
      "\n").

get_type_specs_by_opts(Opts) ->
    Default = false,
    proplists:get_value(type_specs, Opts, Default).

find_last_nonopt_field_index(Fields) ->
    lists:foldl(fun({I, F}, Acc) ->
                        case get_field_occurrence(F) of
                            required -> I;
                            repeated -> I;
                            optional -> Acc
                        end
                end,
                0,
                index_seq(Fields)).

type_to_typestr(#?gpb_field{type=Type, occurrence=Occurrence}, Defs, Opts) ->
    OrUndefined = case get_mapping_and_unset_by_opts(Opts) of
                      records                   -> " | undefined";
                      {maps, present_undefined} -> " | undefined";
                      {maps, omitted}           -> ""
                  end,
    case Occurrence of
        required ->
            type_to_typestr_2(Type, Defs, Opts);
        repeated ->
            case Type of
                {map,_,_} -> type_to_typestr_2(Type, Defs, Opts);
                _         -> "[" ++ type_to_typestr_2(Type, Defs, Opts) ++ "]"
            end;
        optional ->
            type_to_typestr_2(Type, Defs, Opts) ++ OrUndefined
    end;
type_to_typestr(#gpb_oneof{fields=OFields}, Defs, Opts) ->
    OrUndefined = case get_mapping_and_unset_by_opts(Opts) of
                      records                   -> ["undefined"];
                      {maps, present_undefined} -> ["undefined"];
                      {maps, omitted}           -> []
                  end,
    string:join(
      [?f("{~s, ~s}", [Name, type_to_typestr_2(Type, Defs, Opts)])
       || #?gpb_field{name=Name, type=Type} <- OFields]
      ++ OrUndefined,
      " | ").

type_to_typestr_2(sint32, _Defs, _Opts)   -> "integer()";
type_to_typestr_2(sint64, _Defs, _Opts)   -> "integer()";
type_to_typestr_2(int32, _Defs, _Opts)    -> "integer()";
type_to_typestr_2(int64, _Defs, _Opts)    -> "integer()";
type_to_typestr_2(uint32, _Defs, _Opts)   -> "non_neg_integer()";
type_to_typestr_2(uint64, _Defs, _Opts)   -> "non_neg_integer()";
type_to_typestr_2(bool, _Defs, _Opts)     -> "boolean()";
type_to_typestr_2(fixed32, _Defs, _Opts)  -> "non_neg_integer()";
type_to_typestr_2(fixed64, _Defs, _Opts)  -> "non_neg_integer()";
type_to_typestr_2(sfixed32, _Defs, _Opts) -> "integer()";
type_to_typestr_2(sfixed64, _Defs, _Opts) -> "integer()";
type_to_typestr_2(float, _Defs, _Opts)    -> "float()";
type_to_typestr_2(double, _Defs, _Opts)   -> "float()";
type_to_typestr_2(string, _Defs, Opts)    ->
  string_to_typestr(get_strings_as_binaries_by_opts(Opts));
type_to_typestr_2(bytes, _Defs, _Opts)    -> "binary()";
type_to_typestr_2({enum,E}, Defs, _Opts)  -> enum_typestr(E, Defs);
type_to_typestr_2({msg,M}, _Defs, Opts)   -> msg_to_typestr(M, Opts);
type_to_typestr_2({map,KT,VT}, Defs, Opts) ->
    KTStr = type_to_typestr_2(KT, Defs, Opts),
    VTStr = type_to_typestr_2(VT, Defs, Opts),
    case get_records_or_maps_by_opts(Opts) of
        records -> ?f("[{~s, ~s}]", [KTStr, VTStr]);
        maps    -> ?f("#{~s => ~s}", [KTStr, VTStr])
    end.

msg_to_typestr(M, Opts) ->
  case get_records_or_maps_by_opts(Opts) of
    records -> ?f("#~p{}", [M]);
    maps -> ?f("~p()", [M])
  end.

%% when the strings_as_binaries option is requested the corresponding
%% typespec should be spec'ed
string_to_typestr(true) ->
  "binary()";
string_to_typestr(false) ->
  "string()".

enum_typestr(E, Defs) ->
    {value, {{enum,E}, Enumerations}} = lists:keysearch({enum,E}, 1, Defs),
    string:join(["'"++atom_to_list(EName)++"'" || {EName, _} <- Enumerations],
                " | ").

type_to_comment(#?gpb_field{type=Type}, true=_TypeSpec) ->
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
type_to_comment(#?gpb_field{type=Type, occurrence=Occurrence}, false) ->
    case Occurrence of
        required -> ?f("~w", [Type]);
        repeated -> "[" ++ ?f("~w", [Type]) ++ "]";
        optional -> ?f("~w (optional)", [Type])
    end;
type_to_comment(#gpb_oneof{}, _) ->
    "oneof".


lineup(CurrentCol, TargetCol) when CurrentCol < TargetCol ->
    lists:duplicate(TargetCol - CurrentCol, $\s);
lineup(_, _) ->
    " ".

indent(Indent, Str) ->
    lists:duplicate(Indent, $\s) ++ Str.

indent_lines(Indent, Lines) ->
    [indent(Indent, Line) || Line <- Lines].

split_indent_iolist(Indent, IoList) ->
    [if Line == <<>> -> "\n"; %% don't indent empty lines
        true -> [indent(Indent, Line), "\n"]
     end
     || Line <- linesplit_iolist(IoList)].

linesplit_iolist(Iolist) ->
    re:split(Iolist, ["\n"], [trim, {return,binary}]).

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

is_dotted(S) when is_list(S) -> string:str(S, ".") > 0;
is_dotted(S) when is_atom(S) -> is_dotted(atom_to_list(S)).

fmt_maps_as_msgs_record_defs(#anres{maps_as_msgs=MapsAsMsgs}) ->
    [begin
         FNames = [atom_to_list(FName) || #?gpb_field{name=FName} <- Fields],
         ?f("-record(~p,{~s}).~n", [MsgName, string:join(FNames,", ")])
     end
     || {{msg,MsgName},Fields} <- MapsAsMsgs].

%% -- nif c++ code -----------------------------------------------------

possibly_format_nif_cc(Mod, Defs, AnRes, Opts) ->
    case proplists:get_bool(nif, Opts) of
        true  -> format_nif_cc(Mod, Defs, AnRes, Opts);
        false -> '$not_generated'
    end.

format_nif_cc(Mod, Defs, AnRes, Opts) ->
    iolist_to_binary(
      [format_nif_cc_includes(Mod, Defs, Opts),
       format_nif_cc_oneof_version_check_if_present(Defs),
       format_nif_cc_maptype_version_check_if_present(Defs),
       format_nif_cc_map_api_check_if_needed(Opts),
       format_nif_cc_local_function_decls(Mod, Defs, Opts),
       format_nif_cc_mk_atoms(Mod, Defs, AnRes, Opts),
       format_nif_cc_utf8_conversion(Mod, Defs, AnRes, Opts),
       format_nif_cc_encoders(Mod, Defs, Opts),
       format_nif_cc_packers(Mod, Defs, Opts),
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
     "#include <erl_nif.h>\n",
     "\n",
     ?f("#include \"~s.pb.h\"\n", [Mod]),
     ["#include <google/protobuf/message_lite.h>\n" || IsLiteRT],
     "\n"].

format_nif_cc_oneof_version_check_if_present(Defs) ->
    case contains_oneof(Defs) of
        true ->
            ["#if GOOGLE_PROTOBUF_VERSION < 2006000\n"
             "#error \"The proto definitions contain 'oneof' fields.\"\n"
             "#error \"This feature appeared in protobuf 2.6.0, but\"\n"
             "#error \"it appears your protobuf is older.  Please\"\n"
             "#error \"update protobuf.\"\n"
             "#endif\n"
             "\n"];
        false ->
            ""
    end.

contains_oneof([{{msg,_}, Fields} | Rest]) ->
    case lists:any(fun(F) -> is_record(F, gpb_oneof) end, Fields) of
        false -> contains_oneof(Rest);
        true  -> true
    end;
contains_oneof([_ | Rest]) ->
    contains_oneof(Rest);
contains_oneof([]) ->
    false.

format_nif_cc_maptype_version_check_if_present(Defs) ->
    case contains_maptype_field(Defs) of
        true ->
            ["#if GOOGLE_PROTOBUF_VERSION < 3000000\n"
             "#error \"The proto definitions contain 'map' fields.\"\n"
             "#error \"This feature appeared in protobuf 3, but\"\n"
             "#error \"it appears your protobuf is older.  Please\"\n"
             "#error \"update protobuf.\"\n"
             "#endif\n"
             "\n"];
        false ->
            ""
    end.

contains_maptype_field([{{msg,_}, Fields} | Rest]) ->
    case lists:any(fun  is_maptype_field/1, Fields) of
        false -> contains_maptype_field(Rest);
        true  -> true
    end;
contains_maptype_field([_ | Rest]) ->
    contains_maptype_field(Rest);
contains_maptype_field([]) ->
    false.

is_maptype_field(#?gpb_field{type={map,_,_}}) -> true;
is_maptype_field(_) -> false.

format_nif_cc_map_api_check_if_needed(Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            "";
        maps ->
            %% The maps api functions appeared in erl_nif.h version 2.6,
            %% which is Erlang 17, but they were not documented until 18.0.
            %% There were some changes to the iterators in 2.8 (= Erlang 18.0)
            %% but those are not needed.
            ["#if (!(", format_nif_check_version_or_later(2, 6), "))\n"
             "#error \"Maps was specified. The needed nif interface for\"\n"
             "#error \"maps appeared in version 2.6 (Erlang 17), but\" \n"
             "#error \"it appears your erl_nif version is older.  Please\"\n"
             "#error \"update Erlang.\"\n"
             "#endif\n"
             "\n"]
    end.

format_nif_cc_local_function_decls(_Mod, Defs, _Opts) ->
    CPkg = get_cc_pkg(Defs),
    [[begin
          PackFnName = mk_c_fn(p_msg_, MsgName),
          UnpackFnName = mk_c_fn(u_msg_, MsgName),
          CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
          [["static int ",PackFnName,["(ErlNifEnv *env, ",
                                      "const ERL_NIF_TERM r,",
                                      CMsgType," *m);\n"]],
           ["static ERL_NIF_TERM ",UnpackFnName,["(ErlNifEnv *env, ",
                                                 "const ",CMsgType," *m);\n"]]]
      end
      || {{msg, MsgName}, _Fields} <- Defs],
     "\n"].

format_nif_cc_mk_atoms(_Mod, Defs, AnRes, Opts) ->
    Maps = get_records_or_maps_by_opts(Opts) == maps,
    EnumAtoms = lists:flatten([[Sym || {Sym, _V} <- EnumDef]
                               || {{enum, _}, EnumDef} <- Defs]),
    RecordAtoms = [MsgName || {{msg, MsgName}, _Fields} <- Defs],
    OneofNames = collect_oneof_fields(Defs),
    MiscAtoms0 = case is_any_field_of_type_enum(AnRes) of
                     true  -> [undefined];
                     false -> []
                 end,
    MiscAtoms1 = case is_any_field_of_type_bool(AnRes) of
                     true  -> MiscAtoms0 ++ [true, false];
                     false -> MiscAtoms0
                 end,
    FieldAtoms = if Maps ->
                         lists:usort(
                           lists:flatten(
                             [[get_field_name(Field) || Field <- Fields]
                              || {{msg,_MsgName}, Fields} <- Defs]));
                    not Maps ->
                         []
                 end,
    MiscAtoms2 = MiscAtoms1 ++ FieldAtoms,
    Atoms = lists:usort(EnumAtoms ++ RecordAtoms ++ OneofNames ++ MiscAtoms2),
    AtomVars0 = [{mk_c_var(gpb_aa_, A), A} || A <- Atoms],
    NoValue = case get_mapping_and_unset_by_opts(Opts) of
                  records -> undefined;
                  {maps, present_undefined} -> undefined;
                  {maps, omitted} -> '$undef'
              end,
    AtomVars1 = [{"gpb_x_no_value", NoValue} | AtomVars0],
    [[?f("static ERL_NIF_TERM ~s;\n", [Var]) || {Var,_Atom} <- AtomVars1],
     "\n",
     ["static void install_atoms(ErlNifEnv *env)\n"
      "{\n",
      [?f("    ~s = enif_make_atom(env, \"~s\");\n", [AtomVar, Atom])
       || {AtomVar, Atom} <- AtomVars1],
      "}\n",
      "\n"]].

collect_oneof_fields(Defs) ->
    lists:usort(
      lists:flatten(
        [[[FOFName
           || #?gpb_field{name=FOFName} <- OFields]
          || #gpb_oneof{fields=OFields} <- Fields]
         || {{msg,_}, Fields} <- Defs])).

format_nif_cc_utf8_conversion(_Mod, _Defs, AnRes, Opts) ->
    case is_any_field_of_type_string(AnRes) of
        true  -> format_nif_cc_utf8_conversion_code(Opts);
        false -> ""
    end.

is_any_field_of_type_string(#anres{used_types=UsedTypes}) ->
    sets:is_element(string, UsedTypes).

is_any_field_of_type_enum(#anres{used_types=UsedTypes}) ->
    sets:fold(fun({enum,_}, _) -> true;
                 (_, Acc) -> Acc
              end,
              false,
              UsedTypes).

is_any_field_of_type_bool(#anres{used_types=UsedTypes}) ->
    sets:is_element(bool, UsedTypes).

format_nif_cc_utf8_conversion_code(Opts) ->
    [case get_strings_as_binaries_by_opts(Opts) of
         true ->
             ["static ERL_NIF_TERM\n",
              "utf8_to_erl_string(ErlNifEnv *env,\n",
              "                   const char *utf8data,\n",
              "                   unsigned int numOctets)\n"
              "{\n",
              "    ERL_NIF_TERM   b;\n",
              "    unsigned char *data;\n",
              "\n",
              "    data = enif_make_new_binary(env, numOctets, &b);\n",
              "    memmove(data, utf8data, numOctets);\n",
              "    return b;\n",
              "}\n"];
         false ->
             ["/* Source for info is https://www.ietf.org/rfc/rfc2279.txt */\n",
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
              "utf8_to_uint32(unsigned int *dest, const char *src,\n",
              "               int numCodePoints)\n",
              "{\n",
              "    int i;\n",
              "    const unsigned char *s = (unsigned char *)src;\n",
              "\n",
              "\n",
              "    /* Should perhaps check for illegal chars in d800-dfff and\n",
              "     * other illegal chars\n",
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
              "\n",
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
              "        return enif_make_list_from_array(env, es, numcp);\n"
              "    }\n",
              "}\n"]
     end,
     "\n",
     case get_strings_as_binaries_by_opts(Opts) of
         true ->
             "";
         false ->
             ["static int\n",
              "utf8_count_octets(ErlNifEnv *env, ERL_NIF_TERM str)\n",
              "{\n",
              "    int n = 0;\n",
              "\n",
              "    while (!enif_is_empty_list(env, str))\n",
              "    {\n",
              "        ERL_NIF_TERM head, tail;\n",
              "        unsigned int c;\n",
              "\n",
              "        if (!enif_get_list_cell(env, str, &head, &tail))\n",
              "            return -1;\n",
              "        if (!enif_get_uint(env, head, &c))\n",
              "            return -1;\n",
              "\n",
              "        if (c <= 0x7f) n += 1;\n",
              "        else if (c <= 0x7ff) n += 2;\n",
              "        else if (c <= 0xffff) n += 3;\n",
              "        else if (c <= 0x1Fffff) n += 4;\n",
              "        else if (c <= 0x3FFffff) n += 5;\n",
              "        else if (c <= 0x7FFFffff) n += 6;\n",
              "        else return -1;\n",
              "\n",
              "        str = tail;\n",
              "    }\n",
              "    return n;\n",
              "}\n",
              "\n",
              "static int\n",
              "utf8_to_octets(ErlNifEnv *env, ERL_NIF_TERM str, char *dest)\n",
              "{\n",
              "    unsigned char *s = (unsigned char *)dest;\n",
              "\n",
              "    while (!enif_is_empty_list(env, str))\n",
              "    {\n",
              "        ERL_NIF_TERM head, tail;\n",
              "        unsigned int c;\n",
              "\n",
              "        if (!enif_get_list_cell(env, str, &head, &tail))\n",
              "            return -1;\n",
              "        if (!enif_get_uint(env, head, &c))\n",
              "            return -1;\n",
              "\n",
              "        if (c <= 0x7f)\n",
              "            *s++ = c;\n",
              "        else if (c <= 0x7ff)\n",
              "        {\n",
              "            *s++ = 0xc0 | (c >> 6);\n",
              "            *s++ = 0x80 | (c & 0x3f);\n",
              "        }\n",
              "        else if (c <= 0xffff)\n",
              "        {\n",
              "            *s++ = 0xe0 | (c >> 12);\n",
              "            *s++ = 0x80 | ((c >> 6) & 0x3f);\n",
              "            *s++ = 0x80 | (c        & 0x3f);\n",
              "        }\n",
              "        else if (c <= 0x1Fffff)\n",
              "        {\n",
              "            *s++ = 0xf0 | (c >> 18);\n",
              "            *s++ = 0x80 | ((c >> 12) & 0x3f);\n",
              "            *s++ = 0x80 | ((c >>  6) & 0x3f);\n",
              "            *s++ = 0x80 | (c         & 0x3f);\n",
              "        }\n",
              "        else if (c <= 0x3FFffff)\n",
              "        {\n",
              "            *s++ = 0xf0 | (c >> 24);\n",
              "            *s++ = 0x80 | ((c >> 18) & 0x3f);\n",
              "            *s++ = 0x80 | ((c >> 12) & 0x3f);\n",
              "            *s++ = 0x80 | ((c >>  6) & 0x3f);\n",
              "            *s++ = 0x80 | (c         & 0x3f);\n",
              "        }\n",
              "        else if (c <= 0x7FFFffff)\n",
              "        {\n",
              "            *s++ = 0xf0 | (c >> 30);\n",
              "            *s++ = 0x80 | ((c >> 24) & 0x3f);\n",
              "            *s++ = 0x80 | ((c >> 18) & 0x3f);\n",
              "            *s++ = 0x80 | ((c >> 12) & 0x3f);\n",
              "            *s++ = 0x80 | ((c >>  6) & 0x3f);\n",
              "            *s++ = 0x80 | (c         & 0x3f);\n",
              "        }\n",
              "        else\n",
              "            return 0;\n",
              "\n",
              "        str = tail;\n",
              "    }\n",
              "    return 1;\n"
              "}\n"]
     end,
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
     %% Dirty schedulers flags appeared in Erlang 17.3 = enif 2.7
     %% but only if Erlang was configured with --enable-dirty-schedulers
     "#if ", format_nif_check_version_or_later(2, 7), "\n"
     "#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT\n",
     format_nif_cc_nif_funcs_list(Defs, "ERL_NIF_DIRTY_JOB_CPU_BOUND, "),
     "#else /* ERL_NIF_DIRTY_SCHEDULER_SUPPORT */\n",
     format_nif_cc_nif_funcs_list(Defs, ""),
     "#endif /* ERL_NIF_DIRTY_SCHEDULER_SUPPORT */\n",
     "#else /* before 2.7 or 17.3 */\n",
     format_nif_cc_nif_funcs_list(Defs, no_flags),
     "#endif /* before 2.7 or 17.3 */\n"
     "};\n",
     "\n",
     ?f("ERL_NIF_INIT(~s, nif_funcs, load, reload, upgrade, unload)\n",
        [Mod])].

format_nif_check_version_or_later(Major, Minor) ->
    ?f("ERL_NIF_MAJOR_VERSION > ~w"
       " || "
       "(ERL_NIF_MAJOR_VERSION == ~w && ERL_NIF_MINOR_VERSION >= ~w)",
       [Major, Major, Minor]).

format_nif_cc_nif_funcs_list(Defs, Flags) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgFields} <- Defs],
    FlagStr = if Flags == no_flags -> "";
                 true -> ", " ++ Flags
              end,
    [begin
         EncodeFnName = mk_fn(e_msg_, MsgName),
         EncodeCFnName = mk_c_fn(e_msg_, MsgName),
         DecodeFnName = mk_fn(d_msg_, MsgName),
         DecodeCFnName = mk_c_fn(d_msg_, MsgName),
         IsLast = I == length(MsgNames),
         Comma = ["," || not IsLast],
         [?f("    {\"~s\", 1, ~s~s},\n",
             [EncodeFnName, EncodeCFnName, FlagStr]),
          ?f("    {\"~s\", 1, ~s~s}~s\n",
             [DecodeFnName, DecodeCFnName, FlagStr, Comma])]
     end
     || {I, MsgName} <- index_seq(MsgNames)].

format_nif_cc_encoders(Mod, Defs, Opts) ->
    CPkg = get_cc_pkg(Defs),
    [format_nif_cc_encoder(Mod, CPkg, MsgName, Fields, Opts)
     || {{msg, MsgName}, Fields} <- Defs].

format_nif_cc_encoder(_Mod, CPkg, MsgName, _Fields, _Opts) ->
    FnName = mk_c_fn(e_msg_, MsgName),
    PackFnName = mk_c_fn(p_msg_, MsgName),
    CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
    ["static ERL_NIF_TERM\n",
     FnName,"(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])\n",
     "{\n",
     "    ErlNifBinary data;\n",
     "    int byteSize;\n",
     "    ",CMsgType," *m = new ",CMsgType,"();\n",
     "\n"
     "    if (argc != 1)\n"
     "    {\n"
     "        delete m;\n"
     "        return enif_make_badarg(env);\n"
     "    }\n"
     "\n"
     "    if (m == NULL)\n"
     "    {\n"
     "        delete m;\n"
     "        return enif_make_badarg(env);\n"
     "    }\n"
     "\n"
     "    if (!",PackFnName,"(env, argv[0], m))\n"
     "    {\n"
     "        delete m;\n"
     "        return enif_make_badarg(env);\n"
     "    }\n"
     "\n"
     "    byteSize = m->ByteSize();\n"
     "    if (!enif_alloc_binary(byteSize, &data))\n"
     "    {\n"
     "        delete m;\n"
     "        return enif_make_badarg(env);\n"
     "    }\n"
     "\n"
     "    if (!m->SerializeToArray(data.data, byteSize))\n"
     "    {\n"
     "        delete m;\n"
     "        return enif_make_badarg(env);\n"
     "    }\n"
     "\n"
     "    delete m;\n"
     "    return enif_make_binary(env, &data);\n"
     "}\n"
     "\n"].

format_nif_cc_packers(_Mod, Defs, Opts) ->
    CPkg = get_cc_pkg(Defs),
    [format_nif_cc_packer(CPkg, MsgName, Fields, Defs, Opts)
     || {{msg, MsgName}, Fields} <- Defs].

format_nif_cc_packer(CPkg, MsgName, Fields, Defs, Opts) ->
    Maps = get_records_or_maps_by_opts(Opts) == maps,
    PackFnName = mk_c_fn(p_msg_, MsgName),
    CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
    ["static int\n",
     PackFnName,["(ErlNifEnv *env, ",
                 "const ERL_NIF_TERM r,",
                 " ",CMsgType," *m)\n"],
     "{\n",
     if Maps ->
             NFieldsPlus1 = integer_to_list(length(Fields)+1),
             ["    ERL_NIF_TERM k, v;\n",
              "    ErlNifMapIterator iter;\n",
              "    ERL_NIF_TERM elem[",NFieldsPlus1,"];\n",
              "    ErlNifMapIteratorEntry first;\n",
              "    int i;\n",
              "\n"
              "#if ",format_nif_check_version_or_later(2, 8),"\n",
              "    first = ERL_NIF_MAP_ITERATOR_FIRST;\n",
              "#else /* before 2.8 which appeared in 18.0 */\n",
              "    first = ERL_NIF_MAP_ITERATOR_HEAD;\n",
              "#endif\n",
              "    for (i = 1; i < ",NFieldsPlus1,"; i++)\n",
              "        elem[i] = gpb_x_no_value;\n",
              "\n",
              "    if (!enif_map_iterator_create(env, r, &iter, first))\n",
              "        return 0;\n",
              "\n",
              "    while (enif_map_iterator_get_pair(env, &iter, &k, &v))\n",
              "    {\n",
              [?f("        ~sif (enif_is_identical(k, ~s))\n"
                  "            elem[~w] = v;\n",
                  [if I == 1 -> "";
                      I >  1 -> "else "
                   end,
                   mk_c_var(gpb_aa_, get_field_name(Field)),
                   get_field_rnum(Field)-1])
               || {I, Field} <- index_seq(Fields)],
              "        enif_map_iterator_next(env, &iter);\n",
              "    }\n",
              "    enif_map_iterator_destroy(env, &iter);\n",
              "\n"];
        not Maps ->
             ["    int arity;\n"
              "    const ERL_NIF_TERM *elem;\n"
              "\n"
              "    if (!enif_get_tuple(env, r, &arity, &elem))\n"
              "        return 0;\n"
              "\n",
              ?f("    if (arity != ~w)\n"
                 "        return 0;\n",
                 [length(Fields) + 1]),
              "\n"]
     end,
     [begin
          SrcVar = ?f("elem[~w]",[I]),
          format_nif_cc_field_packer(SrcVar, "m", Field, Defs, Opts)
      end
      || {I, Field} <- index_seq(Fields)],
     "\n"
     "    return 1;\n"
     "}\n",
     "\n"].

format_nif_cc_field_packer(SrcVar, MsgVar, #?gpb_field{}=Field, Defs, Opts) ->
    #?gpb_field{occurrence=Occurrence, type=Type}=Field,
    case Occurrence of
        required ->
            format_nif_cc_field_packer_single(SrcVar, MsgVar, Field, Defs,
                                              Opts, set);
        optional ->
            format_nif_cc_field_packer_optional(SrcVar, MsgVar, Field, Defs,
                                                Opts);
        repeated ->
            case Type of
                {map,_,_} ->
                    format_nif_cc_field_packer_maptype(SrcVar, MsgVar, Field,
                                                       Defs, Opts);
                _ ->
                    format_nif_cc_field_packer_repeated(SrcVar, MsgVar, Field,
                                                        Defs, Opts)
            end
    end;
format_nif_cc_field_packer(SrcVar, MsgVar, #gpb_oneof{}=Field, Defs, Opts) ->
    #gpb_oneof{fields=OFields} = Field,
    [split_indent_iolist(
       4,
       ?f("if (!enif_is_identical(~s, gpb_x_no_value))~n"
          "{~n"
          "    int oarity;~n"
          "    const ERL_NIF_TERM *oelem;~n"
          "    if (!enif_get_tuple(env, ~s, &oarity, &oelem) || oarity != 2)~n"
          "        return 0;~n"
          "~n"
          "    ~s~n"
          "}~n",
          [SrcVar, SrcVar,
           format_nif_cc_oneof_packer("oelem[0]", "oelem[1]",
                                      MsgVar, OFields, Defs, Opts)])),
     "\n"].

format_nif_cc_oneof_packer(NameVar, SrcVar, MsgVar, OFields, Defs, Opts) ->
    split_indent_iolist(
      4,
      [[begin
            Else = if I == 1 -> "";
                      I >  1 -> "else "
                   end,
            AtomVar = mk_c_var(gpb_aa_, Name),
            [?f("~sif (enif_is_identical(~s, ~s))~n", [Else, NameVar, AtomVar]),
             split_indent_iolist(
               4,
               format_nif_cc_field_packer_single(SrcVar, MsgVar, OField,
                                                 Defs, Opts, set))]
        end
        || {I, #?gpb_field{name=Name}=OField} <- index_seq(OFields)],
       "else\n"
       "    return 0;\n"]).

format_nif_cc_field_packer_optional(SrcVar, MsgVar, Field, Defs, Opts) ->
    [?f("    if (!enif_is_identical(~s, gpb_x_no_value))\n", [SrcVar]),
     format_nif_cc_field_packer_single(SrcVar, MsgVar, Field, Defs, Opts, set)].

format_nif_cc_field_packer_single(SrcVar, MsgVar, Field, Defs, Opts, Setter) ->
    #?gpb_field{name=FName, type=FType} = Field,
    LCFName = to_lower(FName),
    SetFn = fun(Exprs) ->
                    case Setter of
                        set ->
                            ?f("~s->set_~s(~s);",
                               [MsgVar, LCFName, string:join(Exprs, ", ")]);
                        add ->
                            ?f("~s->add_~s(~s);",
                               [MsgVar, LCFName, string:join(Exprs, ", ")]);
                        {set_var, V} ->
                            case Exprs of
                                [Val] -> ?f("~s = ~s;", [V, Val]);
                                [S,N] -> ?f("~s.assign(~s, ~s);", [V, S, N])
                            end
                    end
            end,
    [split_indent_iolist(
       4,
       case FType of
           float ->
               ?f("{\n"
                  "    double v;\n"
                  "    if (!enif_get_double(env, ~s, &v))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["(float)v"])]);
           double ->
               ?f("{\n"
                  "    double v;\n"
                  "    if (!enif_get_double(env, ~s, &v))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["v"])]);
           _S32 when FType == sint32;
                     FType == int32;
                     FType == sfixed32 ->
               ?f("{\n"
                  "    int v;\n"
                  "    if (!enif_get_int(env, ~s, &v))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["v"])]);
           _S64 when FType == sint64;
                     FType == int64;
                     FType == sfixed64 ->
               ?f("{\n"
                  "    ErlNifSInt64 v;\n"
                  "    if (!enif_get_int64(env, ~s, &v))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["v"])]);
           _U32 when FType == uint32;
                     FType == fixed32 ->
               ?f("{\n"
                  "    unsigned int v;\n"
                  "    if (!enif_get_uint(env, ~s, &v))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["v"])]);
           _U64 when FType == uint64;
                     FType == fixed64 ->
               ?f("{\n"
                  "    ErlNifUInt64 v;\n"
                  "    if (!enif_get_uint64(env, ~s, &v))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["v"])]);
           bool ->
               ?f("{\n"
                  "    if (enif_is_identical(~s, gpb_aa_true))\n"
                  "        ~s\n"
                  "    else\n"
                  "        ~s\n"
                  "}\n",
                  [SrcVar, SetFn(["1"]), SetFn(["0"])]);
           {enum, EnumName} ->
               EPrefix = case is_dotted(EnumName) of
                             false -> "";
                             true  -> dot_replace_s(EnumName, "_") ++ "_"
                         end,
               CPkg = get_cc_pkg(Defs),
               {value, {{enum,EnumName}, Enumerations}} =
                   lists:keysearch({enum,EnumName}, 1, Defs),
               ["{\n",
                [?f("    ~sif (enif_is_identical(~s, ~s))\n"
                    "        ~s\n",
                    [if I == 1 -> "";
                        I >  1 -> "else "
                     end,
                     SrcVar, mk_c_var(gpb_aa_, Sym),
                     SetFn([?f("~s::~s~s", [CPkg, EPrefix, Sym])])])
                 || {I, {Sym, _Val}} <- index_seq(Enumerations)],
                "    else\n"
                "        return 0;\n"
                "}\n"];
           string ->
               case get_strings_as_binaries_by_opts(Opts) of
                   true ->
                       ?f("{\n"
                          "    ErlNifBinary b;\n"
                          "    if (!enif_inspect_binary(env, ~s, &b))\n"
                          "        return 0;\n"
                          "    ~s\n"
                          "}\n",
                          [SrcVar,
                           SetFn(["reinterpret_cast<char *>(b.data)",
                                  "b.size"])]);
                   false ->
                       ?f("{\n"
                          "    size_t num_octs = utf8_count_octets(env, ~s);\n"
                          "\n"
                          "    if (num_octs < 0)\n"
                          "        return 0;\n"
                          "    else\n"
                          "    {\n"
                          "         char s[num_octs];\n"
                          "         utf8_to_octets(env, ~s, s);\n"
                          "         ~s\n"
                          "    }\n"
                          "}\n",
                          [SrcVar, SrcVar, SetFn(["s", "num_octs"])])
               end;
           bytes ->
               ?f("{\n"
                  "    ErlNifBinary b;\n"
                  "    if (!enif_inspect_binary(env, ~s, &b))\n"
                  "        return 0;\n"
                  "    ~s\n"
                  "}\n",
                  [SrcVar,
                   SetFn(["reinterpret_cast<char *>(b.data)", "b.size"])]);
           {msg, Msg2Name} ->
               CMsg2Type = mk_cctype_name(FType, Defs),
               PackFnName = mk_c_fn(p_msg_, Msg2Name),
               NewMsg2 = case Setter of
                             set -> ?f("~s->mutable_~s()", [MsgVar, LCFName]);
                             add -> ?f("~s->add_~s()", [MsgVar, LCFName]);
                             {set_var, V} ->
                                 ?f("~s = new ~s()", [V, CMsg2Type])
                         end,
               ?f("{\n"
                  "    ~s *m2 = ~s;\n"
                  "    if (!~s(env, ~s, m2))\n"
                  "        return 0;\n"
                  "}\n",
                  [CMsg2Type, NewMsg2, PackFnName, SrcVar]);
           {map, KeyType, ValueType} ->
               CMapType = mk_cctype_name(FType, Defs),
               {KeyVar, ValueVar} = SrcVar,
               PtrDeref = case ValueType of
                           {msg,_} -> "*";
                           _       -> ""
                       end,
               KeyDecl = ?f("~s m2k;", [mk_cctype_name(KeyType, Defs)]),
               ValueDecl = ?f("~s ~sm2v;", [mk_cctype_name(ValueType, Defs),
                                            PtrDeref]),
               SetKey = format_nif_cc_field_packer_single(
                          KeyVar, MsgVar, Field#?gpb_field{type=KeyType},
                          Defs, Opts,
                          {set_var, "m2k"}),
               SetValue = format_nif_cc_field_packer_single(
                            ValueVar, MsgVar, Field#?gpb_field{type=ValueType},
                            Defs, Opts,
                            {set_var, "m2v"}),
               ["{\n",
                ?f("    ~s *map = ~s->mutable_~s();\n"
                   "    ~s\n"  % decl of m2k
                   "    ~s\n"  % decl of m2v
                   "\n",
                   [CMapType, MsgVar, LCFName,
                    KeyDecl, ValueDecl]),
                %% Set values for m2k and m2v
                SetKey,
                SetValue,
                ?f("    (*map)[m2k] = ~sm2v;\n", [PtrDeref]),
                "}\n"]
       end),
     "\n"].

format_nif_cc_field_packer_repeated(SrcVar, MsgVar, Field, Defs, Opts) ->
    split_indent_iolist(
      4, [?f("{\n"
             "    ERL_NIF_TERM l = ~s;\n"
             "\n"
             "    while (!enif_is_empty_list(env, l))\n"
             "    {\n"
             "        ERL_NIF_TERM head, tail;\n"
             "\n"
             "        if (!enif_get_list_cell(env, l, &head, &tail))\n"
             "            return -1;\n",
             [SrcVar]),
          "\n",
          split_indent_iolist(4, format_nif_cc_field_packer_single(
                                   "head", MsgVar, Field, Defs, Opts, add)),
          ?f("        l = tail;\n"
             "    }\n"
             "}\n",
             [])]).

format_nif_cc_field_packer_maptype(SrcVar, MsgVar, Field, Defs, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            format_nif_cc_field_packer_maptype_r(SrcVar, MsgVar, Field, Defs,
                                                 Opts);
        maps ->
            format_nif_cc_field_packer_maptype_m(SrcVar, MsgVar, Field, Defs,
                                                 Opts)
    end.

format_nif_cc_field_packer_maptype_r(SrcVar, MsgVar, Field, Defs, Opts) ->
    split_indent_iolist(
      4, [?f("{\n"
             "    ERL_NIF_TERM l = ~s;\n"
             "\n"
             "    while (!enif_is_empty_list(env, l))\n"
             "    {\n"
             "        ERL_NIF_TERM head, tail;\n"
             "\n"
             "        if (!enif_get_list_cell(env, l, &head, &tail))\n"
             "            return 0;\n",
             [SrcVar]),
          ?f("        int arity;\n"
             "        const ERL_NIF_TERM *tuple;\n"
             "        if (!enif_get_tuple(env, head, &arity, &tuple))\n"
             "            return 0;\n"
             "        if (arity != 2)\n"
             "            return 0;\n",
             []),
          split_indent_iolist(
            4, format_nif_cc_field_packer_single(
                 {"tuple[0]", "tuple[1]"}, MsgVar, Field, Defs, Opts, add)),
          ?f("        l = tail;\n"
             "    }\n"
             "}\n",
             [])]).

format_nif_cc_field_packer_maptype_m(SrcVar, MsgVar, Field, Defs, Opts) ->
    split_indent_iolist(
      4, ["{\n"
          "    ERL_NIF_TERM ik, iv;\n",
          "    ErlNifMapIterator iter;\n",
          "    ErlNifMapIteratorEntry first;\n",
          "\n"
          "#if ",format_nif_check_version_or_later(2, 8),"\n",
          "    first = ERL_NIF_MAP_ITERATOR_FIRST;\n",
          "#else /* before 2.8 which appeared in 18.0 */\n",
          "    first = ERL_NIF_MAP_ITERATOR_HEAD;\n",
          "#endif\n",
          ?f("    if (!enif_map_iterator_create(env, ~s, &iter, first))\n"
             "        return 0;\n"
             "\n",
             [SrcVar]),
          "    while (enif_map_iterator_get_pair(env, &iter, &ik, &iv))\n",
          "    {\n",
          split_indent_iolist(
            4, format_nif_cc_field_packer_single(
                 {"ik", "iv"}, MsgVar, Field, Defs, Opts, add)),
          "        enif_map_iterator_next(env, &iter);\n",
          "    }\n",
          "    enif_map_iterator_destroy(env, &iter);\n",
          "}\n"]).

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
     "    {\n",
     "        delete m;\n",
     "        return enif_make_badarg(env);\n",
     "    }\n",
     "\n",
     "    if (m == NULL)\n",
     "    {\n",
     "        delete m;\n",
     "        return enif_make_badarg(env);\n", %% FIXME: enomem?
     "    }\n",
     "\n",
     "    if (!enif_inspect_binary(env, argv[0], &data))\n"
     "    {\n",
     "        delete m;\n",
     "        return enif_make_badarg(env);\n",
     "    }\n",
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

format_nif_cc_unpackers(_Mod, Defs, Opts) ->
    CPkg = get_cc_pkg(Defs),
    [format_nif_cc_unpacker(CPkg, MsgName, Fields, Defs, Opts)
     || {{msg, MsgName}, Fields} <- Defs].

format_nif_cc_unpacker(CPkg, MsgName, Fields, Defs, Opts) ->
    Maps = get_records_or_maps_by_opts(Opts) == maps,
    UnpackFnName = mk_c_fn(u_msg_, MsgName),
    CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
    Is = [I || {I,_} <- index_seq(Fields)],
    ["static ERL_NIF_TERM\n",
     UnpackFnName,"(ErlNifEnv *env, const ",CMsgType," *m)\n",
     "{\n",
     "    ERL_NIF_TERM res;\n",
     [?f("    ERL_NIF_TERM rname = ~s;\n", [mk_c_var(gpb_aa_, MsgName)])
      || not Maps],
     [?f("    ERL_NIF_TERM elem~w;\n", [I]) || I <- Is],
     "\n",
     [begin
          DestVar = ?f("elem~w",[I]),
          format_nif_cc_field_unpacker(DestVar, "m", MsgName, Field, Defs, Opts)
      end
      || {I, Field} <- index_seq(Fields)],
     "\n",
     case get_mapping_and_unset_by_opts(Opts) of
         records ->
             ?f("    res = enif_make_tuple(env, ~w, rname~s);\n",
                [length(Fields) + 1, [?f(", elem~w",[I]) || I <- Is]]);
         {maps, present_undefined} ->
             [?f("    res = enif_make_new_map(env);\n"),
              [?f("    enif_make_map_put(env, res, gpb_aa_~s, elem~w, &res);\n",
                  [get_field_name(Field), I])
               || {I, Field} <- index_seq(Fields)]];
         {maps, omitted} ->
             [?f("    res = enif_make_new_map(env);\n"),
              [begin
                   Put = ?f("enif_make_map_put("++
                                "env, res, gpb_aa_~s, elem~w, &res);",
                            [get_field_name(Field), I]),
                   Test = ?f("if (!enif_is_identical(elem~w, gpb_x_no_value))",
                             [I]),
                   PutLine = Put ++ "\n",
                   TestLine = Test ++ "\n",
                   case get_field_occurrence(Field) of
                       optional ->
                           indent_lines(4, [TestLine, indent(4, PutLine)]);
                       _ ->
                           indent(4, PutLine)
                   end
               end
               || {I, Field} <- index_seq(Fields)]]
     end,
     "    return res;\n"
     "}\n",
     "\n"].

format_nif_cc_field_unpacker(DestVar, MsgVar, _MsgName, #?gpb_field{}=Field,
                             Defs, Opts) ->
    #?gpb_field{occurrence=Occurrence, type=Type}=Field,
    case Occurrence of
        required ->
            format_nif_cc_field_unpacker_single(DestVar, MsgVar, Field, Defs);
        optional ->
            format_nif_cc_field_unpacker_single(DestVar, MsgVar, Field, Defs);
        repeated ->
            case Type of
                {map,_,_} ->
                    format_nif_cc_field_unpacker_maptype(DestVar, MsgVar,
                                                         Field, Defs, Opts);
                _ ->
                    format_nif_cc_field_unpacker_repeated(DestVar, MsgVar,
                                                          Field, Defs)
            end
    end;
format_nif_cc_field_unpacker(DestVar, MsgVar, MsgName, #gpb_oneof{}=Field,
                             Defs, _Opts) ->
    #gpb_oneof{name=OFName, fields=OFields} = Field,
    CPkg = get_cc_pkg(Defs),
    CMsgType = CPkg ++ "::" ++ dot_replace_s(MsgName, "::"),
    LCOFName = to_lower(OFName),
    UCOFName = to_upper(OFName),
    [split_indent_iolist(
       4,
       [?f("switch (~s->~s_case())\n", [MsgVar, LCOFName]),
        ?f("{\n"),
        [begin
             CamelCaseFOFName = camel_case(FOFName),
             AtomVar = mk_c_var(gpb_aa_, FOFName),
             split_indent_iolist(
               4,
               [?f("case ~s::k~s:\n", [CMsgType, CamelCaseFOFName]),
                ?f("    {\n"),
                ?f("        ERL_NIF_TERM ores;\n"),
                split_indent_iolist(
                  8,
                  format_nif_cc_field_unpacker_by_field("ores", MsgVar,
                                                        OField, Defs)),
                ?f("        ~s = enif_make_tuple2(env, ~s, ores);\n",
                   [DestVar, AtomVar]),
                ?f("    }\n"),
                ?f("    break;\n\n")])
         end
         || #?gpb_field{name=FOFName}=OField <- OFields],
        split_indent_iolist(
          4,
          [?f("case ~s::~s_NOT_SET: /* FALL THROUGH */~n", [CMsgType, UCOFName]),
           ?f("default:~n"),
           ?f("    ~s = gpb_x_no_value;\n", [DestVar])]),
        ?f("}\n")]),
     "\n"].

format_nif_cc_field_unpacker_single(DestVar, MsgVar, Field, Defs) ->
    #?gpb_field{name=FName} = Field,
    LCFName = to_lower(FName),
    [?f("    if (!~s->has_~s())\n", [MsgVar, LCFName]),
     ?f("        ~s = gpb_x_no_value;\n", [DestVar]),
     ?f("    else\n"),
     indent_lines(
       8, format_nif_cc_field_unpacker_by_field(DestVar, MsgVar, Field, Defs)),
     "\n"].

format_nif_cc_field_unpacker_by_field(DestVar, MsgVar, Field, Defs) ->
    #?gpb_field{name=FName, type=FType} = Field,
    LCFName = to_lower(FName),
    SrcExpr = ?f("~s->~s()", [MsgVar, LCFName]),
    format_nif_cc_field_unpacker_by_type(DestVar, SrcExpr, FType, Defs).

format_nif_cc_field_unpacker_by_type(DestVar, SrcExpr, FType, Defs) ->
    case FType of
        float ->
            [?f("~s = enif_make_double(env, (double)~s);\n",
                [DestVar, SrcExpr])];
        double ->
            [?f("~s = enif_make_double(env, ~s);\n",
                [DestVar, SrcExpr])];
        _S32 when FType == sint32;
                  FType == int32;
                  FType == sfixed32 ->
            [?f("~s = enif_make_int(env, ~s);\n",
                [DestVar, SrcExpr])];
        _S64 when FType == sint64;
                  FType == int64;
                  FType == sfixed64 ->
            [?f("~s = enif_make_int64(env, (ErlNifSInt64)~s);\n",
                [DestVar, SrcExpr])];
        _U32 when FType == uint32;
                  FType == fixed32 ->
            [?f("~s = enif_make_uint(env, ~s);\n",
                [DestVar, SrcExpr])];
        _U64 when FType == uint64;
                  FType == fixed64 ->
            [?f("~s = enif_make_uint64(env, (ErlNifUInt64)~s);\n",
                [DestVar, SrcExpr])];
        bool ->
            [?f("if (~s)\n", [SrcExpr]),
             ?f("    ~s = gpb_aa_true;\n", [DestVar]),
             ?f("else\n"),
             ?f("    ~s = gpb_aa_false;\n", [DestVar])];
        {enum, EnumName} ->
            EPrefix = case is_dotted(EnumName) of
                          false -> "";
                          true  -> dot_replace_s(EnumName, "_") ++ "_"
                      end,
            CPkg = get_cc_pkg(Defs),
            {value, {{enum,EnumName}, Enumerations}} =
                lists:keysearch({enum,EnumName}, 1, Defs),
            [] ++
                [?f("switch (~s) {\n", [SrcExpr])] ++
                [?f("    case ~s::~s~s: ~s = ~s; break;\n",
                    [CPkg, EPrefix, Sym, DestVar, mk_c_var(gpb_aa_, Sym)])
                 || {Sym, _Value} <- Enumerations] ++
                [?f("    default: ~s = gpb_aa_undefined;\n", [DestVar])] ++
                [?f("}\n")];
        string ->
            [?f("{\n"),
             ?f("    const char    *sData = ~s.data();\n", [SrcExpr]),
             ?f("    unsigned int   sSize = ~s.size();\n", [SrcExpr]),
             ?f("    ~s = utf8_to_erl_string(env, sData, sSize);\n", [DestVar]),
             ?f("}\n")];
        bytes ->
            [?f("{\n"),
             ?f("    unsigned char *data;\n"),
             ?f("    unsigned int   bSize = ~s.size();\n", [SrcExpr]),
             ?f("    const char    *bData = ~s.data();\n", [SrcExpr]),
             ?f("    data = enif_make_new_binary(\n"), %% can data be NULL??
             ?f("               env,\n"),
             ?f("               bSize,\n"),
             ?f("               &~s);\n", [DestVar]),
             ?f("    memmove(data, bData, bSize);\n"),
             ?f("}\n")];
        {msg, Msg2Name} ->
            UnpackFnName = mk_c_fn(u_msg_, Msg2Name),
            [?f("~s = ~s(env, &~s);\n",
                [DestVar, UnpackFnName, SrcExpr])];
        {map, KeyType, ValueType} ->
            {KeyDest, ValueDest} = DestVar,
            {KeyExpr, ValueExpr} = SrcExpr,
            [format_nif_cc_field_unpacker_by_type(KeyDest, KeyExpr,
                                                  KeyType, Defs),
             format_nif_cc_field_unpacker_by_type(ValueDest, ValueExpr,
                                                  ValueType, Defs)]
    end.

format_nif_cc_field_unpacker_repeated(DestVar, MsgVar, Field, Defs) ->
    #?gpb_field{name=FName, type=FType} = Field,
    LCFName = to_lower(FName),
    [?f("    {\n"),
     ?f("        unsigned int numElems = ~s->~s_size();\n", [MsgVar, LCFName]),
     ?f("        ERL_NIF_TERM relem[numElems];\n"),
     ?f("        unsigned int i;\n"),
     "\n",
     ?f("        for (i = 0; i < numElems; i++)\n"),
     indent_lines(
       12,
       format_nif_cc_field_unpacker_by_type(
         "relem[i]", ?f("~s->~s(i)", [MsgVar, LCFName]),
         FType, Defs)),
     ?f("        ~s = enif_make_list_from_array(env, relem, numElems);\n",
        [DestVar]),
     "    }\n",
     "\n"].

format_nif_cc_field_unpacker_maptype(DestVar, MsgVar, Field, Defs, Opts) ->
    #?gpb_field{name=FName, type={map, KeyType, ValueType}=Type} = Field,
    LCFName = to_lower(FName),
    ItType = mk_cctype_name(Type, Defs) ++ "::const_iterator",
    MapsOrRecords = get_records_or_maps_by_opts(Opts),
    split_indent_iolist(
      4,
      ["{\n",
       split_indent_iolist(
         4,
         case MapsOrRecords of
             records ->
                 [?f("~s = enif_make_list(env, 0);\n", [DestVar]),
                  ?f("int i = 0;\n", [])];
             maps ->
                 ?f("~s = enif_make_new_map(env);\n", [DestVar])
         end),
       %% Iterate
       ?f("    for (~s it = ~s->~s().begin();\n"
          "         it != ~s->~s().end();\n"
          "         ++it)\n",
          [ItType, MsgVar, LCFName, MsgVar, LCFName]),
       "    {\n",
       "        ERL_NIF_TERM ek, ev;\n",
       %% FIXME
       split_indent_iolist(
         8,
         [format_nif_cc_field_unpacker_by_type("ek", "it->first", KeyType,
                                               Defs),
          format_nif_cc_field_unpacker_by_type("ev", "it->second", ValueType,
                                               Defs),
          case MapsOrRecords of
              records ->
                  ["ERL_NIF_TERM eitem = enif_make_tuple2(env, ek, ev);\n",
                   ?f("~s = enif_make_list_cell(env, eitem, ~s);\n",
                      [DestVar, DestVar]),
                   "++i;\n"];
              maps ->
                  [?f("enif_make_map_put(env, ~s, ek, ev, &~s);\n",
                      [DestVar, DestVar])]
          end]),
       "    }\n",
       "}\n"]).


mk_cctype_name({enum,EnumName}, Defs) ->
    EPrefix = case is_dotted(EnumName) of
                  false -> atom_to_list(EnumName);
                  true  -> dot_replace_s(EnumName, "_")
              end,
    CPkg = get_cc_pkg(Defs),
    CPkg ++ "::" ++ EPrefix;
mk_cctype_name({msg,MsgName}, Defs) ->
    CPkg = get_cc_pkg(Defs),
    CPkg ++ "::" ++ dot_replace_s(MsgName, "::");
mk_cctype_name({map,KeyType,ValueType}, Defs) ->
    CKeyType = mk_cctype_name(KeyType, Defs),
    CValueType = mk_cctype_name(ValueType, Defs),
    "::google::protobuf::Map< " ++ CKeyType ++ ", " ++ CValueType ++ " >";
mk_cctype_name(Type, _Defs) ->
    case Type of
        sint32   -> "::google::protobuf::int32";
        sint64   -> "::google::protobuf::int64";
        int32    -> "::google::protobuf::int32";
        int64    -> "::google::protobuf::int64";
        uint32   -> "::google::protobuf::uint32";
        uint64   -> "::google::protobuf::uint64";
        bool     -> "bool";
        fixed64  -> "::google::protobuf::uint64";
        sfixed64 -> "::google::protobuf::int64";
        double   -> "double";
        string   -> "::std::string";
        bytes    -> "::std::string";
        fixed32  -> "::google::protobuf::uint32";
        sfixed32 -> "::google::protobuf::int32";
        float    -> "float"
    end.

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

to_upper(A) when is_atom(A) ->
    list_to_atom(string:to_upper(atom_to_list(A))).

camel_case(A) when is_atom(A) ->
    list_to_atom(camel_case(atom_to_list(A), true)).

-define(is_lower_case(C), $a =< C, C =< $z).
-define(is_upper_case(C), $A =< C, C =< $Z).
-define(is_digit(C),      $0 =< C, C =< $9).
camel_case([LC | Tl], CapNextLetter) when ?is_lower_case(LC) ->
    if CapNextLetter     -> [capitalize_letter(LC) | camel_case(Tl, false)];
       not CapNextLetter -> [LC | camel_case(Tl, false)]
    end;
camel_case([UC | Tl], _) when ?is_upper_case(UC) ->
    [UC | camel_case(Tl, false)];
camel_case([D | Tl], _) when ?is_digit(D) ->
    [D | camel_case(Tl, true)];
camel_case([_ | Tl], _) -> %% underscore and possibly more
    camel_case(Tl, true);
camel_case([], _) ->
    [].

capitalize_letter(C) ->
    C + ($A - $a).

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
    {AttrForms0, CodeForms} = split_forms_at_first_code(Forms),
    % extract export_type and type forms from attribute forms
    AttrForms = lists:filter(fun ({attribute, _, export_type, _}) -> false;
                                 ({attribute, _, type, _}) -> false;
                                 (_) -> true
                             end, AttrForms0),
    TypeForms = AttrForms -- AttrForms0,
    FieldDef = field_record_to_attr_form(),
    OneofDef = oneof_record_to_attr_form(),
    RpcDef   = rpc_record_to_attr_form(),
    RecordBaseDefs = [FieldDef, OneofDef, RpcDef],
    MsgRecordForms = msgdefs_to_record_attrs(MsgDefs),
    AllForms = AttrForms ++ RecordBaseDefs ++ MsgRecordForms ++ TypeForms ++ CodeForms,
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
    record_to_attr(?gpb_field, record_info(fields, ?gpb_field)).

oneof_record_to_attr_form() ->
    record_to_attr(gpb_oneof, record_info(fields, gpb_oneof)).

rpc_record_to_attr_form() ->
    record_to_attr(?gpb_rpc, record_info(fields, ?gpb_rpc)).

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

gpb_field_to_record_field(#?gpb_field{name=FName, opts=Opts}) ->
    case proplists:get_value(default, Opts) of
        undefined -> {FName};
        Default   -> {FName, Default}
    end;
gpb_field_to_record_field(#gpb_oneof{name=FName}) ->
    {FName}.

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
        records ->
            record_update(Var, RName, FieldsValues);
        maps ->
            case get_mapping_and_unset_by_opts(Opts) of
                {maps, present_undefined} -> map_update(Var, FieldsValues);
                {maps, omitted}           -> map_set(Var, FieldsValues)
            end
    end.

get_records_or_maps_by_opts(Opts) ->
    Default = false,
    case proplists:get_value(maps, Opts, Default) of
        false -> records;
        true  -> maps
    end.

get_mapping_and_unset_by_opts(Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            records;
        maps ->
            Default = present_undefined,
            {maps, proplists:get_value(maps_unset_optional, Opts, Default)}
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

%% maps
-ifndef(NO_HAVE_MAPS).
map_match(Fields) ->
    erl_syntax:map_expr(
      [erl_syntax:map_field_exact(erl_syntax:atom(FName), Expr)
       || {FName, Expr} <- Fields]).

map_create(Fields) ->
    map_set(none, Fields).

map_update(Var, []) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_update(Var, FieldsValueTrees) ->
    erl_syntax:map_expr(
      Var,
      [erl_syntax:map_field_exact(erl_syntax:atom(FName), Expr)
       || {FName, Expr} <- FieldsValueTrees]).

map_set(Var, []) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_set(Var, FieldsValueTrees) ->
    erl_syntax:map_expr(
      Var,
      [if is_atom(FName) ->
               erl_syntax:map_field_assoc(erl_syntax:atom(FName), Expr);
          true -> % Key can be a variable or other type too.
               erl_syntax:map_field_assoc(FName, Expr)
       end
       || {FName, Expr} <- FieldsValueTrees]).

-else. %% on a pre Erlang 17 system

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

map_set(Var, []) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_set(Var, FieldsValueTrees) ->
    erl_syntax:text(
      ?ff("~s#{~s}",
          [var_literal(Var),
           string:join([?ff("~p => ~s", [FName, Val])
                        || {FName, Val} <- map_kvalues(FieldsValueTrees)],
                       ", ")])).

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

-endif. %% NO_HAVE_MAPS

find_translation(ElemPath, Op, AnRes) ->
    find_translation(ElemPath, Op, AnRes, undefined).
find_translation(ElemPath, Op, #anres{map_translations=Ts}, Default) ->
    case dict:find(ElemPath, Ts) of
        {ok, OpTransls} ->
            case lists:keyfind(Op, 1, OpTransls) of
                {Op, _Fn} ->
                    mk_tr_fn_name(ElemPath, Op);
                false ->
                    default_fn_by_op(Op, Default)
            end;
        error ->
            default_fn_by_op(Op, Default)
    end.

mk_tr_fn_name([MsgName,FieldName,elem], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s.[elem]", [Op, MsgName,FieldName]));
mk_tr_fn_name([MsgName,FieldName], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s", [Op, MsgName,FieldName])).

default_fn_by_op(decode_repeated_add_elem, undefined) ->
    cons;
default_fn_by_op(decode_repeated_finalize, undefined) ->
    lists_reverse;
default_fn_by_op(_, undefined) ->
    id;
default_fn_by_op(_, Fn) ->
    Fn.

%% The "option allow_alias = true;" inside an enum X { ... }
%% says it is ok to have multiple symbols that map to the same numeric value.
%% Appeared in protobuf 2.5.0.
unalias_enum([{_Sym,Value}=Enum | Rest]) ->
    [Enum | unalias_enum([E || {_,V}=E <- Rest, V /= Value])];
unalias_enum([{option,_Name,_Value} | Rest]) ->
    unalias_enum(Rest);
unalias_enum([]) ->
    [].

var_f_n(N) -> var_n("F", N).
var_b_n(N) -> var_n("B", N).

var_n(S, N) ->
    var("~s~w", [S, N]).

var(Fmt, Args) ->
    erl_syntax:variable(?ff(Fmt, Args)).

prefix_var(Prefix, Var) ->
    erl_syntax:variable(Prefix ++ erl_syntax:variable_literal(Var)).

match_bind_var(Pattern, Var) ->
    ?expr('Pattern' = 'Var',
          [replace_tree('Pattern', Pattern),
           replace_tree('Var', Var)]).

enum_to_binary_fields(Value) ->
    <<N:32/unsigned-native>> = <<Value:32/signed-native>>,
    varint_to_binary_fields(N).

key_to_binary_fields(FNum, Type) ->
    Key = (FNum bsl 3) bor gpb:encode_wiretype(Type),
    varint_to_binary_fields(Key).

varint_to_binary_fields(IntValue) ->
    [erl_syntax:binary_field(?expr('<n>', [replace_term('<n>', N)]), [])
     || N <- binary_to_list(gpb:encode_varint(IntValue))].

get_field_name(#?gpb_field{name=FName}) -> FName;
get_field_name(#gpb_oneof{name=FName})  -> FName.

get_field_occurrence(#?gpb_field{occurrence=Occurrence}) -> Occurrence;
get_field_occurrence(#gpb_oneof{})                       -> optional.

get_field_rnum(#?gpb_field{rnum=RNum}) -> RNum;
get_field_rnum(#gpb_oneof{rnum=RNum})  -> RNum.

%% -> {Optionals, NonOptionals}
key_partition_on_optionality(Key, Items) ->
    lists:partition(fun(Item) ->
                            Field = element(Key, Item),
                            get_field_occurrence(Field) == optional
                    end,
                    Items).

is_packed(#?gpb_field{opts=Opts}) ->
    lists:member(packed, Opts).

%% Given a sequence, `Seq', of expressions, and an initial expression,
%% Construct:
%%     TmpVar1 = InitialExpr,
%%     TmpVar2 = <1st expression in sequence, possibly involving TmpVar1>
%%     TmpVar3 = <2st expression in sequence, possibly involving TmpVar2>
%%     ...
%%     <final expression in sequence, possibly involving TmpVarN-1>
do_exprs(F, InitExpr, Seq) ->
    {LastExpr, ExprsReversed, _N} =
        lists:foldl(
          fun(Elem, {PrevE,Es,N}) ->
                  Var = var_n("S", N),
                  BoundPrevE = assign_to_var(Var, PrevE),
                  E = F(Elem, Var),
                  {E, [BoundPrevE | Es], N+1}
          end,
          {InitExpr, [], 1},
          Seq),
    lists:reverse([LastExpr | ExprsReversed]).

get_field_pass(MsgName, #anres{d_field_pass_method=D}) ->
    dict:fetch(MsgName, D).

get_num_fields(MsgName, #anres{num_fields=D}) ->
    dict:fetch(MsgName, D).

smember(Elem, Set) -> %% set-member
    sets:is_element(Elem, Set).

smember_any(Elems, Set) -> %% is any elem a member in the set
    lists:any(fun(Elem) -> smember(Elem, Set) end,
              Elems).

contains_messages(Defs) ->
    lists:any(fun({{msg, _}, _}) -> true;
                 (_)             -> false
              end,
              Defs).

index_seq([]) -> [];
index_seq(L)  -> lists:zip(lists:seq(1,length(L)), L).

%% lists_replace(N, List, New) -> NewList
%% Like erlang:setelement, but for a list:
%% Replace the Nth element in List with a New value.
lists_setelement(1, [_ | Rest], New) ->
    [New | Rest];
lists_setelement(N, [X | Rest], New) when N > 1 ->
    [X | lists_setelement(N - 1, Rest, New)].

zip4([A|T1], [B|T2], [C|T3], [D|T4]) -> [{A,B,C,D} | zip4(T1, T2, T3, T4)];
zip4([], [], [], [])                 -> [].

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

proto3_type_default(Type, Defs, Opts) ->
    if Type == string ->
            case get_strings_as_binaries_by_opts(Opts) of
                true ->
                    list_to_binary(gpb:proto3_type_default(Type, Defs));
                false ->
                    gpb:proto3_type_default(Type, Defs)
            end;
       Type /= string ->
            gpb:proto3_type_default(Type, Defs)
    end.

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
