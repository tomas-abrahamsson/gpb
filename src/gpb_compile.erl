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
-export([string/2, string/3]).
-export([proto_defs/2, proto_defs/3]).
-export([msg_defs/2, msg_defs/3]).
-export([format_error/1, format_warning/1]).
-export([c/0, c/1, c/2]). % Cmd line interface, halts vm---don't use from shell!
-export([parse_opts_and_args/1]).
-export([show_args/0]).
-export([show_version/0]).
-export([locate_import/2]).
-export([read_import/2]).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2]).

%% -- Types -----------------------------------------------------

%% Options
-type boolean_opt(X) :: X | {X, boolean()}.% Just an option `X' means `{X,true}'
-type directory() :: string().

-type opts() :: [opt()].
-type opt() :: type_specs | {type_specs, boolean() | preferably} |
               {verify, optionally | always | never} |
               {copy_bytes, true | false | auto | integer() | float()} |
               {strings_as_binaries, boolean()} | strings_as_binaries |
               boolean_opt(defs_as_proplists) |
               boolean_opt(descriptor) |
               boolean_opt(maps) |
               boolean_opt(msgs_as_maps) |
               boolean_opt(mapfields_as_maps) |
               boolean_opt(defs_as_maps) |
               {maps_unset_optional, omitted | present_undefined} |
               boolean_opt(nif) |
               {load_nif, string()} |
               {i, directory()} |
               {o, directory()} |
               {o_erl, directory()} | {o_hrl, directory()} |
               {o_nif_cc, directory()} |
               binary | to_proto_defs | to_msg_defs |
               return |
               boolean_opt(return_warnings) | boolean_opt(return_errors) |
               report |
               boolean_opt(report_warnings) | boolean_opt(report_errors) |
               boolean_opt(warnings_as_errors) |
               boolean_opt(include_as_lib) |
               boolean_opt(use_packages) |
               {erlc_compile_options,string()} |
               {msg_name_prefix,
                string() | atom() |
                {by_proto, [{atom(), string() | atom()}]}} |
               {msg_name_suffix, string() | atom()} |
               boolean_opt(msg_name_to_snake_case) |
               boolean_opt(msg_name_to_lower) |
               {module_name_prefix, string() | atom()} |
               {module_name_suffix, string() | atom()} |
               {module_name, string() | atom()} |
               {any_translate, [translation()]} |
               boolean_opt(epb_compatibility) |
               boolean_opt(epb_functions) |
               boolean_opt(defaults_for_omitted_optionals) |
               boolean_opt(type_defaults_for_omitted_optionals) |
               {import_fetcher, import_fetcher_fun()} |
               {target_erlang_version, integer() | current} |
               term().

-type translation() :: {encode, mod_fn_argtemplate()} |
                       {decode, mod_fn_argtemplate()} |
                       {merge,  mod_fn_argtemplate()} |
                       {verify, mod_fn_argtemplate()}.
-type fn_name() :: atom().
-type mod_fn_argtemplate() :: {module(), fn_name(), arg_template()}.
-type arg_template() :: [arg()].
-type arg() :: term() | named_arg().
-type named_arg() :: '$1' | '$2' | '$errorf' | '$user_data' | '$op'.

-type fetcher_ret() :: from_file | {ok, string()} | {error, term()}.
-type import_fetcher_fun() :: fun((string()) -> fetcher_ret()).

%% Compilation return values
-type comp_ret() :: mod_ret() | bin_ret() | error_ret().
-type mod_ret() :: ok | {ok, [warning()]}.
-type bin_ret() :: {ok, module(), code()} |
                   {ok, module(), code(), [warning()]}.
-type error_ret() :: error | {error, reason()} | {error, reason(), [warning()]}.
-type warning() :: term().
-type reason() :: term().
-type code() :: binary() | gpb_parse:defs() | [code_item()].
-type code_item() :: {erl, ErlCode :: binary()} |
                     {nif, NifCcText :: string()}.
-export_type([opts/0, opt/0]).
-export_type([comp_ret/0]).


%% @equiv file(File, [])
-spec file(string()) -> comp_ret().
file(File) ->
    file(File, []).

%% @doc
%% Compile a .proto file to a .erl file and to a .hrl file.
%%
%% The `File' argument must not include path to the .proto file. Example:
%% "SomeDefinitions.proto" is ok, while "/path/to/SomeDefinitions.proto"
%% is not ok.
%%
%% The .proto file is expected to be found in a directories specified by an
%% `{i,directory()}' option. It is possible to specify `{i,directory()}'
%% several times, they will be searched in the order specified.
%%
%% The `type_specs' option enables or disables `::Type()' annotations
%% in the generated .hrl file. Default is `true' if there are no
%% cyclic message dependencies. The default changed in gpb version 4.0.0.
%% Previously, the default was `false'.
%% If you have messages referencing other messages cyclically, and get into
%% troubles when compiling the generated files, set this to `false'.
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
%% Upon encoding, both binaries and iolists are accepted.
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
%% uses maps instead of records. This option expands to the following
%% options:
%% <dl>
%%    <dt>`msgs_as_maps'</dt>
%%    <dd>No `.hrl' file will be generated, and the functions
%%        `encode_msg', `merge_msgs' and `verify_msg' will take the
%%        message name as an additional parameter.</dd>
%%    <dt>`mapfields_as_maps'</dt>
%%    <dd>The value for fields of type `map<_,_>' will be a map
%%        instead of a list of 2-tuples.</dd>
%%    <dt>`defs_as_maps'</dt>
%%    <dd>The introspection will generate message field descriptions
%%        as maps instead of as `#field{}' records, unless, of course
%%        `defs_as_proplists' is specified, in which case they will be
%%        proplists instead.</dd>
%% </dl>
%%
%% For messages as maps, for optional fields, if not set, the
%% `maps_unset_optional' option specifies the Erlang-internal
%% representation; both how it is expected to be found at encoding,
%% and how decoding will return it:
%% <dl>
%%   <dt>`omitted'</dt>
%%   <dd>This means it is not included in the map.
%%       This is the default. (since gpb version 4.0.0)
%%   </dd>
%%   <dt>`present_undefined'</dt>
%%   <dd>This means it is present and has the value `undefined'.
%%       This <em>was</em> the default before gpb version 4.0.0.
%%   </dd>
%% </dl>
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
%% works correspondingly. For the case of compatibility with Erlang Protobuffs,
%% the `epb_compatibility' option implies `{module_name_suffix,"_pb"}'
%%
%% The `{module_name,Name}' can be used to specify the module name of the
%% generated code freely, instead of basing it on the proto file name.
%% The name specified with `module_name' can be prefixed and suffixed with
%% the `module_name_prefix' and `module_name_suffix' options.

%% The `any_translate' option can be used to provide packer and
%% unpacker functions for `google.protobuf.Any' messages.  The merge
%% translator is optional, and is called either via the `merge_msgs'
%% function in the generated code, or when the decoder sees another
%% `Any' message. The default merge operation is to let the second
%% element overwrite previous elements. The verify translator is
%% optional too, since verification can be disabled.
%% The translation calls are specified as `{Mod,Fn,ArgTemplate}' where
%% `Mod',`Fn' is a module and function to call, `ArgTemplate' is a list
%% of terms, containing markers, such as `$1', `$2' and so on, for where
%% to place the actual args. This makes it possible to specify additional
%% static argument terms, for instance.
%% The translator functions are called as follows:
%% <dl>
%%   <dt>Encode (Packing)</dt>
%%   <dd>Call `Mod:Fn(Term)' to pack the `Term' (`$1') to
%%       a `google.protobuf.Any' message.</dd>
%%   <dt>Decode (Unpacking)</dt>
%%   <dd>Call `Mod:Fn(Any)' to unpack the `Any' (`$1') to
%%       unpack a `google.protobuf.Any' message to a term.</dd>
%%   <dt>Merge </dt>
%%   <dd>Call `Mod:Fn(Term1, Term2) -> Term3' to merge two
%%       unpacked terms to a resulting Term3. The `$1' is the
%%       previously seen term (during decoding, on encountering a
%%       second `Any' field), or the first argument to the
%%       `merge_msgs' function. The `$2' is the lastly seen term, or
%%       the second argument to the `merge_msgs' function.</dd>
%%   <dt>Verify</dt>
%%   <dd>Call `Mod:Fn(Term) -> _' to verify an unpacked `Term'.
%%       If `Term' (`$1') is valid, the function is expected to just return
%%       any value, which is ignored and discarded.
%%       If `Term' is invalid, the function is exptected to not
%%       return anything, but instead either crash, call
%%       `erlang:error/1', or `throw/1' or `exit/1'.  with the
%%       reason for error.
%%       (For backwards compatibility, it is also possible
%%       to have an error function as argument, using `$errorf',
%%       but this is deprecated.)</dd>
%% </dl>
%% There are additional translator argument markers:
%% <dl>
%%   <dt>`$user_data'</dt>
%%   <dd>This will be replaced by the `user_data' option to the
%%     generated `encode_msg', `decode_msg', `merge_msgs' and
%%     `verify_msg' functions. If that option is not specified, the
%%     value `undefined' is used substituted for `$user_data'.</dd>
%%   <dt>`$op'</dt>
%%   <dd>This will be replaced by `encode', `decode', `merge' or
%%   `verify', depending on from which context it is actually
%%   called. This can be useful because if the message is to be
%%   verified on encoding (see the `verify' option), then the same
%%   options, and thus the same user-data, are used for both
%%   `encode_msg' and for `verify_msg'. The `$op' marker makes it
%%   possible to tell these two call sites apart, if needed.</dd>
%% </dl>
%%
%% The `epb_compatibility' option is an umbrella-option for
%% compatibility with the Erlang protobuffs library. It will expand to
%% the options below. It will expand in-place, meaning any of these
%% can be overridden if specified before the `epb_compatibility'
%% option.
%% <ul>
%%   <li>`epb_functions'</li>
%%   <li>`defaults_for_omitted_optionals'</li>
%%   <li>`{module_name_suffix,"_pb"}'</li>
%%   <li>`{msg_name_to_lower,true}'</li>
%% </ul>
%%
%% If the `epb_functions' option is specified, then for compatibility
%% with Erlang protobuffs, the following functions will be generated:
%% <ul>
%%   <li>`encode/1'</li>
%%   <li>`encode_<MsgName>/1'</li>
%%   <li>`decode/2'</li>
%%   <li>`decode_<MsgName>/1'</li>
%% </ul>
%%
%% The `defaults_for_omitted_optionals' and
%% `type_defaults_for_omitted_optionals' options generates code that
%% set default values or type-defaults respectively, on decoding, if
%% an optional field is not present in the binary to decode. Normally
%% it would otherwise have been set to `undefined'. Note that with
%% these options it is not possible to determine after decoding
%% whether a field contained data in the binary message. Also note
%% that these options are only applicable for proto2 syntax messages,
%% and are ignored for proto3 syntax messages. (For proto3, it
%% effectively <em>must</em> be ignored, since, on the wire, a field
%% set to its type-default value is indistinguishable from an omitted
%% value.)
%%
%% The `import_fetcher' option can be used to catch imports. The
%% option value must be a function taking one argument, the name of
%% the file to import. It must return either `from_file', letting this
%% file pass through the normal file import, or `{ok,string()}' if it
%% has fetched the file itself, or `{error,term()}'.
%%
%% The `target_erlang_version` can be used to specify another major
%% version of Erlang/OTP to generate code for. The default, `current'
%% means that the generated code is expected to be compiled and run
%% on the same major version as gpb runs on.
-spec file(string(), opts()) -> comp_ret().
file(File, Opts) ->
    do_file_or_string(File, Opts).

%% @equiv string(Mod, Str, [])
-spec string(module(), string()) -> comp_ret().
string(Mod, Str) ->
    string(Mod, Str, []).

%% @doc
%% Compile a `.proto' file as string. See {@link file/2} for information
%% on options and return values.
-spec string(module(), string(), opts()) -> comp_ret().
string(Mod, Str, Opts) ->
    do_file_or_string({Mod, Str}, Opts).

do_file_or_string(In, Opts0) ->
    Opts1 = normalize_opts(Opts0),
    case parse_file_or_string(In, Opts1) of
        {ok, Defs} ->
            Mod = find_out_mod(In, Opts1),
            DefaultOutDir = find_default_out_dir(In),
            Opts2 = Opts1 ++ [{o,DefaultOutDir}],
            do_proto_defs_aux1(Mod, Defs, Opts2);
        {error, Reason} = Error ->
            possibly_report_error(Error, Opts1),
            case proplists:get_bool(return_warnings, Opts1) of
                true  -> {error, Reason, []};
                false -> Error
            end
    end.

normalize_opts(Opts0) ->
    normalize_return_report_opts(
      normalize_alias_opts(Opts0)).

normalize_alias_opts(Opts) ->
    lists:foldl(fun(F, OptsAcc) -> F(OptsAcc) end,
                Opts,
                [fun norm_opt_alias_to_msg_proto_defs/1,
                 fun norm_opt_epb_compat_opt/1,
                 fun norm_opt_map_opts/1]).

norm_opt_alias_to_msg_proto_defs(Opts) ->
    lists:map(fun(to_msg_defs)         -> to_proto_defs;
                 ({to_msg_defs, Bool}) -> {to_proto_defs, Bool};
                 (Opt)                 -> Opt
              end,
              Opts).

norm_opt_epb_compat_opt(Opts) ->
    proplists:expand(
      [{epb_compatibility, [epb_functions,
                            defaults_for_omitted_optionals,
                            {module_name_suffix,"_pb"},
                            {msg_name_to_lower, true}]},
       {{epb_compatibility,false}, [{epb_functions,false},
                                    {defaults_for_omitted_optionals,false}]}],
      Opts).

norm_opt_map_opts(Opts) ->
    proplists:expand(
      [{maps, [msgs_as_maps,
               mapfields_as_maps,
               defs_as_maps]},
       {{maps,false}, [{msgs_as_maps, false},
                       {mapfields_as_maps, false},
                       {defs_as_maps, false}]}],
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

find_out_mod({Mod, _S}, _Opts) ->
    Mod;
find_out_mod(File, Opts) ->
    Ext = filename:extension(File),
    list_to_atom(possibly_suffix_mod(
                   possibly_prefix_mod(
                     mod_name_from_opts_or_else_filename(
                       filename:basename(File, Ext),
                       Opts),
                     Opts),
                   Opts)).

mod_name_from_opts_or_else_filename(FileBaseName, Opts) ->
    proplists:get_value(module_name, Opts, FileBaseName).

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

find_default_out_dir({_Mod, _S}) -> ".";
find_default_out_dir(File) -> filename:dirname(File).

%% @equiv proto_defs(Mod, Defs, [])
-spec proto_defs(module(), gpb_parse:defs()) -> comp_ret().
proto_defs(Mod, Defs) ->
    proto_defs(Mod, Defs, []).

%% @doc
%% Compile a list of pre-parsed definitions to file or to a binary.
%% See {@link file/2} for information on options and return values.
-spec proto_defs(module(), gpb_parse:defs(), opts()) -> comp_ret().
proto_defs(Mod, Defs, Opts) ->
    do_proto_defs_aux1(Mod, Defs, normalize_opts(Opts)).

do_proto_defs_aux1(Mod, Defs0, Opts0) ->
    {IsAcyclic, Defs} = try_topsort_defs(Defs0),
    possibly_probe_defs(Defs, Opts0),
    Warns0 = check_unpackables_marked_as_packed(Defs),
    {Warns1, Opts1} = possibly_adjust_typespec_opt(IsAcyclic, Opts0),
    Warns = Warns0 ++ Warns1,
    AnRes = gpb_analyzer:analyze_defs(Defs, Opts1),
    case verify_opts(Defs, Opts1) of
        ok ->
            Res1 = do_proto_defs_aux2(Defs, clean_module_name(Mod), AnRes,
                                      Opts1),
            return_or_report_warnings_or_errors(Res1, Warns, Opts1,
                                                get_output_format(Opts1));
        {error, OptError} ->
            return_or_report_warnings_or_errors({error, OptError}, [], Opts1,
                                                get_output_format(Opts1))
    end.

verify_opts(Defs, Opts) ->
    while_ok([fun() -> verify_opts_any_translate_and_nif(Opts) end,
              fun() -> verify_opts_epb_compat(Defs, Opts) end]).

while_ok(Funs) ->
    lists:foldl(fun(F, ok) -> F();
                   (_, Err) -> Err
                end,
                ok,
                Funs).

verify_opts_any_translate_and_nif(Opts) ->
    case {proplists:get_value(any_translate, Opts),
          proplists:get_bool(nif, Opts)} of
        {Translations, true} when Translations /= undefined ->
            {error, {invalid_options,any_translate,nif}};
        _ ->
            ok
    end.

verify_opts_epb_compat(Defs, Opts) ->
    while_ok(
      [fun() ->
               case {proplists:get_bool(epb_functions, Opts),
                     gpb_lib:get_records_or_maps_by_opts(Opts)} of
                   {true, maps} ->
                       {error, {invalid_options, epb_functions,maps}};
                   _ ->
                       ok
               end
       end,
       fun() ->
               case proplists:get_bool(epb_functions, Opts) of
                   true ->
                       case lists:member(msg, gpb_lib:msg_names(Defs)) of
                           true ->
                               {error, {epb_functions_impossible,
                                        {with_msg_named,msg}}};
                           false ->
                               ok
                       end;
                   false ->
                       ok
               end
       end]).

%% @equiv msg_defs(Mod, Defs, [])
%% @doc Deprecated, use proto_defs/2 instead.
-spec msg_defs(module(), gpb_parse:defs()) -> comp_ret().
msg_defs(Mod, Defs) ->
    msg_defs(Mod, Defs, []).

%% @spec msg_defs(Mod, Defs, Opts) -> CompRet
%% @equiv proto_defs(Mod, Defs, Opts)
%% @doc Deprecated, use proto_defs/2 instead.
-spec msg_defs(module(), gpb_parse:defs(), opts()) -> comp_ret().
msg_defs(Mod, Defs, Opts) ->
    proto_defs(Mod, Defs, Opts).

do_proto_defs_aux2(Defs, Mod, AnRes, Opts) ->
    case get_output_format(Opts) of
        proto_defs ->
            {ok, Defs};
        binary ->
            ErlTxt = format_erl(Mod, Defs, AnRes, Opts),
            HrlTxt = possibly_format_hrl(Mod, Defs, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            compile_to_binary(Mod, HrlTxt, ErlTxt, NifTxt, Opts);
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
-spec format_error(Err) -> iolist() when
      Err :: reason() | {error, reason()} | {error, reason(), [warning()]}.
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
fmt_err({import_not_found, Import, Tried}) ->
    PrettyTried = [begin
                       PrettyReason = file:format_error(Reason),
                       ?f("~n  ~ts (~s (~p))", [File,PrettyReason,Reason])
                   end
                   || {File,Reason} <- Tried],
    TriedTxt = if Tried == [] -> "";
                  true -> ", tried:"
               end,
    ?f("Could not find import file ~p~s~s", [Import, TriedTxt, PrettyTried]);
fmt_err({fetcher_issue, File, Reason}) ->
    ?f("Failed to import file ~p using fetcher, ~p", [File, Reason]);
fmt_err({read_failed, File, Reason}) ->
    ?f("failed to read ~p: ~s (~p)", [File, file:format_error(Reason), Reason]);
fmt_err({post_process, Reasons}) ->
    gpb_parse:format_post_process_error({error, Reasons});
fmt_err({write_failed, File, Reason}) ->
    ?f("failed to write ~s: ~s (~p)", [File, file:format_error(Reason),Reason]);
fmt_err({invalid_options,any_translate,nif}) ->
    "Option error: Not supported: both any_translate and nif";
fmt_err({invalid_options, epb_functions, maps}) ->
    "Option error: Not supported: both epb_compatibility (or epb_functions) "
        "and maps";
fmt_err({epb_compatibility_impossible, {with_msg_named, msg}}) ->
    "Not possible to generate epb compatible functions when a message "
        "is named 'msg' because of collision with the standard gpb functions "
        "'encode_msg' and 'decode_msg'";
fmt_err(X) ->
    ?f("Unexpected error ~p", [X]).

%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link proto_defs/2}.
%% @end
%% Note: do NOT include trailing newline (\n or ~n)
-spec format_warning(warning()) -> iolist().
format_warning(cyclic_message_dependencies) ->
    ?f("Warning: omitting type specs due to cyclic message references.");
format_warning({ignored_field_opt_packed_for_unpackable_type,
                MsgName, FName, Type, _Opts}) ->
    ?f("Warning: ignoring option packed for non-packable field ~s.~s "
       "of type ~p", [MsgName, FName, Type]);
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
%% The options below are supported. Dashes and underscores inside option names
%% are equivalent, ie `-o-erl' and `-o_erl' are the same option.
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
%%   <dt>`-any_translate MsFs'</dt>
%%   <dd>Call functions in `MsFs' to pack, unpack, merge and verify
%%       `google.protobuf.Any' messages. The `MsFs' is a string on the
%%       following format: `e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,V=Mod:Fn]'.
%%       The specified modules and functinos are called and used as follows:
%%       <dl>
%%         <dt>e=Mod:Fn</dt>
%%         <dd>Call `Mod:Fn(Term)' to pack the `Term' to
%%             a `google.protobuf.Any' message.</dd>
%%         <dt>d=Mod:Fn</dt>
%%         <dd>Call `Mod:Fn(Any)' to unpack the `Any' to
%%             unpack a `google.protobuf.Any' message to a term.</dd>
%%         <dt>m=Mod:Fn</dt>
%%         <dd>Call `Mod:Fn(Term1, Term2) -> Term3' to merge two
%%             unpacked terms to a resulting Term3.</dd>
%%         <dt>V=Mod:Fn</dt>
%%         <dd>Call `Mod:Fn(Term) -> _' to verify an unpacked `Term'.
%%             If `Term' is valid, the function is expected to just return
%%             any value, which is ignored and discarded.
%%             If `Term' is invalid, the function is exptected to not
%%             return anything, but instead either crash, call
%%             `erlang:error/1', or `throw/1' or `exit/1'.  with the
%%             reason for error.
%%             If you want to use a verifier, this is the new preferred
%%             approach.</dd>
%%         <dt>v=Mod:Fn</dt>
%%         <dd>Call `Mod:Fn(Term, ErrorF) -> _' to verify an unpacked `Term'.
%%             This exists for backwards compatibility, and its use
%%             is deprecated.</dd>.
%%       </dl>
%%   </dd>
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
%%   <dt>`-modname Name'</dt>
%%   <dd>Specify the name of the generated module.</dd>
%%   <dt>`-msgtolower'</dt>
%%   <dd>ToLower each message. Any prefixes/suffixes are added
%%       after case modification.</dd>
%%   <dt>`-il'</dt>
%%   <dd>Generate code that include gpb.hrl using `-include_lib'
%%       instead of `-include', which is the default.</dd>
%%   <dt>`-type'<br/>`-no_type'</dt>
%%   <dd>Enables or disables `::Type()' annotations in the generated code.
%%       Default is to enable if there are no cyclic dependencies.</dd>
%%   <dt>`-descr'</dt>
%%   <dd>Generate self-description information.</dd>
%%   <dt>`-maps'</dt>
%%   <dd>This option expands to the following options:
%%       <ul>
%%         <li>`-msgs-as-maps'</li>
%%         <li>`-mapfields-as-maps'</li>
%%         <li>`-defs-as-maps'</li>
%%       </ul>
%%       See the `maps' option for the function {@link file/2}
%%       for more info.</dd>
%%   <dt>`-maps_unset_optional omitted | present_undefined'</dt>
%%   <dd>Specifies the internal format for optional fields that are unset.</dd>
%%   <dt>`-msgs-as-maps'</dt>
%%   <dd>Specifies that messages should be maps. No `.hrl' file will
%%       be generated.
%%       Without this option, messages will be records.</dd>
%%   <dt>`-mapfields-as-maps'</dt>
%%   <dd>Specifies that fields of type `map<_,_>' should be maps.
%%       Otherwise, they will be 2-tuples.</dd>
%%   <dt>`-defs-as-maps'</dt>
%%   <dd>Specifies that proto defintions from the generated code
%%       are to be returned as maps. Otherwise, they will be lists
%%       of tuples and records (or proplists if the `-pldefs' option
%%       is specified)</dd>
%%   <dt>`-erlc_compile_options Options'</dt>
%%   <dd>Specifies compilation options, in a comma separated string, to pass
%%       along to the `-compile(...)' directive on the generated code.</dd>
%%   <dt>`-epb'</dt>
%%   <dd>Enable compatibility with the Erlang Protobuffs library:
%%       <ul>
%%         <li>Implies the `-epb-functions' option</li>
%%         <li>Implies the `-defaults-for-omitted-optionals' option</li>
%%         <li>Implies the `-modsuffix _pb' option</li>
%%         <li>Implies the `-msgtolower' option</li>
%%       </ul></dd>
%%   <dt>`-epb-functions'</dt>
%%   <dd>For compatibility with the Erlang Protobuffs library, generate also
%%       the following functions: `encode/1', `decode/2', `encode_MsgName/1'
%%       and `decode_MsgName/1'</dd>
%%   <dt>`-defaults-for-omitted-optionals'</dt>
%%   <dd>For optional fields not present on decoding, set the field to
%%       its default value, if any, instead of to `undefined'.</dd>
%%   <dt>`-type-defaults-for-omitted-optionals'</dt>
%%   <dd>For optional fields not present on decoding, set the field to
%%       its type-default, instead of to `undefined'.</dd>
%%   <dt>`-for-version N'</dt>
%%   <dd>Generate code for Erlang/OTP version N instead of current.</dd>
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
-spec c(opts(), [ProtoFileName::string()]) -> no_return().
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

-spec parse_opts_and_args([string()]) -> {ok, {opts(), Args::[string()]}} |
                                         {error, Reason::string()}.
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
    lists:prefix(norm_uscore_dash(OptName), norm_uscore_dash(Opt));
opt_matches(Opt, {OptName, _Type, _OptTag, _Descr}) ->
    norm_uscore_dash(Opt) == norm_uscore_dash(OptName).

norm_uscore_dash("_"++Tl)  -> "-" ++ norm_uscore_dash(Tl);
norm_uscore_dash([C | Tl]) -> [C | norm_uscore_dash(Tl)];
norm_uscore_dash("")       -> "".

parse_opt(Opt, {OptName, 'string_maybe_appended()', OptTag, _Descr}, Rest) ->
    case {Opt, Rest} of
        {OptName, [H | Rest2]} ->
            {ok, {{OptTag, H}, Rest2}};
        {OptName, []} ->
            {error, "Missing argument for option -" ++ OptName};
        _ ->
            true = lists:prefix(OptName, Opt),
            OptArg = gpb_lib:string_slice(Opt, length(OptName)),
            {ok, {{OptTag, OptArg}, Rest}}
    end;
parse_opt(_, {_OptName, undefined, OptTag, _Descr}, Rest) ->
    {ok, {OptTag, Rest}};
parse_opt(_, {_OptName, 'string()', OptTag, _Descr}, [OptArg | Rest]) ->
    {ok, {{OptTag, OptArg}, Rest}};
parse_opt(_, {OptName, 'integer()', OptTag, _Descr}, [OptArg | Rest]) ->
    try list_to_integer(OptArg) of
        N -> {ok, {{OptTag, N}, Rest}}
    catch error:badarg ->
            {error, ?ff("Invalid version number (integer) for ~s: ~p",
                        [OptName, OptArg])}
    end;
parse_opt(_, {_OptName, F, OptTag, _Descr}, Rest) when is_function(F) ->
    F(OptTag, Rest);
parse_opt(_, {OptName, Alternatives, OptTag, _Descr}, [OptArg | Rest]) ->
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
      " true | false | auto | number()\n"
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
     {"any_translate", fun opt_any_translate/2, any_translate,
      " e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]\n"
      "       For a google.protobuf.Any message, call Mod:Fn to:\n"
      "       - encode (calls Mod:Fn(Term) -> AnyMessage to pack)\n"
      "       - decode (calls Mod:Fn(AnyMessage) -> Term to unpack)\n"
      "       - merge  (calls Mod:Fn(Term,Term2) -> Term3 to merge unpacked)\n"
      "       - verify (calls Mod:Fn(Term) -> _ to verify unpacked)\n"},
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
     {"modname", 'string()', module_name, "Name\n"
      "       Specify the name of the generated module.\n"},
     {"il", undefined, include_as_lib, "\n"
      "       Generate code that includes gpb.hrl using -include_lib\n"
      "       instead of -include, which is the default.\n"},
     {"type", undefined, type_specs, "\n"
      "       Enables `::Type()' annotations in the generated code.\n"},
     {"no_type", fun opt_no_type_specs/2, type_specs, "\n"
      "       Disbles `::Type()' annotations in the generated code.\n"},
     {"descr", undefined, descriptor, "\n"
      "       Generate self-description information.\n"},
     {"maps", undefined, maps, "\n"
      "       This will expand to the following options:\n"
      "         -msgs-as-maps\n"
      "         -msgfields-as-maps\n"
      "         -defs-as-maps\n"},
     {"maps_unset_optional", {omitted, present_undefined}, maps_unset_optional,
      "omitted | present_undefined\n"
      "       Specifies the internal format for optional fields\n"
      "       that are unset.\n"},
     {"msgs-as-maps", undefined, msgs_as_maps, "\n"
      "        Specifies that messages should be maps.\n"
      "        Otherwise, they will be records.\n"},
     {"mapfields-as-maps", undefined, mapfields_as_maps, "\n"
      "        Specifies that fields of type map<_,_> should be maps.\n"
      "        Otherwise, they will be 2-tuples.\n"},
     {"defs-as-maps", undefined, defs_as_maps, "\n"
      "        Specifies that proto defintions from the generated code\n"
      "        are to be returned as maps. Otherwise, they will be lists\n"
      "        of tuples and records (or proplists if the -pldefs option\n"
      "        is specified)\n"},
     {"erlc_compile_options", 'string()', erlc_compile_options, "String\n"
      "       Specifies compilation options, in a comma separated string, to\n"
      "       pass along to the -compile() directive on the generated code.\n"},
     {"epb", undefined, epb_compatibility, "\n"
      "       Enable compatibility with the Erlang Protobuffs library:\n"
      "       * Implies the -epb-functions option\n"
      "       * Implies the -modsuffix _pb option\n"
      "       * Implies the -msgtolower option\n"},
     {"epb-functions", undefined, epb_functions, "\n"
      "       Generate some functions for API compatibility with the\n"
      "       Erlang protobuffs library:\n"
      "       * encode/1 and encode_MsgName/1\n"
      "       * decode/2 and decode_MsgName/1\n"},
     {"defaults-for-omitted-optionals", undefined,
      defaults_for_omitted_optionals, "\n"
      "       For optional fields not present on decoding, set the field\n"
      "       to its default value, if any, instead of to undefined.\n"},
     {"type-defaults-for-omitted-optionals", undefined,
      type_defaults_for_omitted_optionals, "\n"
      "       For optional fields not present on decoding, set the field\n"
      "       to its type-default, instead of to undefined.\n"},
     {"for-version", 'integer()', target_erlang_version, "N\n"
      "       Generate code for Erlang/OTP version N instead of current.\n"},
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
    ] ++
        case os:getenv("GPB_DEV_OPTS") of
            "true" ->
                [{"fp", {pass_as_params,pass_as_record}, field_pass_method,
                  "pass_as_params | pass_as_record\n"
                  "        Override whether message fields are to be passed\n"
                  "        as parameters or as a record (or map, depending\n"
                  "        on the -maps option).  This is purely internal,\n"
                  "        and has no impact neither on input nor output,\n"
                  "        but there may be a performance difference.\n"
                  "        Normally, it is calculated automatically for each\n"
                  "        message, but during development it may be useful\n"
                  "        to be able to force it.\n"}];
            _ ->
                []
        end.



opt_no_type_specs(OptTag, Rest) ->
    Opt = {OptTag, false},
    {ok, {Opt, Rest}}.

opt_any_translate(OptTag, [S | Rest]) ->
    try
        Ts = gpb_lib:string_lexemes(S, ","),
        Opt = {OptTag, [opt_any_translate_mfa(T) || T <- Ts]},
        {ok, {Opt, Rest}}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_any_translate_mfa("e="++MF) -> {encode,opt_mf_str(MF, 1)};
opt_any_translate_mfa("d="++MF) -> {decode,opt_mf_str(MF, 1)};
opt_any_translate_mfa("m="++MF) -> {merge, opt_mf_str(MF, 2)};
opt_any_translate_mfa("V="++MF) -> {verify,opt_mf_str(MF, 1)};
opt_any_translate_mfa("v="++MF) -> {verify,opt_mf_str_verify(MF)};
opt_any_translate_mfa(X) -> throw({badopt,"Invalid translation spec: "++X}).

opt_mf_str(S, Arity) ->
    case gpb_lib:string_lexemes(S, ":") of
        [M,F] -> {list_to_atom(M),list_to_atom(F),opt_arg_template(Arity)};
        _     -> throw({badopt,"Invalid Mod:Fn spec: "++S})
    end.

opt_mf_str_verify(S) ->
    {M,F,[A]} = opt_mf_str(S, 1),
    {M,F,[A,'$errorf']}.

opt_arg_template(Arity) ->
    [list_to_atom(?ff("$~w", [I])) || I <- lists:seq(1,Arity)].

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

-spec show_args() -> _. % side effect is to print valid opts/args
show_args() ->
    io:format(
      "Recognized gpb-opts: (see the edoc for ~p for further details)~n",
      [?MODULE]),
    lists:foreach(fun show_arg/1, opt_specs()).

-spec show_version() -> _. % side effect is to print version
show_version() ->
    io:format("gpb version ~s~n", [gpb:version_as_string()]).

string_to_number(S) ->
    try {ok, list_to_integer(S)}
    catch error:badarg ->
            try {ok, list_to_float(S)}
            catch error:badarg -> error
            end
    end.

parse_file_or_string(In, Opts) ->
    Opts1 = add_curr_dir_as_include_if_needed(Opts),
    case parse_file_and_imports(In, Opts1) of
        {ok, {Defs1, _AllImported}} ->
            case gpb_parse:post_process_all_files(Defs1, Opts1) of
                {ok, Defs2} ->
                    {ok, Defs2};
                {error, Reasons} ->
                    {error, {post_process, Reasons}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

add_curr_dir_as_include_if_needed(Opts) ->
    ImportDirs = [Dir || {i,Dir} <- Opts],
    case lists:member(".", ImportDirs) of
        true  -> Opts;
        false -> Opts ++ [{i,"."}]
    end.

parse_file_and_imports(In, Opts) ->
    FName = file_name_from_input(In),
    parse_file_and_imports(In, [FName], Opts).

file_name_from_input({Mod,_S}) -> lists:concat([Mod, ".proto"]);
file_name_from_input(FName)    -> FName.

parse_file_and_imports(In, AlreadyImported, Opts) ->
    case locate_read_import_int(In, Opts) of
        {ok, Contents} ->
            %% Add to AlreadyImported to prevent trying to import it again: in
            %% case we get an error we don't want to try to reprocess it later
            %% (in case it is multiply imported) and get the error again.
            FName = file_name_from_input(In),
            AlreadyImported2 = [FName | AlreadyImported],
            case scan_and_parse_string(Contents, FName, Opts) of
                {ok, Defs} ->
                    Imports = gpb_parse:fetch_imports(Defs),
                    Opts2 = ensure_include_path_to_wellknown_types_if_proto3(
                              Defs, Imports, Opts),
                    read_and_parse_imports(Imports, AlreadyImported2,
                                           Defs, Opts2);
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
                {ok, PTree} ->
                    case gpb_parse:post_process_one_file(FName, PTree, Opts) of
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

locate_read_import_int({_Mod, Str}, _Opts) ->
    {ok, Str};
locate_read_import_int(Import, Opts) ->
    case proplists:get_value(import_fetcher, Opts) of
        undefined ->
            locate_read_import_aux(Import, Opts);
        Importer when is_function(Importer, 1) ->
            case Importer(Import) of
                from_file ->
                    locate_read_import_aux(Import, Opts);
                {ok, Contents} when is_list(Contents) ->
                    case lists:all(fun is_integer/1, Contents) of
                        true ->
                            {ok, Contents};
                        false ->
                            error({bad_fetcher_return,
                                   {not_a_string, Contents},
                                   Import})
                    end;
                {error, Reason} ->
                    {error, {fetcher_issue, Import, Reason}};
                X ->
                    error({bad_fetcher_return, Import, X})
            end
    end.


locate_read_import_aux(Import, Opts) ->
    ImportPaths = [Path || {i, Path} <- Opts],
    case locate_import_aux(ImportPaths, Import, Opts, []) of
        {ok, File} ->
            read_import(File, Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc Locate an import target.  This function might be potentially
%% useful for instance in an intercepting `import_fetcher' fun that
%% just wants to record the accessed imports.
-spec locate_import(string(), opts()) -> {ok, File::string()} |
                                         {error, reason()}.
locate_import(ProtoFileName, Opts) ->
    Opts1 = ensure_include_path_to_wellknown_types(Opts),
    ImportPaths = [Path || {i, Path} <- Opts1],
    locate_import_aux(ImportPaths, ProtoFileName, Opts1, []).

locate_import_aux([Path | Rest], Import, Opts, Tried) ->
    File = filename:join(Path, Import),
    case file_read_file_info(File, Opts) of
        {ok, #file_info{access = A}} when A == read; A == read_write ->
            {ok, File};
        {ok, #file_info{}} ->
            locate_import_aux(Rest, Import, Opts, Tried);
        {error, Reason} ->
            locate_import_aux(Rest, Import, Opts, [{File,Reason} | Tried])
    end;
locate_import_aux([], Import, _Opts, Tried) ->
    {error, {import_not_found, Import, Tried}}.

%% @doc Read an import file.  This function might be potentially
%% useful for instance in an intercepting `import_fetcher' fun that
%% just wants to record the accessed imports.
-spec read_import(string(), opts()) -> {ok, string()} | {error, reason()}.
read_import(File, Opts) ->
    case file_read_file(File, Opts) of
        {ok,B} ->
            case utf8_decode(B) of
                {ok, {utf8, S}} ->
                    {ok, S};
                {ok, {latin1, S}} ->
                    {ok, S};
                {error, Reason} ->
                    {error, {utf8_decode_failed, Reason, File}}
            end;
        {error, Reason} ->
            {error, {read_failed, File, Reason}}
    end.

ensure_include_path_to_wellknown_types_if_proto3(Defs, Imports, Opts) ->
    case proplists:get_value(syntax, Defs) of
        "proto3" ->
            case lists:any(fun imports_wellknown/1, Imports) of
                true ->
                    ensure_include_path_to_wellknown_types(Opts);
                false ->
                    Opts
            end;
        _ ->
            Opts
    end.

ensure_include_path_to_wellknown_types(Opts) ->
    PrivDir = get_priv_dir(),
    Wellknown = filename:join(PrivDir, "proto3"),
    sanity_check_installation_wellknown_proto3(Wellknown),
    add_opt_unless_present({i,Wellknown}, Opts).

imports_wellknown("google/protobuf/"++_) -> true;
imports_wellknown(_) -> false.

add_opt_unless_present(Opt, [Opt | Rest]) ->
    [Opt | Rest];
add_opt_unless_present(Opt, [H | Rest]) ->
    [H | add_opt_unless_present(Opt, Rest)];
add_opt_unless_present(Opt, []) ->
    [Opt].

get_priv_dir() ->
    case application:get_application(?MODULE) of
        {ok,CurrApp} ->
            code:priv_dir(CurrApp);
        undefined ->
            %% Not loaded as an application, just executing code;
            %% from an escript possibly? (or even from an ez archive?)
            MDir = filename:dirname(code:which(?MODULE)),
            case filename:basename(MDir) of
                "ebin" ->
                    filename:join(filename:dirname(MDir), "priv");
                _ ->
                    case code:priv_dir(gpb) of % hard-wired app name...
                        Dir when is_list(Dir) ->
                            Dir;
                        {error,Reason} ->
                            error({failed_to_locate_privdir,Reason})
                    end
            end
    end.

sanity_check_installation_wellknown_proto3(WellknownDir) ->
    case filelib:is_dir(WellknownDir) of
        true ->
            ok;
        false ->
            error({well_known_proto3_missing,
                   "Your installation is missing the priv/proto3 "
                   "directory, which is expected to house the "
                   "'proto3 well known types' such as "
                   "google/protobuf/timestamp.proto and "
                   "google/protobuf/duration.proto. "
                   "They were expected (calculated) to be found in "
                    ++ WellknownDir})
    end.

try_topsort_defs(Defs) ->
    G = digraph:new(),
    %% Build a dependency graph {msg|group, Name} -> {msg|group,Name}
    [digraph:add_vertex(G, _Key={Type,Name})
     || {Type,Name,_Fields} <- gpb_lib:msgs_or_groups(Defs)],
    gpb_lib:fold_msg_or_group_fields(
      fun(Type, From, #?gpb_field{type={msg,To}}, _) ->
              digraph:add_edge(G, {Type,From}, {msg,To});
         (Type, From, #?gpb_field{type={group,To}}, _) ->
              digraph:add_edge(G, {Type,From}, {group,To});
         (Type, From, #?gpb_field{type={map,_,{msg, To}}}, _) ->
              digraph:add_edge(G, {Type,From}, {msg,To});
         (_Type, _MsgName, _Feild, _Acc) ->
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
            OrderedMsgOrGroupDefs =
                [lists:keyfind(Key,1,Defs) || Key <- ROrder],
            {true, OrderedMsgOrGroupDefs ++ (Defs -- OrderedMsgOrGroupDefs)}
    end.

possibly_adjust_typespec_opt(IsAcyclic, Opts0) ->
    CyclicDeps = not IsAcyclic,
    TypeSpecs = gpb_lib:get_type_specs_by_opts(Opts0),
    Opts1 = lists:keydelete(type_specs, 1, Opts0 -- [type_specs]),
    if not CyclicDeps, TypeSpecs == preferably ->
            {[], [{type_specs, true} | Opts1]};
       CyclicDeps, TypeSpecs == preferably ->
            Opts2 = [{type_specs, false} | Opts1],
            {[], Opts2};
       not CyclicDeps ->
            {[], Opts0};
       CyclicDeps, TypeSpecs ->
            {[cyclic_message_dependencies], [{type_specs, false} | Opts1]};
       CyclicDeps, not TypeSpecs ->
            {[], Opts0}
    end.

%% Input .proto file appears to be expected to be UTF-8 by Google's protobuf.
%% In 3.0.0, it accepts a byte order mark (BOM), but in 2.6.1 it does not.
%% It only accepts a BOM for for UTF-8. It does not accept UTF-16 nor UTF-32
%% input (tried both little and big endian for both, with proper BOMs).
utf8_decode(B) ->
    {Enc, Len} = unicode:bom_to_encoding(B),
    <<_Bom:Len/binary, B2/binary>> = B,
    if Enc == latin1;
       Enc == utf8 ->
            %% Enc == latin1 means just that no Byte order mark was seen,
            %% it might still be UTF-8 encoded, though, so try that first.
            case unicode:characters_to_list(B2) of
                S when is_list(S) ->
                    {ok, {utf8, S}};
                {error, _, _} ->
                    {ok, {latin1, binary_to_list(B2)}}
            end;
       true ->
            {error, {invalid_proto_byte_order_mark, Enc}}
    end.

check_unpackables_marked_as_packed(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, MsgName, #?gpb_field{name=FName, type=Type, opts=Opts}, Acc) ->
              case {lists:member(packed, Opts), gpb:is_type_packable(Type)} of
                  {true, false} ->
                      Warn = {ignored_field_opt_packed_for_unpackable_type,
                              MsgName, FName, Type, Opts},
                      [Warn | Acc];
                  _ ->
                      Acc
              end
      end,
      [],
      Defs).

%% -- generating code ----------------------------------------------

format_erl(Mod, Defs, #anres{maps_as_msgs=MapsAsMsgs}=AnRes, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    NoNif = not DoNif,
    AsLib = proplists:get_bool(include_as_lib, Opts),
    CompileOptsStr = get_erlc_compile_options_str(Opts),
    gpb_lib:iolist_to_utf8_or_escaped_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n",
          [?MODULE, gpb:version_as_string()]),
       ?f("-module(~w).~n", [Mod]),
       case CompileOptsStr of
           ""    -> "";
           [_|_] -> ?f("-compile([~ts]).~n", [CompileOptsStr])
       end,
       "\n",
       case gpb_lib:get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([encode_msg/1, encode_msg/2]).~n");
           maps    -> ?f("-export([encode_msg/2, encode_msg/3]).~n")
       end,
       [[?f("-export([encode/1]). %% epb compatibility~n"),
         [?f("-export([~p/1]).~n", [gpb_lib:mk_fn(encode_, MsgName)])
          || {{msg,MsgName}, _Fields} <- Defs],
         "\n"]
        || gpb_lib:get_epb_functions_by_opts(Opts)],
       ?f("-export([decode_msg/2"),[", decode_msg/3" || NoNif], ?f("]).~n"),
       case gpb_lib:get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([merge_msgs/2, merge_msgs/3]).~n");
           maps    -> ?f("-export([merge_msgs/3, merge_msgs/4]).~n")
       end,
       [[?f("-export([decode/2]). %% epb compatibility~n"),
         [?f("-export([~p/1]).~n", [gpb_lib:mk_fn(decode_, MsgName)])
          || {{msg,MsgName}, _Fields} <- Defs],
         "\n"]
        || gpb_lib:get_epb_functions_by_opts(Opts)],
       case gpb_lib:get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([verify_msg/1, verify_msg/2]).~n");
           maps    -> ?f("-export([verify_msg/2, verify_msg/3]).~n")
       end,
       ?f("-export([get_msg_defs/0]).~n"),
       ?f("-export([get_msg_names/0]).~n"),
       ?f("-export([get_group_names/0]).~n"),
       ?f("-export([get_msg_or_group_names/0]).~n"),
       ?f("-export([get_enum_names/0]).~n"),
       ?f("-export([find_msg_def/1, fetch_msg_def/1]).~n"),
       ?f("-export([find_enum_def/1, fetch_enum_def/1]).~n"),
       gpb_gen_introspect:format_enum_value_symbol_converter_exports(Defs),
       ?f("-export([get_service_names/0]).~n"),
       ?f("-export([get_service_def/1]).~n"),
       ?f("-export([get_rpc_names/1]).~n"),
       ?f("-export([find_rpc_def/2, fetch_rpc_def/2]).~n"),
       ?f("-export([get_package_name/0]).~n"),
       [?f("-export([descriptor/0]).~n")
        || gpb_lib:get_gen_descriptor_by_opts(Opts)],
       ?f("-export([gpb_version_as_string/0, gpb_version_as_list/0]).~n"),
       "\n",
       [["-on_load(load_nif/0).\n",
         "-export([load_nif/0]). %% for debugging of nif loading\n",
         "\n"]
        || DoNif],
       case gpb_lib:get_records_or_maps_by_opts(Opts) of
           records -> ?f("-include(\"~s.hrl\").~n", [Mod]);
           maps    -> ""
       end,
       case gpb_lib:get_defs_as_maps_or_records(Opts) of
           records ->
               [case gpb_lib:get_field_format_by_opts(Opts) of
                    fields_as_records ->
                        if AsLib ->
                                ?f("-include_lib(\"gpb/include/gpb.hrl\").~n");
                           not AsLib ->
                                ?f("-include(\"gpb.hrl\").~n")
                        end;
                    fields_as_proplists ->
                        "";
                    fields_as_maps ->
                        ""
                end];
           maps ->
               ""
       end,
       "\n",
       gpb_gen_types:format_export_types(Defs, Opts),
       "\n",
       if not DoNif ->
               case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts)
               of
                   '2tuples' ->
                       gpb_gen_types:format_maps_as_msgs_record_defs(
                         MapsAsMsgs);
                   maps ->
                       ""
               end;
          DoNif ->
               ""
       end,
       [[?f("~s~n", [gpb_gen_nif:format_load_nif(Mod, Opts)]),
         "\n"]
        || DoNif],
       %% Enabling inlining seems to cause performance to drop drastically
       %% I've seen decoding performance go down from 76000 msgs/s
       %% to about 10000 msgs/s for a set of mixed message samples.
       %% f("-compile(inline).~n"),
       %%
       gpb_gen_encoders:format_encoders_top_function(Defs, Opts),
       "\n",
       if DoNif ->
               ?f("~s~n", [gpb_gen_nif:format_nif_encoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [gpb_gen_encoders:format_msg_encoders(Defs, AnRes, Opts,
                                                     true),
                gpb_gen_encoders:format_map_encoders(MapsAsMsgs, AnRes, Opts,
                                                     false),
                gpb_gen_encoders:format_aux_encoders(Defs, AnRes, Opts)]
       end,
       "\n",
       gpb_gen_decoders:format_decoders_top_function(Defs, Opts),
       "\n\n",
       if DoNif ->
               [gpb_gen_nif:format_nif_decoder_error_wrappers(Defs,
                                                              AnRes, Opts)];
          not DoNif ->
               [gpb_gen_decoders:format_msg_decoders(Defs, AnRes, Opts),
                gpb_gen_decoders:format_map_decoders(MapsAsMsgs, AnRes, Opts),
                gpb_gen_decoders:format_aux_decoders(Defs, AnRes, Opts)]
       end,
       "\n",
       gpb_gen_mergers:format_msg_merge_code(Defs, AnRes, Opts),
       "\n",
       gpb_gen_verifiers:format_verifiers_top_function(Defs, Opts),
       "\n",
       gpb_gen_verifiers:format_verifiers(Defs, AnRes, Opts),
       "\n",
       if not DoNif ->
               [gpb_gen_translators:format_aux_transl_helpers(AnRes),
                gpb_gen_translators:format_translators(Defs, AnRes, Opts)];
          DoNif ->
               [gpb_gen_translators:format_aux_transl_helpers(AnRes),
                gpb_gen_translators:format_merge_translators(Defs, AnRes,
                                                             Opts)]
       end,
       "\n",
       gpb_gen_introspect:format_introspection(Defs, Opts),
       "\n",
       possibly_format_descriptor(Defs, Opts),
       "\n",
       ?f("gpb_version_as_string() ->~n"),
       ?f("    \"~s\".~n", [gpb:version_as_string()]),
       "\n",
       ?f("gpb_version_as_list() ->~n"),
       ?f("    ~s.~n", [gpb_version_as_list_pretty()])],
      Opts).

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

%% -- descr -----------------------------------------------------

possibly_format_descriptor(Defs, Opts) ->
    case gpb_lib:get_gen_descriptor_by_opts(Opts) of
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

%% -- hrl -----------------------------------------------------

possibly_format_hrl(Mod, Defs, Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records -> format_hrl(Mod, Defs, Opts);
        maps    -> '$not_generated'
    end.

format_hrl(Mod, Defs, Opts) ->
    ModVsn = list_to_atom(atom_to_list(Mod) ++ "_gpb_version"),
    gpb_lib:iolist_to_utf8_or_escaped_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n",
          [?MODULE, gpb:version_as_string()]),
       "\n",
       ?f("-ifndef(~p).~n", [Mod]),
       ?f("-define(~p, true).~n", [Mod]),
       "\n",
       ?f("-define(~p, \"~s\").~n", [ModVsn, gpb:version_as_string()]),
       "\n",
       gpb_lib:nl_join(
         [gpb_gen_types:format_msg_record(Msg, Fields, Opts, Defs)
          || {_,Msg,Fields} <- gpb_lib:msgs_or_groups(Defs)]),
       "\n",
       ?f("-endif.~n")],
      Opts).

%% -- nif c++ code -----------------------------------------------------

possibly_format_nif_cc(Mod, Defs, AnRes, Opts) ->
    case proplists:get_bool(nif, Opts) of
        true  -> gpb_gen_nif:format_nif_cc(Mod, Defs, AnRes, Opts);
        false -> '$not_generated'
    end.

%% -- compile to memory -----------------------------------------------------

compile_to_binary(Mod, HrlText, ErlCode, PossibleNifCode, Opts) ->
    ModAsStr = flatten_iolist(?f("~p", [Mod])),
    ErlCode2 = nano_epp(ErlCode, ModAsStr, HrlText),
    {ok, Toks, _EndLine} = erl_scan:string(ErlCode2),
    FormToks = split_toks_at_dot(Toks),
    Forms = [case erl_parse:parse_form(Ts) of
                 {ok, Form} ->
                     Form;
                 {error, Reason} ->
                     io:format(user, "Ts=~p~n", [Ts]),
                     erlang:error(
                       {internal_error,?MODULE,Mod,Ts,Reason,
                        {more_info,[{full_erl,ErlCode2},{hrl,HrlText},
                                    {nif,PossibleNifCode},{opts,Opts}]}})
             end
             || Ts <- FormToks],
    combine_erl_and_possible_nif(compile:noenv_forms(Forms, Opts),
                                 PossibleNifCode).

nano_epp(Code, ModAsStr, HrlText) ->
    %% nepp = nano-erlang-preprocessor. Couldn't find a way to run
    %% the epp from a string, and don't want or need to use the file
    %% system when everything is already in memory.
    D0 = dict:new(),
    {Txt, _EndLine, _Defs} = nepp1(Code, ModAsStr, HrlText, 1, D0, []),
    Txt.

nepp1(<<"%% -*- coding:",_/binary>>=B, ModAsStr, HrlText, N, Ds, Acc) ->
    %% First (non-coding) line must be a -file(...) directive,
    %% or else unused record definitions in included files will
    %% produce warnings: eg: {27,erl_lint,{unused_record,gpb_oneof}}.
    {CodingLine,Rest} = read_until(B, "\n", ""),
    Erl = (ModAsStr -- "''") ++ ".erl",
    CodingAndFileDirective = CodingLine ++ "\n" ++ file_directive(Erl, 1),
    Acc2 = lists:reverse(CodingAndFileDirective, Acc),
    nepp2_nl(Rest, ModAsStr, HrlText, N, Ds, Acc2);
nepp1(Rest, ModAsStr, HrlText, N, Ds, Acc) ->
    Erl = (ModAsStr -- "''") ++ ".erl",
    FileDirective = file_directive(Erl, 1),
    Acc2 = lists:reverse(FileDirective, Acc),
    nepp2_nl(Rest, ModAsStr, HrlText, N, Ds, Acc2).


nepp2(<<"?MODULE", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2(Rest, ModAsStr, Hrl, N, Ds, lists:reverse(ModAsStr, Acc));
nepp2(<<$\n, Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_nl(Rest, ModAsStr, Hrl, N+1, Ds, [$\n | Acc]);
nepp2(<<C, Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2(Rest, ModAsStr, Hrl, N, Ds, [C | Acc]);
nepp2(<<>>, _ModAsStr, _Hrl, N, Ds, Acc) ->
    {lists:reverse(Acc), N, Ds}.

nepp2_nl(<<"-include", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_inc(Rest, ModAsStr, Hrl, N, Ds, Acc);
nepp2_nl(<<"-include_lib", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_inc(Rest, ModAsStr, Hrl, N, Ds, Acc);
nepp2_nl(<<"-define", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_def(Rest, ModAsStr, Hrl, N, Ds, Acc);
nepp2_nl(<<"-ifdef", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_ifdef(Rest, ifdef, ModAsStr, Hrl, N, Ds, Acc);
nepp2_nl(<<"-ifndef", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_ifdef(Rest, ifndef, ModAsStr, Hrl, N, Ds, Acc);
nepp2_nl(<<"-endif.\n", Rest/binary>>, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_nl(Rest, ModAsStr, Hrl, N+1, Ds, Acc);
nepp2_nl(X, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2(X, ModAsStr, Hrl, N, Ds, Acc).

nepp2_inc(Rest, ModAsStr, Hrl, N, Ds, Acc) ->
    {_,    Rest1} = read_until(Rest,  "(", ""),
    {Inc1, Rest2} = read_until(Rest1, ")", ""),
    {_,    Rest3} = read_until(Rest2, "\n", ""),
    Inc = parse_term(Inc1),
    Erl = (ModAsStr -- "''") ++ ".erl",
    case classify_inc(Inc) of
        gpb_hrl ->
            FieldDef = field_record_to_text(),
            OneofDef = oneof_record_to_text(),
            RpcDef   = rpc_record_to_text(),
            Txt = lists:flatten([file_directive(Inc, 1),
                                 FieldDef, OneofDef, RpcDef]),
            Acc2 = lists:reverse(Txt ++ file_directive(Erl, N+1), Acc),
            nepp2_nl(Rest3, ModAsStr, Hrl, N+1, Ds, Acc2);
        mod_hrl when Hrl /= '$not_generated' ->
            {Txt1, _End, Ds2} = nepp2_nl(Hrl, ModAsStr, Hrl, 1, Ds, []),
            Txt2 = lists:flatten([file_directive(Inc, 1), Txt1]),
            Acc2 = lists:reverse(Txt2 ++ file_directive(Erl, N+1), Acc),
            nepp2_nl(Rest3, ModAsStr, Hrl, N+1, Ds2, Acc2)
    end.

nepp2_def(Rest, ModAsStr, Hrl, N, Ds, Acc) ->
    {_,   Rest1} = read_until(Rest,  "(", ""),
    {Sym, Rest2} = read_until(Rest1, ",", ""),
    {Val, Rest3} = read_until(Rest2, ")", ""),
    {_,   Rest4} = read_until(Rest3, "\n", ""),
    Ds1 = dict:store(parse_term(Sym), parse_term(Val), Ds),
    nepp2_nl(Rest4, ModAsStr, Hrl, N+1, Ds1, Acc).

nepp2_ifdef(Rest, SkipCond, ModAsStr, Hrl, N, Ds, Acc) ->
    {_,   Rest1} = read_until(Rest,  "(", ""),
    {Sym, Rest2} = read_until(Rest1, ")", ""),
    {_,   Rest3} = read_until(Rest2, "\n", ""),
    case {dict:is_key(parse_term(Sym), Ds), SkipCond} of
        {true,  ifdef}  -> nepp2_nl(Rest3, ModAsStr, Hrl, N+1, Ds, Acc);
        {false, ifndef} -> nepp2_nl(Rest3, ModAsStr, Hrl, N+1, Ds, Acc);
        _ -> nepp2_skip(Rest3, 1, ModAsStr, Hrl, N+1, Ds, Acc)
    end.

nepp2_skip(<<"-endif.\n", Rest/binary>>, Depth, ModAsStr, Hrl, N, Ds, Acc) ->
    if Depth == 1 -> nepp2_nl(Rest, ModAsStr, Hrl, N+1, Ds, Acc);
       Depth >  1 -> nepp2_skip(Rest, Depth-1, ModAsStr, Hrl, N+1, Ds, Acc)
    end;
nepp2_skip(<<"-ifdef", Rest/binary>>, Depth, ModAsStr, Hrl, N, Ds, Acc) ->
    {_, Rest2} = read_until(Rest, "\n", ""),
    nepp2_skip(Rest2, Depth+1, ModAsStr, Hrl, N+1, Ds, Acc);
nepp2_skip(<<"-ifndef", Rest/binary>>, Depth, ModAsStr, Hrl, N, Ds, Acc) ->
    {_, Rest2} = read_until(Rest, "\n", ""),
    nepp2_skip(Rest2, Depth+1, ModAsStr, Hrl, N+1, Ds, Acc);
nepp2_skip(<<$\n, Rest/binary>>, Depth, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_skip(Rest, Depth, ModAsStr, Hrl, N+1, Ds, Acc);
nepp2_skip(<<_, Rest/binary>>, Depth, ModAsStr, Hrl, N, Ds, Acc) ->
    nepp2_skip(Rest, Depth, ModAsStr, Hrl, N, Ds, Acc).

read_until(<<C, Rest/binary>>, Delims, Acc) ->
    case lists:member(C, Delims) of
        true  -> {lists:reverse(Acc), Rest};
        false -> read_until(Rest, Delims, [C | Acc])
    end.

parse_term(S) ->
    {ok, Tokens, _End} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens++[{dot,1}]),
    Term.

classify_inc(F) ->
    case lists:last(filename:split(F)) of
        "gpb.hrl" -> gpb_hrl;
        _         -> mod_hrl
    end.

file_directive(File, N) ->
    ?ff("-file(\"~s\", ~p).\n", [File, N]).

split_toks_at_dot(AllToks) ->
    case lists:splitwith(fun is_no_dot/1, AllToks) of
        {Toks, [{dot,_}=Dot]}      -> [Toks ++ [Dot]];
        {Toks, [{dot,_}=Dot | Tl]} -> [Toks ++ [Dot] | split_toks_at_dot(Tl)]
    end.

is_no_dot({dot,_}) -> false;
is_no_dot(_)       -> true.

field_record_to_text() ->
    record_to_text(?gpb_field, record_info(fields, ?gpb_field), #?gpb_field{}).

oneof_record_to_text() ->
    record_to_text(gpb_oneof, record_info(fields, gpb_oneof), #gpb_oneof{}).

rpc_record_to_text() ->
    record_to_text(?gpb_rpc, record_info(fields, ?gpb_rpc), #?gpb_rpc{}).

record_to_text(RecordName, Fields, DefaultR) ->
    FieldTexts =
        [if Default == undefined -> ?ff("~p", [FName]);
            Default /= undefined -> ?ff("~p = ~p", [FName, Default])
         end
         || {FName,Default} <- lists:zip(Fields, tl(tuple_to_list(DefaultR)))],
    ?f("-record(~p, {~s}).~n",
       [RecordName, gpb_lib:comma_join(FieldTexts)]).

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

file_op(FnName, Args, Opts) ->
    case proplists:get_value(file_op, Opts) of
        undefined ->
            apply(file, FnName, Args);
        Ops ->
            case proplists:get_value(FnName, Ops) of
                undefined ->
                    apply(file, FnName, Args);
                Fn ->
                    apply(Fn, Args)
            end
    end.

possibly_probe_defs(Defs, Opts) ->
    case proplists:get_value(probe_defs, Opts, '$no') of
        '$no' -> ok;
        Fn    -> Fn(Defs)
    end.
