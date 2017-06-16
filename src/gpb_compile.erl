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

-import(gpb_lib, [mk_fn/2, mk_fn/3]).
-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).
-import(gpb_lib, [msgs_or_groups/1, msg_names/1,
                  get_field_name/1, get_field_occurrence/1,
                  unalias_enum/1]).
-import(gpb_lib, [mapping_match/3, mapping_create/3, mapping_update/4,
                  record_match/2, record_create/2,
                  map_match/1, map_create/1, map_set/2]).
-import(gpb_lib, [get_2tuples_or_maps_for_maptype_fields_by_opts/1,
                  get_records_or_maps_by_opts/1,
                  get_mapping_and_unset_by_opts/1,
                  get_strings_as_binaries_by_opts/1,
                  get_type_specs_by_opts/1,
                  get_gen_descriptor_by_opts/1,
                  get_field_format_by_opts/1]).
-import(gpb_lib, [index_seq/1,
                  iolist_to_utf8_or_escaped_binary/2,
                  nowarn_dialyzer_attr/3]).

-import(gpb_gen_translators, [mk_find_tr_fn/3, mk_find_tr_fn_elem/4,
                              mk_find_tr_fn_elem_or_default/3,
                              mk_find_tr_fn_elem_or_default/4,
                              mk_elempath_elem/3,
                              find_translation/3, find_translation/4,
                              default_fn_by_op/2,
                              default_any_merge_translator/0,
                              default_any_verify_translator/0,
                              exists_tr_for_msg/3,
                              args_by_op2/1]).

%% Varints are processed 7 bits at a time.
%% We can expect that we have processed this number of bits before
%% we expect to see the last varint byte, which must have msb==0.
%% 64 - 7 = 57.
-define(NB, 57).
-define(is_msg_or_group(X),
        is_tuple(X)
        andalso tuple_size(X) =:= 2
        andalso (element(1, X) =:= msg orelse element(1, X) =:= group)).

%% -- Types -----------------------------------------------------

%% Options
-type boolean_opt(X) :: X | {X, boolean()}.% Just an option `X' means `{X,true}'
-type directory() :: string().

-type opts() :: [opt()].
-type opt() :: boolean_opt(type_specs) |
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
%% in the generated .hrl file. Default is currently `false'. If you
%% set it to `true', you may get into troubles for messages
%% referencing other messages cyclically, when compiling the generated
%% files.
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

find_out_mod({Mod, _S}, _Opts) ->
    Mod;
find_out_mod(File, Opts) ->
    Ext = filename:extension(File),
    list_to_atom(possibly_suffix_mod(
                   possibly_prefix_mod(
                     filename:basename(File, Ext),
                     Opts),
                   Opts)).

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
    {Warns, Opts1} = possibly_adjust_typespec_opt(IsAcyclic, Opts0),
    AnRes = analyze_defs(Defs, Opts1),
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
                     get_records_or_maps_by_opts(Opts)} of
                   {true, maps} ->
                       {error, {invalid_options, epb_functions,maps}};
                   _ ->
                       ok
               end
       end,
       fun() ->
               case proplists:get_bool(epb_functions, Opts) of
                   true ->
                       case lists:member(msg, msg_names(Defs)) of
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
parse_opt(OptName, {OptName, 'integer()', OptTag, _Descr}, [OptArg | Rest]) ->
    try list_to_integer(OptArg) of
        N -> {ok, {{OptTag, N}, Rest}}
    catch error:badarg ->
            {error, ?ff("Invalid version number (integer) for ~s: ~p",
                        [OptName, OptArg])}
    end;
parse_opt(OptName, {OptName, F, OptTag, _Descr}, Rest) when is_function(F) ->
    F(OptTag, Rest);
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
     {"il", undefined, include_as_lib, "\n"
      "       Generate code that includes gpb.hrl using -include_lib\n"
      "       instead of -include, which is the default.\n"},
     {"type", undefined, type_specs, "\n"
      "       Enables `::Type()' annotations in the generated .hrl file.\n"},
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





opt_any_translate(OptTag, [S | Rest]) ->
    try
        Ts = string:tokens(S, ","),
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
    case string:tokens(S, ":") of
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
    case parse_file_and_imports(In, Opts) of
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
                    ProtoName = filename:basename(FName, ".proto"),
                    Imports = gpb_parse:fetch_imports(Defs),
                    Opts2 = ensure_include_path_to_wellknown_types_if_proto3(
                              Defs, Imports, Opts),
                    MsgContainment = {{msg_containment, ProtoName},
                                      msg_names(Defs)},
                    read_and_parse_imports(Imports, AlreadyImported2,
                                           [MsgContainment | Defs], Opts2);
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
            error({your_installation_is_missing_the, WellknownDir, directory,
                   which_is_expected_to_house_proto3_well_known_types,
                   such_as, ["google/protobuf/timestamp.proto",
                             "google/protobuf/duration.proto", and_more]})
    end.

try_topsort_defs(Defs) ->
    G = digraph:new(),
    %% Build a dependency graph {msg|group, Name} -> {msg|group,Name}
    [digraph:add_vertex(G, _Key={Type,Name})
     || {Type,Name,_Fields} <- msgs_or_groups(Defs)],
    fold_msg_or_group_fields(
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

%% -- analysis -----------------------------------------------------

analyze_defs(Defs, Opts) ->
    MapTypes = find_map_types(Defs),
    MapsAsMsgs = map_types_to_msgs(sets:to_list(MapTypes)),
    Translations = compute_translations(Defs, Opts),
    KnownMsgSize = find_msgsizes_known_at_compile_time(MapsAsMsgs ++ Defs),
    #anres{used_types          = find_used_types(Defs),
           known_msg_size      = KnownMsgSize,
           fixlen_types        = find_fixlen_types(MapsAsMsgs ++ Defs),
           num_packed_fields   = find_num_packed_fields(MapsAsMsgs ++ Defs),
           num_fields          = find_num_fields(MapsAsMsgs ++ Defs),
           d_field_pass_method = compute_decode_field_pass_methods(
                                   MapsAsMsgs ++ Defs, Opts),
           maps_as_msgs        = MapsAsMsgs,
           translations        = Translations,
           default_transls     = compute_used_default_translators(
                                   Defs, Translations, KnownMsgSize, Opts),
           map_types           = MapTypes,
           group_occurrences   = find_group_occurrences(Defs)}.

find_map_types(Defs) ->
    fold_msg_or_group_fields(
      fun(_, _MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              sets:add_element({KeyType,ValueType}, Acc);
         (_, _MsgName, _Field, Acc) ->
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
    fold_msg_or_group_fields(
      fun(_Type, _MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              Acc1 = sets:add_element(KeyType, Acc),
              sets:add_element(ValueType, Acc1);
         (_Type, _MsgName, #?gpb_field{type=Type}, Acc) ->
              sets:add_element(Type, Acc)
      end,
      sets:new(),
      Defs).

find_fixlen_types(Defs) ->
    fold_msg_or_group_fields(
      fun(_, _, #?gpb_field{type=Type, occurrence=Occ}=FieldDef, Acc) ->
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
    fold_msg_or_group_fields(
      fun(_, _MsgName, FieldDef, Acc) ->
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
    fold_msg_fields_o(
      fun(MsgName, Field, _IsOneOf, Acc) -> Fun(MsgName, Field, Acc) end,
      InitAcc,
      Defs).

fold_msg_or_group_fields(Fun, InitAcc, Defs) ->
    fold_msg_or_group_fields_o(
      fun(Type, Name, Field, _IsOneOf, Acc) -> Fun(Type, Name, Field, Acc) end,
      InitAcc,
      Defs).

fold_msgdef_fields(Fun, InitAcc, Fields) ->
    fold_msgdef_fields_o(
      fun(Field, _IsOneOf, Acc) -> Fun(Field, Acc) end,
      InitAcc,
      Fields).

%% The fun takes 4 args: Fun(Msgname, #?gpb_field{}, IsOneof, Acc) -> Acc1
fold_msg_fields_o(Fun, InitAcc, Defs) ->
    lists:foldl(
      fun({{msg, MsgName}, Fields}, Acc) ->
              FFun = fun(Field, IsOneOf, FAcc) ->
                             Fun(MsgName, Field, IsOneOf, FAcc)
                     end,
              fold_msgdef_fields_o(FFun, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).

%% The fun takes 5 args:
%% Fun(msg | group, Name, #?gpb_field{}, IsOneof, Acc) -> Acc1
fold_msg_or_group_fields_o(Fun, InitAcc, Defs) ->
    lists:foldl(
      fun({{Type, Name}, Fields}, Acc) when Type =:= msg;
                                            Type =:= group ->
              FFun = fun(Field, IsOneOf, FAcc) ->
                             Fun(Type, Name, Field, IsOneOf, FAcc)
                     end,
              fold_msgdef_fields_o(FFun, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).



%% The fun takes 3 args: Fun(#?gpb_field{}, IsOneof, Acc) -> Acc1
fold_msgdef_fields_o(Fun, InitAcc, Fields) ->
    lists:foldl(
      fun(#?gpb_field{}=Field, Acc) ->
              Fun(Field, false, Acc);
         (#gpb_oneof{name=CFName, fields=OFields}, Acc) ->
              IsOneOf = {true, CFName},
              lists:foldl(fun(OField, OAcc) -> Fun(OField, IsOneOf, OAcc) end,
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
    lists:foldl(fun({_msg_or_group, MsgName, MsgDef}, Acc) ->
                        dict:store(MsgName, length(MsgDef), Acc)
                end,
                dict:new(),
                msgs_or_groups(Defs)).

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

find_groupsize(GroupName, Defs, T) ->
    {{group,GroupName}, Fields} = lists:keyfind({group,GroupName}, 1, Defs),
    find_msgsize_2(Fields, 0, Defs, T).

find_msgsize_2([#gpb_oneof{} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=repeated} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=optional} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{type=Type, fnum=FNum} | Rest], AccSize, Defs, T) ->
    FKeySize =
        case Type of
            {group, _} ->
                not_applicable;
            _ ->
                FKey = (FNum bsl 3) bor gpb:encode_wiretype(Type),
                byte_size(gpb:encode_varint(FKey))
        end,
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
        {group,GroupName} ->
            case find_groupsize(GroupName, Defs, T) of
                GroupSize when is_integer(GroupSize) ->
                    StartTag = (FNum bsl 3) + gpb:encode_wiretype(group_start),
                    EndTag   = (FNum bsl 3) + gpb:encode_wiretype(group_end),
                    SizeOfStartTag = byte_size(gpb:encode_varint(StartTag)),
                    SizeOfEndTag = byte_size(gpb:encode_varint(EndTag)),
                    GroupFieldSize = SizeOfStartTag + GroupSize + SizeOfEndTag,
                    find_msgsize_2(Rest, AccSize + GroupFieldSize, Defs, T);
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
                     <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                     byte_size(gpb:encode_varint(N))
                 end
                 || {_EnumSym, Value} <- EnumDef],
    case lists:usort(EnumSizes) of
        [Size] -> {yes, Size};
        _      -> no
    end.

compute_decode_field_pass_methods(Defs, Opts) ->
    lists:foldl(fun({_Type, Name, Fields}, D) ->
                        PassHow = d_field_pass_method(Name, Fields, Opts),
                        %% FIXME:GROUP: are all group+msg names unique?
                        dict:store(Name, PassHow, D)
                end,
                dict:new(),
                msgs_or_groups(Defs)).

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
            NumGroupFields = count_group_fields(MsgDef),
            IsMsgDominatedBySubMsgsOrMaps =
                (NumSubMsgFields + NumMapFields + NumGroupFields) / NF > 0.5,
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

count_group_fields(MsgDef) ->
    fold_msgdef_fields(fun(#?gpb_field{type={group,_}}, N) -> N+1;
                          (#?gpb_field{}, N)               -> N
                       end,
                       0,
                       MsgDef).

compute_translations(Defs, Opts) ->
    remove_empty_translations(
      remove_merge_translations_for_repeated_elements(
        lists:foldl(
          fun({Name, Dict}, D) ->
                  %% For now it is an (internal) error if translations overlap,
                  %% (don't expect that to happen with current translations)
                  %% but in the future (eg with user-specified translations)
                  %% they might stack instead: ie Ts1 ++ Ts2 instead of error.
                  dict:merge(
                    fun(Key, Ts1, Ts2) ->
                            error({error,{duplicate_translation,
                                          {when_adding_transls_for,Name},
                                          {key,Key},
                                          {translations,Ts1,Ts2}}})
                    end,
                    Dict, D)
          end,
          dict:new(),
          [{map_translations, compute_map_translations(Defs, Opts)},
           {any_translations, compute_any_translations(Defs, Opts)}]))).

remove_merge_translations_for_repeated_elements(D) ->
    dict:map(fun(Key, Ops) ->
                     case is_repeated_element_path(Key) of
                         true -> lists:keydelete(merge, 1, Ops);
                         false -> Ops
                     end
             end,
             D).

is_repeated_element_path([_, _, []]) -> true;
is_repeated_element_path(_) -> false.

remove_empty_translations(D) ->
    dict:filter(fun(_Key, Ops) -> Ops /= [] end, D).

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
    MapsOrTuples = get_2tuples_or_maps_for_maptype_fields_by_opts(Opts),
    dict:from_list(
      lists:append(
        [begin
             MapAsMsgName = map_type_to_msg_name(KeyType, ValueType),
             case MapsOrTuples of
                 '2tuples' ->
                     [{[MsgName,FName,[]],
                       [{encode, {mt_maptuple_to_pseudomsg_r,
                                  ['$1',MapAsMsgName]}}]},
                      {[MsgName,FName],
                       [{decode_init_default, {mt_empty_map_r,[]}},
                        {decode_repeated_add_elem,{mt_add_item_r,['$1','$2']}},
                        {decode_repeated_finalize,{mt_finalize_items_r,['$1']}},
                        {merge, {mt_merge_maptuples_r,['$1','$2']}}]}];
                 maps ->
                     [{[MsgName,FName,[]],
                       [{encode, {mt_maptuple_to_pseudomsg_m, ['$1']}}]},
                      {[MsgName,FName],
                       [{encode, {mt_map_to_list_m,['$1']}},
                        {decode_init_default, {mt_empty_map_m,[]}},
                        {decode_repeated_add_elem,{mt_add_item_m,['$1','$2']}},
                        {decode_repeated_finalize,{id,['$1','$user_data']}},
                        {merge, {mt_merge_maps_m,['$1','$2']}}]}]
             end
         end
         || {{MsgName, FName}, {KeyType, ValueType}} <- MapInfos])).

compute_any_translations(Defs, Opts) ->
    case proplists:get_value(any_translate,Opts) of
        undefined ->
            dict:new();
        AnyTranslations ->
            compute_any_translations_2(Defs, AnyTranslations)
    end.

compute_any_translations_2(Defs, AnyTranslations) ->
    P3AnyInfos =
        fold_msg_or_group_fields_o(
          fun(_Type,
              MsgName, #?gpb_field{name=FName, type={msg,Any}, occurrence=Occ},
              Oneof,
              Acc) when Any == 'google.protobuf.Any' ->
                  Path = case {Oneof, Occ} of
                             {false, repeated}  -> [MsgName,FName,[]];
                             {false, _}         -> [MsgName,FName];
                             {{true,CFName}, _} -> [MsgName,CFName,FName]
                         end,
                  [Path | Acc];
             (_Type,
              _MsgName, #?gpb_field{type={map,KeyType,{msg,Any}=ValueType}},
              _Oneof,
              Acc) when Any == 'google.protobuf.Any' ->
                  MsgAsMapName = map_type_to_msg_name(KeyType, ValueType),
                  Path = [MsgAsMapName,value],
                  [Path | Acc];
             (_Type, _MsgName, _Field, _Oneof, Acc) ->
                  Acc
          end,
          [],
          Defs),
    Encode = {encode, fetch_any_translation(encode, AnyTranslations)},
    Decode = {decode, fetch_any_translation(decode, AnyTranslations)},
    Merge  = {merge,  fetch_any_translation(merge,  AnyTranslations,
                                            default_any_merge_translator())},
    Verify = {verify, fetch_any_translation(verify, AnyTranslations,
                                            default_any_verify_translator())},
    dict:from_list(
      [{Path, ([Encode,Decode,Verify]
               ++ [Merge || not is_repeated_elem_path(Path)])}
       || Path <- P3AnyInfos]).

fetch_any_translation(Op, Translations) ->
    fetch_any_translation(Op, Translations, undefined).
fetch_any_translation(Op, Translations, Default) ->
    case proplists:get_value(Op, Translations, Default) of
        undefined ->
            error({error, {missing_any_translation, {op,Op}, Translations}});
        {M,F,ArgTempl} ->
            {M,F,ArgTempl};
        {F,ArgTempl} ->
            {F,ArgTempl}
    end.

is_repeated_elem_path([_MsgName,_FName,[]]) -> true;
is_repeated_elem_path(_) -> false.

compute_used_default_translators(Defs, Translations, KnownMsgSize, Opts) ->
    fold_fields_and_paths(
      fun(Field, Path, _IsOneOf, Acc) ->
              Calls = get_translations(Field,Path, Translations,
                                       KnownMsgSize, Opts),
              lists:foldl(
                fun({FnName,ArgsTmpl}, A) when is_list(ArgsTmpl) ->
                        Arity = length(ArgsTmpl),
                        sets:add_element({FnName, Arity}, A);
                   ({FnName,Arity}, A) when is_integer(Arity) ->
                        sets:add_element({FnName, Arity}, A);
                   (_, A) -> % remote call (ie: to other module)
                        A
                end,
                Acc,
                Calls)
      end,
      sets:new(),
      Defs).

get_translations(#gpb_oneof{}, _Path, _Translations, _KnownMsgSize, _Opts) ->
    [];
get_translations(#?gpb_field{type=Type, occurrence=Occ, opts=FOpts},
                 Path, Translations, KnownMsgSize, Opts) ->
    {IsRepeated,IsPacked,IsKnownSizeElem} =
        if Occ == repeated ->
                {true,
                 lists:member(packed,FOpts),
                 is_known_size_element(Type, KnownMsgSize)};
           true ->
                {false, false, false}
        end,
    IsElem = IsRepeated andalso lists:last(Path) == [],
    DoNif = proplists:get_bool(nif, Opts),
    Ops = if DoNif ->
                  [merge, verify];
             IsElem ->
                  [encode,decode,merge,verify];
             IsRepeated, IsPacked, IsKnownSizeElem ->
                  [encode,
                   decode_repeated_finalize,
                   merge,
                   verify];
             IsRepeated, IsPacked, not IsKnownSizeElem ->
                  [encode,
                   decode_init_default,
                   decode_repeated_finalize,
                   merge,
                   verify];
             IsRepeated, not IsPacked ->
                  [encode,
                   decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize,
                   merge,
                   verify];
             true ->
                  [encode,decode,merge,verify]
          end,
    PathTransls = case dict:find(Path, Translations) of
                      {ok, Ts} -> Ts;
                      error    -> []
                  end,
    [case lists:keyfind(Op, 1, PathTransls) of
         {Op, Transl} ->
             Transl;
         false ->
             if Op == merge, IsRepeated, not IsElem ->
                     {'erlang_++',3};
                true ->
                     FnName = default_fn_by_op(Op, undefined),
                     Arity = length(args_by_op2(Op)) + 1,
                     {FnName, Arity}
             end
     end
     || Op <- Ops].

is_known_size_element(fixed32, _) -> true;
is_known_size_element(fixed64, _) -> true;
is_known_size_element(sfixed32, _) -> true;
is_known_size_element(sfixed64, _) -> true;
is_known_size_element(float, _) -> true;
is_known_size_element(double, _) -> true;
is_known_size_element({msg,MsgName}, KnownMsgSize) ->
    dict:find(MsgName, KnownMsgSize) /= error;
is_known_size_element({group,Name}, KnownMsgSize) ->
    dict:find(Name, KnownMsgSize) /= error;
is_known_size_element({map,KeyType,ValueType}, KnownMsgSize) ->
    MapAsMsgName = map_type_to_msg_name(KeyType, ValueType),
    dict:find(MapAsMsgName, KnownMsgSize) /= error;
is_known_size_element(_Type, _) ->
    false.

fold_fields_and_paths(F, InitAcc, Defs) ->
    lists:foldl(
      fun({{msg, MsgName}, Fields}, Acc) ->
              fold_field_and_path(F, [MsgName], false, Acc, Fields);
         ({{group, GroupName}, Fields}, Acc) ->
              fold_field_and_path(F, [GroupName], false, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).

fold_field_and_path(F, Root, IsOneOf, InitAcc, Fields) ->
    lists:foldl(
      fun(#?gpb_field{name=FName, occurrence=repeated}=Field, Acc) ->
              Path = Root ++ [FName],
              EPath = Root ++ [FName, []],
              F(Field, EPath, IsOneOf, F(Field, Path, IsOneOf, Acc));
         (#?gpb_field{name=FName}=Field, Acc) ->
              Path = Root ++ [FName],
              F(Field, Path, IsOneOf, Acc);
         (#gpb_oneof{name=CFName, fields=OFields}=Field, Acc) ->
              Path = Root ++ [CFName],
              fold_field_and_path(F, Path, {true, CFName},
                                  F(Field, Path, IsOneOf, Acc),
                                  OFields)
      end,
      InitAcc,
      Fields).

find_group_occurrences(Defs) ->
    fold_msg_or_group_fields_o(
      fun(_msg_or_group, _MsgName,
          #?gpb_field{type={group,GroupName}, occurrence=Occurrence},
          _IsOnoeof, D)->
              dict:store(GroupName, Occurrence, D);
         (_msg_or_group, _MsgName, _Field, _IsOnoeof, D) ->
              D
      end,
      dict:new(),
      Defs).

%% -- generating code ----------------------------------------------

format_erl(Mod, Defs, #anres{maps_as_msgs=MapsAsMsgs}=AnRes, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    NoNif = not DoNif,
    AsLib = proplists:get_bool(include_as_lib, Opts),
    CompileOptsStr = get_erlc_compile_options_str(Opts),
    iolist_to_utf8_or_escaped_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n",
          [?MODULE, gpb:version_as_string()]),
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
       [[?f("-export([encode/1]). %% epb compatibility~n"),
         [?f("-export([~p/1]).~n", [mk_fn(encode_, MsgName)])
          || {{msg,MsgName}, _Fields} <- Defs],
         "\n"]
        || get_epb_functions_by_opts(Opts)],
       ?f("-export([decode_msg/2"),[", decode_msg/3" || NoNif], ?f("]).~n"),
       case get_records_or_maps_by_opts(Opts) of
           records -> ?f("-export([merge_msgs/2, merge_msgs/3]).~n");
           maps    -> ?f("-export([merge_msgs/3, merge_msgs/4]).~n")
       end,
       [[?f("-export([decode/2]). %% epb compatibility~n"),
         [?f("-export([~p/1]).~n", [mk_fn(decode_, MsgName)])
          || {{msg,MsgName}, _Fields} <- Defs],
         "\n"]
        || get_epb_functions_by_opts(Opts)],
       case get_records_or_maps_by_opts(Opts) of
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
               case get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
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
       format_encoders_top_function(Defs, Opts),
       "\n",
       if DoNif ->
               ?f("~s~n", [gpb_gen_nif:format_nif_encoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [?f("~s~n", [format_msg_encoders(Defs, AnRes, Opts, true)]),
                ?f("~s~n", [format_map_encoders(MapsAsMsgs,AnRes,Opts,false)]),
                ?f("~s~n", [format_aux_encoders(Defs, AnRes, Opts)])]
       end,
       "\n",
       format_decoders_top_function(Defs, Opts),
       "\n\n",
       if DoNif ->
               ?f("~s~n", [gpb_gen_nif:format_nif_decoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [?f("~s~n", [format_msg_decoders(Defs, AnRes, Opts)]),
                ?f("~s~n", [format_map_decoders(MapsAsMsgs, AnRes, Opts)]),
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
    SpecExtraArgs = case Mapping of
                        records -> "";
                        maps    -> ",_"
                    end,
    [?f("-spec encode_msg(_~s) -> no_return().~n", [SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>') -> encode_msg(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     ?f("-spec encode_msg(_,_~s) -> no_return().~n", [SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(_Msg, '<MsgName>', _Opts) ->
               erlang:error({gpb_error, no_messages})
       end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     [[?f("%% epb compatibility\n"),
       ?f("-spec encode(_) -> no_return().\n"),
       gpb_codegen:format_fn(
         encode,
         fun(_Msg) -> erlang:error({gpb_error, no_messages}) end)]
      || get_epb_functions_by_opts(Opts)]].

format_encoders_top_function_msgs(Defs, Opts) ->
    Verify = proplists:get_value(verify, Opts, optionally),
    Mapping = get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(MsgName)]
                  end,
    DoNif = proplists:get_bool(nif, Opts),
    SpecExtraArgs = case Mapping of
                        records -> "";
                        maps    -> ",atom()"
                    end,
    [?f("-spec encode_msg(_~s) -> binary().~n", [SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>') -> encode_msg(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     ?f("-spec encode_msg(_~s, list()) -> binary().~n", [SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>', '<Opts>') ->
               '<possibly-verify-msg>',
               'TrUserData = proplists:get_value(user_data, Opts)',
               case '<MsgOrMsgName>' of
                   '<msg-match>' -> 'encode'(Msg, 'TrUserData')
               end
       end,
       [replace_tree('<Opts>', case {DoNif, Verify} of
                                   {true,optionally} -> ?expr(Opts);
                                   {true,always}     -> ?expr(Opts);
                                   {true,never}      -> ?expr(_Opts);
                                   {false,_}         -> ?expr(Opts)
                               end),
        splice_trees('<possibly-verify-msg>',
                     case Verify of
                         optionally ->
                             [?expr(case proplists:get_bool(verify, Opts) of
                                        true  -> verify_msg(Msg, '<MsgName>',
                                                            Opts);
                                        false -> ok
                                    end)];
                         always ->
                             [?expr(verify_msg(Msg, '<MsgName>', Opts))];
                         never ->
                             []
                     end),
        splice_trees(
          'TrUserData = proplists:get_value(user_data, Opts)',
          if DoNif -> [];
             true  -> [?expr(TrUserData = proplists:get_value(user_data, Opts))]
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
        splice_trees('<MsgName>', MsgNameVars),
        splice_trees('TrUserData', if DoNif -> [];
                                      true  -> [?expr(TrUserData)]
                                   end)]),
     "\n",
     [[?f("%% epb compatibility\n"),
       ?f("-spec encode(_) -> binary().~n"),
       gpb_codegen:format_fn(
         encode,
         fun(Msg) -> encode_msg(Msg) end),
       [[?f("-spec ~p(_) -> binary().~n", [mk_fn(encode_, MsgName)]),
         gpb_codegen:format_fn(
           mk_fn(encode_, MsgName),
           fun(Msg) -> encode_msg(Msg) end)]
        || {{msg,MsgName}, _Fields} <- Defs]]
      || get_epb_functions_by_opts(Opts)]].

format_aux_encoders(Defs, AnRes, _Opts) ->
    [format_enum_encoders(Defs, AnRes),
     format_type_encoders(AnRes)
    ].

format_enum_encoders(Defs, #anres{used_types=UsedTypes}) ->
    [gpb_codegen:format_fn(
       mk_fn(e_enum_, EnumName),
       fun('<EnumSym>', Bin) -> <<Bin/binary, '<varint-bytes>'>>;
          (V, Bin) -> % integer (for yet unknown enums)
               e_varint(V, Bin)
       end,
       [repeat_clauses('<EnumSym>',
                       [begin
                            ViBytes = enum_to_binary_fields(EnumValue),
                            [replace_term('<EnumSym>', EnumSym),
                             splice_trees('<varint-bytes>', ViBytes)]
                        end
                        || {EnumSym, EnumValue} <- EnumDef])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_map_encoders(Defs, AnRes, Opts0, IncludeStarter) ->
    Opts1 = case get_2tuples_or_maps_for_maptype_fields_by_opts(Opts0) of
                '2tuples' -> [{msgs_as_maps, false} | Opts0];
                maps      -> [{msgs_as_maps, true} | Opts0]
            end,
    format_msg_encoders(Defs, AnRes, Opts1, IncludeStarter).

format_msg_encoders(Defs, AnRes, Opts, IncludeStarter) ->
    [[format_msg_encoder(MsgName, MsgDef, Defs,
                         AnRes, Opts,
                         if Type =:= group -> false;
                            true -> IncludeStarter
                         end)
      || {Type, MsgName, MsgDef} <- msgs_or_groups(Defs)],
     format_special_field_encoders(Defs, AnRes)].

format_msg_encoder(MsgName, [], _Defs, _AnRes, _Opts, _IncludeStarter) ->
    gpb_codegen:format_fn(
      mk_fn(e_msg_, MsgName),
      fun(_Msg, _TrUserData) ->
              <<>>
      end);
format_msg_encoder(MsgName, MsgDef, Defs, AnRes, Opts, IncludeStarter) ->
    FNames = get_field_names(MsgDef),
    FVars = [var_f_n(I) || I <- lists:seq(1, length(FNames))],
    BVars = [var_b_n(I) || I <- lists:seq(1, length(FNames)-1)] ++ [last],
    MsgVar = ?expr(M),
    TrUserDataVar = ?expr(TrUserData),
    {EncodeExprs, _} =
        lists:mapfoldl(
          fun({NewBVar, Field, FVar}, PrevBVar) when NewBVar /= last ->
                  Tr = mk_find_tr_fn(MsgName, Field, AnRes),
                  EncExpr = field_encode_expr(MsgName, MsgVar, Field, FVar,
                                              PrevBVar, TrUserDataVar,
                                              Defs, Tr, AnRes, Opts),
                  E = ?expr('<NewB>' = '<encode-expr>',
                            [replace_tree('<NewB>', NewBVar),
                             replace_tree('<encode-expr>', EncExpr)]),
                  {E, NewBVar};
             ({last, Field, FVar}, PrevBVar) ->
                  Tr = mk_find_tr_fn(MsgName, Field, AnRes),
                  EncExpr = field_encode_expr(MsgName, MsgVar, Field, FVar,
                                              PrevBVar, TrUserDataVar,
                                              Defs, Tr, AnRes, Opts),
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
         fun(Msg, TrUserData) ->
                 call_self(Msg, <<>>, TrUserData)
         end),
       "\n"] || IncludeStarter],
     gpb_codegen:format_fn(
       FnName,
       fun('<msg-matching>', Bin, TrUserData) ->
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
                  FVar, PrevBVar, TrUserDataVar, Defs, Tr, _AnRes, Opts)->
    FEncoder = mk_field_encode_fn_name(MsgName, Field),
    #?gpb_field{occurrence=Occurrence, type=Type, fnum=FNum, name=FName}=Field,
    TrFVar = prefix_var("Tr", FVar),
    Transforms = [replace_term('fieldname', FName),
                  replace_tree('<F>', FVar),
                  replace_tree('TrF', TrFVar),
                  replace_term('Tr', Tr(encode)),
                  replace_tree('TrUserData', TrUserDataVar),
                  splice_trees('MaybeTrUserData',
                               maybe_userdata_param(Field, TrUserDataVar)),
                  replace_term('<enc>', FEncoder),
                  replace_tree('<Bin>', PrevBVar),
                  splice_trees('<Key>', key_to_binary_fields(FNum, Type))],
    case Occurrence of
        optional ->
            EncodeExpr =
                case gpb:is_msg_proto3(MsgName, Defs) of
                    false ->
                        ?expr(begin
                                  'TrF' = 'Tr'('<F>', 'TrUserData'),
                                  '<enc>'('TrF', <<'<Bin>'/binary, '<Key>'>>,
                                          'MaybeTrUserData')
                              end,
                              Transforms);
                    true when Type == string ->
                        ?expr(begin
                                  'TrF' = 'Tr'('<F>', 'TrUserData'),
                                  case iolist_size('TrF') of
                                      0 ->
                                          '<Bin>';
                                      _ ->
                                          '<enc>'('TrF',
                                                  <<'<Bin>'/binary, '<Key>'>>,
                                                  'MaybeTrUserData')
                                  end
                              end,
                              Transforms);
                    true when Type /= string ->
                        TypeDefault = gpb:proto3_type_default(Type, Defs),
                        ?expr(
                           begin
                               'TrF' = 'Tr'('<F>', 'TrUserData'),
                               if 'TrF' =:= '<TypeDefault>' ->
                                       '<Bin>';
                                  true ->
                                       '<enc>'('TrF',
                                               <<'<Bin>'/binary, '<Key>'>>,
                                               'MaybeTrUserData')
                               end
                           end,
                           [replace_term('<TypeDefault>', TypeDefault)
                            | Transforms])
                end,
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(
                       if '<F>' == undefined ->
                               '<Bin>';
                          true ->
                               '<encodeit>'
                       end,
                       [replace_tree('<encodeit>', EncodeExpr) | Transforms]);
                {maps, omitted} ->
                    ?expr(
                       case 'M' of
                           '#{fieldname := <F>}' ->
                               '<encodeit>';
                           _ ->
                               '<Bin>'
                       end,
                       [replace_tree('M', MsgVar),
                        replace_tree('#{fieldname := <F>}',
                                     map_match([{FName,FVar}])),
                        replace_tree('<encodeit>', EncodeExpr)
                        | Transforms])
            end;
        repeated ->
            ?expr(
               begin
                   'TrF' = 'Tr'('<F>', 'TrUserData'),
                   if 'TrF' == [] -> '<Bin>';
                      true -> '<enc>'('TrF', '<Bin>', 'TrUserData')
                   end
               end,
               Transforms);
        required ->
            ?expr(
               begin
                   'TrF' = 'Tr'('<F>', 'TrUserData'),
                   '<enc>'('TrF', <<'<Bin>'/binary, '<Key>'>>,
                           'MaybeTrUserData')
               end,
               Transforms)
    end;
field_encode_expr(MsgName, MsgVar, #gpb_oneof{name=FName, fields=OFields},
                  FVar, PrevBVar, TrUserDataVar, Defs, Tr, AnRes, Opts) ->
    OFVar = prefix_var("O", FVar),
    OneofClauseTransforms =
        [begin
             OFVal = ?expr({'<oneof-name>', '<OF>'},
                           [replace_term('<oneof-name>', Name),
                            replace_tree('<OF>', OFVar)]),
             MatchPattern =
                 case get_mapping_and_unset_by_opts(Opts) of
                     X when X == records;
                            X == {maps, present_undefined} ->
                         OFVal;
                     {maps, omitted} ->
                         map_match([{FName, OFVal}])
                 end,
             %% undefined is already handled, we have a match,
             %% the field occurs, as if it had been required
             OField2 = OField#?gpb_field{occurrence=required},
             Tr2 = Tr({update_elem_path,Name}),
             EncExpr = field_encode_expr(MsgName, MsgVar, OField2, OFVar,
                                         PrevBVar, TrUserDataVar,
                                         Defs, Tr2, AnRes, Opts),
             [replace_tree('<oneof...>', MatchPattern),
              replace_tree('<expr>', EncExpr)]
         end
         || #?gpb_field{name=Name}=OField <- OFields],
    case get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            ?expr(case '<F>' of
                      undefined    -> '<Bin>';
                      '<oneof...>' -> '<expr>'
                  end,
                  [replace_tree('<F>', FVar),
                   replace_tree('<Bin>', PrevBVar),
                   repeat_clauses('<oneof...>', OneofClauseTransforms)]);
        {maps, omitted} ->
            ?expr(case 'M' of
                      '<oneof...>' -> '<expr>';
                      _ -> '<Bin>'
                  end,
                  [replace_term('fieldname', FName),
                   replace_tree('M', MsgVar),
                   replace_tree('<Bin>', PrevBVar),
                   repeat_clauses('<oneof...>', OneofClauseTransforms)])
    end.

mk_field_encode_fn_name(MsgName, #?gpb_field{occurrence=repeated, name=FName})->
    mk_fn(e_field_, MsgName, FName);
mk_field_encode_fn_name(MsgName, #?gpb_field{type={msg,_Msg}, name=FName}) ->
    mk_fn(e_mfield_, MsgName, FName);
mk_field_encode_fn_name(MsgName, #?gpb_field{type={group,_Nm}, name=FName}) ->
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
      fold_msg_or_group_fields(
        fun(_Type, MsgName, #?gpb_field{occurrence=repeated}=FieldDef, Acc) ->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_Type, MsgName, #?gpb_field{type={msg,_}}=FieldDef, Acc)->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_Type, MsgName, #?gpb_field{type={group,_}}=FieldDef, Acc)->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_Type, _MsgName, #?gpb_field{}, Acc) ->
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
             format_packed_field_encoder2(MsgName, FieldDef, AnRes);
         {optional, false} ->
             [];
         {required, false} ->
             []
     end].

possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={msg,SubMsg}}=FieldDef,
                               AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FieldDef),
    case is_msgsize_known_at_generationtime(SubMsg, AnRes) of
        no ->
            gpb_codegen:format_fn(
              FnName,
              fun(Msg, Bin, TrUserData) ->
                      SubBin = '<encode-msg>'(Msg, <<>>, TrUserData),
                      Bin2 = e_varint(byte_size(SubBin), Bin),
                      <<Bin2/binary, SubBin/binary>>
              end,
              [replace_term('<encode-msg>', mk_fn(e_msg_, SubMsg))]);
        {yes, MsgSize} when MsgSize > 0 ->
            MsgSizeBytes = varint_to_binary_fields(MsgSize),
            gpb_codegen:format_fn(
              FnName,
              fun(Msg, Bin, TrUserData) ->
                      Bin2 = <<Bin/binary, '<msg-size>'>>,
                      '<encode-msg>'(Msg, Bin2, TrUserData)
              end,
              [splice_trees('<msg-size>', MsgSizeBytes),
               replace_term('<encode-msg>', mk_fn(e_msg_, SubMsg))]);
        {yes, 0} ->
            %% special case, there will not be any e_msg_<MsgName>/2 function
            %% generated, so don't call it.
            gpb_codegen:format_fn(
              FnName,
              fun(_Msg, Bin, _TrUserData) -> <<Bin/binary, 0>> end)
    end;
possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={map,KType,VType}}=FieldDef,
                               AnRes) ->
    MapAsMsgName = map_type_to_msg_name(KType, VType),
    FieldDef2 = FieldDef#?gpb_field{type = {msg,MapAsMsgName}},
    possibly_format_mfield_encoder(MsgName, FieldDef2, AnRes);
possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={group,GroupName},
                                           fnum=FNum}=FieldDef,
                               _AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FieldDef),
    EndTagBytes = key_to_binary_fields(FNum, group_end),
    gpb_codegen:format_fn(
      FnName,
      fun(Msg, Bin, TrUserData) ->
              GroupBin = '<encode-msg>'(Msg, <<>>, TrUserData),
              <<Bin/binary, GroupBin/binary, 'EndTagBytes'>>
      end,
      [replace_term('<encode-msg>', mk_fn(e_msg_, GroupName)),
       splice_trees('EndTagBytes', EndTagBytes)]);
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
    ElemPath = [MsgName,FName,[]],
    Transl = find_translation(ElemPath, encode, AnRes),
    gpb_codegen:format_fn(
      FnName,
      fun([Elem | Rest], Bin, TrUserData) ->
              Bin2 = <<Bin/binary, '<KeyBytes>'>>,
              Bin3 = '<encode-elem>'('Tr'(Elem, TrUserData), Bin2,
                                     'MaybeTrUserData'),
              call_self(Rest, Bin3, TrUserData);
         ([], Bin, _TrUserData) ->
              Bin
      end,
      [splice_trees('<KeyBytes>', KeyBytes),
       replace_term('<encode-elem>', ElemEncoderFn),
       replace_term('Tr', Transl),
       splice_trees('MaybeTrUserData',
                    maybe_userdata_param(FDef, ?expr(TrUserData)))]).

format_packed_field_encoder2(MsgName, #?gpb_field{type=Type}=FDef, AnRes) ->
    case packed_byte_size_can_be_computed(Type) of
        {yes, BitLen, BitType} ->
            format_knownsize_packed_field_encoder2(MsgName, FDef,
                                                   BitLen, BitType);
        no ->
            format_unknownsize_packed_field_encoder2(MsgName, FDef, AnRes)
    end.

packed_byte_size_can_be_computed(fixed32)  -> {yes, 32, [little]};
packed_byte_size_can_be_computed(sfixed32) -> {yes, 32, [little,signed]};
packed_byte_size_can_be_computed(float)    -> {yes, 32, float};
packed_byte_size_can_be_computed(fixed64)  -> {yes, 64, [little]};
packed_byte_size_can_be_computed(sfixed64) -> {yes, 64, [little,signed]};
packed_byte_size_can_be_computed(double)   -> {yes, 64, double};
packed_byte_size_can_be_computed(_)        -> no.

format_knownsize_packed_field_encoder2(MsgName, #?gpb_field{name=FName,
                                                            fnum=FNum}=FDef,
                                       BitLen, BitType) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    KeyBytes = key_to_binary_fields(FNum, bytes),
    PackedFnName = mk_fn(e_pfield_, MsgName, FName),
    [gpb_codegen:format_fn(
       FnName,
       fun(Elems, Bin, _TrUserData) when Elems =/= [] ->
               Bin2 = <<Bin/binary, '<KeyBytes>'>>,
               Bin3 = e_varint(length(Elems) * '<ElemLen>', Bin2),
               '<encode-packed>'(Elems, Bin3);
          ([], Bin, _TrUserData) ->
               Bin
       end,
       [splice_trees('<KeyBytes>', KeyBytes),
        replace_term('<ElemLen>', BitLen div 8),
        replace_term('<encode-packed>', PackedFnName)]),
     case BitType of
         float ->
             format_packed_float_encoder(PackedFnName);
         double ->
             format_packed_double_encoder(PackedFnName);
         _ ->
             gpb_codegen:format_fn(
               PackedFnName,
               fun([Value | Rest], Bin) ->
                       Bin2 = <<Bin/binary, Value:'<Size>'/'<BitType>'>>,
                       call_self(Rest, Bin2);
                  ([], Bin) ->
                       Bin
               end,
               [replace_term('<Size>', BitLen),
                splice_trees('<BitType>',
                             [erl_syntax:atom(T) || T <- BitType])])
     end].


format_unknownsize_packed_field_encoder2(MsgName,
                                         #?gpb_field{name=FName,
                                                     fnum=FNum}=FDef,
                                         AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(
                      MsgName,
                      FDef#?gpb_field{occurrence=required}),
    KeyBytes = key_to_binary_fields(FNum, bytes),
    PackedFnName = mk_fn(e_pfield_, MsgName, FName),
    ElemPath = [MsgName,FName,[]],
    Transl = find_translation(ElemPath, encode, AnRes),
    [gpb_codegen:format_fn(
       FnName,
       fun(Elems, Bin, TrUserData) when Elems =/= [] ->
               SubBin = '<encode-packed>'(Elems, <<>>, TrUserData),
               Bin2 = <<Bin/binary, '<KeyBytes>'>>,
               Bin3 = e_varint(byte_size(SubBin), Bin2),
               <<Bin3/binary, SubBin/binary>>;
          ([], Bin, _TrUserData) ->
               Bin
       end,
       [splice_trees('<KeyBytes>', KeyBytes),
        replace_term('<encode-packed>', PackedFnName)]),
     gpb_codegen:format_fn(
       PackedFnName,
       fun([Value | Rest], Bin, TrUserData) ->
               Bin2 = '<encode-elem>'('Tr'(Value, TrUserData), Bin,
                                      'MaybeTrUserData'),
               call_self(Rest, Bin2, TrUserData);
          ([], Bin, _TrUserData) ->
               Bin
       end,
       [replace_term('<encode-elem>', ElemEncoderFn),
        replace_term('Tr', Transl),
        splice_trees('MaybeTrUserData',
                     maybe_userdata_param(FDef, ?expr(TrUserData)))])].

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
     [format_fixed_encoder(fixed64,  64, [little])        || NeedsFixed64],
     [format_fixed_encoder(sfixed64, 64, [little,signed]) || NeedsSFixed64],
     [format_float_encoder(float) || NeedsFloat],
     [format_double_encoder(double) || NeedsDouble]].

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
        any_enum_field_exists(UsedTypes) orelse
        any_packed_field_exists(AnRes) orelse
        at_least_one_submsg_with_size_not_known_at_compile_time_exists(AnRes).

any_enum_field_exists(UsedTypes) ->
    sets:fold(fun({enum,_}, _Acc) -> true;
                 (_, Acc)         -> Acc
              end,
              false,
              UsedTypes).

any_packed_field_exists(#anres{num_packed_fields=0}) -> false;
any_packed_field_exists(#anres{num_packed_fields=_}) -> true.

at_least_one_submsg_with_size_not_known_at_compile_time_exists(AnRes) ->
    #anres{used_types=UsedTypes,
           maps_as_msgs=MapsAsMsgs,
           known_msg_size=KnownSize} = AnRes,
    SubMsgNames = [MsgName || {msg,MsgName} <- sets:to_list(UsedTypes)],
    MapMsgNames = [MsgName || {{msg,MsgName},_} <- MapsAsMsgs],
    IsMsgSizeUnknown = fun(Nm) -> dict:fetch(Nm, KnownSize) == undefined end,
    lists:any(IsMsgSizeUnknown, SubMsgNames) orelse
        lists:any(IsMsgSizeUnknown, MapMsgNames).

format_sint_encoder() ->
    gpb_codegen:format_fn(
      e_type_sint,
      fun(Value, Bin) when Value >= 0 ->
              e_varint(Value * 2, Bin);
         (Value, Bin) ->
              e_varint(Value * -2 - 1, Bin)
      end).

format_int_encoder(Type, _BitLen) ->
    gpb_codegen:format_fn(
      mk_fn(e_type_, Type),
      fun(Value, Bin) when 0 =< Value, Value =< 127 ->
              <<Bin/binary, Value>>; %% fast path
         (Value, Bin) ->
              %% Encode as a 64 bit value, for interop compatibility.
              %% Some implementations don't decode 32 bits properly,
              %% and Google's protobuf (C++) encodes as 64 bits
              <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
              e_varint(N, Bin)
      end,
      []).

format_bool_encoder() ->
    gpb_codegen:format_fn(
      e_type_bool,
      fun(true, Bin)  -> <<Bin/binary, 1>>;
         (false, Bin) -> <<Bin/binary, 0>>;
         (1, Bin) -> <<Bin/binary, 1>>;
         (0, Bin) -> <<Bin/binary, 0>>
      end).

format_fixed_encoder(Type, BitLen, BitType) ->
    gpb_codegen:format_fn(
      mk_fn(e_type_, Type),
      fun(Value, Bin) ->
              <<Bin/binary, Value:'<Sz>'/'<T>'>>
      end,
      [replace_term('<Sz>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(T) || T <- BitType])]).

format_packed_float_encoder(FnName) ->
    gpb_codegen:format_fn(
      FnName,
      fun([V | Rest], Bin) when is_number(V) ->
              call_self(Rest, <<Bin/binary, V:32/little-float>>);
         ([infinity | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:16,128,127>>);
         (['-infinity' | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:16,128,255>>);
         ([nan | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:16,192,127>>);
         ([], Bin) ->
              Bin
      end,
      []).

format_packed_double_encoder(FnName) ->
    gpb_codegen:format_fn(
      FnName,
      fun([V | Rest], Bin) when is_number(V) ->
              call_self(Rest, <<Bin/binary, V:64/float-little>>);
         ([infinity | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:48,240,127>>);
         (['-infinity' | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:48,240,255>>);
         ([nan | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:48,248,127>>);
         ([], Bin) ->
              Bin
      end,
      []).

format_float_encoder(Type) ->
    gpb_codegen:format_fn(
      mk_fn(e_type_, Type),
      fun(V, Bin) when is_number(V) -> <<Bin/binary, V:32/little-float>>;
         (infinity, Bin)            -> <<Bin/binary, 0:16,128,127>>;
         ('-infinity', Bin)         -> <<Bin/binary, 0:16,128,255>>;
         (nan, Bin)                 -> <<Bin/binary, 0:16,192,127>>
      end,
      []).

format_double_encoder(Type) ->
    gpb_codegen:format_fn(
      mk_fn(e_type_, Type),
      fun(V, Bin) when is_number(V) -> <<Bin/binary, V:64/little-float>>;
         (infinity, Bin)            -> <<Bin/binary, 0:48,240,127>>;
         ('-infinity', Bin)         -> <<Bin/binary, 0:48,240,255>>;
         (nan, Bin)                 -> <<Bin/binary, 0:48,248,127>>
      end,
      []).

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
      fun(Bytes, Bin) when is_binary(Bytes) ->
              Bin2 = e_varint(byte_size(Bytes), Bin),
              <<Bin2/binary, Bytes/binary>>;
         (Bytes, Bin) when is_list(Bytes) ->
              BytesBin = iolist_to_binary(Bytes),
              Bin2 = e_varint(byte_size(BytesBin), Bin),
              <<Bin2/binary, BytesBin/binary>>
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

maybe_userdata_param(Field, Expr) ->
    case is_primitive_type(Field) of
        true -> [];
        false -> [Expr]
    end.

is_primitive_type(#?gpb_field{type={msg,_}}) -> false;
is_primitive_type(#?gpb_field{type={group,_}}) -> false;
is_primitive_type(#?gpb_field{type={map,_,_}}) -> false;
is_primitive_type(_) -> true.

%% -- decoders -----------------------------------------------------

format_decoders_top_function(Defs, Opts) ->
    case contains_messages(Defs) of
        true  -> format_decoders_top_function_msgs(Defs, Opts);
        false -> format_decoders_top_function_no_msgs(Opts)
    end.

format_decoders_top_function_no_msgs(Opts) ->
    ["-spec decode_msg(binary(), atom()) -> no_return().\n",
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, _MsgName) when is_binary(Bin) ->
               erlang:error({gpb_error, no_messages})
       end),
     "-spec decode_msg(binary(), atom(), list()) -> no_return().\n",
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, _MsgName, _Opts) when is_binary(Bin) ->
               erlang:error({gpb_error, no_messages})
       end),
     "\n",
     [["%% epb compatibility\n",
       ?f("-spec decode(atom(), binary()) -> no_return().\n"),
       gpb_codegen:format_fn(
         decode,
         fun(MsgName, Bin) when is_atom(MsgName), is_binary(Bin) ->
                 erlang:error({gpb_error, no_messages})
         end)]
      || get_epb_functions_by_opts(Opts)]].

format_decoders_top_function_msgs(Defs, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    [if DoNif -> "";
        true ->
             gpb_codegen:format_fn(
               decode_msg,
               fun(Bin, MsgName) when is_binary(Bin) ->
                       call_self(Bin, MsgName, [])
               end,
               [])
     end,
     gpb_codegen:format_fn(
       decode_msg,
       fun(Bin, MsgName, 'Opts') when is_binary(Bin) ->
               'TrUserData = proplists:get_value(user_data, Opts)',
               case MsgName of
                   '<MsgName>' -> '<decode-call>'(Bin, 'TrUserData')
               end
       end,
       [splice_trees('Opts', if DoNif -> [];
                                true  -> [?expr(Opts)]
                             end),
        splice_trees(
          'TrUserData = proplists:get_value(user_data, Opts)',
          if DoNif -> [];
             true  -> [?expr(TrUserData = proplists:get_value(user_data, Opts))]
          end),
        repeat_clauses('<MsgName>',
                       [[replace_term('<MsgName>', MsgName),
                         replace_term('<decode-call>', mk_fn(d_msg_, MsgName))]
                        || {{msg,MsgName}, _Fields} <- Defs]),
        splice_trees('TrUserData', if DoNif -> [];
                                      true  -> [?expr(TrUserData)]
                                   end)]),
     [["\n",
       "%% epb compatibility\n",
       gpb_codegen:format_fn(
         decode,
         fun(MsgName, Bin) when is_atom(MsgName), is_binary(Bin) ->
                 decode_msg(Bin, MsgName)
         end),
       [gpb_codegen:format_fn(
          mk_fn(decode_, MsgName),
          fun(Bin) when is_binary(Bin) ->
                  decode_msg(Bin, 'MsgName')
          end,
          [replace_term('MsgName', MsgName)])
        || {{msg,MsgName}, _Fields} <- Defs]]
      || get_epb_functions_by_opts(Opts)]].

format_aux_decoders(Defs, AnRes, _Opts) ->
    [format_enum_decoders(Defs, AnRes),
     [format_read_group_fn() || contains_messages(Defs)]].

format_enum_decoders(Defs, #anres{used_types=UsedTypes}) ->
    %% FIXME: enum values can be negative, but "raw" varints are positive
    %%        insert a 2-complement in the mapping in order to move computations
    %%        from run-time to compile-time??
    [gpb_codegen:format_fn(
       mk_fn(d_enum_, EnumName),
       fun('<EnumValue>') -> '<EnumSym>';
          (V) -> V % for yet unknown enums
       end,
       [repeat_clauses('<EnumValue>',
                       [[replace_term('<EnumValue>', EnumValue),
                         replace_term('<EnumSym>', EnumSym)]
                        || {EnumSym, EnumValue} <- unalias_enum(EnumDef)])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        smember({enum,EnumName}, UsedTypes)].

format_map_decoders(Defs, AnRes, Opts0) ->
    Opts1 = case get_2tuples_or_maps_for_maptype_fields_by_opts(Opts0) of
                '2tuples' -> [{msgs_as_maps, false} | Opts0];
                maps      -> [{msgs_as_maps, true} | Opts0]
            end,
    format_msg_decoders(Defs, AnRes, Opts1).

format_msg_decoders(Defs, AnRes, Opts) ->
    [format_msg_decoder(Name, MsgDef, Defs, AnRes, Opts)
     || {_Type, Name, MsgDef} <- msgs_or_groups(Defs)].

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
                             {'<Rest>', Rest},
                             {'<TrUserData>', ?expr(TrUserData)}]),
    [format_msg_init_decoder(MsgName, MsgDef, Defs, AnRes, Opts),
     format_msg_fastpath_decoder(Bindings, MsgName, MsgDef, AnRes, Opts),
     format_msg_generic_decoder(Bindings, MsgName, MsgDef, AnRes, Opts)].

format_msg_init_decoder(MsgName, MsgDef, Defs, AnRes, Opts) ->
    gpb_codegen:format_fn(
      mk_fn(d_msg_, MsgName),
      fun(Bin, TrUserData) ->
              '<decode-field-fp>'(Bin, 0, 0, '<initial-params>', TrUserData)
      end,
      [replace_term('<decode-field-fp>', mk_fn(dfp_read_field_def_, MsgName)),
       splice_trees('<initial-params>',
                    msg_decoder_initial_params(MsgName, MsgDef, Defs,
                                               ?expr(TrUserData),
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
      fun('<precomputed-binary-match>', Z1, Z2, '<Params>', TrUserData) ->
              '<calls-to-field-decoding>';
         (<<>>, 0, 0, '<FParams>', 'MaybeTrUserData') ->
              '<finalize-result>';
         (Other, Z1, Z2, '<Params>', TrUserData) ->
              '<decode-general>'(Other, Z1, Z2, '<Params>', TrUserData)
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
                                            MsgName, MsgDef, ?expr(TrUserData),
                                            AnRes, Opts)),
       replace_term('<decode-general>', mk_fn(dg_read_field_def_, MsgName)),
       replace_tree('MaybeTrUserData',
                    case decode_finalizer_needs_tr_userdata(
                           MsgName, MsgDef, AnRes) of
                        true  -> ?expr(TrUserData);
                        false -> ?expr(_)
                    end)]).

decode_finalizer_needs_tr_userdata(MsgName, Fields, AnRes) ->
    any_field_is_repeated(Fields) orelse
        exists_tr_for_msg(MsgName, decode_repeated_finalize, AnRes).

any_field_is_repeated(Fields) ->
    lists:any(fun(#?gpb_field{occurrence=Occ}) -> Occ == repeated;
                 (#gpb_oneof{}) -> false
              end,
              Fields).

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
      fun(<<1:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>', TrUserData)
            when N < (32-7) ->
              call_self('<Rest>', N+7, X bsl N + Acc, '<Params>', TrUserData);
         (<<0:1, X:7, '<Rest>'/binary>>, N, Acc, '<Params>', TrUserData) ->
              '<Key>' = X bsl N + Acc,
              '<calls-to-field-decoding-or-skip>';
         (<<>>, 0, 0, '<FParams>', 'MaybeTrUserData') ->
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
                                            MsgName, MsgDef, ?expr(TrUserData),
                                            AnRes, Opts)),
       replace_tree('MaybeTrUserData',
                   case decode_finalizer_needs_tr_userdata(
                          MsgName,MsgDef,AnRes) of
                       true  -> ?expr(TrUserData);
                       false -> ?expr(_)
                   end)]).

msg_decoder_initial_params(MsgName, MsgDef, Defs, TrUserDataVar, AnRes, Opts) ->
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    UseDefaults = proplists:get_bool(defaults_for_omitted_optionals, Opts),
    UseTypeDefaults = proplists:get_bool(type_defaults_for_omitted_optionals,
                                         Opts),
    ExprInfos1 =
        [case Field of
             #?gpb_field{name=FName, occurrence=Occurrence, type=Type,
                         opts=FOpts} ->
                 HasDefault = lists:keymember(default, 1, FOpts),
                 {Undefined, Undef, P} =
                     if IsProto3 ->
                             TD = proto3_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, m};
                        UseDefaults, HasDefault ->
                             {default,D} = lists:keyfind(default, 1, FOpts),
                             AD = erl_syntax:abstract(D),
                             {AD, AD, m};
                        UseTypeDefaults ->
                             TD = proto2_type_default(Type, Defs, Opts),
                             ATD = erl_syntax:abstract(TD),
                             {ATD, ATD, m};
                        true ->
                             Pr = if HasDefault -> d;
                                     true -> o
                                  end,
                             {?expr(undefined), ?expr('$undef'), Pr}
                     end,
                 case Occurrence of
                     repeated -> {FName, m, ?expr([]),        ?expr([])};
                     required -> {FName, o, ?expr(undefined), ?expr('$undef')};
                     optional -> {FName, P, Undefined,        Undef}
                 end;
             #gpb_oneof{name=FName} ->
                 {FName, o, ?expr(undefined), ?expr('$undef')}
         end
         || Field <- MsgDef],
    ExprInfos2 =
        [begin
             ElemPath = [MsgName, FName],
             TranslFn = find_translation(ElemPath, decode_init_default, AnRes),
             TrInitExpr = ?expr('Tr'('InitExpr', 'TrUserData'),
                                [replace_tree('InitExpr', InitExpr),
                                 replace_term('Tr', TranslFn),
                                 replace_tree('TrUserData', TrUserDataVar)]),
             TrMOExpr = ?expr('Tr'('MOExpr', 'TrUserData'),
                              [replace_tree('MOExpr', MOExpr),
                               replace_term('Tr', TranslFn),
                               replace_tree('TrUserData', TrUserDataVar)]),
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
                       [{FName, Expr} || {FName, P, Expr, _} <- ExprInfos2,
                                         P == m orelse P == d])];
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
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    [begin
         BMatch = ?expr(<<'<field-and-wiretype-bytes>', '<Rest>'/binary>>,
                        [splice_trees('<field-and-wiretype-bytes>',
                                      varint_to_binary_fields(Selector)),
                         replace_tree('<Rest>', Rest)]),
         FnCall = ?expr('decode_field'('<Rest>', Z1, Z2, '<Params>',
                                       'TrUserData'),
                        [replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         splice_trees('<Params>', Params),
                         replace_tree('TrUserData', TrUserDataVar)]),
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
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    FieldSelects = decoder_field_selectors(MsgName, MsgDef),
    ?expr(case '<Key>' of
              '<selector>' -> 'decode_field'('<Rest>', 0, 0, '<Params>',
                                             'TrUserData');
              _            -> '<skip-calls>'
       end,
       [replace_tree('<Key>', Key),
        repeat_clauses('<selector>',
                       [[replace_term('<selector>', Selector),
                         replace_term('decode_field', DecodeFn),
                         replace_tree('<Rest>', Rest),
                         splice_trees('<Params>', Params)]
                        || {Selector, DecodeFn} <- FieldSelects]),
        replace_tree('<skip-calls>', SkipCalls),
        replace_tree('TrUserData', TrUserDataVar)]).

decoder_skip_calls(Bindings, MsgName) ->
    KeyExpr = fetch_binding('<Key>', Bindings),
    FieldNumExpr = ?expr('<Key>' bsr 3, [replace_tree('<Key>', KeyExpr)]),
    WiretypeExpr = fetch_binding('<wiretype-expr>', Bindings),
    RestExpr = fetch_binding('<Rest>', Bindings),
    Params = fetch_binding('<Params>', Bindings),
    TrUserDataVar = fetch_binding('<TrUserData>', Bindings),
    ?expr(case '<wiretype-expr>' of
              0 -> skip_vi('<Rest>', 0, 0, '<Params>', 'TrUserData');
              1 -> skip_64('<Rest>', 0, 0, '<Params>', 'TrUserData');
              2 -> skip_ld('<Rest>', 0, 0, '<Params>', 'TrUserData');
              3 -> skip_gr('<Rest>', 'FNum', 0, '<Params>', 'TrUserData');
              5 -> skip_32('<Rest>', 0, 0, '<Params>', 'TrUserData')
          end,
          [replace_tree('<wiretype-expr>', WiretypeExpr),
           replace_tree('<Rest>', RestExpr),
           splice_trees('<Params>', Params),
           replace_tree('TrUserData', TrUserDataVar),
           replace_term(skip_vi, mk_fn(skip_varint_, MsgName)),
           replace_term(skip_64, mk_fn(skip_64_, MsgName)),
           replace_term(skip_ld, mk_fn(skip_length_delimited_, MsgName)),
           replace_term(skip_gr, mk_fn(skip_group_, MsgName)),
           replace_tree('FNum', FieldNumExpr),
           replace_term(skip_32, mk_fn(skip_32_, MsgName))]).

decoder_field_selectors(MsgName, MsgDef) ->
    map_msgdef_fields_o(
      fun(#?gpb_field{name=FName, fnum=FNum, type=Type}=FieldDef, _IsOneof) ->
              Wiretype = case Type of
                             {group, _} ->
                                 gpb:encode_wiretype(group_start);
                             _ ->
                                 case is_packed(FieldDef) of
                                     true  -> gpb:encode_wiretype(bytes);
                                     false -> gpb:encode_wiretype(Type)
                                 end
                         end,
              Selector = (FNum bsl 3) bor Wiretype,
              DecodeFn = mk_fn(d_field_, MsgName, FName),
              {Selector, DecodeFn}
      end,
      MsgDef).

decoder_finalize_result(Params, FFields, MsgName, MsgDef,
                        TrUserDataVar, AnRes, Opts) ->
    case get_field_pass(MsgName, AnRes) of
        pass_as_params ->
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    decoder_finalize_params_all_present(Params, MsgName, MsgDef,
                                                        TrUserDataVar,
                                                        AnRes, Opts);
                {maps, omitted} ->
                    decoder_finalize_params_opt_omitted(Params, MsgName, MsgDef,
                                                        TrUserDataVar,
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
                    FValueExpr = ?expr('lists:reverse'('<FVar>', 'TrUserData'),
                                       [replace_term('lists:reverse',Finalizer),
                                        replace_tree('<FVar>', FVar),
                                        replace_tree('TrUserData',
                                                     TrUserDataVar)]),
                    {FName, FValueExpr}
                end
                || {FName, FVar} <- FFields],
               Opts)]
    end.

decoder_finalize_params_all_present(Params, MsgName, MsgDef, TrUserDataVar,
                                    AnRes, Opts) ->
    [mapping_create(
       MsgName,
       [decoder_finalize_param_for_mapping(Field, Param, MsgName,
                                           TrUserDataVar, AnRes)
        || {Field, Param} <- lists:zip(MsgDef, Params)],
       Opts)].

decoder_finalize_params_opt_omitted(Params, MsgName, MsgDef, TrUserDataVar,
                                    AnRes, _Opts) ->
    {Optionals, NonOptionals} = key_partition_on_optionality(
                                  1, lists:zip(MsgDef, Params)),
    NonOptionalsMap = map_create(
                        [decoder_finalize_param_for_mapping(
                           Field, Param, MsgName, TrUserDataVar, AnRes)
                         || {Field, Param} <- NonOptionals]),
    do_exprs(fun({Field, Param}, Var) ->
                     FV = decoder_finalize_param_for_mapping(
                            Field, Param, MsgName, TrUserDataVar, AnRes),
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

decoder_finalize_param_for_mapping(Field, Param, MsgName, TrUserDataVar,
                                   AnRes) ->
    FName = get_field_name(Field),
    ElemPath = [MsgName, FName],
    Finalizer = find_translation(ElemPath, decode_repeated_finalize, AnRes),
    FValueExpr = case get_field_occurrence(Field) of
                     required -> Param;
                     optional -> Param;
                     repeated -> ?expr('lists:reverse'('Param', 'TrUserData'),
                                       [replace_term('lists:reverse',Finalizer),
                                        replace_tree('Param', Param),
                                        replace_tree('TrUserData',
                                                     TrUserDataVar)])
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
        float    -> format_floating_point_field_decoder(MsgName, XField,
                                                        float, AnRes, Opts);
        fixed64  -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        sfixed64 -> format_fixlen_field_decoder(MsgName, XField, AnRes, Opts);
        double   -> format_floating_point_field_decoder(MsgName, XField,
                                                        double, AnRes, Opts);
        string   -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        bytes    -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {msg,_}  -> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {map,_,_}-> format_vi_based_field_decoder(MsgName, XField, AnRes, Opts);
        {group,_}-> format_group_field_decoder(MsgName, XField, AnRes, Opts)
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
       fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData)
             when N < ?NB ->
               call_self(Rest, N + 7, X bsl N + Acc, '<Params>', TrUserData);
          (<<0:1, X:7, Rest/binary>>, N, Acc, '<InParams>', TrUserData) ->
               Len = X bsl N + Acc,
               <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
               NewSeq = decode_packed(PackedBytes, 0, 0, '<Param>',
                                      'MaybeTrUserData'),
               '<call-read-field>'(Rest2, 0, 0, '<OutParams>', TrUserData)
       end,
       [splice_trees('<Params>', Params),
        splice_trees('<InParams>', InParams),
        replace_term(decode_packed, mk_fn(d_packed_field_, MsgName, FName)),
        replace_tree('<Param>', Param),
        replace_term('<call-read-field>', mk_fn(dfp_read_field_def_, MsgName)),
        splice_trees('<OutParams>', OutParams),
        splice_trees('MaybeTrUserData',
                     maybe_userdata_param(FieldDef, ?expr(TrUserData)))]),
     "\n",
     format_packed_field_seq_decoder(MsgName, FieldDef, AnRes, Opts)].

format_packed_field_seq_decoder(MsgName, #?gpb_field{type=Type}=Field,
                                AnRes, Opts) ->
    case Type of
        fixed32  -> format_dpacked_nonvi(MsgName, Field, 32, [little]);
        sfixed32 -> format_dpacked_nonvi(MsgName, Field, 32, [little,signed]);
        float    -> format_dpacked_nonvi(MsgName, Field, 32, float);
        fixed64  -> format_dpacked_nonvi(MsgName, Field, 64, [little]);
        sfixed64 -> format_dpacked_nonvi(MsgName, Field, 64, [little,signed]);
        double   -> format_dpacked_nonvi(MsgName, Field, 64, double);
        _        -> format_dpacked_vi(MsgName, Field, AnRes, Opts)
    end.

format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 32, float) ->
    gpb_codegen:format_fn(
      mk_fn(d_packed_field_, MsgName, FName),
      fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [infinity | AccSeq]);
         (<<0:16,128,255, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, ['-infinity' | AccSeq]);
         (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [nan | AccSeq]);
         (<<Value:32/little-float, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [Value | AccSeq]);
         (<<>>, _, _, AccSeq) ->
              AccSeq
      end,
      []);
format_dpacked_nonvi(MsgName, #?gpb_field{name=FName}, 64, double) ->
    gpb_codegen:format_fn(
      mk_fn(d_packed_field_, MsgName, FName),
      fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [infinity | AccSeq]);
         (<<0:48,240,255, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, ['-infinity' | AccSeq]);
         (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [nan | AccSeq]);
         (<<Value:64/little-float, Rest/binary>>, Z1, Z2, AccSeq) ->
              call_self(Rest, Z1, Z2, [Value | AccSeq]);
         (<<>>, _, _, AccSeq) ->
              AccSeq
      end,
      []);
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

format_dpacked_vi(MsgName, #?gpb_field{name=FName}=FieldDef, AnRes, Opts) ->
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest},
                             {'<TrUserData>', TrUserDataVar}]),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                C = ?exprs(call_self('<Rest2>', 0, 0, ['<Res>' | AccSeq],
                                    'MaybeTrUserData'),
                           [replace_tree('<Rest2>', Rest2Var),
                            replace_tree('<Res>', FVar),
                            splice_trees(
                              'MaybeTrUserData',
                              maybe_userdata_param(FieldDef, TrUserDataVar))]),
                DecodeExprs ++ C
        end,
    Tr = mk_find_tr_fn_elem(MsgName, FieldDef, false, AnRes),
    Body = decode_int_value(FVar, Bindings, FieldDef, Tr, TrUserDataVar,
                            Opts, BodyTailFn),
    gpb_codegen:format_fn(
      mk_fn(d_packed_field_, MsgName, FName),
      fun(<<1:1, X:7, Rest/binary>>, N, Acc, AccSeq, 'MaybeTrUserData')
            when N < ?NB ->
              call_self(Rest, N + 7, X bsl N + Acc, AccSeq, 'MaybeTrUserData');
         (<<0:1, X:7, Rest/binary>>, N, Acc, AccSeq, 'MaybeTrUserData') ->
              '<body>';
         (<<>>, 0, 0, AccSeq, 'Maybe_TrUserData') ->
              AccSeq
      end,
      [splice_trees('<body>', Body),
       splice_trees('MaybeTrUserData',
                    maybe_userdata_param(FieldDef, TrUserDataVar)),
       splice_trees('Maybe_TrUserData',
                    maybe_userdata_param(FieldDef, ?expr(_TrUserData)))]).

format_vi_based_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName}=FieldDef, IsOneof}=XFieldDef,
    ExtValue = ?expr(X bsl N + Acc),
    FVar = ?expr(NewFValue), %% result is to be put in this variable
    Rest = ?expr(Rest),
    TrUserDataVar = ?expr(TrUserData),
    Bindings = new_bindings([{'<Value>', ExtValue},
                             {'<Rest>', Rest},
                             {'<TrUserData>', TrUserDataVar}]),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef, AnRes,
                                              Opts),
    BodyTailFn =
        fun(DecodeExprs, Rest2Var) ->
                ReadFieldDefFn = mk_fn(dfp_read_field_def_, MsgName),
                Params2 = updated_merged_params(MsgName, XFieldDef, AnRes,
                                                FVar, PrevValue, Params,
                                                TrUserDataVar, Opts),
                C = ?exprs('<call-read-field>'('<Rest2>', 0, 0, '<Params2>',
                                               'TrUserData'),
                           [replace_term('<call-read-field>', ReadFieldDefFn),
                            replace_tree('<Rest2>', Rest2Var),
                            splice_trees('<Params2>', Params2),
                            replace_tree('TrUserData', TrUserDataVar)]),
                DecodeExprs ++ C
        end,
    Tr = mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof, AnRes),
    Body = decode_int_value(FVar, Bindings, FieldDef, Tr, TrUserDataVar,
                            Opts, BodyTailFn),
    gpb_codegen:format_fn(
      mk_fn(d_field_, MsgName, FName),
      fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData)
            when N < ?NB ->
              call_self(Rest, N + 7, X bsl N + Acc, '<Params>', TrUserData);
         (<<0:1, X:7, Rest/binary>>, N, Acc, '<InParams>', TrUserData) ->
              '<body>'
      end,
      [splice_trees('<Params>', Params),
       splice_trees('<InParams>', InParams),
       splice_trees('<body>', Body)]).

%% -> {[Expr], Rest2VarExpr}
%% where [Expr] is a list of exprs to calculate the resulting decoded value
decode_int_value(ResVar, Bindings, #?gpb_field{type=Type}=F,
                 Tr, TrUserDataVar, Opts, TailFn) ->
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
                          '<Res>' = 'Tr'('d_msg_X'(Bs, 'TrUserData'),
                                         'TrUserData'),
                          [replace_tree('<Value>', Value),
                           replace_tree('<Rest>', Rest),
                           replace_tree('<Res>', ResVar),
                           replace_term('d_msg_X', mk_fn(d_msg_, Msg2Name)),
                           replace_term('Tr', Tr(decode)),
                           replace_tree('TrUserData', TrUserDataVar)]),
                   Rest2);
        {map, KeyType, ValueType} ->
            MapAsMsgMame = map_type_to_msg_name(KeyType, ValueType),
            F2 = F#?gpb_field{type={msg,MapAsMsgMame}},
            decode_int_value(ResVar, Bindings, F2, Tr, TrUserDataVar,
                             Opts, TailFn)
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

format_group_field_decoder(MsgName, XFieldDef, AnRes, Opts) ->
    {#?gpb_field{name=FName, fnum=FNum, type={group,GroupName}}=FieldDef,
     IsOneof}=XFieldDef,
    ResVar = ?expr(NewFValue), %% result is to be put in this variable
    TrUserDataVar = ?expr(TrUserData),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef,
                                              AnRes, Opts),
    OutParams = updated_merged_params(MsgName, XFieldDef, AnRes,
                                      ResVar, PrevValue, Params,
                                      TrUserDataVar, Opts),
    Tr = mk_find_tr_fn_elem(MsgName, FieldDef, IsOneof, AnRes),
    gpb_codegen:format_fn(
      mk_fn(d_field_, MsgName, FName),
      fun(Bin, _, _, 'InParams', TrUserData) ->
              {GroupBin, Rest} = read_group(Bin, 'FieldNum'),
              'Res' = 'Tr'('d_msg_X'(GroupBin, TrUserData), TrUserData),
              'call-read-field'(Rest, 0, 0, 'OutParams', TrUserData)
      end,
      [splice_trees('InParams', InParams),
       replace_term('call-read-field', mk_fn(dfp_read_field_def_, MsgName)),
       replace_term('Tr', Tr(decode)),
       replace_tree('Res', ResVar),
       replace_tree('FieldNum', erl_syntax:integer(FNum)),
       replace_term('d_msg_X', mk_fn(d_msg_, GroupName)),
       splice_trees('OutParams', OutParams)]).

updated_merged_params(MsgName, XFieldDef, AnRes, NewValue, PrevValue,
                      Params, TrUserDataVar, Opts) ->
    Tr = mk_find_tr_fn_elem_or_default(MsgName, XFieldDef, AnRes),
    case {get_field_pass(MsgName, AnRes), XFieldDef} of
        {pass_as_params, {#?gpb_field{rnum=RNum}, _IsOneof}} ->
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, Tr, TrUserDataVar,
                                           AnRes, Opts),
            lists_setelement(RNum - 1, Params, MergedValue);
        {pass_as_record, {#?gpb_field{name=FName}, false}} ->
            MsgVar = hd(Params),
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, Tr, TrUserDataVar,
                                           AnRes, Opts),
            [mapping_update(MsgVar, MsgName, [{FName, MergedValue}], Opts)];
        {pass_as_record, {_OField, {true, CFName}}} ->
            MsgVar = hd(Params),
            MergedValue = merge_field_expr(XFieldDef, PrevValue, NewValue,
                                           MsgName, Tr, TrUserDataVar,
                                           AnRes, Opts),
            [mapping_update(MsgVar, MsgName, [{CFName, MergedValue}], Opts)]
    end.

merge_field_expr({FieldDef, false}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes, Opts) ->
    case classify_field_merge_action(FieldDef) of
        overwrite ->
            NewValue;
        seqadd ->
            ElemPath = [MsgName, get_field_name(FieldDef)],
            Cons = find_translation(ElemPath, decode_repeated_add_elem, AnRes),
            ?expr('[New|Acc]'('<New>', '<Acc>', 'TrUserData'),
                  [replace_term('[New|Acc]', Cons),
                   replace_tree('<New>', NewValue),
                   replace_tree('<Acc>', PrevValue),
                   replace_tree('TrUserData', TrUserDataVar)]);
        msgmerge ->
            FMsgName = case FieldDef of
                           #?gpb_field{type={msg,Nm}} -> Nm;
                           #?gpb_field{type={group,Nm}} -> Nm
                       end,
            MergeFn = mk_fn(merge_msg_, FMsgName),
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if 'Prev' == undefined -> 'New';
                             true -> 'merge_msg_X'('Prev', 'New', 'TrUserData')
                          end,
                          [replace_term('merge_msg_X', Tr(merge, MergeFn)),
                           replace_tree('Prev', PrevValue),
                           replace_tree('New', NewValue),
                           replace_tree('TrUserData', TrUserDataVar)]);
                {maps, omitted} ->
                    case get_field_pass(MsgName, AnRes) of
                        pass_as_params ->
                            ?expr(if 'Prev' =:= '$undef' -> 'New';
                                     true -> 'merge_msg_X'('Prev', 'New',
                                                           'TrUserData')
                                  end,
                                  [replace_tree('Prev', PrevValue),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)]);
                        pass_as_record ->
                            FName = get_field_name(FieldDef),
                            ?expr(case 'Msg' of
                                      '#{fieldname := Prev}' ->
                                          'merge_msg_X'(Prev, 'New',
                                                        'TrUserData');
                                      _ ->
                                          'New'
                                  end,
                                  [replace_tree(
                                     '#{fieldname := Prev}',
                                     map_match([{FName, ?expr(Prev)}])),
                                   replace_tree('Msg', PrevValue),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)])
                    end
            end
    end;
merge_field_expr({FieldDef, {true, CFName}}, PrevValue, NewValue,
                 MsgName, Tr, TrUserDataVar, AnRes, Opts)->
    #?gpb_field{name=FName, type=Type} = FieldDef,
    if ?is_msg_or_group(Type) ->
            {_, FMsgName} = Type,
            MergeFn = mk_fn(merge_msg_, FMsgName),
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    MVPrev = prefix_var("MV", PrevValue),
                    ?expr(case 'Prev' of
                              undefined ->
                                  {'tag', 'New'};
                              {'tag', 'MVPrev'} ->
                                  {'tag', 'merge_msg_X'('MVPrev', 'New',
                                                        'TrUserData')};
                              _ ->
                                  {'tag', 'New'}
                          end,
                          [replace_tree('Prev', PrevValue),
                           replace_term('tag', FName),
                           replace_tree('New', NewValue),
                           replace_term('merge_msg_X', Tr(merge, MergeFn)),
                           replace_tree('MVPrev', MVPrev),
                           replace_tree('TrUserData', TrUserDataVar)]);
                {maps, omitted} ->
                    MsgVar = PrevValue,
                    case get_field_pass(MsgName, AnRes) of
                        pass_as_params ->
                            ?expr(case 'Prev' of
                                      '$undef' ->
                                          {'tag', 'New'};
                                      {'tag', MVPrev} ->
                                          {'tag', 'merge_msg_X'(MVPrev, 'New',
                                                               'TrUserData')};
                                      _ ->
                                          {'tag', 'New'}
                                  end,
                                  [replace_term('tag', FName),
                                   replace_tree('Prev', PrevValue),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)]);
                        pass_as_record ->
                            OFVal = ?expr({tag, MVPrev},
                                          [replace_term(tag, FName)]),
                            ?expr(case 'Msg' of
                                      '#{fieldname := {tag,MVPrev}}' ->
                                          {'tag', 'merge_msg_X'(MVPrev,'New',
                                                               'TrUserData')};
                                      _ ->
                                          {'tag', 'New'}
                                  end,
                                  [replace_tree('#{fieldname := {tag,MVPrev}}',
                                                map_match([{CFName, OFVal}])),
                                   replace_term('tag', FName),
                                   replace_tree('Msg', MsgVar),
                                   replace_term('merge_msg_X',
                                                Tr(merge, MergeFn)),
                                   replace_tree('New', NewValue),
                                   replace_tree('TrUserData', TrUserDataVar)])
                    end
            end;
       true ->
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
    if ?is_msg_or_group(Type) ->
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
       true ->
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
    TrUserDataVar = ?expr(TrUserData),
    Params2 = updated_merged_params(MsgName, XFieldDef, AnRes,
                                    Value, PrevValue, Params,
                                    TrUserDataVar, Opts),
    ReadFieldDefFnName = mk_fn(dfp_read_field_def_, MsgName),
    gpb_codegen:format_fn(
      mk_fn(d_field_, MsgName, FName),
      fun(<<Value:'<N>'/'<T>', Rest/binary>>, Z1, Z2, '<InParams>',
          'TrUserData') ->
              '<call-read-field>'(Rest, Z1, Z2, '<OutParams>', 'TrUserData')
      end,
      [replace_term('<N>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(BT) || BT <- BitTypes]),
       splice_trees('<InParams>', InParams),
       replace_term('<call-read-field>', ReadFieldDefFnName),
       splice_trees('<OutParams>', Params2),
       replace_tree('TrUserData', TrUserDataVar)]).

format_floating_point_field_decoder(MsgName, XFieldDef, Type, AnRes, Opts) ->
    {#?gpb_field{name=FName}, _IsOneof} = XFieldDef,
    TrUserDataVar = ?expr(TrUserData),
    Params = decoder_params(MsgName, AnRes),
    {InParams, PrevValue} = decoder_in_params(Params, MsgName, XFieldDef,
                                              AnRes, Opts),
    OutParamsReplacements =
        [splice_trees(Marker, updated_merged_params(MsgName, XFieldDef, AnRes,
                                                    OutExpr, PrevValue, Params,
                                                    TrUserDataVar, Opts))
         || {Marker, OutExpr} <- [{'OutParams', ?expr(Value)},
                                  {'InfinityOutParams', ?expr(infinity)},
                                  {'-InfinityOutParams', ?expr('-infinity')},
                                  {'NanOutParams', ?expr(nan)}]],
    ReadFieldDefFnName = mk_fn(dfp_read_field_def_, MsgName),
    Replacements =
        [splice_trees('InParams', InParams),
         replace_term('<call-read-field>', ReadFieldDefFnName),
         replace_tree('TrUserData', TrUserDataVar)] ++
        OutParamsReplacements,
    case Type of
        float ->
            gpb_codegen:format_fn(
              mk_fn(d_field_, MsgName, FName),
              fun(<<0:16,128,127, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'InfinityOutParams',
                                          'TrUserData');
                 (<<0:16,128,255, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, '-InfinityOutParams',
                                          'TrUserData');
                 (<<_:16,1:1,_:7,_:1,127:7, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'NanOutParams',
                                          'TrUserData');
                 (<<Value:32/little-float, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'OutParams',
                                          'TrUserData')
              end,
              Replacements);
        double ->
            gpb_codegen:format_fn(
              mk_fn(d_field_, MsgName, FName),
              fun(<<0:48,240,127, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'InfinityOutParams',
                                          'TrUserData');
                 (<<0:48,240,255, Rest/binary>>, Z1, Z2, 'InParams',
                  'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, '-InfinityOutParams',
                                          'TrUserData');
                 (<<_:48,15:4,_:4,_:1,127:7, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'NanOutParams',
                                          'TrUserData');
                 (<<Value:64/little-float, Rest/binary>>, Z1, Z2,
                  'InParams', 'TrUserData') ->
                      '<call-read-field>'(Rest, Z1, Z2, 'OutParams',
                                          'TrUserData')
              end,
              Replacements)
    end.

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
    %% Contrary to the 64 bit encoding done for int32 (and enum),
    %% decode the value as 32 bits, so we decode negatives
    %% given both as 32 bits and as 64 bits wire encodings
    %% to the same integer.
    ?expr(
       <<'<Res>':'<N>'/signed-native>> = <<('<Value>'):'<N>'/unsigned-native>>,
       [replace_term('<N>', NumBits),
        replace_tree('<Res>', ResVar),
        replace_tree('<Value>', ValueExpr)]).

format_read_group_fn() ->
    ["read_group(Bin, FieldNum) ->\n"
     "    {NumBytes, EndTagLen} = read_gr_b(Bin, 0, 0, 0, 0, FieldNum),\n"
     "    <<Group:NumBytes/binary, _:EndTagLen/binary, Rest/binary>> = Bin,\n"
     "    {Group, Rest}.\n"
     "\n"
     "%% Like skipping over fields, but record the total length,\n"
     "%% Each field is <(FieldNum bsl 3) bor FieldType> ++ <FieldValue>\n"
     "%% Record the length because varints may be non-optimally encoded.\n"
     "%%\n"
     "%% Groups can be nested, but assume the same FieldNum cannot be nested\n"
     "%% because group field numbers are shared with the rest of the fields\n"
     "%% numbers. Thus we can search just for an group-end with the same\n"
     "%% field number.\n"
     "%%\n"
     "%% (The only time the same group field number could occur would\n"
     "%% be in a nested sub message, but then it would be inside a\n"
     "%% length-delimited entry, which we skip-read by length.)\n"
     "read_gr_b(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum)\n"
     "  when N < (32-7) ->\n"
     "    read_gr_b(Tl, N+7, X bsl N + Acc, NumBytes, TagLen+1, FieldNum);\n"
     "read_gr_b(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen,\n"
     "          FieldNum) ->\n"
     "    Key = X bsl N + Acc,\n"
     "    TagLen1 = TagLen + 1,\n"
     "    case {Key bsr 3, Key band 7} of\n"
     "        {FieldNum, 4} -> % 4 = group_end\n"
     "            {NumBytes, TagLen1};\n"
     "        {_, 0} -> % 0 = varint\n"
     "            read_gr_vi(Tl, 0, NumBytes + TagLen1, FieldNum);\n"
     "        {_, 1} -> % 1 = bits64\n"
     "            <<_:64, Tl2/binary>> = Tl,\n"
     "            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 8, 0, FieldNum);\n"
     "        {_, 2} -> % 2 = length_delimited\n"
     "            read_gr_ld(Tl, 0, 0, NumBytes + TagLen1, FieldNum);\n"
     "        {_, 3} -> % 3 = group_start\n"
     "            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);\n"
     "        {_, 4} -> % 4 = group_end\n"
     "            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);\n"
     "        {_, 5} -> % 5 = bits32\n"
     "            <<_:32, Tl2/binary>> = Tl,\n"
     "            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 4, 0, FieldNum)\n"
     "    end.\n"
     "\n"
     "read_gr_vi(<<1:1, _:7, Tl/binary>>, N, NumBytes, FieldNum)\n"
     "  when N < (64-7) ->\n"
     "    read_gr_vi(Tl, N+7, NumBytes+1, FieldNum);\n"
     "read_gr_vi(<<0:1, _:7, Tl/binary>>, _, NumBytes, FieldNum) ->\n"
     "    read_gr_b(Tl, 0, 0, NumBytes+1, 0, FieldNum).\n"
     "\n"
     "read_gr_ld(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum)\n"
     "  when N < (64-7) ->\n"
     "    read_gr_ld(Tl, N+7, X bsl N + Acc, NumBytes+1, FieldNum);\n"
     "read_gr_ld(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum) ->\n"
     "    Len = X bsl N + Acc,\n"
     "    NumBytes1 = NumBytes + 1,\n"
     "    <<_:Len/binary, Tl2/binary>> = Tl,\n"
     "    read_gr_b(Tl2, 0, 0, NumBytes1 + Len, 0, FieldNum).\n"].

classify_field_merge_action(FieldDef) ->
    case FieldDef of
        #?gpb_field{occurrence=required, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=optional, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=required, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=optional, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=required}                  -> overwrite;
        #?gpb_field{occurrence=optional}                  -> overwrite;
        #?gpb_field{occurrence=repeated}                  -> seqadd
    end.

format_msg_merge_code(Defs, AnRes, Opts) ->
    case contains_messages(Defs) of
        true  -> format_msg_merge_code_msgs(Defs, AnRes, Opts);
        false -> format_msg_merge_code_no_msgs(Opts)
    end.

format_msg_merge_code_no_msgs(Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            ["-spec merge_msgs(_, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New) ->
                       merge_msgs(Prev, New, [])
               end),
             "-spec merge_msgs(_, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _Opts) ->
                       erlang:error({gpb_error, no_messages})
               end)];
        maps ->
            ["-spec merge_msgs(_, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName) ->
                       merge_msgs(Prev, New, MsgName, [])
               end),
             "-spec merge_msgs(_, _, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _MsgName, _Opts) ->
                       erlang:error({gpb_error, no_messages})
               end)]
    end.

format_msg_merge_code_msgs(Defs, AnRes, Opts) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgDef} <- Defs],
    [format_merge_msgs_top_level(MsgNames, Opts),
     [case Type of
          msg ->
              format_msg_merger(Name, MsgDef, AnRes, Opts);
          group ->
              case is_repeated_group(Name, AnRes) of
                  true ->
                      %% merged with seq-add, not exported as top-level
                      %% ==> such groups are never merged recursively,
                      %%     thus never called
                      [];
                  false ->
                      format_msg_merger(Name, MsgDef, AnRes, Opts)
              end
      end
      || {Type, Name, MsgDef} <- msgs_or_groups(Defs)]].

is_repeated_group(GroupName, #anres{group_occurrences=D}) ->
    dict:fetch(GroupName, D) == repeated.

format_merge_msgs_top_level(MsgNames, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            [gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New) ->
                       merge_msgs(Prev, New, [])
               end),
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, Opts) when element(1, Prev) =:= element(1, New) ->
                       TrUserData = proplists:get_value(user_data, Opts),
                       case Prev of
                           '<msg-type>' -> '<merge-msg>'(Prev, New, TrUserData)
                       end
               end,
               [repeat_clauses(
                  '<msg-type>',
                  [[replace_tree('<msg-type>', record_match(MsgName, [])),
                    replace_term('<merge-msg>', mk_fn(merge_msg_, MsgName))]
                   || MsgName <- MsgNames])])];
        maps ->
            [gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName) ->
                       merge_msgs(Prev, New, MsgName, [])
               end),
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName, Opts) ->
                       TrUserData = proplists:get_value(user_data, Opts),
                       case MsgName of
                           '<msg-type>' -> '<merge-msg>'(Prev, New, TrUserData)
                       end
               end,
               [repeat_clauses(
                  '<msg-type>',
                  [[replace_tree('<msg-type>', erl_syntax:atom(MsgName)),
                    replace_term('<merge-msg>', mk_fn(merge_msg_, MsgName))]
                   || MsgName <- MsgNames])])]
    end.

format_msg_merger(MsgName, [], _AnRes, _Opts) ->
    gpb_codegen:format_fn(
      mk_fn(merge_msg_, MsgName),
      fun(_Prev, New, _TrUserData) -> New end);
format_msg_merger(MsgName, MsgDef, AnRes, Opts) ->
    TrUserDataVar = ?expr(TrUserData),
    {PrevMatch, NewMatch, ExtraInfo} =
        format_msg_merger_fnclause_match(MsgName, MsgDef, Opts),
    {MandatoryMergings, OptMergings} = compute_msg_field_mergers(
                                         ExtraInfo, MsgName, AnRes),
    gpb_codegen:format_fn(
      mk_fn(merge_msg_, MsgName),
      fun('Prev', 'New', 'MaybeTrUserData') ->
              '<merge-it>'
      end,
      [replace_tree('Prev',  PrevMatch),
       replace_tree('New',   NewMatch),
       splice_trees(
         '<merge-it>',
         do_exprs(fun(Elem, Var) ->
                          render_omissible_merger(Elem, Var, TrUserDataVar)
                  end,
                  render_field_mergers(MsgName, MandatoryMergings,
                                       TrUserDataVar, Opts),
                  OptMergings)),
       replace_tree('TrUserData', TrUserDataVar), % needed by some field mergers
       replace_tree('MaybeTrUserData',
                    case any_field_is_sub_msg(MsgDef)
                        orelse any_field_is_repeated(MsgDef)
                        orelse exists_tr_for_msg(MsgName, merge, AnRes) of
                        true  -> TrUserDataVar;
                        false -> ?expr(_)
                    end)]).

any_field_is_sub_msg(Fields) ->
    lists:any(fun(#?gpb_field{type={msg,_}}) -> true;
                 (#?gpb_field{type={group,_}}) -> true;
                 (#?gpb_field{type={map,_,_}}) -> true;
                 (#gpb_oneof{fields=Fs}) -> any_field_is_sub_msg(Fs);
                 (_) -> false
              end,
              Fields).

format_msg_merger_fnclause_match(_MsgName, [], _Opts) ->
    {?expr(PF), ?expr(_), no_fields};
format_msg_merger_fnclause_match(MsgName, MsgDef, Opts) ->
    FNames  = [get_field_name(Field) || Field <- MsgDef],
    PFVars  = [case is_required_overwrite_merge(Field) of
                   true  -> none;
                   false -> var("PF~s", [get_field_name(Field)])
               end
               || Field <- MsgDef],
    NFVars  = [var("NF~s", [FName]) || FName <- FNames],
    PFields = lists:zip(FNames, PFVars),
    NFields = lists:zip(FNames, NFVars),
    Infos = zip4(FNames, PFVars, NFVars, MsgDef),
    case get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            PFields1 = [{FName,PFVar} || {FName,PFVar} <- PFields,
                                         PFVar /= none],
            P = mapping_match(MsgName, PFields1, Opts),
            N = mapping_match(MsgName, NFields, Opts),
            {P, N, {pr, Infos}};
        {maps, omitted} ->
            {OptInfos, MandInfos} = key_partition_on_optionality(4, Infos),
            PMsg = ?expr(PMsg),
            NMsg = ?expr(NMsg),
            P = map_match([{FName, PFVar} || {FName,PFVar,_,_} <- MandInfos,
                                             PFVar /= none]),
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

is_required_overwrite_merge(#?gpb_field{occurrence=required}=Field) ->
    classify_field_merge_action(Field) == overwrite;
is_required_overwrite_merge(_Field) ->
    false.

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

format_field_merge_expr(#?gpb_field{name=FName, occurrence=Occur}=Field,
                        PF, NF, MsgName, AnRes)->
    Transforms = [replace_tree('PF', PF),
                  replace_tree('NF', NF)],
    case classify_field_merge_action(Field) of
        overwrite when Occur == required ->
            {required, {PF, NF}};
        overwrite ->
            {overwrite, {PF, NF}};
        seqadd ->
            ElemPath = [MsgName, FName],
            Append = find_translation(ElemPath, merge, AnRes, 'erlang_++'),
            {expr, ?expr('PF++NF'('PF', 'NF', 'TrUserData'),
                         Transforms ++ [replace_term('PF++NF',Append)])};
        msgmerge ->
            Tr = mk_find_tr_fn_elem_or_default(MsgName, Field, false, AnRes),
            #?gpb_field{type={_msg_or_group,SubMsgName}}=Field,
            {merge, {{PF, NF}, Tr, mk_fn(merge_msg_, SubMsgName)}}
    end;
format_field_merge_expr(#gpb_oneof{name=CFName, fields=OFields},
                        PF, NF, MsgName, AnRes) ->
    case [OField || #?gpb_field{type={msg,_}}=OField <- OFields] of
        [] ->
            {overwrite, {PF, NF}};
        MOFields ->
            {oneof,
             {{PF, NF},
              [begin
                   Tr = mk_find_tr_fn_elem_or_default(
                          MsgName, F, {true, CFName}, AnRes),
                   {OFName, Tr, mk_fn(merge_msg_, M2Name)}
               end
               || #?gpb_field{name=OFName, type={msg,M2Name}}=F <- MOFields]}}
    end.

reshape_cases_for_maps_find(Merges, PMsg, NMsg) ->
    [{FName, case Merge of
                 {overwrite, {_, _}} ->
                     {overwrite, {PMsg, NMsg}};
                 {merge, {{_, _}, Tr, MergeFn}} ->
                     {merge, {{PMsg, NMsg}, Tr, MergeFn}};
                 {oneof, {{_, _}, OFMerges}} ->
                     {oneof, {{PMsg, NMsg}, OFMerges}}
             end}
     || {FName, Merge} <- Merges].

render_field_mergers(MsgName, Mergings, TrUserDataVar, Opts) ->
    Fields = [{FName, render_field_merger(Merge, TrUserDataVar)}
              || {FName, Merge} <- Mergings],
    mapping_create(MsgName, Fields, Opts).

render_field_merger({required, {none, NF}}, _TrUserDataVar) ->
    NF;
render_field_merger({overwrite, {PF, NF}}, _TrUserDataVar) ->
    ?expr(if 'NF' =:= undefined -> 'PF';
             true               -> 'NF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF)]);
render_field_merger({expr, Expr}, _TrUserDataVar) ->
    Expr;
render_field_merger({merge, {{PF, NF}, Tr, MergeFn}}, TrUserDataVar) ->
    ?expr(if 'PF' /= undefined, 'NF' /= undefined -> 'merge'('PF', 'NF',
                                                             'TrUserData');
             'PF' == undefined -> 'NF';
             'NF' == undefined -> 'PF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF),
           replace_term('merge', Tr(merge, MergeFn)),
           replace_tree('TrUserData', TrUserDataVar)]);
render_field_merger({oneof, {{PF, NF}, OFMerges}}, TrUserDataVar) ->
    Transforms = [replace_tree('PF', PF),
                  replace_tree('NF', NF),
                  replace_tree('OPF', prefix_var("O", PF)),
                  replace_tree('ONF', prefix_var("O", NF)),
                  replace_tree('TrUserData', TrUserDataVar)],
    ?expr(case {'PF', 'NF'} of
              '{{tag,OPF},{tag,ONF}}' -> {'tag', 'merge'('OPF','ONF',
                                                         'TrUserData')};
              {_, undefined}          -> 'PF';
              _                       -> 'NF'
          end,
          [repeat_clauses(
             '{{tag,OPF},{tag,ONF}}',
             [[replace_tree('{{tag,OPF},{tag,ONF}}',
                            ?expr({{'tag','OPF'},{'tag','ONF'}})),
               replace_term('tag', OFName),
               replace_term('merge', Tr(merge, OFMergeFn)) | Transforms]
              || {OFName, Tr, OFMergeFn} <- OFMerges])
           | Transforms]).

render_omissible_merger({FName, {overwrite, {PMsg, NMsg}}}, Var,
                        TrUserDataVar) ->
    ?expr(case {'PMsg', 'NMsg'} of
              {_, '#{fname := NF}'} -> 'Var#{fname=>NF}';
              {'#{fname := PF}', _} -> 'Var#{fname=>PF}';
              _                     -> 'Var'
          end,
          std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar));
render_omissible_merger({FName, {merge, {{PMsg, NMsg}, Tr, MergeFn}}}, Var,
                        TrUserDataVar) ->
    Trs = std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar),
    MergeCallTmpl = ?expr('merge'('PF','NF', 'TrUserData'), Trs),
    ?expr(case {'PMsg', 'NMsg'} of
              {'#{fname := PF}', '#{fname := NF}'} ->
                  'Var#{fname=>merge(PF,NF)}';
              {_, '#{fname := NF}'} ->
                  'Var#{fname=>NF}';
              {'#{fname := PF}', _} ->
                  'Var#{fname=>PF}';
              {_, _} ->
                  'Var'
          end,
          [replace_tree('Var#{fname=>merge(PF,NF)}',
                        map_set(Var, [{FName,MergeCallTmpl}]))]
          ++ Trs
          ++ [replace_term('merge', Tr(merge, MergeFn))]);
render_omissible_merger({FName, {oneof, {{PMsg, NMsg}, OFMerges}}}, Var,
                       TrUserDataVar) ->
    OPF = var("OPF~s", [FName]),
    ONF = var("ONF~s", [FName]),
    OneofTransforms = [replace_tree('OPF', OPF),
                       replace_tree('ONF', ONF)],
    ?expr(case {'PMsg', 'NMsg'} of
              '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}' ->
                  'Var#{fname=>{tag,merge(OPF,ONF)}}';
              {_, '#{fname := NF}'} ->
                  'Var#{fname=>NF}';
              {'#{fname := PF}', _} ->
                  'Var#{fname=>PF}';
              {_, _} ->
                  'Var'
          end,
          [repeat_clauses(
             '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}',
             [begin
                  Trs2 = [replace_term('tag', OFName),
                          replace_term('merge', Tr(merge, OFMergeFn))]
                      ++ OneofTransforms,
                  MmO = map_match([{FName, ?expr({'tag', 'OPF'}, Trs2)}]),
                  MmN = map_match([{FName, ?expr({'tag', 'ONF'}, Trs2)}]),
                  MergeCall = ?expr({'tag','merge'('OPF','ONF', 'TrUserData')},
                                    Trs2),
                  [replace_tree(
                     '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}',
                     ?expr({'#{fname := {tag,OPF}}', '#{fname := {tag,ONF}}'},
                           [replace_tree('#{fname := {tag,OPF}}', MmO),
                            replace_tree('#{fname := {tag,ONF}}', MmN)])),
                   replace_tree('Var#{fname=>{tag,merge(OPF,ONF)}}',
                                map_set(Var, [{FName,MergeCall}]))
                   | Trs2]
              end
              || {OFName, Tr, OFMergeFn} <- OFMerges])
           | std_omitable_merge_transforms(PMsg, NMsg, FName, Var,
                                           TrUserDataVar)]).

std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar) ->
    PF = var("PF~s", [FName]),
    NF = var("NF~s", [FName]),
    [replace_term('fname', FName),
     replace_tree('PMsg', PMsg),
     replace_tree('NMsg', NMsg),
     replace_tree('PF', PF),
     replace_tree('NF', NF),
     replace_tree('Var', Var),
     replace_tree('Var#{fname=>NF}', map_set(Var, [{FName, NF}])),
     replace_tree('Var#{fname=>PF}', map_set(Var, [{FName, PF}])),
     replace_tree('#{fname := NF}', map_match([{FName, NF}])),
     replace_tree('#{fname := PF}', map_match([{FName, PF}])),
     replace_tree('TrUserData', TrUserDataVar)].

format_field_skippers(MsgName, AnRes) ->
    SkipVarintFnName = mk_fn(skip_varint_, MsgName),
    SkipLenDelimFnName = mk_fn(skip_length_delimited_, MsgName),
    SkipGroupFnName = mk_fn(skip_group_, MsgName),
    ReadFieldFnName = mk_fn(dfp_read_field_def_, MsgName),
    Params = decoder_params(MsgName, AnRes),
    [%% skip_varint_<MsgName>/2,4
     gpb_codegen:format_fn(
       SkipVarintFnName,
       fun(<<1:1, _:7, Rest/binary>>, Z1, Z2, '<Params>', TrUserData) ->
               call_self(Rest, Z1, Z2, '<Params>', TrUserData);
          (<<0:1, _:7, Rest/binary>>, Z1, Z2, '<Params>', TrUserData) ->
               '<call-read-field>'(Rest, Z1,Z2, '<Params>', TrUserData)
       end,
       [replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_length_delimited_<MsgName>/4
     gpb_codegen:format_fn(
       SkipLenDelimFnName,
       fun(<<1:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData)
             when N < ?NB ->
               call_self(Rest, N+7, X bsl N + Acc, '<Params>', TrUserData);
          (<<0:1, X:7, Rest/binary>>, N, Acc, '<Params>', TrUserData) ->
               Length = X bsl N + Acc,
               <<_:Length/binary, Rest2/binary>> = Rest,
               '<call-read-field>'(Rest2, 0, 0, '<Params>', TrUserData)
       end,
       [replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_group_<MsgName>/4
     gpb_codegen:format_fn(
       SkipGroupFnName,
       fun(Bin, FNum, Z2, '<Params>', TrUserData) ->
          {_, Rest} = read_group(Bin, FNum),
          '<call-read-field>'(Rest, 0, Z2, '<Params>', TrUserData)
       end,
       [replace_term('<call-read-field>', ReadFieldFnName),
        splice_trees('<Params>', Params)]),
     "\n",
     %% skip_32_<MsgName>/2,4
     %% skip_64_<MsgName>/2,4
     [[gpb_codegen:format_fn(
         mk_fn(skip_, NumBits, MsgName),
         fun(<<_:'<NumBits>', Rest/binary>>, Z1, Z2, '<Params>', TrUserData) ->
                 '<call-read-field>'(Rest, Z1, Z2, '<Params>', TrUserData)
         end,
         [replace_term('<call-read-field>', ReadFieldFnName),
          replace_term('<NumBits>', NumBits),
          splice_trees('<Params>', Params)]),
       "\n"]
      || NumBits <- [32, 64]]].

%% -- verifiers -----------------------------------------------------

format_verifiers_top_function(Defs, Opts) ->
    case {contains_messages(Defs), get_records_or_maps_by_opts(Opts)} of
        {false, records} -> format_verifiers_top_no_msgs_r();
        {false, maps}    -> format_verifiers_top_no_msgs_m();
        {true,  _}       -> format_verifiers_top_with_msgs(Defs, Opts)
    end.

format_verifiers_top_no_msgs_r() ->
    [?f("-spec verify_msg(_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg) -> call_self(Msg, []) end),
     ?f("-spec verify_msg(_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg,_Opts) -> mk_type_error(not_a_known_message, Msg, []) end),
     "\n"].

format_verifiers_top_no_msgs_m() ->
    [?f("-spec verify_msg(_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, MsgName) -> call_self(Msg, MsgName, []) end),
     ?f("-spec verify_msg(_,_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, _MsgName, _Opts) ->
               mk_type_error(not_a_known_message, Msg, [])
       end),
     "\n"].

format_verifiers_top_with_msgs(Defs, Opts) ->
    Mapping = get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(MsgName)]
                  end,
    [gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, '<MsgName>') -> call_self(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, '<MsgName>', Opts) ->
               TrUserData = proplists:get_value(user_data, Opts),
               case '<MsgOrMsgName>' of
                   '<msg-match>' -> '<verify-msg>'(Msg, ['<MsgName>'],
                                                   TrUserData);
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
        splice_trees('<MsgName>', MsgNameVars)])].

format_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifiers(Defs, AnRes, Opts),
     format_enum_verifiers(Defs, AnRes, Opts),
     format_type_verifiers(AnRes, Opts),
     format_map_verifiers(AnRes, Opts),
     format_verifier_auxiliaries(Defs)
    ].

format_msg_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifier(MsgName, MsgDef, AnRes, Opts)
     || {_Type, MsgName, MsgDef} <- msgs_or_groups(Defs)].

format_msg_verifier(MsgName, MsgDef, AnRes, Opts) ->
    FNames = get_field_names(MsgDef),
    FVars = [var_f_n(I) || I <- lists:seq(1, length(FNames))],
    MsgVar = ?expr(M),
    {FieldMatching, NonOptKeys} =
        case get_mapping_and_unset_by_opts(Opts) of
            X when X == records;
                   X == {maps, present_undefined} ->
                {mapping_match(MsgName, lists:zip(FNames, FVars), Opts),
                 FNames};
            {maps, omitted} ->
                FMap = zip_for_non_opt_fields(MsgDef, FVars),
                if length(FMap) == length(FNames) ->
                        {map_match(FMap), FNames};
                   length(FMap) < length(FNames) ->
                        {?expr('mapmatch' = 'M',
                               [replace_tree('mapmatch', map_match(FMap)),
                                replace_tree('M', MsgVar)]),
                         [K || {K, _} <- FMap]}
                end
        end,
    NeedsMatchOther = case get_records_or_maps_by_opts(Opts) of
                          records -> can_occur_as_sub_msg(MsgName, AnRes);
                          maps    -> true
                      end,
    FnName = mk_fn(v_msg_, MsgName),
    TrUserDataVar = ?expr(TrUserData),
    [nowarn_dialyzer_attr(FnName,3,Opts),
     gpb_codegen:format_fn(
       FnName,
       fun('<msg-match>', '<Path>', 'MaybeTrUserData') ->
               '<verify-fields>',
               ok;
          ('<M>', Path, _TrUserData) when is_map('<M>') ->
               mk_type_error(
                 {missing_fields, 'NonOptKeys'--maps:keys('<M>'), '<MsgName>'},
                 '<M>', Path);
          ('<X>', Path, _TrUserData) ->
               mk_type_error({expected_msg,'<MsgName>'}, X, Path)
       end,
       [replace_tree('<msg-match>', FieldMatching),
        replace_tree('<Path>', if MsgDef == [] -> ?expr(_Path);
                                  MsgDef /= [] -> ?expr(Path)
                               end),
        replace_tree('MaybeTrUserData',
                     case any_field_is_sub_msg(MsgDef)
                         orelse exists_tr_for_msg(MsgName, verify, AnRes) of
                         true  -> TrUserDataVar;
                         false -> ?expr(_)
                     end),
        splice_trees('<verify-fields>',
                     field_verifiers(MsgName, MsgDef, FVars, MsgVar,
                                     TrUserDataVar,
                                     AnRes, Opts)),
        repeat_clauses('<X>', case NeedsMatchOther of
                                  true  -> [[replace_tree('<X>', ?expr(X))]];
                                  false -> [] %% omit the else clause
                              end),
        repeat_clauses('<M>',
                       case get_records_or_maps_by_opts(Opts) of
                           records ->
                               []; % omit this clause
                           maps ->
                               [[replace_tree('<M>', ?expr(M)),
                                 replace_term('NonOptKeys', NonOptKeys)]]
                       end),
        replace_term('<MsgName>', MsgName)])].

field_verifiers(MsgName, Fields, FVars, MsgVar, TrUserDataVar, AnRes, Opts) ->
    [field_verifier(MsgName, Field, FVar, MsgVar, TrUserDataVar, AnRes, Opts)
     || {Field, FVar} <- lists:zip(Fields, FVars)].

field_verifier(MsgName,
               #?gpb_field{name=FName, type=Type, occurrence=Occurrence}=Field,
               FVar, MsgVar, TrUserDataVar, AnRes, Opts) ->
    FVerifierFn = case Type of
                      {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                      {group,GName}   -> mk_fn(v_msg_, GName);
                      {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                      {map,KT,VT}     -> mk_fn(v_, map_type_to_msg_name(KT,VT));
                      Type            -> mk_fn(v_type_, Type)
                  end,
    ElemPath = mk_elempath_elem(MsgName, Field, false),
    FVerifierFn2 = find_translation(ElemPath, verify, AnRes, FVerifierFn),
    Replacements = [replace_term('<verify-fn>', FVerifierFn2),
                    replace_tree('<F>', FVar),
                    replace_term('<FName>', FName),
                    replace_term('<Type>', Type),
                    replace_tree('TrUserData', TrUserDataVar),
                    splice_trees('MaybeTrUserData',
                                 maybe_userdata_param(Field, TrUserDataVar))],
    IsMapField = case Type of
                     {map,_,_} -> true;
                     _ -> false
                 end,
    case Occurrence of
        required ->
            %% FIXME: check especially for `undefined'
            %% and if found, error out with required_field_not_set
            %% specifying expected type
            ?expr('<verify-fn>'('<F>', ['<FName>' | Path], 'MaybeTrUserData'),
                  Replacements);
        repeated when not IsMapField ->
            ?expr(if is_list('<F>') ->
                          %% _ = [...] to avoid dialyzer error
                          %% "Expression produces a value of type
                          %% ['ok'], but this value is unmatched"
                          %% with the -Wunmatched_returns flag.
                          _ = ['<verify-fn>'(Elem, ['<FName>' | Path],
                                             'MaybeTrUserData')
                               || Elem <- '<F>'],
                          ok;
                     true ->
                          mk_type_error(
                            {invalid_list_of, '<Type>'},
                            '<F>',
                            ['<FName>' | Path])
                  end,
                  Replacements);
        repeated when IsMapField ->
            ?expr('<verify-fn>'('<F>', ['<FName>' | Path], 'TrUserData'),
                  Replacements);
        optional ->
            case get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path],
                                                  'MaybeTrUserData')
                          end,
                          Replacements);
                {maps, omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'MaybeTrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        map_match([{FName, FVar}])),
                           replace_tree('M', MsgVar) | Replacements])
            end
    end;
field_verifier(MsgName, #gpb_oneof{name=FName, fields=OFields},
               FVar, MsgVar, TrUserDataVar, AnRes, Opts) ->
    IsOneof = {true, FName},
    case get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            ?expr(
               case '<F>' of
                   undefined ->
                       ok;
                   '<oneof-pattern>' ->
                       '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path],
                                     'MaybeTrUserData');
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
                       ElemPath = mk_elempath_elem(MsgName, F, IsOneof),
                       FVerifierFn2 = find_translation(ElemPath, verify, AnRes,
                                                       FVerifierFn),
                       OFVar = prefix_var("O", FVar),
                       [replace_tree('M', MsgVar),
                        replace_tree('<oneof-pattern>',
                                     ?expr({'<OFName>','<OFVar>'})),
                        replace_term('<verify-fn>', FVerifierFn2),
                        replace_tree('<OFVar>', OFVar),
                        replace_term('<OFName>', OFName),
                        splice_trees('MaybeTrUserData',
                                     maybe_userdata_param(F, TrUserDataVar))]
                   end
                   || #?gpb_field{name=OFName, type=Type}=F <- OFields])]);
        {maps, omitted} ->
            ?expr(
               case 'M' of
                   '<oneof-pattern>' ->
                       '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path],
                                     'MaybeTrUserData');
                   '#{<FName> := <F>}' ->
                       mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path]);
                   _ ->
                       ok
               end,
               [replace_tree('<F>', FVar),
                replace_term('<FName>', FName),
                replace_tree('M', MsgVar),
                replace_tree('#{<FName> := <F>}', map_match([{FName, FVar}])),
                repeat_clauses(
                  '<oneof-pattern>',
                  [begin
                       FVerifierFn =
                           case Type of
                               {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                               {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                               Type            -> mk_fn(v_type_, Type)
                           end,
                       ElemPath = mk_elempath_elem(MsgName, F, IsOneof),
                       FVerifierFn2 = find_translation(ElemPath, verify, AnRes,
                                                       FVerifierFn),
                       OFVar = prefix_var("O", FVar),
                       Trs1 = [replace_tree('<OFVar>', OFVar),
                               replace_term('<OFName>', OFName)],
                       OFPat = ?expr({'<OFName>','<OFVar>'}, Trs1),
                       [replace_tree('<oneof-pattern>',
                                     map_match([{FName, OFPat}])),
                        replace_term('<verify-fn>', FVerifierFn2),
                        splice_trees('MaybeTrUserData',
                                     maybe_userdata_param(F, TrUserDataVar))
                        | Trs1]
                   end
                   || #?gpb_field{name=OFName, type=Type}=F <- OFields])])
    end.


can_occur_as_sub_msg(MsgName, #anres{used_types=UsedTypes}) ->
    sets:is_element({msg,MsgName}, UsedTypes)
        orelse sets:is_element({group,MsgName}, UsedTypes).

format_enum_verifiers(Defs, #anres{used_types=UsedTypes}, Opts) ->
    [format_enum_verifier(EnumName, Def, Opts)
     || {{enum,EnumName}, Def} <- Defs,
        smember({enum, EnumName}, UsedTypes)].

format_enum_verifier(EnumName, EnumMembers, Opts) ->
    FnName = mk_fn(v_enum_, EnumName),
    [nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun('<sym>', _Path) -> ok;
          (V, Path) when is_integer(V) -> v_type_sint32(V, Path);
          (X, Path) -> mk_type_error({invalid_enum, '<EnumName>'}, X, Path)
       end,
       [repeat_clauses('<sym>', [[replace_term('<sym>', EnumSym)]
                                 || {EnumSym, _Value} <- EnumMembers]),
        replace_term('<EnumName>', EnumName)])].

format_type_verifiers(#anres{used_types=UsedTypes}, Opts) ->
    [[format_int_verifier(sint32, signed, 32, Opts)
      || smember(sint32, UsedTypes) orelse any_enum_field_exists(UsedTypes)],
     [format_int_verifier(Type, Signedness, Bits, Opts)
      || {Type, Signedness, Bits} <- [{sint64,   signed,   64},
                                      {int32,    signed,   32},
                                      {int64,    signed,   64},
                                      {uint32,   unsigned, 32},
                                      {uint64,   unsigned, 64},
                                      {fixed32,  unsigned, 32},
                                      {fixed64,  unsigned, 64},
                                      {sfixed32, signed,   32},
                                      {sfixed64, signed,   64}],
         smember(Type, UsedTypes)],
     [format_bool_verifier(Opts)                || smember(bool, UsedTypes)],
     [format_float_verifier(float, Opts)        || smember(float, UsedTypes)],
     [format_float_verifier(double, Opts)       || smember(double, UsedTypes)],
     [format_string_verifier(Opts)              || smember(string, UsedTypes)],
     [format_bytes_verifier(Opts)               || smember(bytes, UsedTypes)]].

format_int_verifier(IntType, Signedness, NumBits, Opts) ->
    Min = case Signedness of
              unsigned -> 0;
              signed   -> -(1 bsl (NumBits-1))
          end,
    Max = case Signedness of
              unsigned -> 1 bsl NumBits - 1;
              signed   -> 1 bsl (NumBits-1) - 1
          end,
    FnName = mk_fn(v_type_, IntType),
    [nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
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
                                   erl_syntax:integer(NumBits)])])].

format_bool_verifier(Opts) ->
    FnName = mk_fn(v_type_, bool),
    [nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(false, _Path) -> ok;
          (true, _Path)  -> ok;
          (0, _Path)  -> ok;
          (1, _Path)  -> ok;
          (X, Path) -> mk_type_error(bad_boolean_value, X, Path)
       end)].

format_float_verifier(FlType, Opts) ->
    BadTypeOfValue = list_to_atom(lists:concat(["bad_", FlType, "_value"])),
    FnName = mk_fn(v_type_, FlType),
    [nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(N, _Path) when is_float(N) -> ok;
          %% It seems a float for the corresponding integer value is
          %% indeed packed when doing <<Integer:32/little-float>>.
          %% So let verify accept integers too.
          %% When such a value is unpacked, we get a float.
          (N, _Path) when is_integer(N) -> ok;
          (infinity, _Path)    -> ok;
          ('-infinity', _Path) -> ok;
          (nan, _Path)         -> ok;
          (X, Path)            -> mk_type_error('<bad_x_value>', X, Path)
       end,
       [replace_term('<bad_x_value>', BadTypeOfValue)])].

format_string_verifier(Opts) ->
    FnName = mk_fn(v_type_, string),
    [nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
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
       end)].

format_bytes_verifier(Opts) ->
    FnName = mk_fn(v_type_, bytes),
    [nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(B, _Path) when is_binary(B) ->
               ok;
          (B, _Path) when is_list(B) ->
               ok;
          (X, Path) ->
               mk_type_error(bad_binary_value, X, Path)
       end)].

format_map_verifiers(#anres{map_types=MapTypes}=AnRes, Opts) ->
    MapsOrTuples = get_2tuples_or_maps_for_maptype_fields_by_opts(Opts),
    [format_map_verifier(KeyType, ValueType, MapsOrTuples, AnRes, Opts)
     || {KeyType,ValueType} <- sets:to_list(MapTypes)].

format_map_verifier(KeyType, ValueType, MapsOrTuples, AnRes, Opts) ->
    MsgName = map_type_to_msg_name(KeyType, ValueType),
    FnName = mk_fn(v_, MsgName),
    KeyVerifierFn = mk_fn(v_type_, KeyType),
    ValueVerifierFn1 = case ValueType of
                           {msg,FMsgName}  -> mk_fn(v_msg_, FMsgName);
                           {enum,EnumName} -> mk_fn(v_enum_, EnumName);
                           Type            -> mk_fn(v_type_, Type)
                       end,
    ElemPath = [MsgName,value],
    ValueVerifierFn2 = find_translation(ElemPath, verify, AnRes,
                                        ValueVerifierFn1),

    TrUserDataVar = ?expr(TrUserData),
    TrUserDataReplacements =
        case {ValueType,{ValueVerifierFn1, ValueVerifierFn2}} of
            {{msg,_}, _} ->
                [replace_tree('MaybeTrUserDataArg', TrUserDataVar),
                 replace_tree('MaybeTrUserData', TrUserDataVar)];
            {_, {X,Y}} when X /= Y ->
                %% Translation exists
                [replace_tree('MaybeTrUserDataArg', TrUserDataVar),
                 replace_tree('MaybeTrUserData', TrUserDataVar)];
            _ ->
                [replace_tree('MaybeTrUserDataArg', ?expr(_)),
                 splice_trees('MaybeTrUserData', [])]
        end,
    [nowarn_dialyzer_attr(FnName, 3, Opts),
     case MapsOrTuples of
         '2tuples' ->
             gpb_codegen:format_fn(
               FnName,
               fun(KVs, Path, 'MaybeTrUserDataArg') when is_list(KVs) ->
                       [case X of
                            {Key, Value} ->
                                'VerifyKey'(Key, ['key' | Path]),
                                'VerifyValue'(Value, ['value' | Path],
                                             'MaybeTrUserData');
                            _ ->
                                mk_type_error(invalid_key_value_tuple, X, Path)
                        end
                        || X <- KVs],
                       ok;
                  (X, Path, _TrUserData) ->
                       mk_type_error(invalid_list_of_key_value_tuples, X, Path)
               end,
               [replace_term('VerifyKey', KeyVerifierFn),
                replace_term('VerifyValue', ValueVerifierFn2)]
               ++ TrUserDataReplacements);
         maps ->
             gpb_codegen:format_fn(
               FnName,
               fun(M, Path, 'MaybeTrUserDataArg') when is_map(M) ->
                       [begin
                            'VerifyKey'(Key, ['key' | Path]),
                            'VerifyValue'(Value, ['value' | Path],
                                         'MaybeTrUserData')
                        end
                        || {Key, Value} <- maps:to_list(M)],
                       ok;
                  (X, Path, _TrUserData) ->
                       mk_type_error(invalid_map, X, Path)
               end,
               [replace_term('VerifyKey', KeyVerifierFn),
                replace_term('VerifyValue', ValueVerifierFn2)]
               ++ TrUserDataReplacements)
     end].

format_verifier_auxiliaries(Defs) ->
    ["-spec mk_type_error(_, _, list()) -> no_return().\n",
     gpb_codegen:format_fn(
       mk_type_error,
       fun(Error, ValueSeen, Path) ->
               Path2 = prettify_path(Path),
               erlang:error({gpb_type_error,
                             {Error, [{value, ValueSeen},{path, Path2}]}})
       end),
     "\n",
     case contains_messages(Defs) of
         false ->
             gpb_codegen:format_fn(
               prettify_path,
               fun([]) -> top_level end);
         true ->
             gpb_codegen:format_fn(
               prettify_path,
               fun([]) ->
                       top_level;
                  (PathR) ->
                       list_to_atom(
                         string:join(
                           lists:map(fun atom_to_list/1, lists:reverse(PathR)),
                           "."))
               end)
     end].

%% -- descr -----------------------------------------------------

possibly_format_descriptor(Defs, Opts) ->
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

%% -- hrl -----------------------------------------------------

possibly_format_hrl(Mod, Defs, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> format_hrl(Mod, Defs, Opts);
        maps    -> '$not_generated'
    end.

format_hrl(Mod, Defs, Opts) ->
    ModVsn = list_to_atom(atom_to_list(Mod) ++ "_gpb_version"),
    iolist_to_utf8_or_escaped_binary(
      [?f("%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n",
          [?MODULE, gpb:version_as_string()]),
       "\n",
       ?f("-ifndef(~p).~n", [Mod]),
       ?f("-define(~p, true).~n", [Mod]),
       "\n",
       ?f("-define(~p, \"~s\").~n", [ModVsn, gpb:version_as_string()]),
       "\n",
       string:join(
         [gpb_gen_types:format_msg_record(Msg, Fields, Opts, Defs)
          || {_,Msg,Fields} <- msgs_or_groups(Defs)],
         "\n"),
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
    AttrForms = lists:filter(fun(A) ->
                                     case erl_syntax_lib:analyze_attribute(A) of
                                         {export_type, _} -> false;
                                         {type, _}        -> false;
                                         _                -> true
                                     end
                             end,
                             AttrForms0),
    TypeForms = AttrForms -- AttrForms0,
    FieldDef = field_record_to_attr_form(),
    OneofDef = oneof_record_to_attr_form(),
    RpcDef   = rpc_record_to_attr_form(),
    RecordBaseDefs = [FieldDef, OneofDef, RpcDef],
    MsgRecordForms = msgdefs_to_record_attrs(MsgDefs),
    AllForms = AttrForms ++ RecordBaseDefs ++ MsgRecordForms ++ TypeForms ++
        CodeForms,
    combine_erl_and_possible_nif(compile:noenv_forms(AllForms, Opts),
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

split_forms_at_first_code_2([Form | Rest], Acc) ->
    case erl_syntax_lib:analyze_form(Form) of
        {attribute, _Info} -> split_forms_at_first_code_2(Rest, [Form | Acc]);
        {function, _Info}  -> {lists:reverse(Acc), [Form | Rest]}
    end.

field_record_to_attr_form() ->
    record_to_attr(?gpb_field, record_info(fields, ?gpb_field)).

oneof_record_to_attr_form() ->
    record_to_attr(gpb_oneof, record_info(fields, gpb_oneof)).

rpc_record_to_attr_form() ->
    record_to_attr(?gpb_rpc, record_info(fields, ?gpb_rpc)).

msgdefs_to_record_attrs(Defs) ->
    [record_to_attr(Name, lists:map(fun gpb_field_to_record_field/1, Fields))
     || {_msg_or_group, Name, Fields} <- msgs_or_groups(Defs)].

record_to_attr(RecordName, Fields) ->
    erl_syntax:revert(
      erl_syntax:attribute(
        erl_syntax:atom(record),
        [erl_syntax:atom(RecordName),
         erl_syntax:tuple(
           [case F of
                {FName, Default} ->
                    erl_syntax:record_field(erl_syntax:atom(FName),
                                            erl_parse:abstract(Default));
                {FName} ->
                    erl_syntax:record_field(erl_syntax:atom(FName), none);
                FName when is_atom(FName) ->
                    erl_syntax:record_field(erl_syntax:atom(FName), none)
            end
            || F <- Fields])])).

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
    %% Encode as a 64 bit value, for interop compatibility.
    %% Some implementations don't decode 32 bits properly,
    %% and Google's protobuf (C++) encodes as 64 bits
    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
    varint_to_binary_fields(N).

key_to_binary_fields(FNum, {group,_}) ->
    key_to_binary_fields(FNum, group_start);
key_to_binary_fields(FNum, Type) ->
    Key = (FNum bsl 3) bor gpb:encode_wiretype(Type),
    varint_to_binary_fields(Key).

varint_to_binary_fields(IntValue) ->
    [erl_syntax:binary_field(?expr('<n>', [replace_term('<n>', N)]), [])
     || N <- binary_to_list(gpb:encode_varint(IntValue))].

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

%% lists_replace(N, List, New) -> NewList
%% Like erlang:setelement, but for a list:
%% Replace the Nth element in List with a New value.
lists_setelement(1, [_ | Rest], New) ->
    [New | Rest];
lists_setelement(N, [X | Rest], New) when N > 1 ->
    [X | lists_setelement(N - 1, Rest, New)].

zip4([A|T1], [B|T2], [C|T3], [D|T4]) -> [{A,B,C,D} | zip4(T1, T2, T3, T4)];
zip4([], [], [], [])                 -> [].

get_epb_functions_by_opts(Opts) ->
    proplists:get_bool(epb_functions, Opts).

proto2_type_default(Type, Defs, Opts) ->
    type_default(Type, Defs, Opts, fun gpb:proto2_type_default/2).

proto3_type_default(Type, Defs, Opts) ->
    type_default(Type, Defs, Opts, fun gpb:proto3_type_default/2).

type_default(Type, Defs, Opts, GetTypeDefault) ->
    if Type == string ->
            case get_strings_as_binaries_by_opts(Opts) of
                true ->
                    list_to_binary(GetTypeDefault(Type, Defs));
                false ->
                    GetTypeDefault(Type, Defs)
            end;
       Type /= string ->
            GetTypeDefault(Type, Defs)
    end.

%% msgs(Defs) ->
%%     [{Name,Fields} || {{msg, Name}, Fields} <- msgs_or_groups(Defs)].

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
