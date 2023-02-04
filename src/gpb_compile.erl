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

%%% ------------------------------------------------------------------
%%% @doc
%%% Compile protobuf definition files to a module that can encode and decode
%%% values to and from binaries.
%%%
%%% The Erlang types for the values are as follows
%%%
%%% <a id="protobuf-to-erlang-types"/>
%%% <table rules="all" frame="border">
%%% <thead><tr><th align="left">Protobuf type</th>
%%%            <th align="left">Erlang type</th></tr></thead>
%%% <tbody>
%%% <!-- = = = = = = = = = = = = = = = = = = = = = = = = = = = -->
%%% <tr><td>double, float</td>
%%%     <td>float() | infinity | '-infinity' | nan<br/>
%%%         When encoding, integers, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>   int32,    int64<br/>
%%%           uint32,   uint64<br/>
%%%           sint32,   sint64<br/>
%%%          fixed32,  fixed64<br/>
%%%         sfixed32, sfixed64</td>
%%%     <td>integer()</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>bool</td>
%%%     <td>true | false<br/>
%%%         When encoding, the integers 1 and 0, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>enum</td>
%%%     <td>atom()<br/>
%%%         unknown enums decode to integer()</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>message</td>
%%%     <td>record (thus tuple())<br/>
%%%         or map() if the maps (-maps) option is specified</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>string</td>
%%%     <td>unicode string, thus list of integers<br/>
%%%         or binary() if the strings_as_binaries (-strbin) option is
%%%         specified<br/>
%%%         When encoding, iolists, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>bytes</td>
%%%     <td>binary()<br/>
%%%         When encoding, iolists, too, are accepted</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td>oneof</td>
%%%     <td><tt>{ChosenFieldName, Value}</tt><br/>
%%%         or <tt>ChosenFieldName => Value</tt> if the {maps_oneof,flat}
%%%         (-maps_oneof flat) option is specified</td></tr>
%%% <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
%%% <tr><td><![CDATA[map<_,_>]]></td>
%%%     <td>An unordered list of 2-tuples, <tt>[{Key,Value}]</tt><br/>
%%%         or  a map(), if the maps (-maps) option is specified</td></tr>
%%% </tbody></table>
%%%
%%% Repeated fields are represented as lists.
%%%
%%% Optional fields are represented as either the value or `undefined' if
%%% not set. However, for maps, if the option `maps_unset_optional' is set
%%% to `omitted', then unset optional values are omitted from the map,
%%% instead of being set to `undefined' when encoding messages. When
%%% decoding messages, even with `maps_unset_optional' set to `omitted',
%%% the default value will be set in the decoded map.
%%%
%%% @end
%%% ------------------------------------------------------------------


-module(gpb_compile).
%-compile(export_all).
-export([file/1, file/2]).
-export([string/2, string/3]).
-export([proto_defs/2, proto_defs/3, proto_defs/5]).
-export([msg_defs/2, msg_defs/3]).
-export([list_io/2]).
-export([string_list_io/2, string_list_io/3]).
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

-import(gpb_lib, [replace_term/2, repeat_clauses/2]).

%% -- Types -----------------------------------------------------

%% Options
-type boolean_opt(X) :: X | {X, boolean()}.% Just an option `X' means `{X,true}'
-type directory() :: string().
-type filename() :: string().

-type opts() :: [opt()].
-type opt() ::
        %% Input files and output files and formats
        {i, directory()} |
        {o, directory()} |
        {o_erl, directory()} | {o_hrl, directory()} |
        {o_nif_cc, directory()} |
        binary |
        to_proto_defs | to_msg_defs |
        {import_fetcher, import_fetcher_fun()} |
        boolean_opt(ignore_wellknown_types_directory) |
        %% Format of the Erlang representation
        boolean_opt(strings_as_binaries) |
        boolean_opt(maps) |
        boolean_opt(msgs_as_maps) |
        boolean_opt(mapfields_as_maps) |
        {maps_unset_optional, omitted | present_undefined} |
        {maps_oneof, tuples | flat} |
        {maps_key_type, atom | binary} |
        %% Verification of input
        {verify, optionally | always | never} |
        boolean_opt(verify_decode_required_present) |
        boolean_opt(gen_verifiers) |
        %% Renaming for the Erlang side
        {rename, renaming()} |
        {msg_name_prefix, msg_name_prefix()} |
        {msg_name_suffix, name_part()} |
        boolean_opt(msg_name_to_lower) |
        boolean_opt(msg_name_to_snake_case) |
        {module_name_prefix, name_part()} |
        {module_name_suffix, name_part()} |
        {module_name, new_name()} |
        %% What to generate and how
        boolean_opt(use_packages) |
        boolean_opt(descriptor) |
        boolean_opt(include_as_lib) |
        {include_mod_hrl_prepend, string()} |
        boolean_opt(bypass_wrappers) |
        {copy_bytes, true | false | auto | integer() | float()} |
        boolean_opt(type_specs) |
        boolean_opt(defaults_for_omitted_optionals) |
        boolean_opt(type_defaults_for_omitted_optionals) |
        {target_erlang_version, target_erlang_version()} |
        boolean_opt(preserve_unknown_fields) |
        {erlc_compile_options, string()} |
        %% Introspection of the proto definitions
        {proto_defs_version, gpb_defs:version()} |
        {introspect_proto_defs_version, gpb_defs:version() | preferably_1} |
        boolean_opt(introspect_get_proto_defs) |
        boolean_opt(defs_as_proplists) |
        boolean_opt(defs_as_maps) |
        boolean_opt(gen_introspect) |
        %% JSON
        boolean_opt(json) |
        boolean_opt(json_always_print_primitive_fields) |
        boolean_opt(json_preserve_proto_field_names) |
        boolean_opt(json_case_insensitive_enum_parsing) |
        {json_format, json_format()} |
        {json_object_format, json_object_format()} |
        {json_key_format, json_key_format()} |
        {json_array_format, json_array_format()} |
        {json_string_format, json_string_format()} |
        {json_null, atom()} |
        %% NIF
        boolean_opt(nif) |
        {load_nif, string()} |
        boolean_opt(gen_mergers) |
        %% Transslations
        {translate_type, {gpb_field_type(), [translation()]}} |
        {any_translate, [translation()]} |
        {translate_field, {field_path(), [translation()]}} |
        %% Compatibility with Erlang protobuffs
        boolean_opt(epb_compatibility) |
        boolean_opt(epb_functions) |
        %% Querying dependencies
        {list_deps, list_deps_format()} |
        {list_deps_dest_file, filename()} |
        boolean_opt(list_deps_missing_imports_are_generated) |
        boolean_opt(list_deps_makefile_phonies) |
        {list_deps_makefile_target, list_deps_makefile_target()} |
        boolean_opt(list_deps_and_generate) |
        %% Errors and warnings
        return |
        boolean_opt(return_warnings) | boolean_opt(return_errors) |
        report |
        boolean_opt(report_warnings) | boolean_opt(report_errors) |
        boolean_opt(warnings_as_errors) |
        %% Unknown options are allowed but ignored:
        term().

-type msg_name_prefix() :: name_part() | {by_proto, prefix_by_proto()}.
-type name_part() :: string() | atom().
-type new_name() :: string() | atom().

-type renaming() :: {pkg_name, name_change()} |
                    {msg_name, msg_name_change()} |
                    {msg_fqname, msg_name_change()} |
                    {group_name, name_change()} |
                    {group_fqname, name_change()} |
                    {service_name, name_change()} |
                    {service_fqname, name_change()} |
                    {rpc_name, name_change()} |
                    {msg_typename, name_change()} |
                    {enum_typename, name_change()}.

-type name_change() :: {prefix, name_part()} |
                       {suffix, name_part()} |
                       lowercase |
                       snake_case |
                       dots_to_underscores |
                       base_name.

-type msg_name_change() :: name_change() |
                           {prefix, {by_proto, prefix_by_proto()}}.

-type prefix_by_proto() :: [{ProtoName::atom(), Prefix::name_part()}].


-type field_path() :: [atom() | []].
-type translation() :: {encode, mod_fn_argtemplate()} |
                       {decode, mod_fn_argtemplate()} |
                       {decode_init_default, mod_fn_argtemplate()} |
                       {decode_repeated_add_elem, mod_fn_argtemplate()} |
                       {decode_repeated_finalize, mod_fn_argtemplate()} |
                       {merge,  mod_fn_argtemplate()} |
                       {verify, mod_fn_argtemplate()} |
                       {type_spec, string()}.
-type fn_name() :: atom().
-type mod_fn_argtemplate() :: {module(), fn_name(), arg_template()}.
-type arg_template() :: [arg()].
-type arg() :: term() | named_arg().
-type named_arg() :: '$1' | '$2' | '$errorf' | '$user_data' | '$op'.

-type target_erlang_version() :: integer() | current. %% eg: 23, 24, ...

-type fetcher_ret() :: from_file | {ok, string()} | {error, term()}.
-type import_fetcher_fun() :: fun((string()) -> fetcher_ret()).

-type json_format() :: jsx | mochijson2 | jiffy | maps.
%% Convenience shorthand to specify object, key, array and string and null
%% format.
-type json_object_format() :: eep18 | {proplist} | {atom(), proplist} | map.
%% <ul>
%%   <li>`eep18' means objects on format `[{}] | proplist()', such as
%%       for instance for jsx.</li>
%%   <li>`{proplist}' means a `proplist()' in a tuple.</li>
%%   <li>`{atom(),proplist}' means a `proplist()' in a tagged tuple,
%%       such as `{struct, proplist()}' for instance for mochijson2.</li>
%%   <li>`map' means as a map</li>
%% </ul>
-type json_key_format() :: atom | binary | string.
-type json_array_format() :: list | {atom(), list}.
%% A list or a list in a tagged tuple.
-type json_string_format() :: binary | list.

-type list_deps_format() :: makefile_format |
                            {list_imports, newline_terminated} |
                            {list_imports, null_terminated}.
-type list_deps_makefile_target() :: string() | {quote, string()}.

%% Compilation return values
-type comp_ret() :: mod_ret() | bin_ret() | defs_ret() | error_ret().
-type mod_ret() :: ok | {ok, [warning()]}.
-type bin_ret() :: {ok, module(), code()} |
                   {ok, module(), code(), [warning()]}.
-type defs_ret() :: {ok, gpb_defs:defs()} |
                    {ok, gpb_defs:defs(), [warning()]}.
-type error_ret() :: error | {error, reason()} | {error, reason(), [warning()]}.
-type warning() :: term().
-type reason() :: term().
-type code() :: binary() | gpb_defs:defs() | [code_item()].
-type code_item() :: {erl, ErlCode :: binary()} |
                     {nif, NifCcText :: string()}.
-type io_info_item() :: {erl_output, filename()} |
                        {hrl_output, filename()} |
                        {nif_cc_output, filename()} |
                        {sources, [source()]} |
                        {missing, [source()]}.
-type source() :: from_input_string |
                  {from_fetched, Proto::filename()} |
                  filename().

-export_type([opts/0, opt/0]).
-export_type([comp_ret/0]).
-export_type([io_info_item/0]).

-ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R:St ->).
-else. % -ifdef(OTP_RELEASE).
-define(STACKTRACE(C,R,St), C:R -> St = erlang:get_stacktrace(),).
-endif. % -ifdef(OTP_RELEASE).

-record(path,
        {%% The path as located eg on the file system
         %% via the {i,Dir} options:
         full :: source(),
         %% The top .proto as originally specified, or imports
         %% as specified in import statements.
         orig :: source(),
         %% Used if source is supplied from string, otherwise undefined
         data :: {Mod::module(), Src::string()} | undefined}).

-record(import_env,
        {opts     :: opts(),
         importer :: undefined | import_fetcher_fun()
                     %% If the importer accidentally returns something else:
                   | fun((string) -> any()),
         i_paths  :: [#path{}],
         cur_dir  :: undefined % if not available
                   | string(),
         errors :: [Reason::term()]}).

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
%% <h3><a id="optionsection-overview"/>
%%      Option overview
%% </h3>
%%
%% Options that are documented only as an atom can generally be specified
%% either as the atom, or as `{the_atom, boolean()}'.
%%
%% <dl>
%%   <dt>Input files and output files and formats</dt>
%%   <dd><tt>{<a href="#option-i">i</a>, string()}</tt>,
%%       <tt>{<a href="#option-o_erl">o_erl</a>, string()}</tt>,
%%       <tt>{<a href="#option-o_hrl">o_hrl</a>, string()}</tt>,
%%       <tt>{<a href="#option-o_nif_cc">o_nif_cc</a>, string()}</tt>,
%%       <tt><a href="#option-binary">binary</a></tt>,
%%       <tt><a href="#option-to_proto_defs">to_proto_defs</a></tt>,
%%       <tt><a href="#option-to_msg_defs">to_msg_defs</a></tt>,
%%       <tt>{<a href="#option-import_fetcher">import_fetcher</a>,
%%            {@link import_fetcher_fun()}}</tt>,
%%       <tt><a href="#option-ignore_wellknown_types_directory"
%%               >ignore_wellknown_types_directory</a></tt>
%%   </dd>
%%   <dt>Format of the Erlang representation</dt>
%%   <dd><tt><a href="#option-strings_as_binaries">strings_as_binaries</a></tt>,
%%       <tt><a href="#option-maps">maps</a></tt>,
%%       <tt><a href="#option-msgs_as_maps">msgs_as_maps</a></tt>,
%%       <tt><a href="#option-mapfields_as_maps">mapfields_as_maps</a></tt>,
%%       <tt>{<a href="#option-maps_unset_optional">maps_unset_optional</a>,
%%            omitted|present_undefined}</tt>,
%%       <tt>{<a href="#option-maps_oneof">maps_oneof</a>,
%%            tuples|flat}</tt>,
%%       <tt>{<a href="#option-maps_key_type">maps_key_type</a>,
%%            atom|binary}</tt>
%%       <br/>
%%       See also <tt><a href="#option-use_packages">use_packages</a></tt>.
%%   </dd>
%%   <dt>Verification of input</dt>
%%   <dd><tt>{<a href="#option-verify">verify</a>,
%%            always|never|optionally}</tt>,
%%       <tt><a href="#option-verify_decode_required_present"
%%                           >verify_decode_required_present</a></tt>,
%%       <tt><a href="#option-gen_verifiers"
%%                           >gen_verifiers</a></tt>
%%   </dd>
%%   <dt>Renaming for the Erlang side</dt>
%%   <dd><tt>{<a href="#option-rename">rename</a>,
%%            {@link renaming()}}</tt>,
%%       <tt>{<a href="#option-msg_name_prefix">msg_name_prefix</a>,
%%            {@link msg_name_prefix()}</tt>,
%%       <tt>{<a href="#option-msg_name_suffix">msg_name_suffix</a>,
%%            {@link name_part()}}</tt>,
%%       <tt><a href="#option-msg_name_to_lower">msg_name_to_lower</a></tt>,
%%       <tt><a href="#option-msg_name_to_snake_case">msg_name_to_snake_case</a
%%           ></tt>,
%%       <tt>{<a href="#option-module_name_prefix">module_name_prefix</a>,
%%            {@link name_part()}}</tt>,
%%       <tt>{<a href="#option-module_name_suffix">module_name_suffix</a>,
%%            {@link name_part()}}</tt>,
%%       <tt>{<a href="#option-module_name">module_name</a>
%%            {@link new_name()}}</tt>,
%%   </dd>
%%   <dt>What to generate and how</dt>
%%   <dd><tt><a href="#option-use_packages">use_packages</a></tt>,
%%       <tt><a href="#option-descriptor">descriptor</a></tt>,
%%       <tt><a href="#option-include_as_lib">include_as_lib</a></tt>,
%%       <tt>{<a href="#option-include_mod_hrl_prepend"
%%                            >include_mod_hrl_prepend</a>, string()}</tt>,
%%       <tt><a href="#option-bypass_wrappers">bypass_wrappers</a></tt>,
%%       <tt>{<a href="#option-copy_bytes">copy_bytes</a>,
%%            false|true|auto|integer()|float()}</tt>,
%%       <tt><a href="#option-type_specs">type_specs</a></tt>,
%%       <tt><a href="#option-defaults_for_omitted_optionals"
%%                           >defaults_for_omitted_optionals</a></tt>,
%%       <tt><a href="#option-type_defaults_for_omitted_optionals"
%%                           >type_defaults_for_omitted_optionals</a></tt>,
%%       <tt>{<a href="#option-target_erlang_version">target_erlang_version</a>,
%%            {@link target_erlang_version()}}</tt>,
%%       <tt><a href="#option-preserve_unknown_fields"
%%                   >preserve_unknown_fields</a></tt>,
%%       <tt>{<a href="#option-erlc_compile_options">erlc_compile_options</a>,
%%            string()}</tt>
%%       <br/>
%%       See also <tt><a href="#option-gen_introspect">gen_introspect</a></tt>
%%       and <tt><a href="#option-gen_verifiers">gen_verifiers</a></tt>
%%   </dd>
%%   <dt>Introspection of the proto definitions</dt>
%%   <dd><tt>{<a href="#option-proto_defs_version">proto_defs_version</a>,
%%            {@link gpb_defs:version()}}</tt>,
%%       <tt>{<a href="#option-introspect_proto_defs_version"
%%                            >introspect_proto_defs_version</a>,
%%            {@link gpb_defs:version()}|preferably_1}</tt>,
%%       <tt><a href="#option-introspect_get_proto_defs"
%%                           >introspect_get_proto_defs</a></tt>,
%%       <tt><a href="#option-defs_as_proplists">defs_as_proplists</a></tt>,
%%       <tt><a href="#option-defs_as_maps">defs_as_maps</a></tt>,
%%       <tt><a href="#option-gen_introspect">gen_introspect</a></tt>
%%       <br/>
%%       See also <tt><a href="#option-to_proto_defs">to_proto_defs</a></tt>.
%%   </dd>
%%   <dt>JSON</dt>
%%   <dd><tt><a href="#option-json">json</a></tt>,
%%       <tt><a href="#option-json_always_print_primitive_fields"
%%                           >json_always_print_primitive_fields</a></tt>,
%%       <tt><a href="#option-json_preserve_proto_field_names"
%%                           >json_preserve_proto_field_names</a></tt>,
%%       <tt><a href="#option-json_case_insensitive_enum_parsing"
%%                           >json_case_insensitive_enum_parsing</a></tt>,
%%       <tt>{<a href="#option-json_format">json_format</a>,
%%            {@link json_format()}}</tt>,
%%       <tt>{<a href="#option-json_object_format">json_object_format</a>,
%%            {@link json_object_format()}}</tt>,
%%       <tt>{<a href="#option-json_key_format">json_key_format</a>,
%%            {@link json_key_format()}}</tt>,
%%       <tt>{<a href="#option-json_array_format">json_array_format</a>,
%%            {@link json_array_format()}}</tt>,
%%       <tt>{<a href="#option-json_string_format">json_string_format</a>,
%%            {@link json_string_format()}}</tt>,
%%       <tt>{<a href="#option-json_null">json_null</a>, atom()}</tt>
%%   </dd>
%%   <dt>NIF</dt>
%%   <dd><tt><a href="#option-nif">nif</a></tt>,
%%       <tt>{<a href="#option-load_nif">load_nif</a>, string()}</tt>,
%%       <tt><a href="#option-gen_mergers">gen_mergers</a></tt>
%%       <br/>
%%       See also <tt><a href="#option-o_nif_cc">o_nif_cc</a></tt>
%%   </dd>
%%   <dt>Translations</dt>
%%   <dd><tt>{<a href="#option-translate_type">translate_type</a>,
%%            {{@link gpb_field_type()},[{@link translation()}]}}</tt>,
%%       <tt>{<a href="#option-any_translate">any_translate</a>,
%%            [{@link translation()}]}</tt>,
%%       <tt>{<a href="#option-translate_field">translate_field</a>,
%%           {{@link field_path()},[{@link translation()}]}}</tt>
%%   </dd>
%%   <dt>Compatibility with Erlang protobuffs</dt>
%%   <dd><tt><a href="#option-epb_compatibility">epb_compatibility</a></tt>,
%%       <tt><a href="#option-epb_functions">epb_functions</a></tt>
%%   </dd>
%%   <dt>Querying dependencies</dt>
%%   <dd><tt>{<a href="#option-list_deps">list_deps</a>,
%%            {@link list_deps_format()}}</tt>,
%%       <tt>{<a href="#option-list_deps_dest_file">list_deps_dest_file</a>,
%%            string()}</tt>,
%%       <tt><a href="#option-list_deps_and_generate"
%%                           >list_deps_and_generate</a></tt>,
%%       <tt><a href="#option-list_deps_missing_imports_are_generated"
%%                           >list_deps_missing_imports_are_generated</a></tt>,
%%       <tt><a href="#option-list_deps_makefile_phonies"
%%                           >list_deps_makefile_phonies</a></tt>,
%%       <tt>{<a href="#option-list_deps_makefile_target"
%%                            >list_deps_makefile_target</a>,
%%            {@link list_deps_makefile_target()}}</tt>
%%   </dd>
%%   <dt>Errors and warnings</dt>
%%   <dd><tt><a href="#option-report">report</a></tt>,
%%       <tt><a href="#option-report_errors">report_errors</a></tt>,
%%       <tt><a href="#option-report_warnings">report_warnings</a></tt>,<br/>
%%       <tt><a href="#option-return">return</a></tt>,
%%       <tt><a href="#option-return_errors">return_errors</a></tt>,
%%       <tt><a href="#option-return_warnings">return_warnings</a></tt>,<br/>
%%       <tt><a href="#option-warnings_as_errors">warnings_as_errors</a></tt>
%%   </dd>
%% </dl>
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-inputs-outputs"/>
%%     Input files and output files and formats
%% </h3>
%%
%% <h4><a id="option-i"/>`{i, string()}'</h4>
%%
%% The .proto file is expected to be found in a directories specified by an
%% `{i,Directory}' option. It is possible to specify `{i,Directory}'
%% several times, they will be searched in the order specified.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-I">-I</a>.
%%
%% <h4><a id="option-o"/>
%%     <a id="option-o_erl"/>
%%     <a id="option-o_hrl"/>
%%     <a id="option-o_nif_cc"/>
%%     `{o, string()}'<br/>
%%     `{o_erl, string()}'<br/>
%%     `{o_hrl, string()}'<br/>
%%     `{o_nif_cc, string()}'</h4>
%%
%% The `{o,Directory}' option specifies directory to use for storing
%% the generated `.erl' and `.hrl' files. Default is the same
%% directory as for the proto `File'.
%%
%% The `{o_erl,Directory}', `{o_hrl,Directory}', `{o_nif_cc,Directory}',
%% options specify output directories for where to generate the `.erl'
%% and `.hrl' files respectively, and for the NIF C++ file,
%% if the `nif' option is specified. The `{o_erl,Directory}' option
%% overrides any `{o,Directory}' option, and similarly for the
%% other file-type specific output options.
%%
%% Corresponding command line options:
%% <a href="#cmdline-option-o">-o</a>,
%% <a href="#cmdline-option-o-erl">-o-erl</a>,
%% <a href="#cmdline-option-o-hrl">-o-hrl</a>,
%% <a href="#cmdline-option-o-nif-cc">-o-nif-cc</a>.
%%
%% <h4><a id="option-binary"/>`binary'</h4>
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
%% <h4><a id="option-to_proto_defs"/>
%%     <a id="option-to_msg_defs"/>`to_proto_defs'</h4>
%%
%% The `to_proto_defs' option will result in `{ok,Defs}' or
%% `{ok,Defs,Warns}' being returned if the compilation is successful.
%% The returned message definitions can be used with the
%% {@link proto_defs/2} or {@link proto_defs/3} functions.
%%
%% The `to_msg_defs' option is a deprecated alias for `to_proto_defs'.
%%
%% <h4><a id="option-import_fetcher"/>
%%     <tt>{import_fetcher,{@link import_fetcher_fun()}}</tt></h4>
%%
%% The `import_fetcher' option can be used to customize fetching of imports.
%% The option value is be a function taking one argument, the name of
%% the file to import. It must return either `from_file', letting this
%% file pass through the normal file import, or `{ok,string()}' if it
%% has fetched the file itself, or `{error,term()}'.
%% See the {@link import_fetcher_fun()}.
%%
%% <h4><a id="option-ignore_wellknown_types_directory"
%%      />`ignore_wellknown_types_directory'</h4>
%%
%% The `{ignore_wellknown_types_directory, true}' option will stop gpb from
%% looking for a well known types directory by trying to locate the `priv'
%% directory of the `gpb' application. This can be used either when this
%% directory is not available or to provide a custom set of well known types.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-ignore-priv-dir">-ignore-priv-dir</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-formats"/>
%%     Format of the Erlang representation
%% </h3>
%%
%% <h4><a id="option-strings_as_binaries"/>`strings_as_binaries'</h4>
%%
%% The `strings_as_binaries' option specifies whether strings should
%% be returned from decoding as strings (list of Unicode code points),
%% or as binaries (UTF-8 encoded). The `copy_bytes' option applies
%% to strings as well, when the `strings_as_binaries' option is set.
%% Upon encoding, both binaries and iolists are accepted.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-strbin">-strbin</a>.
%%
%% <h4><a id="option-maps"/>
%%     <a id="option-msgs_as_maps"/>
%%     <a id="option-mapfields_as_maps"/>
%%     `maps'<br/>
%%     `msgs_as_maps'<br/>
%%     `mapfields_as_maps'</h4>
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
%%        as maps, see the <a href="#option-defs_as_maps">`defs_as_maps'</a>.
%%        for further info.</dd>
%% </dl>
%%
%% Corresponding command line options:
%% <a href="#cmdline-option-maps">-maps</a>,
%% <a href="#cmdline-option-msgs-as-maps">-msgs-as-maps</a>,
%% <a href="#cmdline-option-mapfields-as-maps">-mapfields-as-maps</a>.
%%
%% <h4><a id="option-maps_unset_optional"/>
%%     `{maps_unset_optional, omitted|present_undefined}'</h4>
%%
%% For messages as maps, for optional fields, if not set, the
%% `maps_unset_optional' option specifies the Erlang-internal
%% representation; both how it is expected to be found at encoding,
%% and how decoding will return it, for `proto2' syntax:
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
%% For `proto3' syntax, the scene is a bit different. In proto3 all
%% fields are kind-of optional, but omitted scalar fields, strings and
%% bytes decode to their type-default. On encoding with proto3, a field
%% that has its type-default value is not included in the encoded
%% binary, so one could say that even though all fields are optional,
%% in a conceptual way, all scalars, strings and bytes always have a value.
%% The exceptions are sub-messages and `oneof' fields, and for these
%% fields, this option has the meaning as indicated above.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-maps_unset_optional">-maps_unset_optional</a>.
%%
%% <h4><a id="option-maps_oneof"/>
%%     `{maps_oneof, tuples|flat}'</h4>
%%
%% The `maps_oneof' option can be used for messages as maps, and can only
%% take effect if `maps_unset_optional' is `omitted' (default since 4.0.0).
%% It changes the representation of oneof fields as described below, if
%% we would have a oneof-field, `xf' with two alternatives `a1' and `a2':
%% <dl>
%%   <dt>`{maps_oneof,tuples}'</dt>
%%   <dd>`#{xf => {a1, Value}}' or `#{xf => {a2, Value}}'</dd>
%%   <dt>`{maps_oneof,flat}'</dt>
%%   <dd>`#{a1 => Value}' or `#{a2 => Value}'</dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-maps_oneof">-maps_oneof</a>.
%%
%% <h4><a id="option-maps_key_type"/>`{maps_key_type, atom|binary}'</h4>
%%
%% For messages as maps, the `maps_key_type' option makes it possible
%% to control whether keys should be atoms (default) or binaries.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-maps_key_type">-maps_key_type</a>.
%%
%% <h4>Related options</h4>
%% <ul>
%%   <li><a href="#option-use_packages">`use_packages'</a></li>
%% </ul>
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-verification"/>
%%     Verification of input
%% </h3>
%%
%% <h4><a id="option-verify"/>`{verify, always|never|optionally}'</h4>
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
%% Note that the `verify_msg' functions are still generated, even with
%% `{verify,never}'. If needed, user code might want to call them explicitly.
%% See the <tt><a href="#option-gen_verifiers">{gen_verifiers, false}</a></tt>
%% option for a way to control this.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-v">-v</a>.
%%
%% <h4><a id="option-verify_decode_required_present"/>
%%     `verify_decode_required_present'</h4>
%%
%% The `verify_decode_required_present' option tells gpb to emit
%% checks that on decoding, required fields are present in the binary
%% to decode. If they are not present, decoding will fail with an error.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-vdrp">-vdrp</a>.
%%
%% <h4><a id="option-gen_verifiers"/>`gen_verifiers'</h4>
%%
%% The `{gen_verifiers,false}' option tells gpb to not emit code that
%% can verify type and range of the Erlang values before encoding.
%% The main purpose is as a means to reduce the size of the generated
%% code. Setting this option to `false' also implicitly sets the
%% <tt><a href="#option-verify">{verify, never}</a></tt> option.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-no-gen-verifiers">-no-gen-verifiers</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-renaming"/>
%%     Renaming for the Erlang side
%% </h3>
%%
%% <h4><a id="option-rename"/><tt>{rename, {@link renaming()}}</tt></h4>
%%
%% The `{rename,{What,How}}' can transform message names, package names,
%% service and rpc names in various ways. This option supersedes the
%% options `{msg_name_prefix,Prefix}', `{msg_name_suffix,Suffix}',
%% `msg_name_to_lower' and `msg_name_to_snake_case', while at the same
%% time giving more fine-grained control. It is for example possible to
%% apply snake_casing only to the message name, while keeping the
%% package name, the service name and the rpc name intact. This can be
%% useful with grpc, where these name components are exposed. The
%% `msg_fqname' refers to the fully qualified message name, as in
%% `Package.MsgName', while the `msg_name' refers to just the message
%% name without package. The `service_fqname' and `service_name' specifiers
%% work analogously. The `enum_typename' and `msg_typename' operate on
%% any enum or msg renamings already applied.
%%
%% It is possible to stack `rename' options, and they will be applied in
%% the order they are specified. So it is for example possible to
%% snake_case a name, and then also prefix it.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-rename">-rename</a>.
%%
%% <h4><a id="option-msg_name_prefix"/>
%%     <a id="option-msg_name_suffix"/>
%%     <tt>{msg_name_prefix, {@link msg_name_prefix()}}</tt><br/>
%%     <tt>{msg_name_suffix, {@link name_part()}}</tt></h4>
%%
%% The `{msg_name_prefix,Prefix}' will add `Prefix' (a string or an atom)
%% to each message. This might be useful for resolving colliding names,
%% when incorporating several protocol buffer definitions into the same
%% project. The `{msg_name_suffix,Suffix}' works correspondingly.
%%
%% The `{msg_name_prefix,Prefix}' option expands
%% to `[{rename,{pkg_name,Prefix}},{rename,{msg_fqname,{prefix,Prefix}}},
%% {rename,{group_fqname,{prefix,Prefix}}}}]',
%% and ditto for suffixes.
%%
%% For backwards compatibility, the `{msg_name_prefix,{by_proto,PrefixList}}'
%% expands to just `[{rename,{msg_fqname,{prefix,PrefixList}}}]'.
%%
%% Corresponding command line options:
%% <a href="#cmdline-option-msgprefix">-msgprefix</a> and
%% <a href="#cmdline-option-msgsuffix">-msgsuffix</a>.
%%
%% <h4><a id="option-msg_name_to_lower"/>
%%     <a id="option-msg_name_to_snake_case"/>
%%     `msg_name_to_lower'<br/>
%%     `msg_name_to_snake_case'</h4>
%%
%% The `msg_name_to_lower' and `msg_name_to_snake_case' options expands
%% to `[{rename,{pkg_name,X}},{rename,{service_fqname,X}},
%% {rename,{rpc_name,X}},{rename,{msg_fqname,X}},
%% {rename,{rpc_name,X}},{rename,{group_fqname,X}}]' where `X' is
%% `lowercase' or `snake_case' respectively.
%%
%% <h4><a id="option-module_name_prefix"/>
%%     <a id="option-module_name_suffix"/>
%%     <tt>{module_name_prefix, {@link name_part()}}</tt><br/>
%%     <tt>{module_name_suffix, {@link name_part()}}</tt></h4>
%%
%% The `{module_name_prefix,Prefix}' will add `Prefix' (a string or an atom)
%% to the generated code and definition files. The `{module_name_suffix,Suffix}'
%% works correspondingly. For the case of compatibility with Erlang Protobuffs,
%% the <a href="#option-epb_compatibility">`epb_compatibility'</a> option
%% implies `{module_name_suffix,"_pb"}'
%%
%% Corresponding command line options:
%% <a href="#cmdline-option-modprefix">-modprefix</a> and
%% <a href="#cmdline-option-modsuffix">-modsuffix</a>.
%%
%% <h4><a id="option-module_name"/>
%%     <tt>{module_name, {@link new_name()}}</tt></h4>
%%
%% The `{module_name,Name}' can be used to specify the module name of the
%% generated code freely, instead of basing it on the proto file name.
%% The name specified with `module_name' can be prefixed and suffixed with
%% the `module_name_prefix' and `module_name_suffix' options.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-modname">-modname</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-generate"/>
%%     What to generate and how
%% </h3>
%%
%% <h4><a id="option-use_packages"/>`use_packages'</h4>
%%
%% The `use_packages' option instructs gpb to prepend the name of a package
%% to every message it contains. If no package is defined, nothing will be
%% prepended. This enables the reference of messages in other packages which
%% would otherwise not be possible. However, for reasons of backward
%% compatibility, this option is disabled by default.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-pkgs">-pkgs</a>.
%%
%% <h4><a id="option-descriptor"/>`descriptor'</h4>
%%
%% The `descriptor' option specifies whether or not to generate a
%% function, descriptor/0, which returns a binary that describes the
%% proto file(s) contents according to the protobuf's `descriptor.proto'.
%% The default is to not generate such a description.  The generated
%% description binary is most likely not identical to what `protoc'
%% would generate, but the contents is roughly equivalent.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-descr">-descr</a>.
%%
%% <h4><a id="option-include_as_lib"/>`include_as_lib'</h4>
%%
%% Generate code that includes `gpb.hrl' using `-include_lib'
%% instead of `-include', which is the default.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-il">-il</a>.
%%
%% <h4><a id="option-include_mod_hrl_prepend"/>
%%     `{include_mod_hrl_prepend, string()}'</h4>
%%
%% Generate code that prepends the specified string before `<output mod>.hrl'.
%% Default is the empty string. No slashes are added. If you need to prepend
%% a directory, you also need to include the slash in the string to prepend.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-include-mod-hrl-prepend"
%%                        >-include-mod-hrl-prepend</a>.
%%
%% <h4><a id="option-bypass_wrappers"/>`bypass_wrappers'</h4>
%%
%% The `bypass_wrappers' option exposes the more-or-less internal
%% top-level encode and decode functions without wrappers. The list
%% below describe what functionality the wrappers provide. The main
%% purpose of being able to bypass the wrappers is performance,
%% especially when combined with the `nif' option. This option causes the following extra functions to be exported:
%% <ul>
%%   <li><code>encode_msg_<i>MsgName</i>/1</code></li>
%%   <li><code>encode_msg_<i>MsgName</i>/2</code>
%%       unless <a href="#option-nif">`nif'</a></li>
%%   <li><code>decode_msg_<i>MsgName</i>/1</code></li>
%%   <li><code>decode_msg_<i>MsgName</i>/2</code>
%%       unless <a href="#option-nif">`nif'</a></li>
%% </ul>
%% <dl>
%%   <dt>For encode, the wrapper takes care of:</dt>
%%   <dd><ul>
%%     <li>Any calling of verifiers.</li>
%%     <li>Reading of options for any translation `user_data'.</li>
%%   </ul></dd>
%%   <dt>For decode, the wrapper takes care of:</dt>
%%   <dd><ul>
%%     <li>Wrapping the decode in a try catch to provide a uniform error
%%       format when the binary to be decoded is invalid.</li>
%%     <li>Reading of options for any translation `user_data'.</li>
%%   </ul></dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-bypass-wrappers">-bypass-wrappers</a>,
%%
%% <h4><a id="option-copy_bytes"/>
%%     `{copy_bytes, false|true|auto|integer()|float()}'</h4>
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
%%   <dt>`auto'</dt><dd>Synonym for `true'. (This is the default)</dd>
%%   <dt>integer() | float()</dt><dd>Copy the bytes/(sub-)binaries if the
%%           message this many times or more larger than the size of the
%%           bytes/(sub-)binary.</dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-c">-c</a>.
%%
%% <h4><a id="option-type_specs"/>`type_specs'</h4>
%%
%% The `type_specs' option enables or disables `::Type()' annotations
%% in the generated .hrl file. Default is `true'. The default changed
%% in gpb version 4.0.0. Previously, the default was `false'.
%% If you have messages referencing other messages cyclically, and get into
%% troubles when compiling the generated files, set this to `false'.
%%
%% Corresponding command line options:
%% <a href="#cmdline-option-type">-type</a>.
%% <a href="#cmdline-option-notype">-no_type</a>.
%%
%% <h4><a id="option-defaults_for_omitted_optionals"/>
%%     <a id="option-type_defaults_for_omitted_optionals"/>
%%       `defaults_for_omitted_optionals'<br/>
%%       `type_defaults_for_omitted_optionals'</h4>
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
%% Corresponding command line options:
%% <a href="#cmdline-option-defaults-for-omitted-optionals"
%%    >-defaults-for-omitted-optionals</a>,
%% <a href="#cmdline-option-type-defaults-for-omitted-optionals"
%%    >-type-defaults-for-omitted-optionals</a>.
%%
%% <h4><a id="option-target_erlang_version"/>
%%     <tt>{target_erlang_version, {@link target_erlang_version()}}</tt></h4>
%%
%% The `target_erlang_version' can be used to specify another major
%% version of Erlang/OTP to generate code for. The default, `current'
%% means that the generated code is expected to be compiled and run
%% on the same major version as gpb runs on.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-for-version">-for-version</a>,
%%
%% <h4><a id="option-preserve_unknown_fields"/>`preserve_unknown_fields'</h4>
%%
%% The `preserve_unknown_fields' option will add a field to records and
%% maps. On decoding, info on unknown fields will be stored in here,
%% such that the unknown fields can be preserved on encoding.
%% An unknown field is a field with an unknown number. Without this option,
%% such fields are skipped on decoding. There is no guarantee that
%% a message with unknowns will be byte-by-byte identical when re-encoded.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-preserve-unknown-fields"
%%    >-preserve-unknown-fields</a>.
%%
%% <h4><a id="option-erlc_compile_options"/>
%%     `{erlc_compile_options, string()}'</h4>
%%
%% If the the `{erlc_compile_options,string()}' option is set,
%% then the generated code will contain a directive `-compile([String]).'
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-erlc_compile_options">-erlc_compile_options</a>.
%%
%% <h4>Related options</h4>
%% <ul>
%%   <li><a href="#option-gen_introspect">`gen_introspect'</a></li>
%%   <li><a href="#option-gen_verifiers">`gen_verifiers'</a></li>
%% </ul>
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-introspection"/>
%%     Introspection of the proto definitions
%% </h3>
%%
%% <h4><a id="option-defs_as_proplists"/>`defs_as_proplists'</h4>
%%
%% The `defs_as_proplists' option changes the generated introspection
%% functions `find_msg_def', `get_msg_defs' and `get_proto_defs'
%% to return the description of each message field as a proplist,
%% instead of as a `#field{}' record. The purpose is to make the
%% generated code completely independent of gpb, at compile-time
%% (it is already independent at run-time). The keys of the proplist
%% are the names of the record fields in the `#field{}' record.
%% See also {@link gpb:proplists_to_field_records()} and related
%% functions for conversion functions between these two formats.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-pldefs">-pldefs</a>.
%%
%% <h4><a id="option-defs_as_maps"/>`defs_as_maps'</h4>
%%
%% The introspection will generate message field descriptions
%% as maps instead of as `#field{}' records, unless, of course
%% `defs_as_proplists' is specified, in which case they will be
%% proplists instead.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-defs-as-maps">-defs-as-maps</a>.
%%
%% <h4><a id="option-proto_defs_version"/>
%%     <a id="option-introspect_proto_defs_version"/>
%%     <tt>{proto_defs_version, {@link gpb_defs:version()}}</tt><br/>
%%     <tt>{introspect_proto_defs_version,
%%          {@link gpb_defs:version()}|preferably_1}</tt></h4>
%%
%% The `proto_defs_version' can be used to specify version of definitions
%% returned with the `to_proto_defs' option.  See the file
%% `doc/dev-guide/proto-defs-versions.md' for some more info.
%% Not all proto definitions may be expressible in all versions.
%% In gpb-4.x.y it defaults to 1.
%% The `introspect_proto_defs_version' can be used to specify the version
%% returned by the generated introspection functions, default is 1
%% if possible, else 2.
%%
%% <h4><a id="option-introspect_get_proto_defs"/>
%%     `introspect_get_proto_defs'</h4>
%%
%% When the `introspect_get_proto_defs' option is set, the introspection
%% function will include `get_proto_defs/0' instead of `get_msg_defs/0'. The
%% `get_msg_defs/0' returns a list of messages and enums, while the
%% `get_proto_defs/0' returns the same definitions returned when the
%% `to_proto_defs' option is used.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-introspect-get_proto_defs"
%%    >-introspect-get_proto_defs</a>.
%%
%% <h4><a id="option-gen_introspect"/>`gen_introspect'</h4>
%%
%% The `{gen_introspect,false}' option will cause gpb to not generate code
%% for introspection. One rationale for this is option is to reduce the size of
%% the generated code.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-no-gen-introspect">-no-gen-introspect</a>.
%%
%% <h4>Related options</h4>
%% <ul>
%%   <li><a href="#option-to_proto_defs">`to_proto_defs'</a></li>
%% </ul>
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-json"/>
%%     JSON
%% </h3>
%%
%% <h4><a id="option-json"/>`json'</h4>
%%
%% The `json' option will cause gpb to also generate functions for
%% converting between internal format and a JSON representation.
%%
%% Note that gpb will not encode to an actual JSON text.
%% Instead, it returns an Erlang structure that can be used with some other
%% JSON library to turn it into actual JSON text. Ditto for decoding.
%% It is possible to flexibly specify details of the JSON representation,
%% with shorthand presets for some common libraries.
%%
%% However, with the `nif' option, the generated code uses the Google
%% C++ protobuf library, which produces already-formatted JSON text, as
%% binaries, with no further processing required. When `nif' is
%% specified, the various JSON format options are thus not used.
%% The `json_always_print_primitive_fields', the
%% `json_preserve_proto_field_names' and the
%% `json_case_insensitive_enum_parsing' options are honoured with `nif',
%% though.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json">-json</a>.
%%
%% <h4><a id="option-json_always_print_primitive_fields"/>
%%     `json_always_print_primitive_fields'</h4>
%%
%% The `json_always_print_primitive_fields' makes the generated
%% `to_json' function always emit json key-value items also when the
%% value is the type's default value.  The default is to omit such
%% values, as per the language guide.  This holds for messages in files
%% with proto3 syntax.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-always-print-primitive-fields"
%%    >-json-always-print-primitive-fields</a>.
%%
%% <h4><a id="option-json_preserve_proto_field_names"/>
%%     `json_preserve_proto_field_names'</h4>
%%
%% The `json_preserve_proto_field_names' makes the generated `to_json'
%% function always use the field name in the `.proto' file. The default
%% is to use lowerCamelCase, as per the language guide.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-preserve-proto-field-names"
%%    >-json-preserve-proto-field-names</a>.
%%
%% <h4><a id="option-json_case_insensitive_enum_parsing"/>
%%     `json_case_insensitive_enum_parsing'</h4>
%%
%% If the the `json_case_insensitive_enum_parsing' option is specified,
%% case is not significant when parsing json enums. Also, dashes instead
%% of underscores are allowed. Default is that that case <em>is</em>
%% significant.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-case-insensitive-enum-parsing"
%%    >-json-case-insensitive-enum-parsing</a>.
%%
%% <h4><a id="option-json_format"/>
%%     <tt>{json_format, {@link json_format()}}</tt></h4>
%%
%% The `{json_format,Format}' option is a convenience shorthand, and will expand
%% as indicated below. If the json_format is not specified, it defaults to
%% `map' if the `maps' option is specified, and otherwise to `eep18' when
%% generating code for records.
%% <dl>
%%   <dt>jsx</dt>
%%   <dd><code>[{json_object_format, eep18},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%%   <dt>mochijson2</dt>
%%   <dd><code>[{json_object_format, {struct, proplist}},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%%   <dt>jiffy</dt>
%%   <dd><code>[{json_object_format, {proplist}},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%%   <dt>map</dt>
%%   <dd><code>[{json_object_format, map},
%%              {json_key_format, binary},
%%              {json_array_format, list},
%%              {json_string_format, binary},
%%              {json_null, null}]</code></dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-format">-json-format</a>.
%%
%% <h4><a id="option-json_object_format"/>
%%     <tt>{json_object_format, {@link json_object_format()}}</tt></h4>
%%
%% The `{json_object_format,Format}' option specifies the format
%% of json object, as indicated below. (Note that the format of the keys
%% is specified by the `json_key_format' option, see further below.)
%% <dl>
%%   <dt>`eep18'</dt>
%%   <dd>The empty json object is represented as `[{}]'.<br/>
%%       Non-empty json objects are represented as proplists.</dd>
%%   <dt>`{proplist}'</dt>
%%   <dd>A json object is represented as a proplist in a tuple.</dd>
%%   <dt>`{atom(), proplist}'</dt>
%%   <dd>A json object is represented as a proplist in a tagged tuple,
%%       with the possibility to specify the tag.</dd>
%%   <dt>`map'</dt>
%%   <dd>The json object is represented as an Erlang map.</dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-object-format">-json-object-format</a>.
%%
%% <h4><a id="option-json_key_format"/>
%%     <tt>{json_key_format, {@link json_key_format()}}</tt></h4>
%%
%% The `{json_key_format,Format}' option specifies the format
%% of json object keys, as follows:
%% <ul>
%%   <li>`atom'</li>
%%   <li>`binary' (default)</li>
%%   <li>`string'</li>
%% </ul>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-key-format">-json-key-format</a>.
%%
%% <h4><a id="option-json_array_format"/>
%%     <tt>{json_array_format, {@link json_array_format()}}</tt></h4>
%%
%% The `{json_array_format,Format}' option specifies the format
%% of json arrays, as follows:
%% <ul>
%%   <li>`list' (default)</li>
%%   <li>`{atom(), list}' A list in a tagged tuple, with the possibility
%%       to specify the tag.</li>
%% </ul>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-array-format">-json-array-format</a>.
%%
%% <h4><a id="option-json_string_format"/>
%%     <tt>{json_string_format, {@link json_string_format()}}</tt></h4>
%%
%% The `{json_string_format,Format}' option specifies the format
%% of json arrays, as follows:
%% <ul>
%%   <li>`list'</li>
%%   <li>`binary' (default)</li>
%% </ul>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-string-format">-json-string-format</a>.
%%
%% <h4><a id="option-json_null"/>`{json_null, atom()}'</h4>
%%
%% The `{json_null,atom()}' option specifies the atom to use
%% for the JSON `null' value. The default is to use the atom `null'.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-json-null">-json-null</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-nif"/>
%%     NIF
%% </h3>
%%
%% <h4><a id="option-nif"/>`nif'</h4>
%%
%% The `nif' option will cause the compiler to generate nif C++ code
%% for encoding and decoding. The generated nif C++ code can be linked
%% with the Google protobuf C++ library.  Read the file
%% `README.nif-cc' for more info.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-nif">-nif</a>.
%%
%% <h4><a id="option-load_nif"/>`{load_nif, string()}'</h4>
%%
%% The option `{load_nif,FunctionDefinition}' allows to specify
%% `FunctionDefinition' as the text that defines the function load_nif/0.
%% This is called as the `-on_load.' hook for loading the NIF.
%%
%% The string can contain some special terms that will get replaced as follows:
%% <dl>
%%   <dt>`{{nifbase}}'</dt>
%%   <dd>This will be replaced with the string `"Mod.nif"'.
%%       where `Mod' is the name of the Erlang module.</dd>
%%   <dt>`{{loadinfo}}'</dt>
%%   <dd>This will be replaced with the version of gpb, as a list
%%       of integers and strings, see {@link gpb:version_as_list/0}.</dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-load_nif">-load_nif</a>.
%%
%% <h4><a id="option-gen_mergers"/>`gen_mergers'</h4>
%%
%% The `{gen_mergers,false}' option will cause gpb to not generate code for
%% merging of messages. This is only useful with the option `nif'. One
%% rationale for this is option is to reduce the size of the generated code.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-no-gen-mergers">-no-gen-mergers</a>.
%%
%% <h4>Related options</h4>
%% <ul>
%%   <li><tt>{<a href="#option-o_nif_cc">o_nif_cc</a>,string()}</tt></li>
%% </ul>
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-translations"/>
%%     Translations
%% </h3>
%%
%% <h4><a id="option-translate_type"/>
%%     <tt>{translate_type, {{@link gpb_field_type()},[{@link translation()}]}}
%%     </tt></h4>
%%
%% The `translate_type' option can be used to provide packer and unpacker
%% functions for message fields of a certain type.
%% For messages, the `MsgName' refers to a name <em>after</em>
%% renaming has taken place.
%% The merge translator is optional, and is called either via the `merge_msgs'
%% function in the generated code, or when the decoder sees another
%% field of the same type. The default merge operation is to let the second
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
%%       a value of the suitable for normal gpb encoding.</dd>
%%   <dt>Decode (Unpacking)</dt>
%%   <dd>Call `Mod:Fn(Any)' to unpack the `Any' (`$1') to
%%       unpack a normal gpb decoded value to a term.</dd>
%%   <dt>Merge</dt>
%%   <dd>Call `Mod:Fn(Term1, Term2) -> Term3' to merge two
%%       unpacked terms to a resulting Term3. The `$1' is the
%%       previously seen term (during decoding, on encountering a
%%       second field of the same type), or the first argument to the
%%       `merge_msgs' function. The `$2' is the lastly seen term, or
%%       the second argument to the `merge_msgs' function.</dd>
%%   <dt>Verify</dt>
%%   <dd>Call `Mod:Fn(Term) -> _' to verify an unpacked `Term'.
%%       If `Term' (`$1') is valid, the function is expected to just return
%%       any value, which is ignored and discarded.
%%       If `Term' is invalid, the function is expected to not
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
%%   <dd>This will be replaced by `encode', `decode', `merge',
%%   `verify', `decode_init_default', `decode_repeated_add_elem' or
%%   `decode_repeated_finalize', depending on from which context it
%%   is actually called. This can be useful because if the message is
%%   to be verified on encoding (see the <a href="#option-verify"
%%   >`verify'</a> option), then the
%%   same options, and thus the same user-data, are used for both
%%   `encode_msg' and for `verify_msg'. The `$op' marker makes it
%%   possible to tell these two call sites apart, if needed.</dd>
%% </dl>
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-translate_type">-translate_type</a>.
%%
%% <h4><a id="option-any_translate"/>
%%     <tt>{any_translate, [{@link translation()}]}}</tt></h4>
%%
%% The option `{any_translate,Translations}' is retained for backwards
%% compatibility, and expands to
%% <tt>{<a href="#option-translate_type">translate_type</a>,
%%      {'google.protobuf.Any',Translations}}</tt>.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-any_translate">-any_translate</a>.
%%
%% <h4><a id="option-translate_field"/>
%%     <tt>{translate_field, {{@link field_path()},[{@link translation()}]}
%%     </tt></h4>
%% The `translate_field' option can be used to translate individual fields.
%% The option format is `{translate_field,{FieldPath,Translations}}' where
%% each `Translation' consists of `{Op,{Mod,Fn,ArgTemplate}}' elements,
%% just as for `translate_type'. The `FieldPath' is a list on the
%% following format:
%% <ul>
%%   <li>`[MsgName]' for the message itself on the top-level</li>
%%   <li>`[MsgName,FieldName]' for fields, generally</li>
%%   <li>`[MsgName,FieldName,[]]' for elements of repeated fields</li>
%%   <li>`[MsgName,OneofFieldName,FieldName]' for elements of oneof
%%     fields.</li>
%% </ul>
%% For repeated fields, the additional operations `decode_init_default',
%% `decode_repeated_add_elem' and `decode_repeated_finalize' also exist
%% and must all be specified.
%%
%% For translated proto3 message fields -- ie fields for messages in files
%% with `syntax="proto3";' -- the `decode' callback will be invoked also
%% initially when the decoding of the message binary is about to start.
%% This holds for non-repeated non-oneof fields of integer types, enums,
%% strings and bytes. The `decode' callback will be invoked with the type's
%% default value. This is because in proto3, a field with the type's default
%% value is never included in the resulting wire octets, so on decoding,
%% gpb initially assumes such fields have the type's default value,
%% and the translator needs to be invoked accordingly.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-translate_field">-translate_field</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-epb-compatibility"/>
%%     Compatibility with Erlang protobuffs
%% </h3>
%%
%% These are for compatibility with
%% <a href="https://github.com/basho/erlang_protobuffs">Erlang protobuffs</a>.
%% library.
%%
%% <h4><a id="option-epb_compatibility"/>
%%     <a id="option-epb_functions"/>
%%     `epb_compatibility'<br/>
%%     `epb_functions'</h4>
%%
%% The `epb_compatibility' option is an umbrella-option for
%% compatibility with the Erlang protobuffs library. It will expand to
%% the options below. It will expand in-place, meaning any of these
%% can be overridden if specified before the `epb_compatibility'
%% option.
%% <ul>
%%   <li>`epb_functions'</li>
%%   <li><a href="#option-defaults_for_omitted_optionals"
%%                      >`defaults_for_omitted_optionals'</a></li>
%%   <li><tt>{<a href="#option-module_name_suffix">module_name_suffix</a>,
%%            "_pb"}</tt></li>
%%   <li><tt>{<a href="#option-msg_name_to_lower">msg_name_to_lower</a>,
%%            true}</tt></li>
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
%% Corresponding command line options:
%% <a href="#cmdline-option-epb">-epb</a>,
%% <a href="#cmdline-option-epb-functions">-epb-functions</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-dependencies"/>
%%     Querying dependencies
%% </h3>
%%
%% <h4><a id="option-list_deps"/>
%%     <a id="option-list_deps_dest_file"/>
%%     <a id="option-list_deps_and_generate"/>
%%     <a id="option-list_deps_missing_imports_are_generated"/>
%%     <a id="option-list_deps_makefile_phonies"/>
%%     <a id="option-list_deps_makefile_target"/>
%%     <tt>{list_deps, {@link list_deps_format()}}</tt><br/>
%%     <tt>{list_deps_dest_file, string()}</tt><br/>
%%     <tt>list_deps_and_generate</tt><br/>
%%     <tt>list_deps_missing_imports_are_generated</tt><br/>
%%     <tt>list_deps_makefile_phonies</tt><br/>
%%     <tt>{list_deps_makefile_target, {@link list_deps_makefile_target()}}</tt>
%%     </h4>
%%
%% A set of options will cause {@link file/2} and {@link string/3} to
%% list `import' dependencies as described below. To retrieve dependency
%% information as Erlang terms, see {@link list_io/2} and
%% {@link string_list_io/3}.
%% <dl>
%%   <dt>`{list_deps, Format}'<br/>
%%       `{list_deps_dest_file, Filename::string()}'</dt>
%%   <dd>Either or both of these two options without the
%%       `list_deps_and_generate' option described below, will cause
%%       dependencies to be listed, and no code to be generated.
%%       The default format is Makefile format, ie with only the
%%       `list_deps_dest_file' option. The default destination is
%%       to print to standard output, ie with only the `list_deps' option.</dd>
%%   <dt>`list_deps_and_generate'</dt>
%%   <dd>If specified, dependencies will be listed, and code will be
%%       generated. .</dd>
%% </dl>
%% Formats:
%% <dl>
%%   <dt>`{list_deps, makefile_format}'</dt>
%%   <dd>Makefile rules will be generated. The top-level .proto file
%%       will be the first dependency followed by imported .proto files,
%%       and the .erl file will be the target. Outdir options as well
%%       as module renaming options are considered for target.
%%       Include path options are considered for the dependencies.</dd>
%%   <dt>`{list_deps, {list_imports, newline_terminated}}'</dt>
%%   <dd>Imports of the .proto file will be listed, one per line.</dd>
%%   <dt>`{list_deps, {list_imports, null_terminated}}'</dt>
%%   <dd>Like `newline_terminated' but instead, each import is followed
%%       by a NUL character. This can be useful when paths have spaces
%%       or newline or other strange characters.</dd>
%%   <dt>`list_deps_missing_imports_are_generated'</dt>
%%   <dd>Consider missing imports to be generated, and include them
%%       in the dependency list</dd>
%% </dl>
%% Some of the options apply only when the `Target' is `makefile_format':
%% <dl>
%%   <dt>`{list_deps_makefile_target, Target}'</dt>
%%   <dd>Override the default target for the Makefile rule, as follows:
%%      <dl>
%%         <dt>`Target :: string()'</dt>
%%         <dd>Use the specified value instead.</dd>
%%         <dt>`Target :: {quote, string()}'</dt>
%%         <dd>Same, but quote characters special to make.</dd>
%%      </dl></dd>
%%   <dt>`list_deps_makefile_phonies'</dt>
%%   <dd>Generate phony Makefile targets for dependencies.</dd>
%% </dl>
%%
%% Corresponding command line options:
%% <a href="#cmdline-option-m">-M</a>,
%% <a href="#cmdline-option-ml">-ML</a>,
%% <a href="#cmdline-option-m0">-M0</a>,
%% <a href="#cmdline-option-mf">-MF</a>,
%% <a href="#cmdline-option-mg">-MG</a>,
%% <a href="#cmdline-option-mp">-MP</a>,
%% <a href="#cmdline-option-mt">-MT</a>,
%% <a href="#cmdline-option-mq">-MQ</a> and
%% <a href="#cmdline-option-mmd">-MMD</a>.
%%
%% <!-- ======================================================== -->
%% <h3><a id="optionsection-errors"/>
%%     <a id="optionsection-warnings"/>
%%     Errors and warnings
%% </h3>
%%
%% <h4><a id="option-report"/>
%%     <a id="option-report_errors"/>
%%     <a id="option-report_warnings"/>
%%     <a id="option-return"/>
%%     <a id="option-return_errors"/>
%%     <a id="option-return_warnings"/>
%%     `report', `report_errors', `report_warnings'<br/>
%%     `return', `return_errors', `return_warnings'</h4>
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
%% <h4><a id="option-warnings_as_errors"/>`warnings_as_errors'</h4>
%%
%% Setting the `warnings_as_errors' option will cause warnings to be
%% treated as errors.  If there are warnings but no errors, and
%% `return_warnings' is not specified, then `error' will be returned.
%%
%% See {@link format_error/1} for a way to turn an error <i>Reason</i> to
%% plain text.
%%
%% Corresponding command line option:
%% <a href="#cmdline-option-W">-Werror</a>.
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
    case list_deps_or_generate(Opts1) of
        generate ->
            do_generate_from_file_or_string(In, Opts1);
        list_deps ->
            do_list_deps_from_file_or_string(In, Opts1);
        list_deps_and_generate ->
            do_list_deps_from_file_or_string(In, Opts1),
            do_generate_from_file_or_string(In, Opts1)
    end.

list_deps_or_generate(Opts) ->
    OptM   = proplists:get_value(list_deps, Opts) /= undefined,
    OptMMD = proplists:get_bool(list_deps_and_generate, Opts),
    if OptMMD -> list_deps_and_generate;
       OptM   -> list_deps;
       true   -> generate
    end.

do_generate_from_file_or_string(In, Opts) ->
    case parse_file_or_string(In, Opts) of
        {ok, {Defs, Sources}} ->
            case gpb_names:compute_renamings(Defs, Opts) of
                {ok, Renamings} ->
                    Defs1 = gpb_names:apply_renamings(Defs, Renamings),
                    Mod = find_out_mod(In, Opts),
                    DefaultOutDir = find_default_out_dir(In),
                    Opts1 = Opts ++ [{o,DefaultOutDir}],
                    Opts2 = possibly_adjust_proto_defs_version_opt(Opts1),
                    do_proto_defs_aux1(Mod, Defs1, Defs, Sources, Renamings,
                                       Opts2);
                {error, Reason} = Error ->
                    possibly_report_error(Error, Opts),
                    case proplists:get_bool(return_warnings, Opts) of
                        true  -> {error, Reason, []};
                        false -> Error
                    end
            end;
        {error, Reason} = Error ->
            possibly_report_error(Error, Opts),
            case proplists:get_bool(return_warnings, Opts) of
                true  -> {error, Reason, []};
                false -> Error
            end
    end.

normalize_opts(Opts0) ->
    normalize_list_deps_rules(
      normalize_return_report_opts(
        normalize_alias_opts(Opts0))).

normalize_alias_opts(Opts) ->
    lists:foldl(fun(F, OptsAcc) -> F(OptsAcc) end,
                Opts,
                [fun norm_opt_alias_to_msg_proto_defs/1,
                 fun norm_opt_epb_compat_opt/1,
                 fun norm_opt_map_opts/1,
                 fun norm_opt_any_translate/1,
                 fun norm_opt_json_format/1,
                 fun norm_opt_gen_verifiers/1]).

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

norm_opt_any_translate(Opts) ->
    AnyType = {msg, 'google.protobuf.Any'},
    lists:map(fun({any_translate, Transls}) ->
                      {translate_type, {AnyType, Transls}};
                 (Opt) ->
                      Opt
              end,
              Opts).

norm_opt_json_format(Opts) ->
    proplists:expand(
      [{{json_format, jsx},        [{json_object_format, eep18},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, mochijson2}, [{json_object_format, {struct, proplist}},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, jiffy},      [{json_object_format, {proplist}},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, maps},       [{json_object_format, map},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]}],
      Opts).

norm_opt_gen_verifiers(Opts) ->
    proplists:expand(
      [{{gen_verifiers, false}, [{verify, never},
                                 {gen_verifiers, false}]}],
      Opts).

normalize_return_report_opts(Opts1) ->
    Opts2 = expand_opt(return, [return_warnings, return_errors], Opts1),
    Opts3 = expand_opt(report, [report_warnings, report_errors], Opts2),
    Opts4 = unless_defined_set(return_warnings, report_warnings, Opts3),
    Opts5 = unless_defined_set(return_errors,   report_errors, Opts4),
    Opts5.

normalize_list_deps_rules(Opts) ->
    OptM = proplists:get_value(list_deps, Opts),
    OptMF = proplists:get_value(list_deps_dest_file, Opts),
    OptMMD = proplists:get_bool(list_deps_and_generate, Opts),
    if OptMF /= undefined, OptM == undefined;
       OptMMD, OptM == undefined ->
            %% -MF <file> implies -M
            %% -MMD implies -M
            [{list_deps, makefile_rules} | Opts];
       true ->
            Opts
    end.

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
    gpb_names:file_name_to_module_name(File, Opts).

find_default_out_dir({_Mod, _S}) -> ".";
find_default_out_dir(File) -> filename:dirname(File).

possibly_adjust_proto_defs_version_opt(Opts) ->
    case get_output_format(Opts) of
        proto_defs ->
            case proplists:get_value(proto_defs_version, Opts) of
                undefined ->
                    %% Default version is 1 for now
                    [{proto_defs_version, 1} | Opts];
                _Vsn ->
                    Opts
            end;
        binary ->
            Opts;
        file ->
            Opts
    end.

possibly_adjust_introspect_proto_defs_version_opt(Opts) ->
    OutFormat = get_output_format(Opts),
    if OutFormat == proto_defs ->
            Opts;
       OutFormat == binary;
       OutFormat == file ->
            case proplists:is_defined(introspect_proto_defs_version, Opts) of
                true  -> Opts;
                false -> [{introspect_proto_defs_version, preferably_1} | Opts]
            end
    end.

%% @equiv proto_defs(Mod, Defs, [])
-spec proto_defs(module(), gpb_defs:defs()) -> comp_ret().
proto_defs(Mod, Defs) ->
    proto_defs(Mod, Defs, []).

%% @doc
%% Compile a list of pre-parsed definitions to file or to a binary.
%% See {@link file/2} for information on options and return values.
-spec proto_defs(module(), gpb_defs:defs(), opts()) -> comp_ret().
proto_defs(Mod, Defs, Opts) ->
    proto_defs(Mod, Defs, Defs, no_renamings, Opts).

%% @doc
%% Compile a list of pre-parsed definitions to file or to a binary.
%% This is useful when there are renamings, and one nifs or descriptors
%% are to be generated, since these need the original definitions before
%% any renamings. The renaming must be applied separately, see
%% {@link gpb_names:compute_renamings/2} and
%% {@link gpb_names:apply_renamings/2}. See
%% {@link gpb_names:is_renaming_opt/1} for how to filter options.
%% See {@link file/2} for information on options and return values.
proto_defs(Mod, Defs, DefsNoRenamings, Renamings, Opts) ->
    Sources = [lists:concat([Mod, ".proto"])],
    Opts1 = normalize_opts(Opts),
    case gpb_defs:convert_defs_to_latest_version(Defs) of
        {ok, Defs1} ->
            {ok, DefsNoRenamings1} =
                gpb_defs:convert_defs_to_latest_version(DefsNoRenamings),
            do_proto_defs_aux1(Mod, Defs1, DefsNoRenamings1,
                               Sources, Renamings, Opts1);
        {error, Reason} ->
            Reason2 = {cvt_proto_defs_version_to_latest_error, Reason},
            return_or_report_warnings_or_errors({error, Reason2}, [], Opts1,
                                                get_output_format(Opts1))
    end.

do_proto_defs_aux1(Mod, Defs, DefsNoRenamings, Sources, Renamings, Opts) ->
    possibly_probe_defs(Defs, Opts),
    Warns0 = check_unpackables_marked_as_packed(Defs),
    Warns1 = check_maps_flat_oneof_may_fail_on_compilation(Opts),
    Warns = Warns0 ++ Warns1,
    Defs1 = case proplists:get_bool(preserve_unknown_fields, Opts) of
                true  -> gpb_defs:extend_with_field_for_unknowns(Defs);
                false -> Defs
            end,
    AnRes = gpb_analyzer:analyze_defs(Defs1, Sources, Renamings, Opts),
    case verify_opts(Defs1, Opts) of
        ok ->
            Res1 = do_proto_defs_aux2(Defs1, DefsNoRenamings,
                                      clean_module_name(Mod), AnRes, Opts),
            return_or_report_warnings_or_errors(Res1, Warns, Opts,
                                                get_output_format(Opts));
        {error, OptError} ->
            return_or_report_warnings_or_errors({error, OptError}, [], Opts,
                                                get_output_format(Opts))
    end.

verify_opts(Defs, Opts) ->
    while_ok([fun() -> verify_opts_translation_and_nif(Opts) end,
              fun() -> verify_opts_preserve_unknown_fields_and_json(Opts) end,
              fun() -> verify_opts_epb_compat(Defs, Opts) end,
              fun() -> verify_opts_flat_oneof(Opts) end,
              fun() -> verify_opts_no_gen_mergers(Opts) end,
              fun() -> verify_opts_no_gen_verifiers(Opts) end]).

while_ok(Funs) ->
    lists:foldl(fun(F, ok) -> F();
                   (_, Err) -> Err
                end,
                ok,
                Funs).

verify_opts_translation_and_nif(Opts) ->
    TranslType = lists:keymember(translate_type, 1, Opts),
    TranslField = lists:keymember(translate_field, 1, Opts),
    DoNif = proplists:get_bool(nif, Opts),
    if (TranslType or TranslField) and DoNif ->
            {error, {invalid_options, translation, nif}};
       true ->
            ok
    end.

verify_opts_preserve_unknown_fields_and_json(Opts) ->
    Preserve = proplists:get_bool(preserve_unknown_fields, Opts),
    DoJson = gpb_lib:json_by_opts(Opts),
    if Preserve and DoJson ->
            {error, {invalid_options, preserve_unknown_fields, json}};
       true ->
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

verify_opts_flat_oneof(Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        #maps{oneof=flat} ->
            case gpb_lib:target_can_do_flat_oneof_for_maps(Opts) of
                true ->
                    ok;
                false -> {error, maps_flat_oneof_not_supported_for_target_version}
            end;
        _ ->
            ok
    end.

check_maps_flat_oneof_may_fail_on_compilation(Opts) ->
    CanFlatOneof = gpb_lib:target_can_do_flat_oneof_for_maps(Opts),
    MayFail = gpb_lib:target_may_fail_compilation_for_flat_oneof_for_maps(Opts),
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        #maps{oneof=flat} ->
            if CanFlatOneof, MayFail ->
                    [maps_flat_oneof_generated_code_may_fail_to_compile];
               not CanFlatOneof ->
                    []; % a later check will signal an error
               true ->
                    []
            end;
        _ ->
            []
    end.

verify_opts_no_gen_mergers(Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    GenMergers = proplists:get_value(gen_mergers, Opts),
    case {DoNif, GenMergers} of
        {_,     undefined} -> ok; % default for gen_mergers is true
        {_,     true} -> ok;
        {true,  false} -> ok;
        {false, false} -> {error, {invalid_options, nif, {gen_mergers,false}}}
    end.

verify_opts_no_gen_verifiers(Opts) ->
    Verify = proplists:get_value(verify, Opts),
    GenVerifiers = proplists:get_value(gen_verifiers, Opts),
    case {Verify, GenVerifiers} of
        {_,     undefined} -> ok; % default for gen_mergers is true
        {_,     true} -> ok;
        {never, false} -> ok;
        {_, false} -> {error, {invalid_options,
                               {verify,Verify}, {gen_verifiers,false}}}
    end.

%% @equiv msg_defs(Mod, Defs, [])
%% @doc Deprecated, use proto_defs/2 instead.
-spec msg_defs(module(), gpb_defs:defs()) -> comp_ret().
msg_defs(Mod, Defs) ->
    msg_defs(Mod, Defs, []).

%% @equiv proto_defs(Mod, Defs, Opts)
%% @doc Deprecated, use proto_defs/2 instead.
-spec msg_defs(module(), gpb_defs:defs(), opts()) -> comp_ret().
msg_defs(Mod, Defs, Opts) ->
    proto_defs(Mod, Defs, Opts).

do_proto_defs_aux2(Defs, DefsNoRenamings, Mod, AnRes, Opts) ->
    case gpb_lib:get_gen_introspect(Opts) of
        false ->
            do_proto_defs_aux3(Defs, DefsNoRenamings, Defs,
                               Mod, AnRes, Opts);
        true ->
            Opts2 = possibly_adjust_introspect_proto_defs_version_opt(Opts),
            case convert_introspect_proto_defs(Defs, Opts2) of
                {ok, DefsForIntrospect} ->
                    do_proto_defs_aux3(Defs, DefsNoRenamings, DefsForIntrospect,
                                       Mod, AnRes, Opts2);
                {error, {IVsn, Reason}} ->
                    {error, {cvt_proto_defs_for_introspect_error, IVsn, Reason}}
            end
    end.

convert_introspect_proto_defs(Defs, Opts) ->
    LatestVsn = gpb_defs:latest_defs_version(),
    IVsn = proplists:get_value(introspect_proto_defs_version, Opts, LatestVsn),
    if is_integer(IVsn) ->
            case gpb_defs:convert_defs_from_latest_version(Defs, IVsn) of
                {ok, DefsForIntrospect} ->
                    {ok, DefsForIntrospect};
                {error, Reason} ->
                    {error, {IVsn, Reason}}
            end;
       IVsn == preferably_1 ->
            %% The idea is that when introducing the new proto3 field presence,
            %% do it in a mostly backwards compatible way;
            %% * Work fully backwards compatible when no new proto3 field
            %%   presence ("optional") is used.
            %% * When new proto3 field presence is used, allow some minor
            %%   non-backwards compatibility.
            case gpb_defs:convert_defs_from_latest_version(Defs, 1) of
                {ok, DefsForIntrospect} ->
                    {ok, DefsForIntrospect};
                {error, _Reason} ->
                    {ok, Defs}
            end
    end.

do_proto_defs_aux3(Defs, DefsNoRenamings, DefsForIntrospect,
                   Mod, AnRes, Opts) ->
    case get_output_format(Opts) of
        proto_defs ->
            case proplists:get_value(proto_defs_version, Opts) of
                undefined ->
                    {ok, Defs};
                Vsn ->
                    case gpb_defs:convert_defs_from_latest_version(Defs, Vsn) of
                        {ok, Defs1} ->
                            {ok, Defs1};
                        {error, Reason} ->
                            {error, {cvt_proto_defs_version_to_return_failed,
                                     Vsn, Reason}}
                    end
            end;
        binary ->
            ErlTxt = format_erl(Mod, Defs, DefsNoRenamings, DefsForIntrospect,
                                AnRes, Opts),
            HrlTxt = possibly_format_hrl(Mod, Defs, AnRes, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            compile_to_binary(Mod, HrlTxt, ErlTxt, NifTxt, Opts);
        file ->
            ErlTxt = format_erl(Mod, Defs, DefsNoRenamings, DefsForIntrospect,
                                AnRes, Opts),
            HrlTxt = possibly_format_hrl(Mod, Defs, AnRes, Opts),
            NifTxt = possibly_format_nif_cc(Mod, Defs, AnRes, Opts),
            {Erl, Hrl, NifCc} = get_output_files(Mod, Opts),
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

get_output_files(Mod, Opts) ->
    ErlOutDir = get_erl_outdir(Opts),
    HrlOutDir = get_hrl_outdir(Opts),
    NifCcOutDir = get_nif_cc_outdir(Opts),
    Erl = filename:join(ErlOutDir, atom_to_list(Mod) ++ ".erl"),
    Hrl =
        case gpb_lib:get_records_or_maps_by_opts(Opts) of
            records ->
                filename:join(HrlOutDir, atom_to_list(Mod) ++ ".hrl");
            maps ->
                '$not_generated'
        end,
    NifCc =
        case proplists:get_bool(nif, Opts) of
            true ->
                filename:join(NifCcOutDir, atom_to_list(Mod) ++ ".nif.cc");
            false ->
                '$not_generated'
        end,
    {Erl, Hrl, NifCc}.

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

%% @doc List inputs and file outputs, based on an input file.  The
%% elements `erl_output', `hrl_output' and `nif_cc_output' are present
%% if output would be generated to those based on the options. The file
%% is scanned for `import "file.proto";' declarations recursively.
%%
%% The imported files are located the same way {@link file/2} would find
%% them, using options such as <tt>{<a href="#option-i">i</a>,Directory}</tt>.
%% For more info, See the  <a href="#optionsection-inputs-outputs">inputs and
%% outputs section</a>.
%%
%% If there is an error parsing the input proto file or an imported
%% file, it is treated as it it was a file with no imports. Thus it is
%% still included in `sources'. If a file in an `import' declaration
%% cannot be found, it is included among the `missing' items.
%%
%% The first element in `sources' is always the input file.  The rest of
%% the elements are any imported files in breadth-first order. An imported
%% file is in `sources' only once, even if it is imported multiple
%% times.
%%
%% Any renaming options are used to compute the output file names.
%% Any import fetcher options are used to retrieve files.
-spec list_io(filename(), opts()) -> [io_info_item()].
list_io(FileName, Opts) ->
    Opts1 = normalize_opts(Opts),
    list_deps(FileName, Opts1).

%% @equiv string_list_io(Mod, Str, [])
-spec string_list_io(module(), string()) -> [io_info_item()].
string_list_io(Mod, Str) ->
    string_list_io(Mod, Str, []).

%% @doc List inputs and file outputs based on proto definitions in a
%% string instead of in a file, similar to {@link string/3}.  See
%% {@link list_io/2} for further information.
-spec string_list_io(module(), string(), opts()) -> [io_info_item()].
string_list_io(Mod, Str, Opts) ->
    Opts1 = normalize_opts(Opts),
    list_deps({Mod, Str}, Opts1).

list_deps(In, Opts) ->
    {Sources, Missing} = collect_inputs(In, Opts),
    Mod = find_out_mod(In, Opts),
    DefaultOutDir = find_default_out_dir(In),
    Opts1 = Opts ++ [{o,DefaultOutDir}],
    OutputFiles =
        case get_output_format(Opts1) of
            proto_defs ->
                [];
            binary ->
                [];
            file ->
                {Erl, Hrl, NifCc} = get_output_files(Mod, Opts1),
                lists:append(
                  [[{erl_output, Erl}],
                   [{hrl_output, Hrl} || Hrl /= '$not_generated'],
                   [{nif_cc_output, NifCc} || NifCc /= '$not_generated']])
        end,
    OutputFiles ++
        [{sources, Sources},
         {missing, Missing}].

do_list_deps_from_file_or_string(In, Opts) ->
    Listing = list_deps(In, Opts),
    case get_deps_output_dest(Opts) of
        {file, DestFileName} ->
            ListingTxt = render_deps_listing(Listing, Opts),
            case file_write_file(DestFileName, ListingTxt, Opts) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {write_failed, DestFileName, Reason}}
            end;
        stdout ->
            ListingTxt = render_deps_listing(Listing, Opts),
            io:format("~s", [ListingTxt]),
            ok
    end.

render_deps_listing(Listing, Opts) ->
    Erl = proplists:get_value(erl_output, Listing),
    Sources = proplists:get_value(sources, Listing),
    Missing = proplists:get_value(missing, Listing),
    OptMG = proplists:get_bool(list_deps_missing_imports_are_generated, Opts),
    %% The first among Dependencies is the input source to analyzs
    Dependencies = if OptMG -> Sources ++ Missing;
                      true  -> Sources
                   end,
    Target = get_listing_target(Erl, Opts),
    case proplists:get_value(list_deps, Opts) of
        makefile_rules ->
            Phonies =
                case proplists:get_bool(list_deps_makefile_phonies, Opts) of
                    true  -> only_files(tl(Dependencies));
                    false -> []
                end,
            [?f("~s: ~s~n", [Target, format_deps(only_files(Dependencies))]),
             [?f("~n~s:~n", [Phony]) || Phony <- Phonies]];
        {list_imports, newline_terminated} ->
            [?f("~s~n", [Dep]) || Dep <- only_files(tl(Dependencies))];
        {list_imports, null_terminated} ->
            %% Find -print0 is null terminated rather than null separated.
            %% so do the same here.
            [?f("~s\000", [Dep]) || Dep <- only_files(tl(Dependencies))]
    end.

format_deps(Dependencies) ->
    gpb_lib:string_join(Dependencies, " \\\n    ").

get_deps_output_dest(Opts) ->
    case proplists:get_value(list_deps_dest_file, Opts) of
        undefined -> stdout;
        File      -> {file, File}
    end.

only_files(Sources) ->
    %% Filter away from_input_string and {from_fetched,_}
    [Source || Source <- Sources,
               is_list(Source)].

get_listing_target(Erl, Opts) ->
    case proplists:get_value(list_deps_makefile_target, Opts) of
        undefined         -> Erl;
        S when is_list(S) -> S;
        {quote, S}        -> make_quote(S)
    end.

make_quote("$" ++ Rest) -> "$$" ++ make_quote(Rest);
make_quote([C | Rest]) -> [C | make_quote(Rest)];
make_quote("") -> "".

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
fmt_err({parse_errors, FileName, Reasons}) ->
    gpb_lib:nl_join(
      [?f("~s:~w: ~s", [FileName, Line, Module:format_error(ErrInfo)])
       || {Line, Module, ErrInfo} <- Reasons]);
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
    gpb_defs:format_post_process_error({error, Reasons});
fmt_err({write_failed, File, Reason}) ->
    ?f("failed to write ~s: ~s (~p)", [File, file:format_error(Reason),Reason]);
fmt_err({invalid_options, translation, nif}) ->
    "Option error: Not supported: both translation option and nif";
fmt_err({invalid_options, preserve_unknown_fields, json}) ->
    "Option error: Not supported: both preserve_unknown_fields and json";
fmt_err({unsupported_translation, _Type, non_msg_type}) ->
    "Option to translate is supported only for message types, for now";
fmt_err({invalid_options, epb_functions, maps}) ->
    "Option error: Not supported: both epb_compatibility (or epb_functions) "
        "and maps";
fmt_err({invalid_options, nif, {gen_mergers, false}}) ->
    "Option error: It is only possible to omit mergers with nif";
fmt_err({invalid_options, {verify,Verify}, {gen_verifiers,false}}) ->
    ?f("Option error: It is not possible to omit verifiers when verify = ~p",
       [Verify]);
fmt_err({epb_compatibility_impossible, {with_msg_named, msg}}) ->
    "Not possible to generate epb compatible functions when a message "
        "is named 'msg' because of collision with the standard gpb functions "
        "'encode_msg' and 'decode_msg'";
fmt_err(maps_flat_oneof_not_supported_for_target_version) ->
    "Flat oneof for maps is only supported on Erlang 18 and later";
fmt_err({rename_defs, Reason}) ->
    gpb_names:format_error(Reason);
fmt_err({cvt_proto_defs_version_to_latest_error, Reason}) ->
    ?f("Failed to convert supplied proto definitions version "
       "to latest: ~s",
       [gpb_defs:format_error(Reason)]);
fmt_err({cvt_proto_defs_version_to_return_failed, Vsn, Reason}) ->
    ?f("Failed to convert definitions to version ~w: ~s",
       [Vsn, gpb_defs:format_error(Reason)]);
fmt_err({cvt_proto_defs_for_introspect_error, DefsVsn, Reason}) ->
    ?f("Failed to convert definitions to version ~w "
       "for introspection functions: ~s",
       [DefsVsn, gpb_defs:format_error(Reason)]);
fmt_err(X) ->
    ?f("Unexpected error ~p", [X]).

%% @doc Produce a plain-text error message from a reason returned by
%% for instance {@link file/2} or {@link proto_defs/2}.
%% @end
%% Note: do NOT include trailing newline (\n or ~n)
-spec format_warning(warning()) -> iolist().
format_warning({ignored_field_opt_packed_for_unpackable_type,
                MsgName, FName, Type, _Opts}) ->
    ?f("Warning: ignoring option packed for non-packable field ~s.~s "
       "of type ~w", [MsgName, FName, Type]);
format_warning(maps_flat_oneof_generated_code_may_fail_to_compile) ->
    "Warning: Generated code for flat oneof for maps may fail to compile "
        "on 18.3.4.6, or later Erlang 18 versions, due to a compiler issue";
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
    show_help([]),
    halt(0).

%% @doc This function is intended as a command line interface for the compiler.
%% Call it from the command line as follows:
%% ```
%%    erl <erlargs> [gpb-opts] -s gpb_compile c File.proto ...
%%    erl <erlargs> -s gpb_compile c File.proto ... -extra [gpb-opts]
%% '''
%% The `<erlargs>' can be `-noshell -noinput +B -boot start_clean -pa SomeDir'
%%
%% If several files are specified, each is compiled individually, no
%% checking is done for instance for multiply defined messages or
%% fields across files, such as the `protoc' does.
%%
%% The options below are supported. Dashes and underscores inside option names
%% are equivalent, ie `-o-erl' and `-o_erl' are the same option.
%%
%% Input files and output files and formats
%% <dl>
%%   <dt><a id="cmdline-option-I"/>
%%       `-IDir' `-I Dir'</dt>
%%     <dd>Specify include directory.
%%       Option may be specified more than once to specify
%%       several include directories.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-i">i</a></dd>
%%   <dt><a id="cmdline-option-o"/>
%%       `-o Dir'</dt>
%%     <dd>Specify output directory for where to generate
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl</dd>
%%   <dt><a id="cmdline-option-o-erl"/>
%%       <a id="cmdline-option-o-hrl"/>
%%       <a id="cmdline-option-o-nif-cc"/>
%%       `-o-erl Dir' | `-o-hrl Dir' | `-o-nif-cc Dir'</dt>
%%     <dd>Specify output directory for where to generate
%%       the <i>ProtoFile</i>.erl and <i>ProtoFile</i>.hrl respectively,
%%       and for the NIF C++ file, if the `-nif' option is specified.
%%       The `-o-erl Dir' option overrides any `-o Dir' option, and
%%       similarly for the other file-type specific output options.<br/>
%%       Corresponding Erlang-level options:
%%       <a href="#option-o">o</a>,
%%       <a href="#option-o_erl">o_erl</a>,
%%       <a href="#option-o_hrl">o_hrl</a> and
%%       <a href="#option-o_nif_cc">o_nif_cc</a>.</dd>
%%    <dt><a id="cmdline-option-ignore-priv-dir"/>
%%        `-ignore-priv-dir'</dt>
%%     <dd>Will stop gpb from looking for a well known types directory
%%       by trying to locate the `priv' directory of the gpb app.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-ignore_wellknown_types_directory"
%%                       >ignore_wellknown_types_directory</a></dd>
%% </dl>
%%
%% Format of the Erlang representation
%% <dl>
%%   <dt><a id="cmdline-option-strbin"/>
%%       `-strbin'</dt>
%%     <dd>Specify that decoded strings should be returned as binaries,
%%       instead of as strings (lists).<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-strings_as_binaries">strings_as_binaries</a></dd>
%%   <dt><a id="cmdline-option-maps"/>
%%       `-maps'</dt>
%%     <dd>This option expands to the following options:
%%       <ul>
%%         <li>`-msgs-as-maps'</li>
%%         <li>`-mapfields-as-maps'</li>
%%         <li>`-defs-as-maps'</li>
%%       </ul>
%%       See the <a href="#option-maps">`maps'</a> option
%%       for the function {@link file/2} for more info.<br/>
%%       Corresponding Erlang-level options:
%%       <a href="#option-maps">maps</a></dd>
%%   <dt><a id="cmdline-option-msgs-as-maps"/>
%%       `-msgs-as-maps'</dt>
%%     <dd>Specifies that messages should be maps. No `.hrl' file will
%%       be generated.
%%       Without this option, messages will be records.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-msgs_as_maps">msgs_as_maps</a></dd>
%%   <dt><a id="cmdline-option-mapfields-as-maps"/>
%%       `-mapfields-as-maps'</dt>
%%     <dd>Specifies that fields of type `map<_,_>' should be maps.
%%       Otherwise, they will be 2-tuples.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-mapfields_as_maps">mapfields_as_maps</a></dd>
%%   <dt><a id="cmdline-option-maps_unset_optional"/>
%%       `-maps_unset_optional omitted | present_undefined'</dt>
%%     <dd>Specifies the internal format for optional fields that are unset.
%%       <br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-maps_unset_optional">maps_unset_optional</a></dd>
%%   <dt><a id="cmdline-option-maps_oneof"/>
%%       `-maps_oneof tuples | flat'</dt>
%%     <dd>Specifies the internal format for oneof fields in maps. (Requires
%%       `-maps' and `-maps_unset_optional omitted', of which the latter
%%       is default since 4.0.0.)<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-maps_oneof">maps_oneof</a></dd>
%%   <dt><a id="cmdline-option-maps_key_type"/>
%%       `-maps_key_type atom | binary'</dt>
%%     <dd>Specifies the key type for maps.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-maps_key_type">maps_key_type</a></dd>
%% </dl>
%%
%% Verification of input
%% <dl>
%%   <dt><a id="cmdline-option-v"/>
%%       `-v optionally | always | never'</dt>
%%     <dd>Specify how the generated encoder should
%%       verify the message to be encoded.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-verify">verify</a></dd>
%%   <dt><a id="cmdline-option-vdrp"/>
%%       `-vdrp'</dt>
%%     <dd>Specify if the generated decoder should
%%       verify presence of required fields.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-verify_decode_required_present"
%%                       >verify_decode_required_present</a></dd>
%%   <dt><a id="cmdline-option-no-gen-verifiers"/>
%%       `-no-gen-verifiers'</dt>
%%     <dd>Do not generate `verify_msg' functions. Implies `-v never'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-gen_verifiers">gen_verifiers</a></dd>
%% </dl>
%%
%% Renaming for the Erlang side
%% <dl>
%%   <dt><a id="cmdline-option-rename"/>
%%       `-rename What:How'</dt>
%%     <dd>The following `What' values are available:
%%       <dl>
%%         <dt>`pkg_name'</dt>
%%         <dd>Modify the package name. Useful with the `-pkgs' option.</dd>
%%         <dt>`msg_name'</dt>
%%         <dd>Modify message names, but not the package part of them</dd>
%%         <dt>`msg_fqname'</dt>
%%         <dd>Modify message names, including their package parts.
%%             Useful with the `-pkgs' option.</dd>
%%         <dt>`group_name'</dt>
%%         <dd>Group names.</dd>
%%         <dt>`group_fqname'</dt>
%%         <dd>Group names including their package parts.</dd>
%%         <dt>`service_name'</dt>
%%         <dd>Service names.</dd>
%%         <dt>`service_fqname'</dt>
%%         <dd>Service names including their package parts.</dd>
%%         <dt>`rpc_name'</dt>
%%         <dd>The RPC name.</dd>
%%         <dt>`msg_typename'</dt>
%%         <dd>Erlang type names for messages and groups.</dd>
%%         <dt>`enum_typename'</dt>
%%         <dd>Erlang type names for enums.</dd>
%%       </dl>
%%       The following `How' values are available:
%%       <dl>
%%         <dt>`prefix=Prefix'</dt>
%%         <dd>Prepend the Prefix to the beginning of the name.</dd>
%%         <dt>`suffix=Suffix'</dt>
%%         <dd>Append the Suffix to the end of the name.</dd>
%%         <dt>`lower_case'</dt>
%%         <dd>Example: from `MsgName' to `msgname'</dd>
%%         <dt>`snake_case'</dt>
%%         <dd>Example: from `MsgName' to `msg_name'</dd>
%%         <dt>`dots_to_underscores'</dt>
%%         <dd>Example: from `Msg.Sub' to `Msg_Sub'</dd>
%%         <dt>`base_name'</dt>
%%         <dd>Example: from `Domain.Pkg.Msg' to `Msg'</dd>
%%         <dt>`proto=Proto:prefix=Prefix[,...]'</dt>
%%         <dd>For each message belonging the the .proto Proto, without the
%%             `.proto' suffix, prepend `Prefix' to the beginning of the name.
%%             This only works for `msg_name' and `msg_fqname'.</dd>
%%       </dl>
%%       It is possible to specify more than one -rename option,
%%       and they are applied in the order specified.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-rename">rename</a></dd>
%%   <dt><a id="cmdline-option-msgprefix"/>
%%       `-msgprefix Prefix'</dt>
%%     <dd>Prefix each message with `Prefix'. This can be useful to
%%       when including different sub-projects that have colliding
%%       message names.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-msg_name_prefix">msg_name_prefix</a></dd>
%%   <dt><a id="cmdline-option-msgsuffix"/>
%%       `-msgsuffix Suffix'</dt>
%%     <dd>Suffix each message name with `Suffix'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-msg_name_suffix">msg_name_suffix</a></dd>
%%   <dt><a id="cmdline-option-msgtolower"/>
%%       `-msgtolower'</dt>
%%     <dd>ToLower each message. Any prefixes/suffixes are added
%%       after case modification.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-msg_name_to_lower">msg_name_to_lower</a></dd>
%%   <dt><a id="cmdline-option-modprefix"/>
%%       `-modprefix Prefix'</dt>
%%     <dd>Prefix each module with `Prefix'. Normally the module name of
%%       the generated code is based on the name of the `.proto' file.
%%       This option prepends a prefix to the module name, which can be
%%       useful when including different sub-projects that have
%%       colliding proto file names.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-module_name_prefix">module_name_prefix</a></dd>
%%   <dt><a id="cmdline-option-modsuffix"/>
%%       `-modsuffix Suffix'</dt>
%%     <dd>Suffix each module name with `Suffix'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-module_name_suffix">module_name_suffix</a></dd>
%%   <dt><a id="cmdline-option-modname"/>
%%       `-modname Name'</dt>
%%     <dd>Specify the name of the generated module.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-module_name">module_name</a></dd>
%% </dl>
%%
%% What to generate and how
%% <dl>
%%   <dt><a id="cmdline-option-pkgs"/>
%%       `-pkgs'</dt>
%%     <dd>Prepend the name of a package to every message it contains.
%%       If no package is defined, nothing will be prepended.
%%       Default is to not prepend package names for backwards
%%       compatibility, but it is needed for some proto files.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-use_packages">use_packages</a></dd>
%%   <dt><a id="cmdline-option-descr"/>
%%       `-descr'</dt>
%%     <dd>Generate self-description information.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-descriptor">descriptor</a></dd>
%%   <dt><a id="cmdline-option-il"/>
%%       `-il'</dt>
%%     <dd>Generate code that include gpb.hrl using `-include_lib'
%%       instead of `-include', which is the default.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-include_as_lib">include_as_lib</a></dd>
%%   <dt><a id="cmdline-option-include-mod-hrl-prepend"/>
%%       `-include-mod-hrl-prepend string()'</dt>
%%     <dd>
%%       Specify a string to prepend to the generated `-include("mod.hrl").'
%%       (when generating for records).<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-include_mod_hrl_prepend"
%%                       >include_mod_hrl_prepend</a></dd>
%%   <dt><a id="cmdline-option-bypass-wrappers"/>
%%       `-bypass-wrappers'</dt>
%%     <dd>Bypass wrappers.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-bypass_wrappers">bypass_wrappers</a></dd>
%%   <dt><a id="cmdline-option-c"/>
%%       `-c true | false | auto | integer() | float()'</dt>
%%     <dd>Specify how or when the generated decoder should
%%       copy fields of type `bytes'. See the <a href="#option-copy_bytes"
%%       >`copy_bytes'</a> option for the function {@link file/2}
%%       for more info.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-copy_bytes">copy_bytes</a></dd>
%%   <dt><a id="cmdline-option-type"/>
%%       <a id="cmdline-option-notype"/>
%%       `-type'<br/>`-no_type'</dt>
%%     <dd>Enables or disables `::Type()' annotations in the generated code.
%%       Default is to enable if there are no cyclic dependencies.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-type_specs">type_specs</a></dd>
%%   <dt><a id="cmdline-option-defaults-for-omitted-optionals"/>
%%       `-defaults-for-omitted-optionals'</dt>
%%     <dd>For optional fields not present on decoding, set the field to
%%       its default value, if any, instead of to `undefined'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-defaults_for_omitted_optionals"
%%                       >defaults_for_omitted_optionals</a></dd>
%%   <dt><a id="cmdline-option-type-defaults-for-omitted-optionals"/>
%%       `-type-defaults-for-omitted-optionals'</dt>
%%     <dd>For optional fields not present on decoding, set the field to
%%       its type-default, instead of to `undefined'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-type_defaults_for_omitted_optionals"
%%                       >type_defaults_for_omitted_optionals</a></dd>
%%   <dt><a id="cmdline-option-for-version"/>
%%       `-for-version N'</dt>
%%     <dd>Generate code for Erlang/OTP version N instead of current.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-target_erlang_version">target_erlang_version</a></dd>
%%   <dt><a id="cmdline-option-preserve-unknown-fields"/>
%%       `-preserve-unknown-fields'</dt>
%%     <dd>Preserve unknown fields. An extra field, `$unknowns', will be added
%%       to each record or map for storing unknown fields.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-preserve_unknown_fields"
%%                       >preserve_unknown_fields</a></dd>
%%   <dt><a id="cmdline-option-erlc_compile_options"/>
%%       `-erlc_compile_options Options'</dt>
%%     <dd>Specifies compilation options, in a comma separated string, to pass
%%       along to the `-compile(...)' directive on the generated code.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-erlc_compile_options">erlc_compile_options</a></dd>
%% </dl>
%%
%% Introspection of the proto definitions
%% <dl>
%%   <dt><a id="cmdline-option-introspect-get_proto_defs"/>
%%       `-introspect-get_proto_defs'</dt>
%%     <dd>For introspection, generate a `get_proto_defs/0' function\n"
%%       instead of `get_msg_defs/0'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-introspect_get_proto_defs"
%%                       >introspect_get_proto_defs</a></dd>
%%   <dt><a id="cmdline-option-pldefs"/>
%%       `-pldefs'</dt>
%%     <dd>Specify that introspection functions shall return proplists
%%       instead of `#field{}' records, to make the generated code
%%       completely free of even compile-time dependencies to gpb.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-defs_as_proplists">defs_as_proplists</a></dd>
%%   <dt><a id="cmdline-option-defs-as-maps"/>
%%       `-defs-as-maps'</dt>
%%     <dd>Specifies that proto definitions from the generated code
%%       are to be returned as maps. Otherwise, they will be lists
%%       of tuples and records (or proplists if the `-pldefs' option
%%       is specified)<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-defs_as_maps">defs_as_maps</a></dd>
%%   <dt><a id="cmdline-option-no-gen-introspect"/>
%%       `-no-gen-introspect'</dt>
%%     <dd>Do not generate code for introspection.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-gen_introspect">gen_introspect</a></dd>
%% </dl>
%%
%% JSON
%% <dl>
%%   <dt><a id="cmdline-option-json"/>
%%       `-json'</dt>
%%     <dd>Generate functions for converting to and from a JSON
%%       representation.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json">json</a></dd>
%%   <dt><a id="cmdline-option-json-always-print-primitive-fields"/>
%%       `-json-always-print-primitive-fields'</dt>
%%     <dd>Print also fields whose value is their type's default.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_always_print_primitive_fields"
%%                       >json_always_print_primitive_fields</a></dd>
%%   <dt><a id="cmdline-option-json-preserve-proto-field-names"/>
%%       `-json-preserve-proto-field-names'</dt>
%%     <dd>Print the fields' names as in `.proto' file, not
%%       as lowerCamelCase.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_preserve_proto_field_names"
%%                       >json_preserve_proto_field_names</a></dd>
%%   <dt><a id="cmdline-option-json-case-insensitive-enum-parsing"/>
%%       `-json-case-insensitive-enum-parsing'</dt>
%%     <dd>Make case insignificant when parsing enums in JSON. Also allow
%%       dash as alternative to undercore.
%%       Default is that case <em>is</em> significant when parsing enums.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_case_insensitive_enum_parsing"
%%                       >json_case_insensitive_enum_parsing</a></dd>
%%   <dt><a id="cmdline-option-json-format"/>
%%       `-json-format jsx | mochijson2 | jiffy | maps'</dt>
%%     <dd>Specify format for the JSON representation.
%%       `maps' is default if the `-maps' option is specified,
%%       otherwise the jsx format is default.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_format">json_format</a></dd>
%%   <dt><a id="cmdline-option-json-object-format"/>
%%       `-json-object-format eep18 | tpl | tpl:Tag | map'</dt>
%%     <dd>Specify JSON object format:
%%       <ul>
%%         <li>`eep18' means `[{}] | proplist()', this is the default.</li>
%%         <li>`tpl' means `{proplist()}'.</li>
%%         <li>`tpl:Tag' means `{Tag, proplist()}'.</li>
%%         <li>`map' means `map()'.</li>
%%       </ul>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_object_format">json_object_format</a>
%%   </dd>
%%   <dt><a id="cmdline-option-json-key-format"/>
%%       `-json-key-format binary | atom | string'</dt>
%%     <dd>Specify JSON object key format. `binary' is the default.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_key_format">json_key_format</a></dd>
%%   <dt><a id="cmdline-option-json-array-format"/>
%%       `-json-array-format list | tl:Tag'</dt>
%%     <dd>Specify JSON object array format.
%%       <ul>
%%         <li>`list' means `list()', this is the default.</li>
%%         <li>`tl:Tag' means `{Tag, list()}'.</li>
%%       </ul>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_array_format">json_array_format</a>
%%   </dd>
%%   <dt><a id="cmdline-option-json-string-format"/>
%%       `-json-string-format binary | list'</dt>
%%     <dd>Specify JSON string format. `binary' is the default.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_string_format">json_string_format</a></dd>
%%   <dt><a id="cmdline-option-json-null"/>
%%       `-json-null Null'</dt>
%%     <dd>Specify JSON null value, `null' is the default.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-json_null">json_null</a></dd>
%% </dl>
%%
%% NIF
%% <dl>
%%   <dt><a id="cmdline-option-nif"/>
%%       `-nif'</dt>
%%     <dd>Generate nifs for linking with the protobuf C(++) library.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-nif">nif</a></dd>
%%   <dt><a id="cmdline-option-load_nif"/>
%%       `-load_nif FunctionDefinition'</dt>
%%     <dd>Specify `FunctionDefinition' as the text that defines the
%%       function `load_nif/0'.  This is called as the `on_load'
%%       hook for loading the NIF.  See also the doc for the `load_nif'
%%       option in the {@link file/2} function.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-load_nif">load_nif</a></dd>
%%   <dt><a id="cmdline-option-no-gen-mergers"/>
%%       `-no-gen-mergers'</dt>
%%     <dd>Do not generate code for merging of messages. This is only useful
%%       with the option `-nif'.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-gen_mergers">gen_mergers</a></dd>
%% </dl>
%%
%% Translations
%% <dl>
%%   <dt><a id="cmdline-option-translate_type"/>
%%       `-translate_type TMsFs'</dt>
%%     <dd>Call functions in `TMsFs' to pack, unpack, merge and verify
%%       for the specified type. The `TMsFs' is a string on the
%%       following format: `type=Type,e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,V=Mod:Fn]'.
%%       The Type and specified modules and functions are called and used
%%       as follows:
%%       <dl>
%%         <dt>`type=Type'</dt>
%%         <dd>Specifies that the translations apply to fields of type.
%%             The `Type' may be either of:
%%             `msg:MsgName' (after any renaming operations),
%%             `enum:EnumName', `int32', `int64', `uint32', `uint64',
%%             `sint32', `sint64', `fixed32', `fixed64', `sfixed32',
%%             `sfixed64', `bool', `double', `string', `bytes' or
%%             `map<KeyType,ValueType>'. The last may need quoting in
%%             the shell.</dd>
%%         <dt>`e=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term)' to pack the `Term' to a value of type
%%             `Type', ie to a value that gpb knows how to wire-encode.</dd>
%%         <dt>`d=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Value)' to unpack the just wire-decoded `Value'
%%             of type `Type', to something of your choice.</dd>
%%         <dt>`m=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term1, Term2) -> Term3' to merge two
%%             unpacked terms to a resulting Term3. Note that this function
%%             is never called for scalar types.</dd>
%%         <dt>`V=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term) -> _' to verify an unpacked `Term'.
%%             If `Term' is valid, the function is expected to just return
%%             any value, which is ignored and discarded.
%%             If `Term' is invalid, the function is expected to not
%%             return anything, but instead either crash, call
%%             `erlang:error/1', or `throw/1' or `exit/1' with the
%%             reason for error.
%%             If you want to use a verifier, this is the new preferred
%%             approach.</dd>
%%         <dt>`v=Mod:Fn'</dt>
%%         <dd>Call `Mod:Fn(Term, ErrorF) -> _' to verify an unpacked `Term'.
%%             This exists for backwards compatibility, and its use
%%             is deprecated.</dd>
%%       </dl>
%%       Corresponding Erlang-level option:
%%       <a href="#option-translate_type">translate_type</a>
%%   </dd>
%%   <dt><a id="cmdline-option-any_translate"/>
%%       `-any_translate MsFs'</dt>
%%     <dd>Call functions in `MsFs' to pack, unpack, merge and verify
%%       `google.protobuf.Any' messages. The `MsFs' is a string on the
%%       following format: `e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,V=Mod:Fn]'.
%%       See the <a href="#cmdline-option-translate_type">translate</a> option
%%       for details on the string components.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-any_translate">any_translate</a></dd>
%%   <dt><a id="cmdline-option-translate_field"/>
%%       `-translate_field FMsFs'</dt>
%%     <dd>Call functions in FMsFs to pack, unpack, merge, and verify.
%%       This is similar to the `-translate_type' option, except that
%%       a message field is specified instead of a type. The `FMsFs'
%%       is a string on the following format:
%%       `field=Path,e=...,d=...,m=...,V=...[,i=Mod:Fn][,a=Mod:Fn][,f=Mod:Fn]'
%%       See the <a href="#cmdline-option-translate_type">`-translate_type'</a>
%%       option for info on `e=', `d=', `m=' and `V=' items.
%%       Additionally for this `-translate_field' option, these exist:
%%       <dl>
%%         <dt>`field=Path'</dt>
%%         <dd>The `Path' indicates the element to translate as follows:
%%           <ul>
%%             <li>`MsgName' for the message itself. (This is actually
%%                  equivalent to `-translate_type type=msg:MsgName,...')</li>
%%             <li>`MsgName.FieldName' for fields generally</li>
%%             <li>`MsgName.OneofName.FieldName' for oneof fields</li>
%%             <li>`MsgName.FieldName.[]' for elements of repeated fields</li>
%%           </ul>
%%         </dd>
%%         <dt>`i=Mod:Fn'</dt>
%%         <dd>For repeated fields, call `Mod:Fn()' on decoding to initialize
%%             the field to some value</dd>
%%         <dt>`a=Mod:Fn'</dt>
%%         <dd>For repeated fields, call `Mod:Fn(Elem,S)' on decoding
%%             to add an item)</dd>
%%         <dt>`f=Mod:Fn'</dt>
%%         <dd>For repeated fields, call `Mod:Fn(S)' on decoding
%%             to finalize the field</dd>
%%       </dl>
%%       Corresponding Erlang-level option:
%%       <a href="#option-translate_field">translate_field</a>
%%   </dd>
%% </dl>
%%
%% Compatibility with Erlang protobuffs
%% <dl>
%%   <dt><a id="cmdline-option-epb"/>
%%       `-epb'</dt>
%%     <dd>Enable compatibility with the Erlang Protobuffs library:
%%       <ul>
%%         <li>Implies the `-epb-functions' option</li>
%%         <li>Implies the `-defaults-for-omitted-optionals' option</li>
%%         <li>Implies the `-modsuffix _pb' option</li>
%%         <li>Implies the `-msgtolower' option</li>
%%       </ul>
%%       Corresponding Erlang-level option:
%%       <a href="#option-epb_compatibility">epb_compatibility</a></dd>
%%   <dt><a id="cmdline-option-epb-functions"/>
%%       `-epb-functions'</dt>
%%     <dd>For compatibility with the Erlang Protobuffs library, generate also
%%       the following functions: `encode/1', `decode/2', `encode_MsgName/1'
%%       and `decode_MsgName/1'<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-epb_functions">epb_functions</a></dd>
%% </dl>
%%
%% Querying dependencies
%% <dl>
%%   <dt><a id="cmdline-option-m"/>
%%       `-M'</dt>
%%     <dd>Generate Makefile rule(s) to stdout for dependencies.
%%       No code is generated unless `-MMD' is specified.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps">list_deps</a></dd>
%%   <dt><a id="cmdline-option-ml"/>
%%       `-ML'</dt>
%%     <dd>Print imports, one per line instead of on Makefile format.
%%       No code is generated unless `-MMD' is specified.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps">list_deps</a></dd>
%%   <dt><a id="cmdline-option-m0"/>
%%       `-M0'</dt>
%%     <dd>Print imports, each terminated by a null character, instead of
%%       on Makefile format.
%%       No code is generated unless `-MMD' is specified.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps">list_deps</a></dd>
%%   <dt><a id="cmdline-option-mf"/>
%%       `-MF file'</dt>
%%     <dd>Specify a file to write dependency rules to, instead of printing them
%%       to stdout. No code is generated unless `-MMD' is specified.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps_dest_file">list_deps_dest_file</a></dd>
%%   <dt><a id="cmdline-option-mg"/>
%%       `-MG'</dt>
%%     <dd>Consider missing imports to be generated and include\n"
%%       them in the listed dependencies or rules.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps_missing_imports_are_generated"
%%                       >list_deps_missing_imports_are_generated</a></dd>
%%   <dt><a id="cmdline-option-mp"/>
%%       `-MP'</dt>
%%     <dd>Generate phony Makefile targets for dependencies.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps_makefile_phonies"
%%                       >list_deps_makefile_phonies</a></dd>
%%   <dt><a id="cmdline-option-mt"/>
%%       `-MT target'</dt>
%%     <dd>Override the Makefile rule target.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps_makefile_target"
%%                       >list_deps_makefile_target</a></dd>
%%   <dt><a id="cmdline-option-mq"/>
%%       `-MQ target'</dt>
%%     <dd>Same as `-MT' but quote characters special to make.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps_makefile_target"
%%                       >list_deps_makefile_target</a></dd>
%%   <dt><a id="cmdline-option-mmd"/>
%%       `-MMD'</dt>
%%     <dd>List imports or Makefile rules and generate code.
%%       This option works like in erlc, which contrasts to gcc.<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-list_deps_and_generate"
%%                       >list_deps_and_generate</a></dd>
%% </dl>
%%
%% Errors and warnings
%% <dl>
%%   <dt><a id="cmdline-option-W"/>
%%       `-Werror', `-W1', `-W0', `-W', `-Wall'</dt>
%%     <dd>`-Werror' means treat warnings as errors<br></br>
%%       `-W1' enables warnings, `-W0' disables warnings.<br></br>
%%       `-W' and `-Wall' are the same as `-W1'<br/>
%%       Corresponding Erlang-level option:
%%       <a href="#option-warnings_as_errors">warnings_as_errors</a></dd>
%% </dl>
%%
%% Help and info
%% <dl>
%%   <dt><a id="cmdline-option-help"/>
%%       <a id="cmdline-option-h"/>
%%       `--help' or `-h'</dt>
%%     <dd>Show help.</dd>
%%   <dt><a id="cmdline-option-version"/>
%%       `--version' or `-V'</dt>
%%     <dd>Show the version number of gpb.</dd>
%% </dl>
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
            show_help(Opts),
            halt(1);
        show_help  ->
            show_help(Opts),
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

opt_matches(_Opt, {{section, _}}) ->
    false;
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
parse_opt(_, {_OptName, 'atom()', OptTag, _Descr}, [OptArg | Rest]) ->
    {ok, {{OptTag, list_to_atom(OptArg)}, Rest}};
parse_opt(_, {OptName, 'integer()', OptTag, _Descr}, [OptArg | Rest]) ->
    try list_to_integer(OptArg) of
        N -> {ok, {{OptTag, N}, Rest}}
    catch error:badarg ->
            {error, ?ff("Invalid version number (integer) for ~s: ~p",
                        [OptName, OptArg])}
    end;
parse_opt(_, {_OptName, {'opt_value()', OptValue}, OptTag, _Descr}, Rest) ->
    {ok, {{OptTag, OptValue}, Rest}};
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
    [{{section, "Input files and output files and formats"}},
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
     {"ignore-priv-dir", undefined, ignore_wellknown_types_directory, "\n"
      "       Will stop gpb from looking for a well known types directory\n"
      "       by trying to locate the priv directory of the gpb app.\n"},
     {{section, "Format of the Erlang representation"}},
     {"strbin", undefined, strings_as_binaries, "\n"
      "       Specify that decoded strings should be returned as binaries,\n"
      "       instead of as strings (lists).\n"},
     {"maps", undefined, maps, "\n"
      "       This will expand to the following options:\n"
      "         -msgs-as-maps\n"
      "         -msgfields-as-maps\n"
      "         -defs-as-maps\n"},
     {"msgs-as-maps", undefined, msgs_as_maps, "\n"
      "        Specifies that messages should be maps.\n"
      "        Otherwise, they will be records.\n"},
     {"mapfields-as-maps", undefined, mapfields_as_maps, "\n"
      "        Specifies that fields of type map<_,_> should be maps.\n"
      "        Otherwise, they will be 2-tuples.\n"},
     {"maps_unset_optional", {omitted, present_undefined}, maps_unset_optional,
      "omitted | present_undefined\n"
      "       Specifies the internal format for optional fields\n"
      "       that are unset.\n"},
     {"maps_oneof", {tuples, flat}, maps_oneof,
      "tuples | flat\n"
      "       Specifies the representation for oneof fields in maps:\n"
      "       as tuples, #{..., OneofField => {Tag, Value}, ...}   (default)\n"
      "       or flat,   #{..., Tag => Value, ...}\n"},
     {"maps_key_type", {atom, binary}, maps_key_type,
      "atom | binary\n"
      "       Specifies the key type for maps.\n"},
     {{section, "Verification of inputs"}},
     {"v", {optionally, always, never}, verify, " optionally | always | never\n"
      "       Specify how the generated encoder should\n"
      "       verify the message to be encoded.\n"},
     {"vdrp", undefined, verify_decode_required_present, "\n"
      "       Verify that on decoding, required fields are present."},
     {"no-gen-verifiers", {'opt_value()', false}, gen_verifiers, "\n"
      "       Do not generate verify_msg functions.\n"
      "       Implies `-v never'.\n"},
     {{section, "Renaming for the Erlang side"}},
     {"rename", fun opt_rename/2, rename, " What:How\n"
      "       What:\n"
      "         pkg_name       Modify the package name. Useful\n"
      "                        with the -pkgs option.\n"
      "         msg_name       Modify message names, but not the package\n"
      "                        part of them\n"
      "         msg_fqname     Modify message names, including their\n"
      "                        package part. Useful with the -pkgs option.\n"
      "         group_name     Group names.\n"
      "         group_fqname   Group names including their package parts.\n"
      "         service_name   Service names.\n"
      "         service_fqname Service names including their package parts.\n"
      "         rpc_name       The RPC name.\n"
      "         msg_typename   Erlang type names for messages and groups.\n"
      "         enum_typename  Erlang type names for enums.\n"
      "       How:\n"
      "          prefix=Prefix        Prepend the Prefix.\n"
      "          suffix=Suffix        Append the Suffix.\n"
      "          lower_case           Example: from MsgName to msgname\n"
      "          snake_case           Example: from MsgName to msg_name\n"
      "          dots_to_underscores  Example: from Msg.Sub to Msg_Sub\n"
      "          base_name            Example: from Domain.Pkg.Msg to Msg\n"
      "          proto=Proto:prefix=Prefix[,...]\n"
      "                               For each message belonging the the\n"
      "                               proto Proto, without the `.proto'\n"
      "                               suffix, prepend Prefix. This only\n"
      "                               works for msg_name and msg_fqname.\n"
      "       It is possible to specify more than one -rename option,\n"
      "       and they are applied in the order specified.\n"},
     {"msgprefix", 'string()', msg_name_prefix, "Prefix\n"
      "       Prefix each message with Prefix.\n"},
     {"msgsuffix", 'string()', msg_name_suffix, "Suffix\n"
      "       Suffix each message with Suffix.\n"},
     {"msgtolower", undefined, msg_name_to_lower, "ToLower\n"
      "       ToLower each message.  Any prefixes/suffixes are added\n"
      "       after case modification.\n"},
     {"modprefix", 'string()', module_name_prefix, "Prefix\n"
      "       Prefix the module name with Prefix.\n"},
     {"modsuffix", 'string()', module_name_suffix, "Suffix\n"
      "       Suffix the module name with Suffix.\n"},
     {"modname", 'string()', module_name, "Name\n"
      "       Specify the name of the generated module.\n"},
     {{section, "What to generate and how"}},
     {"pkgs", undefined, use_packages, "\n"
      "       Prepend the name of a package to every message it contains.\n"
      "       If no package is defined, nothing will be prepended.\n"
      "       Default is to not prepend package names for backwards\n"
      "       compatibility, but it is needed for some proto files.\n"},
     {"descr", undefined, descriptor, "\n"
      "       Generate self-description information.\n"},
     {"il", undefined, include_as_lib, "\n"
      "       Generate code that includes gpb.hrl using -include_lib\n"
      "       instead of -include, which is the default.\n"},
     {"include-mod-hrl-prepend", 'string()', include_mod_hrl_prepend, "\n"
      "       Specify a string to prepend to the generated\n"
      "       -include(\"mod.hrl\"). (when generating for records).\n"},
     {"bypass-wrappers", undefined, bypass_wrappers, "\n"
      "       Bypass wrappers.\n"},
     {"c", {true, false, auto, 'number()'}, copy_bytes,
      " true | false | auto | number()\n"
      "       Specify how or when the generated decoder should\n"
      "       copy fields of type bytes.\n"},
     {"type", undefined, type_specs, "\n"
      "       Enables `::Type()' annotations in the generated code.\n"},
     {"no_type", {'opt_value()', false}, type_specs, "\n"
      "       Disables `::Type()' annotations in the generated code.\n"},
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
     {"preserve-unknown-fields", undefined, preserve_unknown_fields, "\n"
      "       Preserve unknown fields.\n"},
     {"erlc_compile_options", 'string()', erlc_compile_options, "String\n"
      "       Specifies compilation options, in a comma separated string, to\n"
      "       pass along to the -compile() directive on the generated code.\n"},
     {{section, "Introspection of the proto definitions"}},
     {"introspect-get_proto_defs", undefined, introspect_get_proto_defs, "\n"
      "       For introspection, generate a get_proto_defs/0 function\n"
      "       instead of get_msg_defs/0.\n"},
     {"pldefs", undefined, defs_as_proplists, "\n"
      "       Specify that introspection functions shall return proplists\n"
      "       instead of #field{} records, to make the generated code\n"
      "       completely free of even compile-time dependencies to gpb.\n"},
     {"defs-as-maps", undefined, defs_as_maps, "\n"
      "        Specifies that proto definitions from the generated code\n"
      "        are to be returned as maps. Otherwise, they will be lists\n"
      "        of tuples and records (or proplists if the -pldefs option\n"
      "        is specified)\n"},
     {"no-gen-introspect", {'opt_value()', false}, gen_introspect, "\n"
      "       Do not generate code for introspection.\n"},
     {{section, "JSON"}},
     {"json", undefined, json, "\n"
      "       Generate functions for converting to and from\n"
      "       a JSON representation.\n"},
     {"json-always-print-primitive-fields", undefined,
      json_always_print_primitive_fields, "\n"
      "       Print also fields whose value is their type's default.\n"},
     {"json-preserve-proto-field-names", undefined,
      json_preserve_proto_field_names, "\n"
      "       Print the fields' names as in the .proto file, not as\n"
      "       lowerCamelCase.\n"},
     {"json-case-insensitive-enum-parsing", undefined,
      json_case_insensitive_enum_parsing, "\n"
      "       Make case insignificant when parsing enums in JSON. Also allow\n"
      "       dash as alternative to underscore.\n"
      "       Default is that case _is_ significant when parsing enums.\n"},
     {"json-format", {jsx,mochijson2,jiffy,maps}, json_format, "\n"
      "       Specify format for JSON representation:\n"
      "       * jsx          (default if -maps is not specified)\n"
      "       * mochijson2\n"
      "       * jiffy\n"
      "       * maps         (default if -maps is specified)\n"},
     {"json-object-format", fun opt_json_object_format/2,json_object_format,"\n"
      "       Specify format for JSON object representation:\n"
      "       * eep18        [{}] | proplist()  this is the default\n"
      "       * tpl          {proplist()}\n"
      "       * tpl:Tag      {Tag, proplist()}\n"
      "       * map          map()\n"},
     {"json-key-format", {binary,atom,string}, json_key_format, "\n"
      "       Specify format for JSON object keys:\n"
      "       * binary       (default)\n"
      "       * atom\n"
      "       * string\n"},
     {"json-array-format", fun opt_json_array_format/2, json_array_format, "\n"
      "       Specify format for JSON arrays:\n"
      "       * list       list() this is the default\n"
      "       * tl:Tag     {Tag,list()}\n"},
     {"json-string-format", {binary,list}, json_string_format, "\n"
      "       Specify format for JSON strings:\n"
      "       * binary       this is the default\n"
      "       * list\n"},
     {"json-null", 'atom()', json_null, "\n"
      "       Specify format for the JSON null value, null is the default.\n"},
     {{section, "NIF"}},
     {"nif", undefined, nif, "\n"
      "       Generate nifs for linking with the protobuf C(++) library.\n"},
     {"load_nif", 'string()', load_nif, "FunctionDefinition\n"
      "       Specify FunctionDefinition as the text that defines the\n"
      "       function load_nif/0.  This is called as the -on_load.\n"
      "       hook for loading the NIF.\n"},
     {"no-gen-mergers", {'opt_value()', false}, gen_mergers, "\n"
      "       Do not generate code for merging of messages. This is only\n"
      "       useful with the option -nif.\n"},
     {{section, "Translations"}},
     {"translate_type", fun opt_translate_type/2, translate_type,
      " type=Type,e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]\n"
      "       For fields of the specified type, call Mod:Fn to:\n"
      "       - encode (calls Mod:Fn(Term) -> AnyMessage to pack)\n"
      "       - decode (calls Mod:Fn(AnyMessage) -> Term to unpack)\n"
      "       - merge  (calls Mod:Fn(Term,Term2) -> Term3 to merge unpacked)\n"
      "       - verify (calls Mod:Fn(Term) -> _ to verify unpacked)\n"
      "       Type can be any of msg:MsgName (after any renaming operations)\n"
      "       enum:EnumName, int32, int64, uint32, uint64, sint32 sint64,\n"
      "       fixed32, fixed64, sfixed32, sfixed64, bool, double, string,\n"
      "       bytes, map<KeyType,ValueType>. The last may need quoting in\n"
      "       the shell. No merge function is called for scalar fields.\n"},
     {"any_translate", fun opt_any_translate/2, any_translate,
      " e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]\n"
      "       For a google.protobuf.Any message, call Mod:Fn to:\n"
      "       - encode (calls Mod:Fn(Term) -> AnyMessage to pack)\n"
      "       - decode (calls Mod:Fn(AnyMessage) -> Term to unpack)\n"
      "       - merge  (calls Mod:Fn(Term,Term2) -> Term3 to merge unpacked)\n"
      "       - verify (calls Mod:Fn(Term) -> _ to verify unpacked)\n"},
     {"translate_field", fun opt_translate_field/2, translate_field,
      " field=Field,e=Mod:Fn,d=Mod:Fn[,m=Mod:Fn][,v=Mod:Fn]"
      "[,i=Mod:Fn][,a=Mod:Fn][,f=Mod:Fn]\n"
      "       For the specified field, call Mod:Fn. Specify Field as one of:\n"
      "       - MsgName for the message itself\n"
      "       - MsgName.FieldName for fields generally\n"
      "       - MsgName.OneofName.FieldName for oneof fields\n"
      "       - MsgName.FieldName.[] for elements of repeated fields.\n"
      "       For repeated fields, ie for the field itself, not its elements,\n"
      "       the following extra translations are to be specified:\n"
      "       - i=Mod:Fn (calls Mod:Fn() on decoding to initialize the field)\n"
      "       - a=Mod:Fn (calls Mod:Fn(Elem,S) on decoding to add an item)\n"
      "       - f=Mod:Fn (calls Mod:Fn(S) on decoding to finalize the field)\n"
      ""},
     {{section, "Compatibility with Erlang protobuffs"}},
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
     {{section, "Querying dependencies"}},
     {"M", {'opt_value()', makefile_rules}, list_deps, "\n"
      "       Generate Makefile rule(s) for dependencies.\n"
      "       No code is generated unless -MMD.\n"},
     {"ML", {'opt_value()', {list_imports, newline_terminated}}, list_deps,"\n"
      "       Print imports, one per line instead of on Makefile format.\n"
      "       No code is generated unless -MMD.\n"},
     {"M0", {'opt_value()', {list_imports, null_terminated}}, list_deps, "\n"
      "       Print imports, each terminated by a null character, instead of\n"
      "       on Makefile format.\n"
      "       No code is generated unless -MMD.\n"},
     {"MF", 'string()', list_deps_dest_file, "\n"
      "       Specify a file to write dependency rules to. -MF implies -M.\n"
      "       No code is generated unless -MMD.\n"},
     {"MG", undefined, list_deps_missing_imports_are_generated, "\n"
      "       Consider missing imports to be generated and include\n"
      "       them in the listed dependencies or rules.\n"},
     {"MP", undefined, list_deps_makefile_phonies, "\n"
      "       Generate phony Makefile targets for dependencies.\n"},
     {"MT", 'string()', list_deps_makefile_target, "\n"
      "       Override the Makefile rule target.\n"},
     {"MQ", fun opt_quote_makefile_target/2, list_deps_makefile_target, "\n"
      "       Same as -MT but quote characters special to make.\n"},
     {"MMD", undefined, list_deps_and_generate, "\n"
      "       List imports and generate code.\n"
      "       This option works like in erlc, which contrasts to gcc.\n"},
     {{section, "Errors and warnings"}},
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
     {{section, "Help and info"}},
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


opt_rename(OptTag, [S | Rest]) ->
    try
        {What, S2} =opt_rename_what(S),
        How = opt_rename_how(What, S2),
        Opt = {OptTag, {What, How}},
        {ok, {Opt, Rest}}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_rename_what(S) ->
    case S of
        "pkg_name:"++S2       -> {pkg_name, S2};
        "msg_name:"++S2       -> {msg_name, S2};
        "msg_fqname:"++S2     -> {msg_fqname, S2};
        "group_name:"++S2     -> {group_name, S2};
        "group_fqname:"++S2   -> {group_fqname, S2};
        "service_name:"++S2   -> {service_name, S2};
        "service_fqname:"++S2 -> {service_fqname, S2};
        "rpc_name:"++S2       -> {rpc_name, S2};
        "msg_typename:"++S2   -> {msg_typename, S2};
        "enum_typename:"++S2  -> {enum_typename, S2};
        _ -> throw({badopt, "Invalid thing to rename: "++S})
    end.

opt_rename_how(What, S) ->
    case S of
        "prefix="++Prefix -> {prefix, Prefix};
        "suffix="++Suffix -> {suffix, Suffix};
        "lower_case" -> lower_case;
        "snake_case" -> snake_case;
        "dots_to_underscores" -> dots_to_underscores;
        "base_name" -> base_name;
        "proto="++_ when What == msg_name;
                         What == msg_fqname ->
            {prefix, {by_proto, opt_rename_how_proto_prefix(S)}};
        "proto="++_ ->
            throw({badopt, "Expected proto= only allowed for msg_name and "
                   "msg_fqname: " ++ ?ff("~p", [What])});
        _ ->
            throw({badopt, "Invalid renaming " ++ S})
    end.

opt_rename_how_proto_prefix("proto="++S) ->
    case read_s(S, $:, "") of
        {Proto, "prefix="++S2} ->
            try read_s(S2, $,, "") of
                {Prefix, Rest} ->
                    [{Proto, Prefix} | opt_rename_how_proto_prefix(Rest)]
            catch throw:{badopt, _} -> % no comma, end of list
                    [{Proto, S2}]
            end;
        _ ->
            throw({badopt, "Expected prefix= following proto="})
    end.

opt_translate_type(OptTag, [S | Rest]) ->
    try S of
        "type="++S2 ->
            {Type,Rest2} = opt_translate_type(S2),
            Ts = gpb_lib:string_lexemes(Rest2, ","),
            Opt = {OptTag, {Type, [opt_translate_mfa(T) || T <- Ts]}},
            {ok, {Opt, Rest}};
        _ ->
            {error, "Translation is expected to begin with type="}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_translate_field(OptTag, [S | Rest]) ->
    try S of
        "field="++S2 ->
            {Path,Rest2} = opt_translate_elempath(S2),
            Ts = gpb_lib:string_lexemes(Rest2, ","),
            Opt = {OptTag, {Path, [opt_translate_mfa(T) || T <- Ts]}},
            {ok, {Opt, Rest}};
        _ ->
            {error, "Translation is expected to begin with field="}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_any_translate(OptTag, [S | Rest]) ->
    try
        Ts = gpb_lib:string_lexemes(S, ","),
        Opt = {OptTag, [opt_translate_mfa(T) || T <- Ts]},
        {ok, {Opt, Rest}}
    catch throw:{badopt,ErrText} ->
            {error, ErrText}
    end.

opt_translate_type("msg:"++Rest)  -> opt_to_comma_with_tag(Rest, msg);
opt_translate_type("enum:"++Rest) -> opt_to_comma_with_tag(Rest, enum);
opt_translate_type("map<"++Rest)  -> opt_translate_map_type(Rest);
opt_translate_type(Other) ->
    {S, Rest} = read_s(Other, $,, ""),
    Type = s2a(S),
    Allowed = [int32, int64, uint32, uint64, sint32, sint64, fixed32, fixed64,
               sfixed32, sfixed64, bool, float, double, string, bytes],
    case lists:member(Type, Allowed) of
        true -> {Type, Rest};
        false -> throw({badopt,"Invalid translation type: "++S})
    end.

opt_translate_map_type(S) ->
    {KeyType, Rest} = opt_translate_type(S),
    case gpb:is_allowed_as_key_type(KeyType) of
        true ->
            {S2, Rest2} = read_s(Rest, $>, ""),
            case opt_translate_type(S2++",") of
                {ValueType, ""} ->
                    {{map,KeyType,ValueType}, Rest2};
                {_ValueType, _} ->
                    throw({badopt,"Trailing garbage text"})
            end;
        false ->
            throw({badopt,"Not allowed as map key type"})
    end.

opt_to_comma_with_tag(S, Tag) ->
    {S2, Rest} = read_s(S, $,, ""),
    {{Tag, s2a(S2)}, Rest}.

opt_translate_elempath(S) ->
    {S2, Rest} = read_s(S, $,, ""),
    case gpb_lib:string_lexemes(S2, ".") of
        [Msg]              -> {[s2a(Msg)], Rest};
        [Msg,Field]        -> {[s2a(Msg),s2a(Field)], Rest};
        [Msg,Field,"[]"]   -> {[s2a(Msg),s2a(Field),[]], Rest};
        [Msg,Field,OFName] -> {[s2a(Msg),s2a(Field),s2a(OFName)], Rest};
        _ -> throw({badopt, "Invalid element path"})
    end.

read_s([Delim|Rest], Delim, Acc) -> {lists:reverse(Acc), Rest};
read_s([C|Rest], Delim, Acc)     -> read_s(Rest, Delim, [C | Acc]);
read_s("", _Delim, _Acc)         -> throw({badopt, "Unexpected end of string"}).

opt_translate_mfa("e="++MF) -> {encode,opt_mf_str(MF, 1)};
opt_translate_mfa("d="++MF) -> {decode,opt_mf_str(MF, 1)};
opt_translate_mfa("m="++MF) -> {merge, opt_mf_str(MF, 2)};
opt_translate_mfa("V="++MF) -> {verify,opt_mf_str(MF, 1)};
opt_translate_mfa("v="++MF) -> {verify,opt_mf_str_verify(MF)};
opt_translate_mfa("i="++MF) -> {decode_init_default,opt_mf_str(MF, 0)};
opt_translate_mfa("a="++MF) -> {decode_repeated_add_elem, opt_mf_str(MF, 2)};
opt_translate_mfa("f="++MF) -> {decode_repeated_finalize, opt_mf_str(MF, 1)};
opt_translate_mfa(X) -> throw({badopt,"Invalid translation spec: "++X}).

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

opt_json_object_format(OptTag, [S | Rest]) ->
    case S of
        "eep18"     -> {ok, {{OptTag, eep18}, Rest}};
        "tpl"       -> {ok, {{OptTag, {proplist}}, Rest}};
        "tpl:"++Tag -> {ok, {{OptTag, {s2a(Tag), proplist}}, Rest}};
        "map"       -> {ok, {{OptTag, map}, Rest}};
        _           -> {error, "Invalid JSON object format: "++S}
    end;
opt_json_object_format(_OptTag, []) ->
    {error, "Missing JSON object format"}.


opt_json_array_format(OptTag, [S | Rest]) ->
    case S of
        "list"     -> {ok, {{OptTag, list}, Rest}};
        "tl:"++Tag -> {ok, {{OptTag, {s2a(Tag),list}}, Rest}};
        _          -> {error, "Invalid JSON array format: "++S}
    end;
opt_json_array_format(_OptTag, []) ->
    {error, "Missing JSON array format"}.

opt_quote_makefile_target(list_deps_makefile_target, [S | Rest]) ->
    {ok, {{list_deps_makefile_target, {quote, S}}, Rest}};
opt_quote_makefile_target(_OptTag, []) ->
    {error, "Missing target"}.


s2a(S) -> list_to_atom(S).

determine_cmdline_op(Opts, FileNames) ->
    case {lists:member(help, Opts), lists:member(version, Opts)} of
        {true, _} -> show_help;
        {_, true} -> show_version;
        _         -> if FileNames == [] -> error;
                        FileNames /= [] -> compile
                     end
    end.

show_help(Opts) ->
    case proplists:get_value(show_usage_fn, Opts) of
        undefined ->
            show_help_c();
        Fn ->
            Fn()
    end.

show_help_c() ->
    io:format(
      "gpb version ~s~n"
      "Usage: erl <erlargs> [gpb-opts] -s ~p c <ProtoFile>.proto~n"
      "   or: erl <erlargs> -s ~p c <ProtoFile>.proto -extra [gpb-opts]~n"
      "Typical erlargs = -noshell -noinput +B -boot start_clean -pa SomeDir~n"
      "~n",
      [gpb:version_as_string(), ?MODULE, ?MODULE]),
    show_args().

show_arg({{section,Section}}) ->
    io:format("  ~s~n", [Section]);
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
    Opts2 = ensure_include_path_to_wellknown_types(Opts1),
    ImEnv = new_import_env(Opts2),
    case parse_input(In, ImEnv) of
        {ok, {Defs, Sources}} ->
            case gpb_defs:post_process_all_files(Defs, Opts2) of
                {ok, Defs2} ->
                    {ok, {Defs2, Sources}};
                {error, Reasons} ->
                    {error, {post_process, Reasons}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_input(Input, ImEnv) ->
    {Res, Sources} =
        process_each_input_once(
          fun({ok_read, {Content, Path}}, {ok, Acc}) ->
                  case parse_one_input(Path, Content, ImEnv) of
                      {ok, {Defs, Imports}} ->
                          {Imports, {ok, [Defs | Acc]}};
                      {error, Reason} ->
                          ToEnqueue = [],
                          {ToEnqueue, {error, Reason}}
                  end;
             ({error, {locate, _FileName, Reason}}, {ok, _Acc}) ->
                  %% The Reason already contains the file path, due to being
                  %% exposed also from locate_import/2.
                  ToEnqueue = [],
                  {ToEnqueue, {error, Reason}};
             ({error, {read, _Path, Reason}}, {ok, _Acc}) ->
                  %% The Reason already contains the file path, due to being
                  %% exposed also from read_import/2.  so don't wrap it again.
                  ToEnqueue = [],
                  {ToEnqueue, {error, Reason}};
             (_In, {error, Reason}) ->
                  ToEnqueue = [],
                  {ToEnqueue, {error, Reason}}
          end,
          {ok, []},
          queue:from_list([Input]),
          ImEnv),
    case Res of
        {ok, AllDefs} ->
            SourceFiles = [path_to_filename(Path, full) || Path <- Sources],
            {ok, {lists:append(lists:reverse(AllDefs)), SourceFiles}};
        {error, Reason} ->
            {error, Reason}
    end.

collect_inputs(Input, Opts) ->
    Opts1 = add_curr_dir_as_include_if_needed(Opts),
    Opts2 = ensure_include_path_to_wellknown_types(Opts1),
    ImEnv = new_import_env(Opts2),
    {{ImportPaths, Missing}, _} =
        process_each_input_once(
          fun({ok_read, {Content, Path}}, {AccImports, AccMissing}) ->
                  case parse_one_input(Path, Content, ImEnv) of
                      {ok, {_Defs, MoreImports}} ->
                          Acc1 = {[Path | AccImports], AccMissing},
                          {MoreImports, Acc1};
                      {error, _Reason} ->
                          Acc1 = {[Path | AccImports], AccMissing},
                          {[], Acc1}
                  end;
             ({error, {locate, Import, _Reason}}, {AccImports, AccMissing}) ->
                  Acc1 = {AccImports, [Import | AccMissing]},
                  {[], Acc1};
             ({error, {read, Path, _Reason}}, {AccImports, AccMissing}) ->
                  FileName = path_to_filename(Path, orig),
                  Acc1 = {AccImports, [FileName | AccMissing]},
                  {[], Acc1}
          end,
          {[], []},
          queue:from_list([Input]),
          ImEnv),
    Imports = [Orig || #path{orig=Orig} <- ImportPaths],
    {lists:reverse(Imports), lists:reverse(Missing)}.

%% Like lists:foldl, but over a queue, but process each only once
%%
%% The queue consists of paths in the form of strings, since an
%% import statement specifies a string.
%%
%% To check whether we've seen an import already, we compare full
%% paths, as located using the {i,Dir} options in order.
%%
%% This means we do not know until after trying to locate the
%% import, whether we've already seen it or not. We cannot use
%% the paths in the queue to do that.
%%
%% An import is located using only the {i,Dir} options, ie, the
%% importing .proto's dir is not used as basis for locating the
%% import. This is how protoc works, as well.
process_each_input_once(F, AccIn, Queue, ImEnv) ->
    %% First time is reading a file or string, the import_fetcher
    %% is only used for imports, so it is not used for the first locate/read.
    ImEnvFirstTime = ImEnv#import_env{importer = undefined},
    %% Iterate over the queue
    {AccOut, _N, PathsSeen} =
        process_input_queue(
          fun(Input, {Acc, I, PathsSeen}) ->
                  CurImEnv = if I == 0 -> ImEnvFirstTime;
                                I > 0  -> ImEnv
                             end,
                  CheckSeen =
                      fun(Path) -> is_input_already_seen(Path, PathsSeen) end,
                  NextI = I + 1,
                  case locate_read_input_once(Input, CheckSeen, CurImEnv) of
                      {ok_read, {Content, Path}} ->
                          QEl = {ok_read, {Content, Path}},
                          {ToEnqueue, Acc2} = F(QEl, Acc),
                          Seen2 = [Path | PathsSeen],
                          {ToEnqueue, {Acc2, NextI, Seen2}};
                      {already_seen, _Path} ->
                          ToEnqueue = [],
                          {ToEnqueue, {Acc, NextI, PathsSeen}};
                      {error, {locate, Reason}} ->
                          QEl = {error, {locate, Input, Reason}},
                          {ToEnqueue, Acc2} = F(QEl, Acc),
                          {ToEnqueue, {Acc2, NextI, PathsSeen}};
                      {error, {read, Path, Reason}} ->
                          QEl = {error, {read, Path, Reason}},
                          {ToEnqueue, Acc2} = F(QEl, Acc),
                          Seen2 = [Path | PathsSeen],
                          {ToEnqueue, {Acc2, NextI, Seen2}}
                  end
          end,
          {AccIn, 0, []},
          Queue),
    {AccOut, lists:reverse(PathsSeen)}.

is_input_already_seen(#path{full=Path}, Paths) ->
    lists:keymember(Path, #path.full, Paths).

locate_read_input_once(Input, CheckSeen, ImEnv) ->
    case locate_import_aux(Input, ImEnv) of
        {ok, Path} ->
            IsAlreadySeen = CheckSeen(Path),
            if IsAlreadySeen ->
                    {already_seen, Path};
               not IsAlreadySeen ->
                    case read_import_aux(Path, ImEnv) of
                        {ok, {Content, _Path}} ->
                            {ok_read, {Content, Path}};
                        {error, Reason} ->
                            {error, {read, Path, Reason}};
                        restart_locate_from_file ->
                            ImEnv2 = ImEnv#import_env{importer = undefined},
                            locate_read_input_once(Input, CheckSeen, ImEnv2)
                    end
            end;
        {error, Reason} ->
            {error, {locate, Reason}}
    end.

parse_one_input(Path, Content, #import_env{opts=Opts}) ->
    FName = path_to_filename(Path, orig),
    case scan_and_parse_string(Content, FName, Opts) of
        {ok, Defs} ->
            Imports = gpb_defs:fetch_imports(Defs),
            {ok, {Defs, Imports}};
        {error, Reason} ->
            {error, Reason}
    end.

%% A bit like lists:foldl, but over a queue, and
%% the F returns more elements to fold over as well as an acc.
process_input_queue(F, Acc, Queue) ->
    try queue:get(Queue) of
        Item ->
            QRest = queue:drop(Queue),
            {ToEnqueue, Acc2} = F(Item, Acc),
            Queue2 = lists:foldl(fun queue:in/2, QRest, ToEnqueue),
            process_input_queue(F, Acc2, Queue2)
    catch error:empty ->
            Acc
    end.

add_curr_dir_as_include_if_needed(Opts) ->
    ImportDirs = [Dir || {i,Dir} <- Opts],
    %% FIXME: maybe need to add "." first if not present?? Or rework
    %%        based on current dir? (if available)
    %% Change of semantics in this commit?
    %%   - now: import dir options {i,Dir}
    %%   - previously: based from current dir
    %% (re-describe the above in a more clear way??)
    case lists:member(".", ImportDirs) of
        true  -> Opts;
        false -> Opts ++ [{i,"."}]
    end.

scan_and_parse_string(S, FName, Opts) ->
    case gpb_scan:binary(unicode:characters_to_binary(S)) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens) of
                {ok, PTree} ->
                    case gpb_defs:post_process_one_file(FName, PTree, Opts) of
                        {ok, Result} ->
                            {ok, Result};
                        {error, Reason} ->
                            {error, {post_process, Reason}}
                    end;
                {error, Reasons} when is_list(Reasons) ->
                    {error, {parse_errors, FName, Reasons}}
            end;
        {error, {_Line0, _Module, _ErrInfo}=Reason, _Line1} ->
            {error, {scan_error, FName, Reason}}
    end.

%% @doc Locate an import target.  This function might be potentially
%% useful for instance in an intercepting {@link import_fetcher_fun()} that
%% just wants to record the accessed imports.
-spec locate_import(string(), opts()) -> {ok, File::string()} |
                                         {error, reason()}.
locate_import(ProtoFileName, Opts) ->
    ImEnv = new_import_env(Opts),
    case locate_import_aux(ProtoFileName, ImEnv) of
        {ok, #path{orig=Orig}} ->
            {ok, Orig};
        {error, Reason} ->
            {error, Reason}
    end.

new_import_env(Opts) ->
    Opts1 = ensure_include_path_to_wellknown_types(Opts),
    Importer = proplists:get_value(import_fetcher, Opts1),
    case file_get_cwd(Opts) of
        {ok, Cwd} ->
            ImportPaths = [absolutify_path(Path, Cwd) || {i, Path} <- Opts1],
            #import_env{opts = Opts1,
                        importer = Importer,
                        i_paths = ImportPaths,
                        cur_dir = Cwd,
                        errors = []};
        {error, Reason} ->
            {ImportPaths, Relative} =
                partition_import_opts_on_relativity(Opts),
            Errors = [{relative_but_no_cwd, RPath, Reason}
                      || RPath <- Relative],
            #import_env{opts = Opts1,
                        importer = Importer,
                        i_paths = ImportPaths,
                        cur_dir = undefined,
                        errors = Errors}
    end.


locate_import_aux({Mod, Str}, _ImEnv) ->
    Source = from_input_string,
    {ok, #path{full = Source,
               orig = Source,
               data = {Mod, Str}}};
locate_import_aux(ProtoFileName,
                  #import_env{importer=Importer, i_paths=ImportPaths}=ImEnv) ->
    if Importer == undefined ->
            %% Search the file system
            locate_import_aux2(ImportPaths, ProtoFileName, ImEnv, []);
       is_function(Importer) ->
            Source = {from_fetched, ProtoFileName},
            {ok, #path{full = Source,
                       orig = Source}}
    end.

locate_import_aux2([#path{full=Path, orig=Orig} | Rest],
                   Import, ImEnv, Tried) ->
    #import_env{opts=Opts} = ImEnv,
    File = filename:join(Path, Import),
    case lists:keymember(File, 1, Tried) of
        true ->
            %% can happen if Import is a full path
            locate_import_aux2(Rest, Import, ImEnv, Tried);
        false ->
            case file_read_file_info(File, Opts) of
                {ok, #file_info{access = A}} when A == read; A == read_write ->
                    {ok, #path{full = File,
                               orig = filename:join(Orig, Import)}};
                {ok, #file_info{}} ->
                    locate_import_aux2(Rest, Import, ImEnv, Tried);
                {error, Reason} ->
                    Tried2 = [{File,Reason} | Tried],
                    locate_import_aux2(Rest, Import, ImEnv, Tried2)
            end
    end;
locate_import_aux2([], Import, _ImEnv, Tried) ->
    {error, {import_not_found, Import, Tried}}.


%% @doc Read an import file.  This function might be potentially
%% useful for instance in an intercepting `import_fetcher' fun that
%% just wants to record the accessed imports.
-spec read_import(string(), opts()) -> {ok, string()} | {error, reason()}.
read_import(File, Opts) ->
    Path = #path{full = File,
                 orig = File},
    ImEnv = new_import_env(Opts),
    case read_import_aux(Path, ImEnv) of
        {ok, {S, _Path}} -> {ok, S};
        {error, Reason}  -> {error, Reason}
    end.

read_import_aux(#path{data={_Mod, Str}}, _ImEnv) ->
    Path = #path{full = from_input_string,
                 orig = from_input_string},
    {ok, {Str, Path}};
read_import_aux(#path{}=Path, #import_env{importer=Importer}=ImEnv) ->
    if Importer == undefined ->
            read_import_aux2(Path, ImEnv);
       is_function(Importer, 1) ->
            #path{full={from_fetched, FileName}} = Path,
            case Importer(FileName) of
                from_file ->
                    restart_locate_from_file;
                {ok, Contents} when is_list(Contents) ->
                    case lists:all(fun is_integer/1, Contents) of
                        true ->
                            {ok, {Contents, Path}};
                        false ->
                            error({bad_fetcher_return,
                                   {not_a_string, Contents},
                                   FileName})
                    end;
                {error, Reason} ->
                    {error, {fetcher_issue, FileName, Reason}};
                X ->
                    error({bad_fetcher_return, FileName, X})
            end
    end.

read_import_aux2(#path{orig=File}=Path, #import_env{opts=Opts}) ->
    case file_read_file(File, Opts) of
        {ok,B} ->
            case utf8_decode(B) of
                {ok, {utf8, S}} ->
                    {ok, {S, Path}};
                {ok, {latin1, S}} ->
                    {ok, {S, Path}};
                {error, Reason} ->
                    {error, {utf8_decode_failed, Reason, File}}
            end;
        {error, Reason} ->
            {error, {read_failed, File, Reason}}
    end.

ensure_include_path_to_wellknown_types(Opts) ->
    case proplists:get_bool(ignore_wellknown_types_directory, Opts) of
        true ->
            Opts;
        false ->
            PrivDir = get_priv_dir(),
            Wellknown = filename:join(PrivDir, "proto3"),
            sanity_check_installation_wellknown_proto3(Wellknown),
            add_opt_unless_present({i,Wellknown}, Opts)
    end.

add_opt_unless_present(Opt, [Opt | Rest]) ->
    [Opt | Rest];
add_opt_unless_present(Opt, [H | Rest]) ->
    [H | add_opt_unless_present(Opt, Rest)];
add_opt_unless_present(Opt, []) ->
    [Opt].

get_priv_dir() ->
    case locate_app_dir() of
        {ok,CurrApp} ->
            code:priv_dir(CurrApp);
        undefined ->
            %% Not loaded as an application, just executing code;
            %% from an escript possibly? (or even from an ez archive?)
            case locate_priv_dir_by_module(?MODULE) of
                {ok, PrivDir} ->
                    PrivDir;
                {error, Reason} ->
                    error({failed_to_locate_privdir,Reason})
            end
    end.

locate_app_dir() ->
    case application:get_application(?MODULE) of
        {ok,CurrApp} ->
            %% Have seen situations where there is a code path (eg via
            %% $ERL_LIBS) to another app named gpb-addon or similar.
            %% This can fool the code loader functions into wrongly
            %% thinking that the `-addon' part indicates a version
            %% number of an app named `gpb'.
            %% So do an extra safety check: this module should be located
            %% in the app's ebin dir.
            SelfBeamBase = filename:basename(code:which(?MODULE)),
            AppEbinDir = filename:join(code:lib_dir(CurrApp), "ebin"),
            case filelib:is_file(filename:join(AppEbinDir, SelfBeamBase)) of
                true ->
                    {ok, CurrApp};
                false ->
                    %% todo: Maybe we should attempt to find the true `gpb'
                    %% by searching further towards the end of the code path
                    %% but for now keep it simple(ish): rely on the fallback.
                    undefined
            end;
        undefined ->
            undefined
    end.

locate_priv_dir_by_module(Mod) ->
    case calc_priv_dir_by_module_aux(Mod) of
        {ok, PrivDir} ->
            case filelib:is_dir(PrivDir) of
                true ->
                    {ok, PrivDir};
                false ->
                    {error, {candidate_for_mod_is_no_dir, Mod, PrivDir}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

calc_priv_dir_by_module_aux(Mod) ->
    MDir = filename:dirname(code:which(Mod)),
    case filename:basename(MDir) of
        "ebin" ->
            {ok, filename:join(filename:dirname(MDir), "priv")};
        ".eunit" -> % Probably rebar2: a .eunit dir in the app's top dir
            {ok, filename:join(filename:dirname(MDir), "priv")};
        _ ->
            case code:priv_dir(gpb) of % hard-wired app name...
                Dir when is_list(Dir) ->
                    {ok, Dir};
                {error,Reason} ->
                    {error, {priv_failed_for_fallback, Reason, MDir}}
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
    gpb_lib:fold_msg_or_group_fields_skip_field_for_unknowns(
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

format_erl(Mod, Defs, DefsNoRenamings, DefsForIntrospect,
           #anres{maps_as_msgs=MapsAsMsgs,
                  dec_maps_as_msgs=DMapsAsMsgs}=AnRes,
           Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    DoNifsDirective = gpb_lib:target_has_nifs_directive(Opts),
    IncludeModHrlPrepend = proplists:get_value(include_mod_hrl_prepend, Opts,
                                               ""),
    AsLib = proplists:get_bool(include_as_lib, Opts),
    DoJson = gpb_lib:json_by_opts(Opts),
    DoMergers = gpb_lib:get_gen_mergers(Opts),
    DoVerifiers = gpb_lib:get_gen_verifiers(Opts),
    DoIntrospect = gpb_lib:get_gen_introspect(Opts),
    CompileOptsStr = get_erlc_compile_options_str(Opts),
    gpb_lib:iolist_to_utf8_or_escaped_binary(
      [?f("%% @private~n"
          "%% Automatically generated, do not edit~n"
          "%% Generated by ~p version ~s~n"
          "%% Version source: ~s~n",
          [?MODULE, gpb:version_as_string(), gpb:version_source()]),
       ?f("-module(~w).~n", [Mod]),
       case CompileOptsStr of
           ""    -> "";
           [_|_] -> ?f("-compile([~ts]).~n", [CompileOptsStr])
       end,
       "\n",
       gpb_gen_encoders:format_exports(Defs, Opts),
       gpb_gen_decoders:format_exports(Defs, Opts),
       [gpb_gen_mergers:format_exports(Defs, Opts) || DoMergers],
       [gpb_gen_verifiers:format_exports(Defs, Opts) || DoVerifiers],
       [[gpb_gen_json_encoders:format_exports(Defs, Opts),
         gpb_gen_json_decoders:format_exports(Defs, Opts)]
        || DoJson],
       [gpb_gen_introspect:format_exports(Defs, AnRes, Opts) || DoIntrospect],
       [?f("-export([descriptor/0, descriptor/1]).~n")
        || gpb_lib:get_gen_descriptor_by_opts(Opts)],
       ?f("-export([gpb_version_as_string/0, gpb_version_as_list/0]).~n"),
       ?f("-export([gpb_version_source/0]).~n"),
       "\n",
       [[[?f("-nifs([~s]).~n",
             [gpb_lib:comma_join(
                lists:append(
                  [gpb_gen_nif:format_encoder_nifs_fns(Defs, AnRes, Opts),
                   gpb_gen_nif:format_decoder_nifs_fns(Defs, AnRes, Opts)] ++
                  [gpb_gen_nif:format_to_json_nifs_fns(Defs, AnRes, Opts)
                   || DoJson] ++
                  [gpb_gen_nif:format_from_json_nifs_fns(Defs, AnRes, Opts)
                  || DoJson]))])
          || DoNifsDirective],
         "-on_load(load_nif/0).\n",
         "-export([load_nif/0]). %% for debugging of nif loading\n",
         "\n"]
        || DoNif],
       case gpb_lib:get_records_or_maps_by_opts(Opts) of
           records ->
               ?f("-include(\"~s~s.hrl\").~n", [IncludeModHrlPrepend, Mod]);
           maps ->
               ""
       end,
       case gpb_lib:get_defs_as_maps_or_records(Opts) of
           records when DoIntrospect ->
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
           records when not DoIntrospect ->
               "";
           maps ->
               ""
       end,
       "\n",
       gpb_gen_types:format_export_types(Defs, AnRes, Opts),
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
       gpb_gen_encoders:format_encoders_top_function(Defs, AnRes, Opts),
       "\n",
       if DoNif ->
               ?f("~s~n", [gpb_gen_nif:format_nif_encoder_error_wrappers(
                             Defs, AnRes, Opts)]);
          not DoNif ->
               [gpb_gen_encoders:format_msg_encoders(Defs, AnRes, Opts,
                                                     true),
                gpb_gen_encoders:format_map_encoders(MapsAsMsgs, AnRes, Opts,
                                                     false),
                gpb_gen_encoders:format_aux_encoders(Defs, AnRes, Opts),
                gpb_gen_encoders:format_aux_common_encoders(Defs, AnRes, Opts)]
       end,
       "\n",
       gpb_gen_decoders:format_decoders_top_function(Defs, AnRes, Opts),
       "\n\n",
       if DoNif ->
               [gpb_gen_nif:format_nif_decoder_error_wrappers(Defs,
                                                              AnRes, Opts)];
          not DoNif ->
               [gpb_gen_decoders:format_msg_decoders(Defs, AnRes, Opts),
                gpb_gen_decoders:format_map_decoders(DMapsAsMsgs, AnRes, Opts),
                gpb_gen_decoders:format_aux_decoders(Defs, AnRes, Opts)]
       end,
       "\n",
       [gpb_gen_mergers:format_msg_merge_code(Defs, AnRes, Opts)
        || DoMergers],
       "\n",
       [[gpb_gen_verifiers:format_verifiers_top_function(Defs, AnRes, Opts),
         "\n",
         gpb_gen_verifiers:format_verifiers(Defs, AnRes, Opts)]
        || DoVerifiers],
       "\n",
       if not DoNif ->
               [gpb_gen_translators:format_aux_transl_helpers(),
                gpb_gen_translators:format_translators(Defs, AnRes, Opts)];
          DoNif ->
               [gpb_gen_translators:format_aux_transl_helpers(),
                gpb_gen_translators:format_merge_translators(Defs, AnRes,
                                                             Opts)]
       end,
       "\n",
       [[gpb_gen_json_encoders:format_top_function(Defs, AnRes, Opts),
         if DoNif ->
                [gpb_gen_nif:format_nif_to_json_error_wrappers(
                   Defs, AnRes, Opts)];
           not DoNif ->
                [gpb_gen_json_encoders:format_encoders(Defs, AnRes, Opts)]
         end,
         gpb_gen_json_decoders:format_top_function(Defs, AnRes, Opts),
         if DoNif ->
                 [gpb_gen_nif:format_nif_from_json_error_wrappers(
                    Defs, AnRes, Opts)];
            not DoNif ->
                 [gpb_gen_json_decoders:format_decoders(Defs, AnRes, Opts)]
         end]
        || DoJson],
       "\n",
       [gpb_gen_introspect:format_introspection(DefsForIntrospect, AnRes, Opts)
        || DoIntrospect],
       "\n",
       possibly_format_descriptor(DefsNoRenamings, Opts),
       "\n",
       ?f("gpb_version_as_string() ->~n"),
       ?f("    \"~s\".~n", [gpb:version_as_string()]),
       "\n",
       ?f("gpb_version_as_list() ->~n"),
       ?f("    ~s.~n", [gpb_version_as_list_pretty()]),
       "\n",
       ?f("gpb_version_source() ->~n"),
       ?f("    ~p.~n", [gpb:version_source()])],
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
            try gpb_compile_descr:encode_defs_to_descriptors(Defs, Opts) of
                {Bin, PBins} when is_binary(Bin), is_list(PBins) ->
                    [gpb_codegen:format_fn(
                       descriptor, fun() -> 'bin' end,
                       [replace_term(bin, Bin)]),
                     ["-spec descriptor(_) -> no_return().\n" || PBins == []],
                     gpb_codegen:format_fn(
                       descriptor,
                       fun('"base"') -> '<<PBin>>';
                          (X) -> error({gpb_error, {badname, X}})
                       end,
                       [repeat_clauses(
                          '"base"',
                          [[replace_term('"base"', ProtoBase),
                            replace_term('<<PBin>>', PBin)]
                           || {ProtoBase, PBin} <- PBins])])]
            catch ?STACKTRACE(error,undef,ST) % ->
                    case {element(1,hd(ST)), element(2,hd(ST))} of
                        {gpb_compile_descr, encode_defs_to_descriptors} ->
                            ["-spec descriptor() -> no_return().\n",
                             gpb_codegen:format_fn(
                               descriptor,
                               fun() -> erlang:error(descr_not_avail) end),
                             "-spec descriptor(_) -> no_return().\n",
                             gpb_codegen:format_fn(
                               descriptor,
                               fun(_) -> erlang:error(descr_not_avail) end)];
                        _ ->
                            %% other error
                            erlang:raise(error, undef, ST)
                    end
            end;
        false ->
            ""
    end.

%% -- hrl -----------------------------------------------------

possibly_format_hrl(Mod, Defs, AnRes, Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records -> format_hrl(Mod, Defs, AnRes, Opts);
        maps    -> '$not_generated'
    end.

format_hrl(Mod, Defs, AnRes, Opts1) ->
    Opts = [{module, Mod}|Opts1],
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
         [gpb_gen_types:format_msg_record(Msg, Fields, AnRes, Opts, Defs)
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
    ErlCode2 = nano_epp(ErlCode, ModAsStr, HrlText, Opts),
    {ok, Toks, _EndLine} = erl_scan:string(ErlCode2),
    FormToks = split_toks_at_dot(Toks),
    Forms = [case erl_parse:parse_form(Ts) of
                 {ok, Form} ->
                     Form;
                 {error, Reason} ->
                     erlang:error(
                       {internal_error,?MODULE,Mod,Ts,Reason,
                        {more_info,[{full_erl,ErlCode2},{hrl,HrlText},
                                    {nif,PossibleNifCode},{opts,Opts}]}})
             end
             || Ts <- FormToks],
    combine_erl_and_possible_nif(compile:noenv_forms(Forms, Opts),
                                 PossibleNifCode).

-record(nepp, %% nano-epp state
        {depth, %% for ifdef/else/endif processing
         mod, %% ModAsStr,
         hrl,
         defs}).

nano_epp(Code, ModAsStr, HrlText, Opts) ->
    %% nepp = nano-erlang-preprocessor. Couldn't find a way to run
    %% the epp from a string, and don't want or need to use the file
    %% system when everything is already in memory.

    %% Setup a dictionary, mostly to handle -ifdef...-endif
    %% in hrls and in the decoders.
    %% The OTP_RELEASE first appeared in Erlang 21.
    D0 = dict:new(),
    OtpRelease = gpb_lib:current_otp_release(),
    TargetOtpRelease = proplists:get_value(target_erlang_version, Opts,
                                           OtpRelease),
    D1 = if TargetOtpRelease >= 21 ->
                 dict:store('OTP_RELEASE', OtpRelease, D0);
            TargetOtpRelease < 21 ->
                 D0
         end,
    NState = #nepp{depth=1, mod=ModAsStr, hrl=HrlText, defs=D1},
    {Txt, <<>>, _EndNState, _EndLine} = nepp1(Code, NState, _Line=1, []),
    Txt.

nepp1(<<"%% -*- coding:",_/binary>>=B, #nepp{mod=ModAsStr}=NState, N, Acc) ->
    %% First (non-coding) line must be a -file(...) directive,
    %% or else unused record definitions in included files will
    %% produce warnings: eg: {27,erl_lint,{unused_record,gpb_oneof}}.
    {CodingLine,Rest} = read_until(B, "\n", ""),
    Erl = (ModAsStr -- "''") ++ ".erl",
    CodingAndFileDirective = CodingLine ++ "\n" ++ file_directive(Erl, 1),
    Acc2 = lists:reverse(CodingAndFileDirective, Acc),
    nepp2_nl(Rest, NState, N, Acc2);
nepp1(Rest, #nepp{mod=ModAsStr}=NState, N, Acc) ->
    Erl = (ModAsStr -- "''") ++ ".erl",
    FileDirective = file_directive(Erl, 1),
    Acc2 = lists:reverse(FileDirective, Acc),
    nepp2_nl(Rest, NState, N, Acc2).

nepp2(<<"?MODULE", Rest/binary>>, #nepp{mod=ModAsStr}=NState, N, Acc) ->
    nepp2(Rest, NState, N, lists:reverse(ModAsStr, Acc));
nepp2(<<$\n, Rest/binary>>, NState, N, Acc) ->
    nepp2_nl(Rest, NState, N+1, [$\n | Acc]);
nepp2(<<C, Rest/binary>>, NState, N, Acc) ->
    nepp2(Rest, NState, N, [C | Acc]);
nepp2(<<>>, NState, N, Acc) ->
    {lists:reverse(Acc), <<>>, NState, N}.

%% collect and handle pre-processor directives
nepp2_nl(<<"-include", Rest/binary>>, NState, N, Acc) ->
    nepp2_inc(Rest,NState, N, Acc);
nepp2_nl(<<"-include_lib", Rest/binary>>, NState, N, Acc) ->
    nepp2_inc(Rest, NState, N, Acc);
nepp2_nl(<<"-define", Rest/binary>>, NState, N, Acc) ->
    nepp2_def(Rest, NState, N, Acc);
nepp2_nl(<<"-ifdef", Rest/binary>>, NState, N, Acc) ->
    nepp2_ifdef(Rest, ifdef, NState, N, Acc);
nepp2_nl(<<"-ifndef", Rest/binary>>, NState, N, Acc) ->
    nepp2_ifdef(Rest, ifndef, NState, N, Acc);
nepp2_nl(<<"-if", Rest/binary>>, NState, N, Acc) ->
    nepp2_if(Rest, NState, N, Acc);
nepp2_nl(<<"-else.\n", Rest/binary>>, #nepp{depth=1}=NState, N, Acc) ->
    nepp2_skip(Rest, NState, N+1, Acc);
nepp2_nl(<<"-endif.\n", Rest/binary>>, #nepp{depth=1}=NState, N, Acc) ->
    {lists:reverse(Acc), Rest, NState, N+1};
nepp2_nl(X, NState, N, Acc) ->
    nepp2(X, NState, N, Acc).

nepp2_inc(Rest, #nepp{mod=ModAsStr, hrl=Hrl}=NState, N, Acc) ->
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
            nepp2_nl(Rest3, NState, N+1, Acc2);
        mod_hrl when Hrl /= '$not_generated' ->
            {Txt1, <<>>, NState2, _EndLine} = nepp2_nl(Hrl, NState, 1, []),
            Txt2 = lists:flatten([file_directive(Inc, 1), Txt1]),
            Acc2 = lists:reverse(Txt2 ++ file_directive(Erl, N+1), Acc),
            nepp2_nl(Rest3, NState2, N+1, Acc2)
    end.

nepp2_def(Rest, #nepp{defs=Ds}=NState, N, Acc) ->
    {_,   Rest1} = read_until(Rest,  "(", ""),
    {Sym, Rest2} = read_until(Rest1, ",", ""),
    {Val, Rest3} = read_until(Rest2, ")", ""),
    {_,   Rest4} = read_until(Rest3, "\n", ""),
    Ds1 = dict:store(parse_term(Sym), parse_term(Val), Ds),
    nepp2_nl(Rest4, NState#nepp{defs=Ds1}, N+1, Acc).

nepp2_ifdef(Rest, SkipCond, #nepp{depth=Depth, defs=Ds}=NState, N, Acc) ->
    {_,   Rest1} = read_until(Rest,  "(", ""),
    {Sym, Rest2} = read_until(Rest1, ")", ""),
    {_,   Rest3} = read_until(Rest2, "\n", ""),
    {Txt, Rest4, NState2, N2} =
        case {dict:is_key(parse_term(Sym), Ds), SkipCond} of
            {true,  ifdef}  -> nepp2_nl(Rest3, NState#nepp{depth=1}, N+1, []);
            {false, ifndef} -> nepp2_nl(Rest3, NState#nepp{depth=1}, N+1, []);
            _ -> nepp2_skip(Rest3, NState#nepp{depth=1}, N+1, [])
        end,
    nepp2_nl(Rest4, NState2#nepp{depth=Depth}, N2, lists:reverse(Txt, Acc)).

nepp2_if(Rest, #nepp{depth=Depth, defs=Ds}=NState, N, Acc) ->
    {_,    Rest1} = read_until(Rest,  "(", ""),
    {Cond, Rest2} = read_until(Rest1, ")", ""),
    {_,    Rest3} = read_until(Rest2, "\n", ""),
    {Txt,  Rest4, NState2, N2} =
        case nepp2_eval_cond(Cond, Ds) of
            true  -> nepp2_nl(Rest3, NState#nepp{depth=1}, N+1, []);
            false -> nepp2_skip(Rest3, NState#nepp{depth=1}, N+1, [])
        end,
    nepp2_nl(Rest4, NState2#nepp{depth=Depth}, N2, lists:reverse(Txt, Acc)).

nepp2_skip(<<"-else.\n", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    if Depth == 1 -> nepp2_nl(Rest, NState, N+1, Acc);
       Depth >  1 -> nepp2_skip(Rest, NState, N+1, Acc)
    end;
nepp2_skip(<<"-endif.\n", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    if Depth == 1 -> {lists:reverse(Acc), Rest, NState, N+1};
       Depth >  1 -> nepp2_skip(Rest, NState#nepp{depth=Depth-1}, N+1, Acc)
    end;
nepp2_skip(<<"-ifdef", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    {_, Rest2} = read_until(Rest, "\n", ""),
    nepp2_skip(Rest2, NState#nepp{depth=Depth+1}, N+1, Acc);
nepp2_skip(<<"-ifndef", Rest/binary>>, #nepp{depth=Depth}=NState, N, Acc) ->
    {_, Rest2} = read_until(Rest, "\n", ""),
    nepp2_skip(Rest2, NState#nepp{depth=Depth+1}, N+1, Acc);
nepp2_skip(<<$\n, Rest/binary>>, NState, N, Acc) ->
    nepp2_skip(Rest, NState, N+1, Acc);
nepp2_skip(<<_, Rest/binary>>, NState, N, Acc) ->
    nepp2_skip(Rest, NState, N, Acc).

nepp2_eval_cond(Str, Ds) ->
    {ok, Tokens, End} = erl_scan:string(Str),
    Tokens2 = nepp2_simple_expand(Tokens, Ds),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens2 ++ [{dot, End}]),
    InitBinds = erl_eval:new_bindings(),
    {value, Result, _Binds} = erl_eval:exprs(Exprs, InitBinds),
    Result.

nepp2_simple_expand([{'?', _}, {var, _, Sym} | Rest], Ds) ->
    nepp2_assert_not_parameterized(Sym, Rest),
    Val = dict:fetch(Sym, Ds),
    [erl_parse:abstract(Val) | nepp2_simple_expand(Rest, Ds)];
nepp2_simple_expand([Tok | Rest], Ds) ->
    [Tok | nepp2_simple_expand(Rest, Ds)];
nepp2_simple_expand([], _Ds) ->
    [].

nepp2_assert_not_parameterized(Sym, [{'(', _} | _]) ->
    error({not_implemeted, nepp2, parameterized_macros, Sym});
nepp2_assert_not_parameterized(_Sym, _) ->
    ok.

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

file_get_cwd(Opts) ->
    file_op(get_cwd, [], Opts).

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

partition_import_opts_on_relativity(Opts) ->
    {Paths, Relatives} =
        lists:foldl(
          fun({i, Path}, {Ps, Rs}) ->
                  case filename:pathtype(Path) of
                      relative       -> {Ps, [Path | Rs]};
                      volumerelative -> {Ps, [Path | Rs]};
                      absolute ->
                          P = #path{full = Path,
                                    orig = Path},
                          {[P | Ps], Rs}
                  end;
             (_OtherOpt, Acc) ->
                  Acc
          end,
          {[], []},
          Opts),
    {lists:reverse(Paths), lists:reverse(Relatives)}.

absolutify_path(Path, Cwd) ->
    #path{full = normalize_path(filename:join(Cwd, Path)),
          orig = Path}.

normalize_path(P) ->
    filename:join(norm_comp(filename:split(P), [])).

norm_comp([".." | Rest], [Top])     -> norm_comp(Rest, [Top]);
norm_comp([".." | Rest], [_ | Par]) -> norm_comp(Rest, Par);
norm_comp(["." | Rest], Acc)        -> norm_comp(Rest, Acc);
norm_comp([Elem | Rest], Acc)       -> norm_comp(Rest, [Elem | Acc]);
norm_comp([], Acc)                  -> lists:reverse(Acc).

path_to_filename(Path, OrigOrFull) ->
    case OrigOrFull of
        orig ->
            case Path of
                #path{data = {Mod, _Src},
                      orig = from_input_string} ->
                    atom_to_list(Mod) ++ ".proto";
                #path{orig={from_fetched, ImportFileName}} ->
                    ImportFileName;
                #path{orig=OrigFile} ->
                    OrigFile
            end;
        full ->
            case Path of
                #path{data = {Mod, _Src},
                      full = from_input_string} ->
                    atom_to_list(Mod) ++ ".proto";
                #path{full={from_fetched, ImportFileName}} ->
                    ImportFileName;
                #path{full=OrigFile} ->
                    OrigFile
            end
    end.
