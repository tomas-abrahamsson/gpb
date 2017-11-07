%% This line tells emacs to use -*- erlang -*- mode for this file

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

Nonterminals
        proto
        syntax_def
        elements element
        enum_def enum_fields enum_field
        opt_enum_opts enum_opts enum_opt
        message_def msg_elems msg_elem
        opt_field_opts field_opts field_opt occurrence type
        map_type map_key_type
        package_def
        import_def
        identifiers
        option_name_ident
        extend_def extensions_def exts ext
        reserved_def res_numbers res_number res_names
        oneof_def oneof_elems oneof_elem
        option_def
        group_def
        service_def rpc_defs rpc_def rpc_arg rpc_ret m_opts
        name
        option_name
        constant
        integer
        string_expr
        fidentifier
        .

Terminals
        package
        message enum
        required optional repeated
        double float int32 int64 uint32
        uint64 sint32 sint64 fixed32 fixed64
        sfixed32 sfixed64 bool string bytes map
        identifier str_lit dec_lit oct_lit hex_lit float_lit bool_lit
        default
        import
        option
        extensions extend max to reserved
        oneof
        group
        service rpc returns stream
        packed deprecated
        syntax
        '.' ';' '(' ')' '{' '}' '[' ']' '=' ',' '<' '>'
        .

Rootsymbol
        proto.

Endsymbol
        '$end'.


%% TODO: implement verification of references
%% TODO: implement (custom) options: allowed everywhere

proto -> elements:                      '$1'.
proto -> syntax_def elements:           ['$1' | '$2'].

syntax_def -> syntax '=' str_lit ';':   verify_syntax('$3').

elements -> element elements:           ['$1' | '$2'].
elements -> ';' elements:               '$2'.
elements -> '$empty':                   [].

element -> package_def:                 '$1'.
element -> import_def:                  '$1'.
element -> enum_def:                    '$1'.
element -> message_def:                 '$1'.
element -> extend_def:                  '$1'.
element -> option_def:                  '$1'.
element -> service_def:                 '$1'.

package_def -> package name ';':        {package, '$2'}.

name -> '.' identifiers:                ['.' | '$2'].
name -> identifiers:                    '$1'.

option_name_ident -> identifier:        [identifier_name('$1')].
option_name_ident -> '(' name ')':      '$2'.

option_name -> option_name_ident:           '$1'.
option_name -> option_name_ident '.' name:  '$1' ++ '$3'.

identifiers -> identifier '.' identifiers:      [identifier_name('$1'), '.'
                                                 | '$3'].
identifiers -> identifier:                      [identifier_name('$1')].

import_def -> import str_lit ';':       {import, literal_value('$2')}.

option_def -> option option_name '=' constant: {option, '$2', '$4'}.

enum_def -> enum fidentifier '{' enum_fields '}':
                                        {{enum,identifier_name('$2')},'$4'}.

enum_fields -> enum_field enum_fields:  ['$1' | '$2'].
enum_fields -> option_def enum_fields:  ['$1' | '$2'].
enum_fields -> ';' enum_fields:         '$2'.
enum_fields -> '$empty':                [].

enum_field -> fidentifier '=' integer ';':
                                        {identifier_name('$1'), '$3'}.
enum_field -> fidentifier '=' integer '[' opt_enum_opts ']' ';':
                                        {identifier_name('$1'), '$3'}.

opt_enum_opts -> enum_opts:             '$1'.
opt_enum_opts -> '$empty':              [].

enum_opts -> enum_opt ',' enum_opts:    ['$1' | '$2'].
enum_opts -> enum_opt:                  ['$1'].

enum_opt -> name '=' constant:          {'$1', '$3'}.


message_def -> message fidentifier '{' msg_elems '}':
                                        {{msg,identifier_name('$2')},'$4'}.

msg_elems -> msg_elem msg_elems:        ['$1' | '$2'].
msg_elems -> ';' msg_elems:             '$2'.
msg_elems -> '$empty':                  [].

msg_elem -> occurrence type fidentifier '=' dec_lit ';':
                                        #?gpb_field{occurrence='$1',
                                                    type='$2',
                                                    name=identifier_name('$3'),
                                                    fnum=literal_value('$5'),
                                                    opts=[]}.
msg_elem -> occurrence type fidentifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #?gpb_field{occurrence='$1',
                                                    type='$2',
                                                    name=identifier_name('$3'),
                                                    fnum=literal_value('$5'),
                                                    opts='$7'}.
msg_elem -> type fidentifier '=' dec_lit ';': % proto3
                                        #?gpb_field{occurrence=optional,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts=[]}.
msg_elem -> type fidentifier '=' dec_lit '[' opt_field_opts ']' ';': % proto3
                                        #?gpb_field{occurrence=optional,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts='$6'}.
msg_elem -> map_type fidentifier '=' dec_lit ';':
                                        #?gpb_field{occurrence=repeated,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4')}.
msg_elem -> map_type fidentifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #?gpb_field{occurrence=repeated,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts='$6'}.

msg_elem -> message_def:                '$1'.
msg_elem -> enum_def:                   '$1'.
msg_elem -> extensions_def:             {extensions,lists:sort('$1')}.
msg_elem -> oneof_def:                  '$1'.
msg_elem -> extend_def:                 '$1'.
msg_elem -> reserved_def:               '$1'.
msg_elem -> group_def:                  '$1'.
msg_elem -> option_def:                 '$1'.

fidentifier -> identifier:              '$1'.
fidentifier -> package:                 kw_to_identifier('$1').
fidentifier -> service:                 kw_to_identifier('$1').
fidentifier -> enum:                    kw_to_identifier('$1').
fidentifier -> message:                 kw_to_identifier('$1').
fidentifier -> required:                kw_to_identifier('$1').
fidentifier -> optional:                kw_to_identifier('$1').
fidentifier -> repeated:                kw_to_identifier('$1').
fidentifier -> double:                  kw_to_identifier('$1').
fidentifier -> 'float':                 kw_to_identifier('$1').
fidentifier -> int32:                   kw_to_identifier('$1').
fidentifier -> int64:                   kw_to_identifier('$1').
fidentifier -> uint32:                  kw_to_identifier('$1').
fidentifier -> uint64:                  kw_to_identifier('$1').
fidentifier -> sint32:                  kw_to_identifier('$1').
fidentifier -> sint64:                  kw_to_identifier('$1').
fidentifier -> fixed32:                 kw_to_identifier('$1').
fidentifier -> fixed64:                 kw_to_identifier('$1').
fidentifier -> sfixed32:                kw_to_identifier('$1').
fidentifier -> sfixed64:                kw_to_identifier('$1').
fidentifier -> bool:                    kw_to_identifier('$1').
fidentifier -> string:                  kw_to_identifier('$1').
fidentifier -> bytes:                   kw_to_identifier('$1').
fidentifier -> bool_lit:                kw_to_identifier(literal_value('$1')).
fidentifier -> default:                 kw_to_identifier('$1').
fidentifier -> import:                  kw_to_identifier('$1').
fidentifier -> option:                  kw_to_identifier('$1').
fidentifier -> extensions:              kw_to_identifier('$1').
fidentifier -> extend:                  kw_to_identifier('$1').
fidentifier -> max:                     kw_to_identifier('$1').
fidentifier -> to:                      kw_to_identifier('$1').
fidentifier -> rpc:                     kw_to_identifier('$1').
fidentifier -> returns:                 kw_to_identifier('$1').
fidentifier -> stream:                  kw_to_identifier('$1').
fidentifier -> packed:                  kw_to_identifier('$1').
fidentifier -> deprecated:              kw_to_identifier('$1').
fidentifier -> syntax:                  kw_to_identifier('$1').
fidentifier -> map:                     kw_to_identifier('$1').
fidentifier -> reserved:                kw_to_identifier('$1').
fidentifier -> group:                   kw_to_identifier('$1').

opt_field_opts -> field_opts:           '$1'.
opt_field_opts -> '$empty':             [].


field_opts -> field_opt ',' field_opts: ['$1' | '$3'].
field_opts -> field_opt:                ['$1'].


field_opt -> default '=' constant:      {default, '$3'}.
field_opt -> packed:                    {packed, true}.
field_opt -> packed '=' bool_lit:       {packed, literal_value('$3')}.
field_opt -> deprecated:                {deprecated, true}.
field_opt -> deprecated '=' bool_lit:   {deprecated, literal_value('$3')}.
field_opt -> option_name:               {'$1', true}.
field_opt -> option_name '=' constant:  {'$1', '$3'}.

occurrence -> required:                 required.
occurrence -> optional:                 optional.
occurrence -> repeated:                 repeated.

type -> double:                         double.
type -> float:                          float.
type -> int32:                          int32.
type -> int64:                          int64.
type -> uint32:                         uint32.
type -> uint64:                         uint64.
type -> sint32:                         sint32.
type -> sint64:                         sint64.
type -> fixed32:                        fixed32.
type -> fixed64:                        fixed64.
type -> sfixed32:                       sfixed32.
type -> sfixed64:                       sfixed64.
type -> bool:                           bool.
type -> string:                         string.
type -> bytes:                          bytes.
type -> name:                           {ref, '$1'}.

map_type -> map '<' map_key_type ',' type '>': {map,'$3','$5'}.

map_key_type -> int32:                  int32.
map_key_type -> int64:                  int64.
map_key_type -> uint32:                 uint32.
map_key_type -> uint64:                 uint64.
map_key_type -> sint32:                 sint32.
map_key_type -> sint64:                 sint64.
map_key_type -> fixed32:                fixed32.
map_key_type -> fixed64:                fixed64.
map_key_type -> sfixed32:               sfixed32.
map_key_type -> sfixed64:               sfixed64.
map_key_type -> bool:                   bool.
map_key_type -> string:                 string.
%% missing from type: double | float | bytes | message name | enum name

group_def -> occurrence group fidentifier '=' dec_lit '{' msg_elems '}':
                 begin
                     TmpGName = identifier_name('$3'),
                     {group1,TmpGName,'$7',
                      #?gpb_field{occurrence='$1',
                                  type={ref,['...expanded-later']},
                                  name=identifier_name('$3'),
                                  fnum=literal_value('$5'),
                                  opts=[]}}
                 end.

constant -> identifier:                 identifier_name('$1').
constant -> integer:                    '$1'.
constant -> float_lit:                  literal_value('$1').
constant -> string_expr:                '$1'.
constant -> bool_lit:                   literal_value('$1').

integer -> dec_lit:                     literal_value('$1').
integer -> oct_lit:                     literal_value('$1').
integer -> hex_lit:                     literal_value('$1').

%% the protoc parser sports a c[++] style string concatenation feature
string_expr -> str_lit string_expr:     literal_value('$1') ++ '$2'.
string_expr -> str_lit:                 literal_value('$1').

extensions_def -> extensions exts ';':  '$2'.

exts -> ext ',' exts:                   ['$1' | '$3'].
exts -> ext:                            ['$1'].

ext -> integer:                         {'$1','$1'}.
ext -> integer to integer:              {'$1','$3'}.
ext -> integer to max:                  {'$1',max}.

reserved_def -> reserved res_numbers:   {reserved_numbers,'$2'}.
reserved_def -> reserved res_names:     {reserved_names,'$2'}.

res_numbers -> res_number ',' res_numbers: ['$1' | '$3'].
res_numbers -> res_number:                 ['$1'].

res_number -> integer:                  '$1'.
res_number -> integer to integer:       {'$1','$3'}.

res_names -> string_expr ',' res_names: ['$1' | '$3'].
res_names -> string_expr:               ['$1'].

oneof_def -> 'oneof' fidentifier '{' oneof_elems '}':
                                        #gpb_oneof{name=identifier_name('$2'),
                                                   fields='$4'}.

oneof_elems -> oneof_elem oneof_elems:  ['$1' | '$2'].
oneof_elems -> oneof_elem:              ['$1'].

oneof_elem -> type fidentifier '=' dec_lit ';':
                                        #?gpb_field{occurrence=optional,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts=[]}.
oneof_elem -> type fidentifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #?gpb_field{occurrence=optional,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts='$6'}.

extend_def -> extend name '{' msg_elems '}':
                                        {{extend,{eref1,'$2'}},'$4'}.


service_def -> service fidentifier '{' rpc_defs '}':
                                        {{service,identifier_name('$2')},'$4'}.

rpc_defs -> rpc_def rpc_defs:           ['$1' | '$2'].
rpc_defs -> ';' rpc_defs:               '$2'.
rpc_defs -> '$empty':                   [].

rpc_def -> rpc fidentifier rpc_arg returns rpc_ret ';':
                                        {identifier_name('$2'), '$3','$5',[]}.
rpc_def -> rpc fidentifier rpc_arg returns rpc_ret '{' m_opts '}':
                                        {identifier_name('$2'), '$3','$5','$7'}.

rpc_arg -> '(' name ')':                {'$2', false}.
rpc_arg -> '(' stream name ')':         {'$3', true}.

rpc_ret -> '(' name ')':                {'$2', false}.
rpc_ret -> '(' stream name ')':         {'$3', true}.

m_opts -> option_def ';' m_opts:        ['$1' | '$3'].
m_opts -> ';' m_opts:                   '$2'.
m_opts -> '$empty':                     [].



Header
"%%% @doc The yecc grammar for the protobuf language,"
"%%% both for syntax = proto2 and for proto3."
"%%% @private"
"".

Erlang code.

-include("../include/gpb.hrl").

-export([post_process_one_file/3]).
-export([post_process_all_files/2]).
-export([format_post_process_error/1]).
-export([fetch_imports/1]).

-type defs() :: [def()].
-type def() :: {{msg, Name::atom()}, [field()]} |
               {{group, Name::atom()}, [field()]} |
               {{enum, Name::atom()}, [{Sym::atom(), Value::integer()}]} |
               {{service, Name::atom()}, [#?gpb_rpc{}]} |
               {package, Name::atom()} |
               {syntax, string()} | % "proto2" | "proto3"
               {{extensions, MsgName::atom()}, [field_number_extension()]} |
               {{extend, MsgName::atom()}, MoreFields::[field()]} |
               {proto3_msgs, [MsgName::atom()]} |
               {{msg_containment, ProtoName::string()},[MsgName::atom()]} |
               {{reserved_numbers, MsgName::atom()}, [integer()]} |
               {{reserved_names, MsgName::atom()}, [FieldName::atom()]} |
               {import, ProtoFile::string()} |
               {{msg_options, MsgName::atom()}, [msg_option()]} |
               {{msg_containment, ProtoName::string()}, MsgNames::[atom()]}.
-type field() :: #?gpb_field{} | #gpb_oneof{}.
-type field_number_extension() :: {Lower::integer(), Upper::integer() | max}.
-type msg_option() :: {[NameComponent::atom()], OptionValue::term()}.

-export_type([defs/0, def/0]).
-export_type([field/0]).


verify_syntax({str_lit, _Line, "proto2"}) ->
    {syntax, "proto2"};
verify_syntax({str_lit, _Line, "proto3"}) ->
    {syntax, "proto3"};
verify_syntax({str_lit, Line, "proto"++_ = Unsupported}) ->
    return_error(Line, "Unsupported proto version: " ++ Unsupported);
verify_syntax({str_lit, Line, Unsupported}) ->
    return_error(Line, "Unsupported proto syntax: " ++ Unsupported).

identifier_name({identifier, _Line, Name}) -> list_to_atom(Name).

kw_to_identifier({Kw, Line}) ->
    {identifier, Line, atom_to_list(Kw)}.

literal_value({_TokenType, _Line, Value}) -> Value.

post_process_one_file(FileName, Defs, Opts) ->
    case find_package_def(Defs, Opts) of
        {ok, Package} ->
            Defs1 = handle_proto_syntax_version_one_file(
                      join_any_msg_options(
                        convert_default_values(
                          flatten_qualify_defnames(Defs, Package)))),
            FileExt = filename:extension(FileName),
            ProtoName = filename:basename(FileName, FileExt),
            MsgContainment = {{msg_containment, ProtoName},
                              lists:sort(gpb_lib:msg_names(Defs1))},
            {ok, [MsgContainment | Defs1]};
        {error, Reasons} ->
            {error, Reasons}
    end.

post_process_all_files(Defs, Opts) ->
    case resolve_names(Defs) of
        {ok, Defs2} ->
            {ok, normalize_msg_field_options(
                   handle_proto_syntax_version_all_files(
                     possibly_prefix_suffix_msgs(
                       enumerate_msg_fields(
                         reformat_names(
                           extend_msgs(Defs2))),
                       Opts)))};
        {error, Reasons} ->
            {error, Reasons}
    end.

%% -> {ok, Defs} | {error, [Reason]}
resolve_names(Defs) ->
    case resolve_refs(Defs) of
        {ok, RDefs} ->
            case verify_defs(RDefs) of
                ok ->
                    {ok, RDefs};
                {error, Reasons} ->
                    {error, Reasons}
            end;
        {error, Reasons} ->
            {error, Reasons}
    end.

%% Find any package specifier. At most one such package specifier
%% may exist, and it can exist anywhere (top-level) in the proto file,
%% yet it still applies to the whole file.
find_package_def(Defs, Opts) ->
    case proplists:get_bool(use_packages, Opts) of
        true ->
            case [Pkg || {package, Pkg} <- Defs] of
                [] ->
                    {ok, empty_pkg_root()};
                [Pkg] ->
                    {ok, ['.' | Pkg]};
                Pkgs when length(Pkgs) >= 2 ->
                    PrettyPkgs = [reformat_name(Pkg) || Pkg <- Pkgs],
                    {error, [{multiple_pkg_specifiers, PrettyPkgs}]}
            end;
        false ->
            {ok, empty_pkg_root()}
    end.

empty_pkg_root() ->
    ['.'].

%% For nested message definitions such as
%% ```
%%    message m1 {
%%      required uint32 f1 = 1;
%%      message m2 { ... }
%%      enum e2 { ... }
%%    };",
%% '''
%% the parser will produce a nested structure, such as:
%% ```
%%   [{{msg,M1},[#field{},
%%               {{msg,M2}, [...]},
%%               {{enum,E2}, [...]}]}]
%% '''
%% Flattening means to lift the nested m2 and e2 definition to the top-level,
%% so the above turns into:
%% ```
%%   [{{msg,M1},[#field{}]},
%%    {{msg,M2}, [...]},
%%    {{enum,E2}, [...]}]
%% '''
%%
%% During this process, the message and enum names and similar get
%% fully qualified into absolute rooted name-paths. In the example
%% above, this applies to m1, m2 and e2. Note that at this stage,
%% nothing is done to resolve reference to names, such as message
%% types for fields. A name-path is a list of path components,
%% separated by the dot-atom, '.', and an absolute rooted name-path is
%% a path that begins with the dot-atom, '.', much like a slash or a
%% backslash in a file name path.
flatten_qualify_defnames(Defs, Root) ->
    lists:reverse(
      lists:foldl(
        fun({{msg,Name}, FieldsOrDefs}, Acc) ->
                FullName = prepend_path(Root, Name),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, FullName),
                [{{msg,FullName},Fields2} | Defs2] ++ Acc;
           ({{group,FullName}, FieldsOrDefs}, Acc) ->
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, FullName),
                [{{group,FullName},Fields2} | Defs2] ++ Acc;
           ({{enum,Name}, ENs}, Acc) ->
                FullName = prepend_path(Root, Name),
                [{{enum,FullName}, ENs} | Acc];
           ({extensions,Exts}, Acc) ->
                [{{extensions,Root},Exts} | Acc];
           ({{extend,{eref1,Name}}, FieldsOrDefs}, Acc) ->
                FullNameCandidates =
                    compute_roots(prepend_path(Root, Name)) ++
                    compute_roots(prepend_path(empty_pkg_root(), Name)),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, Root),
                [{{extend,{eref2,Root,FullNameCandidates}},Fields2} | Defs2] ++
                    Acc;
           ({{service, Name}, RPCs}, Acc) ->
                FullName = prepend_path(Root, Name),
                [{{service,FullName}, RPCs} | Acc];
           (OtherElem, Acc) ->
                [OtherElem | Acc]
        end,
        [],
        Defs)).

flatten_fields(FieldsOrDefs, FullName) ->
    {RFields2, Defs2} =
        lists:foldl(
          fun(#?gpb_field{}=F, {Fs,Ds}) ->
                  {[F | Fs], Ds};
             (#gpb_oneof{}=O, {Fs,Ds}) ->
                  {[O | Fs], Ds};
             ({group1,TmpGName,GFields,MField}, {Fs,Ds}) ->
                  FullGroupName = prepend_path(FullName, TmpGName),
                  Group0 = {{group,FullGroupName}, GFields},
                  QDefs = flatten_qualify_defnames([Group0], FullGroupName),
                  MField1 = MField#?gpb_field{type={ref,FullGroupName}},
                  {[MField1 | Fs], QDefs++Ds};
             ({{extend, _Ref},_}=Def, {Fs,Ds}) ->
                  QDefs = flatten_qualify_defnames([Def], FullName),
                  {Fs, QDefs ++ Ds};
             ({reserved_numbers, Ns}, {Fs,Ds}) ->
                  Def = {{reserved_numbers,FullName}, Ns},
                  {Fs, [Def | Ds]};
             ({reserved_names, Ns}, {Fs,Ds}) ->
                  Def = {{reserved_names,FullName}, Ns},
                  {Fs, [Def | Ds]};
             ({option,OptName,OptValue}, {Fs,Ds}) ->
                  {Fs, [{{msg_option,FullName},{OptName,OptValue}} | Ds]};
             (Def, {Fs,Ds}) ->
                  QDefs = flatten_qualify_defnames([Def], FullName),
                  {Fs, QDefs++Ds}
          end,
          {[],[]},
          FieldsOrDefs),
    {lists:reverse(RFields2), Defs2}.

%% Resolve any refs
resolve_refs(Defs) ->
    Root = ['.'],
    {ResolvedRefs, Reasons} =
        lists:mapfoldl(
          fun({{msg,FullName}, Fields}, Acc) ->
                  {NewFields, Acc2} =
                      resolve_field_refs(Fields, Defs, Root, FullName, Acc),
                  {{{msg,FullName}, NewFields}, Acc2};
             ({{group,FullName}, Fields}, Acc) ->
                  {NewFields, Acc2} =
                      resolve_field_refs(Fields, Defs, Root, FullName, Acc),
                  {{{group,FullName}, NewFields}, Acc2};
             ({{service,FullName}, Rpcs}, Acc) ->
                  {NewRPCs, Acc2} =
                      resolve_rpc_refs(Rpcs, Defs, Root, FullName, Acc),
                  {{{service,FullName}, NewRPCs}, Acc2};
             ({{extend,ExtendeeCandidates}, Fields}, Acc) ->
                  {Extendee, NewFields, Acc2} =
                      resolve_extend_refs(ExtendeeCandidates, Fields, Defs,
                                          Root, Acc),
                  {{{extend,Extendee}, NewFields}, Acc2};
             (OtherElem, Acc) ->
                  {OtherElem, Acc}
          end,
          [],
          Defs),
    if Reasons == [] -> {ok, ResolvedRefs};
       Reasons /= [] -> {error, lists:reverse(Reasons)}
    end.



resolve_field_refs(Fields, Defs, Root, FullName, Reasons) ->
    lists:mapfoldl(
      fun(#?gpb_field{name=FName, type={ref,Ref}}=Field, Acc) ->
              case resolve_ref(Defs, Ref, Root, FullName) of
                  {found, TypeName} ->
                      {Field#?gpb_field{type=TypeName}, Acc};
                  not_found ->
                      Reason = {ref_to_undefined_msg_or_enum,
                                {{FullName, FName}, Ref}},
                      {Field, [Reason | Acc]}
              end;
         (#?gpb_field{name=FName, type={map,KeyType,{ref,Ref}}}=Field, Acc) ->
              case resolve_ref(Defs, Ref, Root, FullName) of
                  {found, TypeName} ->
                      {Field#?gpb_field{type={map,KeyType,TypeName}}, Acc};
                  not_found ->
                      Reason = {ref_to_undefined_msg_or_enum,
                                {{FullName, FName}, Ref}},
                      {Field, [Reason | Acc]}
              end;
         (#?gpb_field{}=Field, Acc) ->
              {Field, Acc};
         (#gpb_oneof{fields=OFields1}=Oneof, Acc) ->
              {OFields2, Acc2} =
                  resolve_field_refs(OFields1, Defs, Root, FullName, Acc),
              {Oneof#gpb_oneof{fields=OFields2}, Acc2}
      end,
      Reasons,
      Fields).

resolve_rpc_refs(Rpcs, Defs, Root, FullName, Reasons) ->
    lists:mapfoldl(
      fun({RpcName, {Arg, ArgIsStream}, {Return, ReturnIsStream}, Opts}=Rpc,
          Acc) ->
              case resolve_ref(Defs, Arg, Root, FullName) of
                  {found, {msg, MArg}} ->
                      case resolve_ref(Defs, Return, Root, FullName) of
                          {found, {msg, MReturn}} ->
                              NewOpts = [{reformat_name(Name), Value}
                                         || {option,Name,Value} <- Opts],
                              NewRpc = #?gpb_rpc{name=RpcName,
                                                 input=MArg,
                                                 input_stream=ArgIsStream,
                                                 output=MReturn,
                                                 output_stream=ReturnIsStream,
                                                 opts=NewOpts},
                              {NewRpc, Acc};
                          {found, {BadType, MReturn}} ->
                              Reason = {rpc_return_ref_to_non_msg,
                                        {{FullName, RpcName, Return},
                                         BadType, MReturn}},
                              {Rpc, [Reason | Acc]};
                          not_found ->
                              Reason = {rpc_return_ref_to_undefined_msg,
                                        {{FullName, RpcName}, Return}},
                              {Rpc, [Reason | Acc]}
                      end;
                  {found, {BadType, MArg}} ->
                      Reason = {rpc_arg_ref_to_non_msg,
                                {{FullName, RpcName, Arg}, BadType, MArg}},
                      {Rpc, [Reason | Acc]};
                  not_found ->
                      Reason = {rpc_arg_ref_to_undefined_msg,
                                {{FullName, RpcName}, Arg}},
                      {Rpc, [Reason | Acc]}
              end
      end,
      Reasons,
      Rpcs).

resolve_extend_refs({eref2, Ctxt, ExtendeeCandidates}, Fields, Defs,
                    Root, Acc) ->
    case resolve_ref_candidates(Defs, ExtendeeCandidates) of
        {found, {msg,NewToBeExtended}} ->
            {NewFields, Acc2} =
                resolve_field_refs(Fields, Defs, Root, Ctxt, Acc),
            {NewToBeExtended, NewFields, Acc2};
        not_found ->
            Reason = {extend_ref_to_undefined_msg, hd(ExtendeeCandidates)},
            {hd(ExtendeeCandidates), Fields, [Reason | Acc]}
    end.

%% -> {found, {msg,FullName}|{enum,FullName}} | not_found
resolve_ref(Defs, Ref, Root, FullName) ->
    case is_absolute_ref(Ref) of
        true  ->
            FullRef = ensure_path_prepended(Root, Ref),
            find_typename(FullRef, Defs);
        false ->
            PossibleRoots = compute_roots(FullName),
            find_ref_rootwards(PossibleRoots, Ref, Defs)
    end.

resolve_ref_candidates(Defs, [Cand1 | Rest]) ->
    case find_typename(Cand1, Defs) of
        {found, TypeName} -> {found, TypeName};
        not_found -> resolve_ref_candidates(Defs, Rest)
    end;
resolve_ref_candidates(_Defs, []) ->
    not_found.

find_ref_rootwards([PossibleRoot | Rest], Ref, Defs) ->
    FullRef = ensure_path_prepended(PossibleRoot, Ref),
    case find_typename(FullRef, Defs) of
        {found, TypeName} -> {found, TypeName};
        not_found -> find_ref_rootwards(Rest, Ref, Defs)
    end;
find_ref_rootwards([], _Ref, _Defs) ->
    not_found.

is_absolute_ref(['.' | _]) -> true;
is_absolute_ref(_Other)    -> false.

find_typename(Name, [{{enum,Name}, _Values} | _])  -> {found, {enum,Name}};
find_typename(Name, [{{msg,Name}, _SubElems} | _]) -> {found, {msg,Name}};
find_typename(Name, [{{group,Name}, _Elems} | _])  -> {found, {group,Name}};
find_typename(Name, [_ | Rest])                    -> find_typename(Name, Rest);
find_typename(_Name,[])                            -> not_found.

%% Turn ['.',m1,'.',m2,'.',m3]
%% into [['.',m1,'.',m2,'.',m3],
%%       ['.',m1,'.',m2],
%%       ['.',m1],
%%       ['.']]
compute_roots(['.']) -> [['.']];
compute_roots(DeeperPath) ->
    [DeeperPath | compute_roots(drop_last_level(DeeperPath))].

drop_last_level(['.']) -> ['.'];
drop_last_level(['.', X]) when is_atom(X) -> ['.'];
drop_last_level(DeeperPath) when length(DeeperPath) >= 3 ->
    [_X, '.' | RestReversed] = lists:reverse(DeeperPath),
    lists:reverse(RestReversed).

prepend_path(['.'], Id) when is_atom(Id)           -> ['.', Id];
prepend_path(['.'], SubPath) when is_list(SubPath) -> ['.' | SubPath];
prepend_path(Path,  Id) when is_atom(Id)           -> Path ++ ['.', Id];
prepend_path(Path,  SubPath) when is_list(SubPath) -> Path ++ ['.' | SubPath].

ensure_path_prepended(Pkg, Path)   ->
    case lists:prefix(Pkg, Path) of
        false -> prepend_path(Pkg, Path);
        true ->  Path
    end.

convert_default_values(Defs) ->
    lists:map(
      fun({{msg,Name},Fields}) ->
              Fields2 = lists:map(fun convert_default_values_field/1, Fields),
              {{msg,Name},Fields2};
         ({{group,Name},Fields}) ->
              Fields2 = lists:map(fun convert_default_values_field/1, Fields),
              {{group,Name},Fields2};
         (Other) ->
              Other
      end,
      Defs).

convert_default_values_field(#?gpb_field{type=Type, opts=Opts}=Field) ->
    case {Type, lists:keyfind(default, 1, Opts)} of
        {bytes, {default, Default}} when is_list(Default) ->
            %% Default values for type bytes are written as a string
            Default2 = list_to_binary(Default),
            Opts2 = lists:keyreplace(default, 1, Opts, {default, Default2}),
            Field#?gpb_field{opts=Opts2};
        _ ->
            Field
    end;
convert_default_values_field(#gpb_oneof{fields=OFs}=Field) ->
    OFs2 = lists:map(fun convert_default_values_field/1, OFs),
    Field#gpb_oneof{fields=OFs2}.

join_any_msg_options(Defs) ->
    {NonMsgOptDefs, MsgOptsDict} =
        lists:foldl(
          fun({{msg_option,MsgName},Opt}, {Ds,MsgOptsDict}) ->
                  {Ds, dict:append(MsgName, Opt, MsgOptsDict)};
             (OtherDef, {Ds, MsgOptsDict}) ->
                  {[OtherDef | Ds], MsgOptsDict}
          end,
          {[], dict:new()},
          Defs),
    MsgOpts = [{{msg_options, MsgName}, MsgOpts}
               || {MsgName, MsgOpts} <- dict:to_list(MsgOptsDict)],
    lists:reverse(NonMsgOptDefs, MsgOpts).

handle_proto_syntax_version_one_file(Defs) ->
    case proplists:get_value(syntax, Defs) of
        undefined -> handle_proto2_1(Defs);
        "proto2"  -> handle_proto2_1(Defs);
        "proto3"  -> handle_proto3_1(Defs)
    end.

handle_proto2_1(Defs) ->
    Defs.

handle_proto3_1(Defs) ->
    %% FIXME: Verify no 'extensions' or 'extend'
    %% FIXME: Verify no 'required' occurrences
    %% FIXME: Verify enums start with 0

    %% Remember which msgs were defined using proto3 syntax,
    %% so we can treat them differently later on.
    anno_msgs_proto3_origin(Defs).

anno_msgs_proto3_origin(Defs) ->
    anno_msgs_proto3_origin_2(Defs, []).

anno_msgs_proto3_origin_2([{{msg,Msg},_Fields}=Def | Rest], P3Msgs) ->
    [Def | anno_msgs_proto3_origin_2(Rest, [Msg | P3Msgs])];
anno_msgs_proto3_origin_2([Def | Rest], Acc) ->
    [Def | anno_msgs_proto3_origin_2(Rest, Acc)];
anno_msgs_proto3_origin_2([], Acc) ->
    [{proto3_msgs,lists:reverse(Acc)}].

handle_proto_syntax_version_all_files(Defs) ->
    P3Items = [X || {proto3_msgs,_}=X <- Defs],
    if P3Items == [] ->
            Defs;
       P3Items /= [] ->
            Proto3Msgs = lists:append([Msgs || {proto3_msgs,Msgs} <- P3Items]),
            Defs1 = Defs -- P3Items,
            Defs2 = Defs1 ++ [{proto3_msgs, lists:sort(Proto3Msgs)}],

            %% The protobuf language guide for proto3 says: "In proto3,
            %% repeated fields of scalar numeric types use packed encoding by
            %% default."
            default_repeated_to_packed(Defs2, Proto3Msgs)
    end.

default_repeated_to_packed(Defs, P3Msgs) ->
    lists:map(
      fun({{msg,MsgName},Fields}=MsgDef) ->
              case lists:member(MsgName, P3Msgs) of
                  true ->
                      Fields1 = default_repeated_fields_to_packed(Fields),
                      {{msg,MsgName}, Fields1};
                  false ->
                      MsgDef
              end;
         (Other) ->
              Other
      end,
      Defs).

default_repeated_fields_to_packed(Fields) ->
    lists:map(
      fun(#?gpb_field{occurrence=repeated, opts=Opts, type=Type}=F) ->
              case {proplists:get_value(packed, Opts),
                    is_scalar_numeric(Type)} of
                  {undefined, true} ->
                      NewOpts = [{packed, true} | Opts],
                      F#?gpb_field{opts=NewOpts};
                  _ ->
                      F
              end;
         (F) ->
              F
      end,
      Fields).

is_scalar_numeric(int32)    -> true;
is_scalar_numeric(int64)    -> true;
is_scalar_numeric(uint32)   -> true;
is_scalar_numeric(uint64)   -> true;
is_scalar_numeric(sint32)   -> true;
is_scalar_numeric(sint64)   -> true;
is_scalar_numeric(fixed32)  -> true;
is_scalar_numeric(fixed64)  -> true;
is_scalar_numeric(sfixed32) -> true;
is_scalar_numeric(sfixed64) -> true;
is_scalar_numeric(bool)     -> true;
is_scalar_numeric(float)    -> true;
is_scalar_numeric(double)   -> true;
is_scalar_numeric({enum,_}) -> true;
is_scalar_numeric(_)        -> false. % not: string | bytes | msg | map

%% Find inconsistencies
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and may or may not be reformatted.
verify_defs(Defs) ->
    collect_errors(Defs,
                   [{msg,     [fun verify_field_defaults/2]},
                    {group,   [fun verify_field_defaults/2]},
                    {extend,  [fun verify_extend/2]},
                    {service, [fun verify_service/2]},
                    {'_',     [fun(_Def, _AllDefs) -> ok end]}]).

collect_errors(Defs, VerifiersList) ->
    collect_errors(Defs, Defs, VerifiersList, ok).

collect_errors([{{ElemType,_},_}=Def | Rest], AllDefs, VerifiersList, Acc) ->
    Result = lists:foldl(
               fun(Verifier, A) -> add_acc(A, Verifier(Def, AllDefs)) end,
               Acc,
               find_verifiers(ElemType, VerifiersList)),
    collect_errors(Rest, AllDefs, VerifiersList, Result);
collect_errors([_OtherDef | Rest], AllDefs, VerifiersList, Acc) ->
    %% Example: import, package, ...
    collect_errors(Rest, AllDefs, VerifiersList, Acc);
collect_errors([], _AllRefs, _VerifiersList, Acc) ->
    case Acc of
        ok                       -> ok;
        {error, ReasonsReversed} -> {error, lists:reverse(ReasonsReversed)}
    end.

add_acc(AnyPreviousResult, ok)         -> AnyPreviousResult;
add_acc(ok,                {error, R}) -> {error, add_reason([], R)};
add_acc({error, Reasons},  {error, R}) -> {error, add_reason(Reasons, R)}.

add_reason(Reasons, Reason) when not is_list(Reason) ->
    [Reason | Reasons];
add_reason(Reasons, MoreReasons) when is_list(MoreReasons) ->
    lists:reverse(MoreReasons, Reasons).

find_verifiers(Type,  [{Type, Verifiers} | _]) -> Verifiers;
find_verifiers(_Type, [{'_', Verifiers} | _])  -> Verifiers;
find_verifiers(Type,  [_Other | Rest])         -> find_verifiers(Type, Rest).

verify_field_defaults({{msg,M}, Fields}, AllDefs) ->
    lists:foldl(fun(#?gpb_field{name=Name, type=Type, opts=FOpts}, Acc) ->
                        Res = case lists:keysearch(default, 1, FOpts) of
                                  {value, {default, Default}} ->
                                      verify_scalar_default_if_present(
                                        M, Name, Type, Default, AllDefs);
                                  false ->
                                      ok
                              end,
                        add_acc(Acc, Res);
                   (#gpb_oneof{fields=OFields}, Acc) ->
                        Res = verify_field_defaults({{msg,M},OFields}, AllDefs),
                        add_acc(Acc, Res)
                end,
                ok,
                Fields);
verify_field_defaults({{group,G}, Fields}, AllDefs) ->
    verify_field_defaults({{msg,G}, Fields}, AllDefs).


verify_scalar_default_if_present(MsgName, FieldName, Type, Default, AllDefs) ->
    case Type of
        {enum,Ref} ->
            case lists:keysearch({enum, Ref}, 1, AllDefs) of
                {value, {{enum,Ref}, Enumerators}} ->
                    case lists:keysearch(Default, 1, Enumerators) of
                        {value, {Default, _Value}} ->
                            ok;
                        false ->
                            {error,
                             {{invalid_default_enum_value, Default},
                              {name_to_dstr(MsgName), atom_to_list(FieldName)}}}
                    end;
                false ->
                    ok %% caught by another verification step
            end;
        ScalarType when is_atom(ScalarType) ->
            case gpb:check_scalar(Default, ScalarType) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {Reason, {name_to_dstr(MsgName),
                                      atom_to_list(FieldName)}}}
            end
    end.

verify_extend(_, _AllDefs) ->
    %% FIXME
    ok.

verify_service(_, _AllDefs) ->
    %% FIXME
    ok.

name_to_absdstr(['.' | Name]) -> "." ++ name_to_dstr(Name);
name_to_absdstr(Name) -> name_to_dstr(Name).

name_to_dstr(Name) when is_list(Name) ->
    gpb_lib:dot_join([atom_to_list(P) || P <- Name, P /= '.']);
name_to_dstr(Name) when is_atom(Name) ->
    atom_to_list(Name).

format_post_process_error({error, Reasons}) ->
    lists:flatten([[fmt_err(Reason),"\n"] || Reason <- Reasons]).

-define(f(F, A), io_lib:format(F, A)).

fmt_err({multiple_pkg_specifiers, Pkgs}) ->
    ?f("package specified more than once: ~s~n",
       [gpb_lib:comma_join([atom_to_list(Pkg) || Pkg <- Pkgs])]);
fmt_err({ref_to_undefined_msg_or_enum, {{Msg, Field}, To}}) ->
    ?f("in msg ~s, field ~s: undefined reference  ~s",
       [name_to_dstr(Msg), name_to_dstr(Field), name_to_absdstr(To)]);
fmt_err({extend_ref_to_undefined_msg, Msg}) ->
    ?f("extend of unknown message ~s", [name_to_absdstr(Msg)]);
fmt_err({rpc_return_ref_to_non_msg,
         {{FullName, RpcName, Return}, BadType, MReturn}}) ->
    ?f("in service ~s, rpc ~s, the return type, ~s, refers to "
       " a ~p, ~s, instead of to a message",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Return),
        BadType, name_to_dstr(MReturn)]);
fmt_err({rpc_return_ref_to_undefined_msg, {{FullName, RpcName}, Ret}}) ->
    ?f("in service ~s, rpc ~s, return: undefined reference ~s",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Ret)]);
fmt_err({rpc_arg_ref_to_non_msg, {{FullName, RpcName, Arg}, BadType, MArg}}) ->
    ?f("in service ~s, rpc ~s, the arg type, ~s, refers to "
       " a ~p, ~s, instead of to a message",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Arg),
        BadType, name_to_dstr(MArg)]);
fmt_err({rpc_arg_ref_to_undefined_msg, {{FullName, RpcName}, Arg}}) ->
    ?f("in service ~s, rpc ~s, arg: undefined reference ~s",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Arg)]);
fmt_err({{invalid_default_enum_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: undefined enumerator in default value ~s",
       [Msg, Field, Default]);
fmt_err({{{value_out_of_range, Signedness, Bits}, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: default value ~p out of range for ~p ~p bit int",
       [Msg, Field, Default, Signedness, Bits]);
fmt_err({{{bad_integer_value, Signedness, Bits}, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for ~p ~p bit int",
       [Msg, Field, Default, Signedness, Bits]);
fmt_err({{bad_floating_point_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad floating point default value ~p",
       [Msg, Field, Default]);
fmt_err({{bad_boolean_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for boolean",
       [Msg, Field, Default]);
fmt_err({{bad_unicode_string, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for string",
       [Msg, Field, Default]);
fmt_err({{bad_binary_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for bytes",
       [Msg, Field, Default]).

%% Rewrites for instance ['.','m1','.',m2] into 'm1.m2'
%% Example: {{msg,['.','m1','.',m2]}, [#field{type={msg,['.','m1','.',m3]}}]}
%% becomes: {{msg,'m1.m2'},           [#field{type={msg,'m1.m3'}}]}
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and names and references
%% are expected to have been resolved
reformat_names(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{group,Name}, Fields}) ->
                      {{group,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{msg_containment, ProtoName}, Msgs}) ->
                      {{msg_containment,ProtoName},
                       [reformat_name(N) || N <- Msgs]};
                 ({{enum,Name}, ENs}) ->
                      {{enum,reformat_name(Name)}, reformat_enum_opt_names(ENs)};
                 ({{extensions,Name}, Exts}) ->
                      {{extensions,reformat_name(Name)}, Exts};
                 ({{extend,Name}, Fields}) ->
                      %% FIXME: extend
                      {{extend,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{service,Name}, RPCs}) ->
                      {{service,reformat_name(Name)}, reformat_rpcs(RPCs)};
                 ({package, Name}) ->
                      {package, reformat_name(Name)};
                 ({proto3_msgs,Names}) ->
                      {proto3_msgs,[reformat_name(Name) || Name <- Names]};
                 ({{reserved_numbers,Name}, Ns}) ->
                      {{reserved_numbers,reformat_name(Name)}, Ns};
                 ({{reserved_names,Name}, FieldNames}) ->
                      {{reserved_names,reformat_name(Name)}, FieldNames};
                 ({{msg_options,MsgName}, Opt}) ->
                      {{msg_options,reformat_name(MsgName)}, Opt};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

reformat_fields(Fields) ->
    lists:map(
      fun(#?gpb_field{type={T,Nm}}=F) ->
              F#?gpb_field{type={T,reformat_name(Nm)}};
         (#?gpb_field{type={map,KeyType,{T,Nm}}}=F) ->
              F#?gpb_field{type={map,KeyType,{T,reformat_name(Nm)}}};
         (#?gpb_field{}=F) ->
              F;
         (#gpb_oneof{fields=Fs}=O) ->
              O#gpb_oneof{fields=reformat_fields(Fs)}
      end,
      Fields).

%% `Defs' is expected to be parsed.
reformat_enum_opt_names(Def) ->
    [case Item of
         {option, Name, Value} ->
             {option, reformat_name(Name), Value};
         Other ->
             Other
     end
     || Item <- Def].

reformat_name(Name) ->
    list_to_atom(gpb_lib:dot_join([atom_to_list(P) || P <- Name, P /= '.'])).

reformat_rpcs(RPCs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}=R) ->
                      R#?gpb_rpc{name=RpcName,
                                 input=reformat_name(Arg),
                                 output=reformat_name(Return)}
              end,
              RPCs).

%% `Defs' is expected to be flattened and may or may not be reformatted
%% `Defs' is expected to be verified, to not extend missing messages
extend_msgs(Defs0) ->
    Extendings = [E || {{extend,_MsgToExtend},_Mor91eFields}=E <- Defs0],
    lists:foldl(fun possibly_extend_msg/2, Defs0, Extendings).


possibly_extend_msg({{extend,Msg}, MoreFields}=Extending, Defs) ->
    case lists:keyfind({msg,Msg}, 1, Defs) of
        {{msg,Msg}, OrigFields} ->
            NewDef = {{msg,Msg}, OrigFields ++ MoreFields},
            lists:keyreplace({msg,Msg}, 1, Defs, NewDef) -- [Extending];
        false ->
            Defs
    end.

%% `Defs' is expected to be flattened
enumerate_msg_fields(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, enumerate_fields(Fields)};
                 ({{group,Name}, Fields}) ->
                      {{group, Name}, enumerate_fields(Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

enumerate_fields(Fields) ->
    lists:map(fun({I, #?gpb_field{}=F}) ->
                      F#?gpb_field{rnum=I};
                 ({I, #gpb_oneof{fields=Fs}=O}) ->
                      NewFields = [F#?gpb_field{rnum=I} || F <- Fs],
                      O#gpb_oneof{rnum=I, fields=NewFields}
              end,
              index_seq(2, Fields)).

index_seq(_Start, []) -> [];
index_seq(Start, L)   -> lists:zip(lists:seq(Start, length(L) + Start - 1), L).

%% `Defs' is expected to be parsed.
normalize_msg_field_options(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, normalize_field_options(Fields)};
                 ({{group,Name}, Fields}) ->
                      {{group, Name}, normalize_field_options(Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

normalize_field_options(Fields) ->
    lists:map(fun(#?gpb_field{type={map,_KeyType,_ValueType}, opts=Opts}=F) ->
                      Opts1    = normalize_field_options_2(Opts),
                      Opts2    = Opts1 -- [packed],
                      F#?gpb_field{opts = Opts2};
                 (#?gpb_field{opts=Opts}=F) ->
                      Opts1    = normalize_field_options_2(Opts),
                      F#?gpb_field{opts = Opts1};
                 (#gpb_oneof{fields=Fs}=O) ->
                      O#gpb_oneof{fields=normalize_field_options(Fs)}
              end,
              Fields).

normalize_field_options_2(Opts) ->
    Opts1 = opt_tuple_to_atom_if_defined_true(packed, Opts),
    opt_tuple_to_atom_if_defined_true(deprecated, Opts1).

opt_tuple_to_atom_if_defined_true(Opt, Opts) ->
    case proplists:get_bool(Opt, Opts) of
        false -> lists:keydelete(Opt, 1, Opts);
        true  -> [Opt | lists:keydelete(Opt, 1, Opts)]
    end.

possibly_prefix_suffix_msgs(Defs, Opts) ->
    Prefix = proplists:get_value(msg_name_prefix, Opts, ""),
    Suffix = proplists:get_value(msg_name_suffix, Opts, ""),
    ToLower = case proplists:get_value(msg_name_to_lower, Opts, false) of
                  false ->
                      false;
                  true ->
                      to_lower
              end,
    ToLowerOrSnake =
        case proplists:get_value(msg_name_to_snake_case, Opts, ToLower) of
            true ->
                snake_case;
            T ->
                T
        end,

    if Prefix == "", Suffix == "", ToLowerOrSnake == false ->
            Defs;
       true ->
            Defs1 = prefix_suffix_msgs(Prefix, Suffix, ToLowerOrSnake, Defs),
            prefix_suffix_msg_containment(Prefix, Suffix, ToLowerOrSnake, Defs1)
    end.

find_proto(_, []) ->
    undefined;
find_proto(Name, [{{msg_containment, Proto}, Msgs} | Rest]) ->
      case lists:member(Name, Msgs) of
          true ->
              Proto;
          false ->
              find_proto(Name, Rest)
      end;
find_proto(Name, [_ | Rest]) ->
    find_proto(Name, Rest).

maybe_prefix_by_proto(Name, {by_proto, PrefixList}, Defs) ->
    case find_proto(Name, Defs) of
        undefined ->
            "";
        ProtoName ->
            proplists:get_value(list_to_atom(ProtoName), PrefixList, "")
    end;
maybe_prefix_by_proto(_Name, Prefix, _Defs) ->
    Prefix.

prefix_suffix_msgs(Prefix, Suffix, ToLowerOrSnake, Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                      {{msg,prefix_suffix_name(Prefix1, Suffix,
                                             ToLowerOrSnake, Name)},
                        prefix_suffix_fields(Prefix, Suffix,
                                             ToLowerOrSnake, Fields, Defs)};
                 ({{group,Name}, Fields}) ->
                      Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                      {{group,prefix_suffix_name(Prefix1, Suffix,
                                                 ToLowerOrSnake, Name)},
                        prefix_suffix_fields(Prefix, Suffix,
                                             ToLowerOrSnake, Fields, Defs)};
                 ({{extensions,Name}, Exts}) ->
                      Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                      {{extensions,
                        prefix_suffix_name(Prefix1, Suffix,
                                           ToLowerOrSnake, Name)},
                       Exts};
                 ({{service,Name}, RPCs}) ->
                      {{service, maybe_tolower_or_snake_name(Name,
                                                             ToLowerOrSnake)},
                        prefix_suffix_rpcs(Prefix, Suffix,
                                           ToLowerOrSnake, RPCs, Defs)};
                 ({package,Name}) ->
                      {package, maybe_tolower_or_snake_name(Name,
                                                            ToLowerOrSnake)};
                 ({proto3_msgs,Names}) ->
                      {proto3_msgs,
                       [begin
                            Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                            prefix_suffix_name(Prefix1, Suffix,
                                               ToLowerOrSnake, Name)
                        end || Name <- Names]};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

prefix_suffix_fields(Prefix, Suffix, ToLowerOrSnake, Fields, Defs) ->
    lists:map(
      fun(#?gpb_field{type={msg,MsgName}}=F) ->
              Prefix1 = maybe_prefix_by_proto(MsgName, Prefix, Defs),
              NewMsgName = prefix_suffix_name(Prefix1, Suffix,
                                              ToLowerOrSnake, MsgName),
              F#?gpb_field{type={msg,NewMsgName}};
         (#?gpb_field{type={group,MsgName}}=F) ->
              Prefix1 = maybe_prefix_by_proto(MsgName, Prefix, Defs),
              NewMsgName = prefix_suffix_name(Prefix1, Suffix,
                                              ToLowerOrSnake, MsgName),
              F#?gpb_field{type={group,NewMsgName}};
         (#?gpb_field{type={map,KeyType,{msg,MsgName}}}=F) ->
              Prefix1 = maybe_prefix_by_proto(MsgName, Prefix, Defs),
              NewMsgName = prefix_suffix_name(Prefix1, Suffix,
                                              ToLowerOrSnake, MsgName),
              F#?gpb_field{type={map,KeyType,{msg,NewMsgName}}};
         (#gpb_oneof{fields=Fs}=F) ->
              Fs2 = prefix_suffix_fields(Prefix, Suffix,
                                         ToLowerOrSnake, Fs, Defs),
              F#gpb_oneof{fields=Fs2};
         (#?gpb_field{}=F) ->
              F
      end,
      Fields).

prefix_suffix_msg_containment(Prefix, Suffix, ToLowerOrSnake, Defs) ->
    lists:map(
      fun({{msg_containment, Proto}, MsgNames}=Elem) ->
              MsgNames1 = [prefix_suffix_name(
                             maybe_prefix_by_proto(Name, Prefix, [Elem]),
                             Suffix, ToLowerOrSnake, Name)
                           || Name <- MsgNames],
              {{msg_containment, Proto}, MsgNames1};
         (OtherElem) ->
              OtherElem
      end,
      Defs).

prefix_suffix_name(Prefix, Suffix, ToLowerOrSnake, Name) ->
    Name1 = maybe_tolower_or_snake_name(Name, ToLowerOrSnake),
    Name2 = lists:concat([Prefix, Name1, Suffix]),
    list_to_atom(Name2).

maybe_tolower_or_snake_name(Name, false) -> Name;
maybe_tolower_or_snake_name(Name, to_lower) ->
    list_to_atom(gpb_lib:lowercase(atom_to_list(Name)));
maybe_tolower_or_snake_name(Name, snake_case) ->
    NameString = atom_to_list(Name),
    Snaked = lists:foldl(fun(RE, Snaking) ->
                             re:replace(Snaking, RE, "\\1_\\2", [{return, list},
                                                                 global])
                         end, NameString, [%% uppercase followed by lowercase
                                          "(.)([A-Z][a-z]+)",
                                          %% any consecutive digits
                                          "(.)([0-9]+)",
                                          %% uppercase with lowercase
                                          %% or digit before it
                                          "([a-z0-9])([A-Z])"]),
    list_to_atom(gpb_lib:lowercase(Snaked)).

prefix_suffix_rpcs(Prefix, Suffix, ToLowerOrSnake, RPCs, Defs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}=R) ->
                      PrefixArg = maybe_prefix_by_proto(Arg, Prefix, Defs),
                      PrefixReturn = maybe_prefix_by_proto(Return,Prefix,Defs),
                      NewArg = prefix_suffix_name(PrefixArg, Suffix,
                                                  ToLowerOrSnake, Arg),
                      NewReturn = prefix_suffix_name(PrefixReturn, Suffix,
                                                     ToLowerOrSnake, Return),
                      R#?gpb_rpc{name=maybe_tolower_or_snake_name(RpcName, ToLowerOrSnake),
                                 input=NewArg,
                                 output=NewReturn}
              end,
              RPCs).

%% Fetch the `import'ed files.
%% `Defs' is expected to be parsed, but not necessarily post_processed.
-spec fetch_imports(defs()) -> [ProtoFile::string()].
fetch_imports(Defs) ->
    [Path || {import,Path} <- Defs].
