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
        extend_def extensions_def exts ext
        oneof_def oneof_elems oneof_elem
        option_def
        service_def rpc_defs rpc_def m_opts
        name
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
        extensions extend max to
        oneof
        service rpc returns
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

identifiers -> identifier '.' identifiers:      [identifier_name('$1'), '.'
                                                 | '$3'].
identifiers -> identifier:                      [identifier_name('$1')].

import_def -> import str_lit ';':       {import, literal_value('$2')}.

option_def -> option name '=' constant: {option, '$2', '$4'}.

enum_def -> enum identifier '{' enum_fields '}':
                                        {{enum,identifier_name('$2')},'$4'}.

enum_fields -> enum_field enum_fields:  ['$1' | '$2'].
enum_fields -> option_def enum_fields:  ['$1' | '$2'].
enum_fields -> ';' enum_fields:         '$2'.
enum_fields -> '$empty':                [].

enum_field -> identifier '=' integer ';':
                                        {identifier_name('$1'), '$3'}.
enum_field -> identifier '=' integer '[' opt_enum_opts ']' ';':
                                        {identifier_name('$1'), '$3'}.

opt_enum_opts -> enum_opts:             '$1'.
opt_enum_opts -> '$empty':              [].

enum_opts -> enum_opt ',' enum_opts:    ['$1' | '$2'].
enum_opts -> enum_opt:                  ['$1'].

enum_opt -> name '=' constant:          {'$1', '$3'}.


message_def -> message identifier '{' msg_elems '}':
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
                                        #?gpb_field{occurrence=required,
                                                    type='$1',
                                                    name=identifier_name('$2'),
                                                    fnum=literal_value('$4'),
                                                    opts=[]}.
msg_elem -> type fidentifier '=' dec_lit '[' opt_field_opts ']' ';': % proto3
                                        #?gpb_field{occurrence=required,
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
msg_elem -> extend identifier '{' msg_elems '}':
                                 {{extend,identifier_name('$2')},'$4'}.

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
fidentifier -> packed:                  kw_to_identifier('$1').
fidentifier -> deprecated:              kw_to_identifier('$1').
fidentifier -> syntax:                  kw_to_identifier('$1').
fidentifier -> map:                     kw_to_identifier('$1').

opt_field_opts -> field_opts:           '$1'.
opt_field_opts -> '$empty':             [].


field_opts -> field_opt ',' field_opts: ['$1' | '$3'].
field_opts -> field_opt:                ['$1'].

field_opt -> default '=' constant:      {default, '$3'}.
field_opt -> packed:                    {packed, true}.
field_opt -> packed '=' bool_lit:       {packed, literal_value('$3')}.
field_opt -> deprecated:                {deprecated, true}.
field_opt -> deprecated '=' bool_lit:   {deprecated, literal_value('$3')}.
field_opt -> name:                      {identifier_name('$1'), true}.
field_opt -> name '=' constant:         {identifier_name('$1'), '$3'}.

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

oneof_def -> 'oneof' identifier '{' oneof_elems '}':
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

extend_def -> extend identifier '{' msg_elems '}':
                                        {{extend,identifier_name('$2')},'$4'}.


service_def -> service identifier '{' rpc_defs '}':
                                        {{service,identifier_name('$2')},'$4'}.

rpc_defs -> rpc_def rpc_defs:           ['$1' | '$2'].
rpc_defs -> ';' rpc_defs:               '$2'.
rpc_defs -> '$empty':                   [].

rpc_def -> rpc identifier '(' name ')' returns '(' name ')' ';':
                                        {identifier_name('$2'), '$4', '$8'}.
rpc_def -> rpc identifier '(' name ')' returns '(' name ')' '{' m_opts '}' ';':
                                        {identifier_name('$2'), '$4', '$8'}.

m_opts -> ';' m_opts:                   '$2'.
m_opts -> '$empty':                     [].

Erlang code.

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-export([post_process_one_file/2]).
-export([post_process_all_files/2]).
-export([format_post_process_error/1]).
-export([fetch_imports/1]).

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

post_process_one_file(Defs, Opts) ->
    case find_package_def(Defs, Opts) of
        {ok, Package} ->
            {ok, handle_proto_syntax_version(
                   flatten_qualify_defnames(Defs, Package))};
        {error, Reasons} ->
            {error, Reasons}
    end.

post_process_all_files(Defs, Opts) ->
    case resolve_names(extend_msgs(Defs)) of
        {ok, Defs2} ->
            {ok, possibly_prefix_suffix_msgs(
                   normalize_msg_field_options( %% Sort it?
                     enumerate_msg_fields(
                       reformat_names(Defs2))),
                   Opts)};
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
    DefaultPkg = ['.'],
    case proplists:get_bool(use_packages, Opts) of
        true ->
            case [Pkg || {package, Pkg} <- Defs] of
                [] ->
                    {ok, DefaultPkg};
                [Pkg] ->
                    {ok, ['.' | Pkg]};
                Pkgs when length(Pkgs) >= 2 ->
                    PrettyPkgs = [reformat_name(Pkg) || Pkg <- Pkgs],
                    {error, [{multiple_pkg_specifiers, PrettyPkgs}]}
            end;
        false ->
            {ok, DefaultPkg}
    end.

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
           ({{enum,Name}, ENs}, Acc) ->
                FullName = prepend_path(Root, Name),
                [{{enum,FullName}, ENs} | Acc];
           ({extensions,Exts}, Acc) ->
                [{{extensions,Root},Exts} | Acc];
           ({{extend,Name}, FieldsOrDefs}, Acc) ->
                FullName = prepend_path(Root, Name),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, FullName),
                [{{extend,FullName},Fields2} | Defs2] ++ Acc;
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
        lists:foldl(fun(#?gpb_field{}=F, {Fs,Ds}) ->
                            {[F | Fs], Ds};
                       (#gpb_oneof{}=O, {Fs,Ds}) ->
                            {[O | Fs], Ds};
                       ({{extend, _Msg},_}=Def, {Fs,Ds}) ->
                            QDefs = flatten_qualify_defnames(
                                      [Def], drop_last_level(FullName)),
                            {Fs, QDefs ++ Ds};
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
             ({{service,FullName}, Rpcs}, Acc) ->
                  {NewRPCs, Acc2} =
                      resolve_rpc_refs(Rpcs, Defs, Root, FullName, Acc),
                  {{{service,FullName}, NewRPCs}, Acc2};
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
      fun({RpcName, Arg, Return}=Rpc, Acc) ->
              case resolve_ref(Defs, Arg, Root, FullName) of
                  {found, {msg, MArg}} ->
                      case resolve_ref(Defs, Return, Root, FullName) of
                          {found, {msg, MReturn}} ->
                              NewRpc = #?gpb_rpc{name=RpcName,
                                                 input=MArg,
                                                 output=MReturn},
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

handle_proto_syntax_version(Defs) ->
    case proplists:get_value(syntax, Defs) of
        undefined -> handle_proto2(Defs);
        "proto2"  -> handle_proto2(Defs);
        "proto3"  -> handle_proto3(Defs)
    end.

handle_proto2(Defs) ->
    Defs.

handle_proto3(Defs) ->
    %% FIXME: Verify no 'extensions' or 'extend'
    %% FIXME: Verify no 'required' occurrences
    %% FIXME: Verify enums start with 0

    %% The protobuf language guide for proto3 says: "In proto3,
    %% repeated fields of scalar numeric types use packed encoding by
    %% default."
    default_repeated_to_packed(Defs).

default_repeated_to_packed([{{msg,MsgName},Fields} | Rest]) ->
    NewDef = {{msg,MsgName}, default_repeated_fields_to_packed(Fields)},
    [NewDef | default_repeated_to_packed(Rest)];
default_repeated_to_packed([Other | Rest]) ->
    [Other | default_repeated_to_packed(Rest)];
default_repeated_to_packed([]) ->
    [].

default_repeated_fields_to_packed(Fields) ->
    lists:map(fun(#?gpb_field{occurrence=repeated, opts=Opts}=F) ->
                      case proplists:get_value(packed, Opts) of
                          undefined ->
                              NewOpts = [{packed, true} | Opts],
                              F#?gpb_field{opts=NewOpts};
                          _ ->
                              F
                      end;
                 (F) ->
                      F
              end,
              Fields).

%% Find inconsistencies
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and may or may not be reformatted.
verify_defs(Defs) ->
    collect_errors(Defs,
                   [{msg,     [fun verify_field_defaults/2]},
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
                Fields).

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
    string:join([atom_to_list(P) || P <- Name, P /= '.'],
                ".");
name_to_dstr(Name) when is_atom(Name) ->
    atom_to_list(Name).

format_post_process_error({error, Reasons}) ->
    lists:flatten([[fmt_err(Reason),"\n"] || Reason <- Reasons]).

-define(f(F, A), io_lib:format(F, A)).

fmt_err({multiple_pkg_specifiers, Pkgs}) ->
    ?f("package specified more than once: ~s~n",
       [string:join([atom_to_list(Pkg) || Pkg <- Pkgs], ", ")]);
fmt_err({ref_to_undefined_msg_or_enum, {{Msg, Field}, To}}) ->
    ?f("in msg ~s, field ~s: undefined reference  ~s",
       [name_to_dstr(Msg), name_to_dstr(Field), name_to_absdstr(To)]);
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
                 ({{enum,Name}, ENs}) ->
                      {{enum,reformat_name(Name)}, reformat_enum_opt_names(ENs)};
                 ({{extensions,Name}, Exts}) ->
                      {{extensions,reformat_name(Name)}, Exts};
                 ({{extend,Name}, Fields}) ->
                      {{extend,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{service,Name}, RPCs}) ->
                      {{service,reformat_name(Name)}, reformat_rpcs(RPCs)};
                 ({package, Name}) ->
                      {package, reformat_name(Name)};
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
    list_to_atom(string:join([atom_to_list(P) || P <- Name,
                                                 P /= '.'],
                             ".")).

reformat_rpcs(RPCs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}) ->
                      #?gpb_rpc{name=RpcName,
                                input=reformat_name(Arg),
                                output=reformat_name(Return)}
              end,
              RPCs).

%% `Defs' is expected to be flattened and may or may not be reformatted
%% `Defs' is expected to be verified, to not extend missing messages
extend_msgs(Defs) ->
    [possibly_extend_msg(Def, Defs) || Def <- Defs,
                                       not is_extended(Def, Defs)].

is_extended({{msg,Msg}, _Fields}, Defs) ->
    lists:keymember({extend,Msg}, 1, Defs);
is_extended(_OtherDef, _Defs) ->
    false.

possibly_extend_msg({{extend,Msg}, MoreFields}, Defs) ->
    {value, {{msg,Msg}, OrigFields}} = lists:keysearch({msg,Msg}, 1, Defs),
    {{msg,Msg}, OrigFields ++ MoreFields};
possibly_extend_msg(OtherElem, _Defs) ->
    OtherElem.


%% `Defs' is expected to be flattened
enumerate_msg_fields(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, enumerate_fields(Fields)};
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
    ToLower = proplists:get_value(msg_name_to_lower, Opts, false),

    if Prefix == "", Suffix == "", ToLower == false ->
            Defs;
       true ->
            prefix_suffix_msgs(Prefix, Suffix, ToLower, Defs)
    end.


prefix_suffix_msgs(Prefix, Suffix, ToLower, Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,prefix_suffix_name(Prefix, Suffix, ToLower, Name)},
                       prefix_suffix_fields(Prefix, Suffix, ToLower, Fields)};
                 ({{extensions,Name}, Exts}) ->
                      {{extensions,
                        prefix_suffix_name(Prefix, Suffix, ToLower, Name)},
                       Exts};
                 ({{service,Name}, RPCs}) ->
                      {{service,maybe_tolower_name(Name, ToLower)},
                       prefix_suffix_rpcs(Prefix, Suffix, ToLower, RPCs)};
                 ({package,Name}) ->
                      {package,maybe_tolower_name(Name,ToLower)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

prefix_suffix_fields(Prefix, Suffix, ToLower, Fields) ->
    lists:map(
      fun(#?gpb_field{type={msg,MsgName}}=F) ->
              NewMsgName = prefix_suffix_name(Prefix, Suffix, ToLower, MsgName),
              F#?gpb_field{type={msg,NewMsgName}};
         (#?gpb_field{}=F) ->
              F
      end,
      Fields).

prefix_suffix_name(Prefix, Suffix, ToLower, Name) ->
    Name1 = maybe_tolower_name(Name, ToLower),
    Name2 = lists:concat([Prefix, Name1, Suffix]),
    list_to_atom(Name2).

maybe_tolower_name(Name, false) -> Name;
maybe_tolower_name(Name, true) ->
    list_to_atom(string:to_lower(atom_to_list(Name))).

prefix_suffix_rpcs(Prefix, Suffix, ToLower, RPCs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}) ->
                      NewArg = prefix_suffix_name(Prefix, Suffix, ToLower, Arg),
                      NewReturn = prefix_suffix_name(Prefix, Suffix, ToLower, Return),
                      #?gpb_rpc{name=RpcName,
                                input=NewArg,
                                output=NewReturn}
              end,
              RPCs).

%% `Defs' is expected to be parsed, but not necessarily post_processed.
fetch_imports(Defs) ->
    [Path || {import,Path} <- Defs].
