%% This line tells emacs to use -*- erlang -*- mode for this file

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

Nonterminals
        proto
        elements element
        enum_def enum_fields enum_field
        opt_enum_opts enum_opts enum_opt
        message_def msg_elems msg_elem
        opt_field_opts field_opts field_opt cardinality type
        package_def
        import_def
        identifiers
        extend_def extensions_def exts ext
        option_def
        service_def rpc_defs rpc_def m_opts
        name
        constant
        integer
        string_expr
        .

Terminals
        package
        message enum
        required optional repeated
        double float int32 int64 uint32
        uint64 sint32 sint64 fixed32 fixed64
        sfixed32 sfixed64 bool string bytes
        identifier str_lit dec_lit oct_lit hex_lit float_lit bool_lit
        default
        import
        option
        extensions extend max to
        service rpc returns
        packed deprecated
        '.' ';' '(' ')' '{' '}' '[' ']' '=' ','
        .

Rootsymbol
        proto.

Endsymbol
        '$end'.


%% TODO: implement verification of references
%% TODO: implement (custom) options: allowed everywhere
%% TODO: implement syntax as in syntax = "proto2"; (must come first)

proto -> elements:                      '$1'.
%% proto -> syntax_def elements:           '$1'.

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

msg_elem -> cardinality type identifier '=' dec_lit ';':
                                        #field{occurrence='$1',
                                               type='$2',
                                               name=identifier_name('$3'),
                                               fnum=literal_value('$5'),
                                               opts=[]}.
msg_elem -> cardinality type identifier '=' dec_lit '[' opt_field_opts ']' ';':
                                        #field{occurrence='$1',
                                               type='$2',
                                               name=identifier_name('$3'),
                                               fnum=literal_value('$5'),
                                               opts='$7'}.
msg_elem -> message_def:                '$1'.
msg_elem -> enum_def:                   '$1'.
msg_elem -> extensions_def:             {extensions,lists:sort('$1')}.

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

cardinality -> required:                required.
cardinality -> optional:                optional.
cardinality -> repeated:                repeated.

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
-include("gpb.hrl").

-export([absolutify_names/1]).
-export([flatten_defs/1]).
-export([verify_refs/1]).
-export([reformat_names/1]).
-export([resolve_refs/1]).
-export([extend_msgs/1]).
-export([enumerate_msg_fields/1]).
-export([normalize_msg_field_options/1]).
-export([fetch_imports/1]).

identifier_name({identifier, _Line, Name}) -> list_to_atom(Name).

literal_value({_TokenType, _Line, Value}) -> Value.

absolutify_names(Defs) ->
    %% FIXME: Search for {package, ...} in Defs,
    %%        use that as initial Path instead of ['.'] ?
    %%        Control this behaviour by an option?
    abs_names_2(['.'], Defs).

abs_names_2(Path, Elems) ->
    lists:map(fun({{msg,Msg}, FieldsOrDefs}) ->
                      MsgPath = prepend_path(Path, Msg),
                      {{msg, MsgPath}, abs_names_2(MsgPath, FieldsOrDefs)};
                 ({{enum,E}, ENs}) ->
                      {{enum, prepend_path(Path, E)}, ENs};
                 (#field{type={ref,To}}=F) ->
                      case is_absolute_ref(To) of
                          true  ->
                              F;
                          false ->
                              FullPath = case refers_to_peer_elem(To, Elems) of
                                             true  -> prepend_path(Path, To);
                                             false -> prepend_path(['.'], To)
                                         end,
                              F#field{type={ref, FullPath}}
                      end;
                 ({extensions,Exts}) ->
                      {{extensions,Path},Exts};
                 ({{extend,Msg}, FieldsOrDefs}) ->
                      MsgPath = prepend_path(Path, Msg),
                      {{extend, MsgPath}, abs_names_2(MsgPath, FieldsOrDefs)};
                 ({package, Name}) ->
                      {package, prepend_path(['.'], Name)};
                 ({{service, Name}, RPCs}) ->
                      {{service,Name}, abs_rpcs(Path, RPCs)};
                 (OtherElem) ->
                      OtherElem
              end,
              Elems).

is_absolute_ref(['.' | _]) -> true;
is_absolute_ref(_Other)    -> false.

refers_to_peer_elem(['.' | Rest], Elems) ->
    refers_to_peer_elem(Rest, Elems);
refers_to_peer_elem([To], Elems) ->
    find_name(To, Elems) /= not_found;
refers_to_peer_elem([To | Rest], Elems) ->
    case find_name(To, Elems) of
        not_found        -> false;
        {found,SubElems} -> refers_to_peer_elem(Rest, SubElems)
    end.

find_name(Name, [{{enum,Name}, _Values} | _]) -> {found, []};
find_name(Name, [{{msg,Name}, SubElems} | _]) -> {found, SubElems};
find_name(Name, [_ | Rest])                   -> find_name(Name, Rest);
find_name(_Name,[])                           -> not_found.

prepend_path(['.'], Id) when is_atom(Id)           -> ['.', Id];
prepend_path(['.'], SubPath) when is_list(SubPath) -> ['.' | SubPath];
prepend_path(Path,  Id) when is_atom(Id)           -> Path ++ ['.', Id];
prepend_path(Path,  SubPath) when is_list(SubPath) -> Path ++ ['.' | SubPath].

abs_rpcs(Path, RPCs) ->
    lists:map(
      fun({RpcName, Arg, Return}) ->
              {RpcName, prepend_path(Path, Arg), prepend_path(Path,Return)}
      end,
      RPCs).

%% `Defs' is expected to be absolutified
flatten_defs(Defs) ->
    lists:reverse(
      lists:foldl(fun({{msg,Name}, FieldsOrDefs}, Acc) ->
                          {Fields2, Defs2} = flatten_fields(FieldsOrDefs),
                          [{{msg,Name},Fields2} | Defs2] ++ Acc;
                     ({{extend,Name}, FieldsOrDefs}, Acc) ->
                          {Fields2, Defs2} = flatten_fields(FieldsOrDefs),
                          [{{extend,Name},Fields2} | Defs2] ++ Acc;
                     (OtherElem, Acc) ->
                          [OtherElem | Acc]
                  end,
                  [],
                  Defs)).

flatten_fields(FieldsOrDefs) ->
    {RFields2, Defs2} =
        lists:foldl(fun(#field{}=F, {Fs,Ds}) -> {[F | Fs], Ds};
                       (Def,        {Fs,Ds}) -> {Fs, flatten_defs([Def])++Ds}
                    end,
                    {[],[]},
                    FieldsOrDefs),
    {lists:reverse(RFields2), Defs2}.



verify_refs(_Defs) ->
    %% FIXME: detect dangling references
    %% FIXME: detect extending of missing messages
    %% FIXME: detect missing rpc service arg or return message references
    ok.

%% `Defs' is expected to be absolutified and flattened
reformat_names(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{enum,Name}, ENs}) ->
                      {{enum,reformat_name(Name)}, ENs};
                 ({{extensions,Name}, Exts}) ->
                      {{extensions,reformat_name(Name)}, Exts};
                 ({{extend,Name}, Fields}) ->
                      {{extend,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{service,Name}, RPCs}) ->
                      {{service,Name}, reformat_rpcs(RPCs)};
                 ({package, Name}) ->
                      {package, reformat_name(Name)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

reformat_fields(Fields) ->
    lists:map(
      fun(#field{type={ref,Nm}}=F) -> F#field{type={ref,reformat_name(Nm)}};
         (#field{}=F)              -> F
      end,
      Fields).

reformat_name(Name) ->
    list_to_atom(string:join([atom_to_list(P) || P <- Name,
                                                 P /= '.'],
                             "_")).

reformat_rpcs(RPCs) ->
    lists:map(fun({RpcName, Arg, Return}) ->
                      {RpcName, reformat_name(Arg), reformat_name(Return)}
              end,
              RPCs).

%% `Defs' is expected to be flattened and may or may not be reformatted
%% `Defs' is expected to be verified, to have no dangling references
resolve_refs(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,Name},
                       lists:map(fun(#field{type={ref,Name2}}=F) ->
                                         Type = fetch_ref(Name2, Defs),
                                         F#field{type=Type};
                                    (#field{}=F) ->
                                         F
                                 end,
                                 Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

fetch_ref(Name, [{{enum,Name}=Key,_} | _]) -> Key;
fetch_ref(Name, [{{msg,Name}=Key,_} | _])  -> Key;
fetch_ref(Name, [_ | T])                   -> fetch_ref(Name, T).


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
    lists:map(fun({I, #field{}=F}) -> F#field{rnum=I} end,
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
    lists:map(fun(#field{opts=Opts}=F) ->
                      F#field{opts=normalize_field_options_2(Opts)}
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

%% `Defs' is expected to be parsed.
fetch_imports(Defs) ->
    [Path || {import,Path} <- Defs].
