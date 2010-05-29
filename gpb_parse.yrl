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
        message_def msg_elems msg_elem
        enum_def enum_fields enum_field
        field_opts field_opt cardinality type
        package_def
        identifiers
        option_def
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
        uint64 sint32 sint64 fixed32
        sfixed32 sfixed64 bool string bytes
        identifier str_lit dec_lit oct_lit hex_lit float_lit bool_lit
        default
        %% import
        option
        %% '(' and ')' for options
        %% 'extensions', 'extend', 'max' and 'to' for extensions
        %% 'service' and 'rpc'
        packed deprecated
        '.' ';' '{' '}' '[' ']' '=' ','
        .

Rootsymbol
        proto.

Endsymbol
        '$end'.


proto -> elements:                      '$1'.

elements -> element elements:           ['$1' | '$2'].
elements -> ';' elements:               '$2'.
elements -> '$empty':                   [].

element -> enum_def:                    '$1'.
element -> message_def:                 '$1'.
%% element -> extend_def:                  '$1'.
%% element -> pimport_def:                 '$1'.
element -> package_def:                 '$1'.
element -> option_def:                  '$1'.

%% pimport -> import str_lit:              {import, '$2'}.

package_def -> package name ';':            {package, '$2'}.

name -> '.' identifiers:                    ['.' | '$2'].
name -> identifiers:                        '$1'.

identifiers -> identifier '.' identifiers:      [identifier_name('$1'), '.'
                                                 | '$3'].
identifiers -> identifier:                      [identifier_name('$1')].

option_def -> option name '=' constant:  {option, '$2', '$4'}.

enum_def -> enum identifier '{' enum_fields '}':
                                        {{enum,identifier_name('$2')},'$4'}.

enum_fields -> enum_field enum_fields:  ['$1' | '$2'].
enum_fields -> enum_field:              ['$1'].

enum_field -> identifier '=' integer ';':
                                        {identifier_name('$1'), '$3'}.

message_def -> message identifier '{' msg_elems '}':
                                        {{msg,identifier_name('$2')},'$4'}.

msg_elems -> msg_elem msg_elems:        ['$1' | '$2'].
msg_elems -> '$empty':                  [].

msg_elem -> cardinality type identifier '=' dec_lit ';':
                                        #field{occurrence='$1',
                                               type='$2',
                                               name=identifier_name('$3'),
                                               fnum=literal_value('$5'),
                                               opts=[]}.
msg_elem -> cardinality type identifier '=' dec_lit '[' field_opts ']' ';':
                                        #field{occurrence='$1',
                                               type='$2',
                                               name=identifier_name('$3'),
                                               fnum=literal_value('$5'),
                                               opts='$7'}.
msg_elem -> message_def:                '$1'.
msg_elem -> enum_def:                   '$1'.

field_opts -> field_opt ',' field_opts: ['$1' | '$2'].
field_opts -> field_opt:                ['$1'].

field_opt -> default '=' constant:      {default, '$3'}.
field_opt -> identifiers:               {'$1', true}.
field_opt -> identifiers '=' constant:  {'$1', '$3'}.
field_opt -> packed:                    {packed, true}.
field_opt -> packed '=' bool_lit:       {packed, literal_value('$3')}.
field_opt -> deprecated:                {deprecated, true}.
field_opt -> deprecated '=' bool_lit:   {'$1', literal_value('$3')}.

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

Erlang code.

-include_lib("eunit/include/eunit.hrl").
-include("gpb.hrl").

-export([absolutify_names/1]).
-export([flatten_defs/1]).
-export([verify_refs/1]).
-export([reformat_names/1]).
-export([resolve_refs/1]).


identifier_name({identifier, _Line, Name}) -> list_to_atom(Name).

literal_value({_TokenType, _Line, Value}) -> Value.

absolutify_names(Defs) ->
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

%% `Defs' is expected to be absolutified
flatten_defs(Defs) ->
    lists:reverse(
      lists:foldl(fun({{msg,Name}, FieldsOrDefs}, Acc) ->
                          {RFields2, Defs2} =
                              lists:foldl(fun(#field{}=F, {Fs,Ds}) ->
                                                  {[F | Fs], Ds};
                                             (Def, {Fs,Ds}) ->
                                                  {Fs, flatten_defs([Def])++Ds}
                                          end,
                                          {[],[]},
                                          FieldsOrDefs),
                          Fields2 = lists:reverse(RFields2),
                          [{{msg,Name},Fields2} | Defs2] ++ Acc;
                     (OtherElem, Acc) ->
                          [OtherElem | Acc]
                  end,
                  [],
                  Defs)).

verify_refs(_Defs) ->
    %% FIXME: detect dangling references
    ok.

%% `Defs' is expected to be absolutified and flattened
reformat_names(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,reformat_name(Name)},
                       lists:map(fun(#field{type={ref,N2}}=F) ->
                                         F#field{type={ref,reformat_name(N2)}};
                                    (#field{}=F) ->
                                         F
                                 end,
                                 Fields)};
                 ({{enum,Name}, ENs}) ->
                      {{enum,reformat_name(Name)}, ENs};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

reformat_name(['.' | Rest]) ->
    list_to_atom(lists:concat([if NPart == '.' -> "_";
                                  true     -> atom_to_list(NPart)
                               end
                               || NPart <- Rest])).

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

%%----------------------------------------------------------------------

parses_simple_msg_test() ->
    {ok, [{{msg,'Msg'}, [#field{name=x, type=uint32, fnum=1,
                                occurrence=optional, opts=[]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1;",
           "}"]).

parses_default_value_test() ->
    {ok, [{{msg,'Msg'}, [#field{name=x, type=uint32, fnum=1,
                                occurrence=optional, opts=[{default,12}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional uint32 x = 1 [default = 12];",
           "}"]).

parses_string_concatenation_test() ->
    {ok, [{{msg,'Msg'}, [#field{name=x, type=string, fnum=1,
                                occurrence=optional,
                                opts=[{default,"abc"}]}]}]} =
        parse_lines(
          ["message Msg {",
           "  optional string x=1 [default='a''b' 'c'];", %% = "abc" hopefully
           "}"]).

parses_nested_messages_test() ->
    {ok, [{{msg,'Msg'}, [{{msg,'Msg2'},[#field{name=x}]},
                         #field{name=y}]}]} =
        parse_lines(
          ["message Msg {",
           "  message Msg2 { optional uint32 x=1; }",
           "  repeated string y=1;",
           "}"]).

parses_enum_def_test() ->
    {ok, [{{enum,e1}, [{ee1,1},{ee2,2}]}]} =
        parse_lines(
          ["enum e1 {",
           "  ee1 = 1;",
           "  ee2 = 2;",
           "}"]).

parses_nested_enum_def_test() ->
    {ok, [{{msg,'Msg'}, [{{enum,e1}, [{ee1,1},{ee2,2}]},
                         #field{name=ef}]}]} =
        parse_lines(
          ["message Msg {"
           "  enum e1 {",
           "    ee1 = 1;",
           "    ee2 = 2;",
           "  }",
           "  required e1 ef = 1;",
           "}"
          ]).

parses_dotted_references_test() ->
    {ok, [{{msg,'Msg3'}, [#field{name=y, type={ref,['Msg','.','Msg2']}}]}]} =
        parse_lines(
          ["message Msg3 {",
           "  repeated Msg.Msg2 y=1;",
           "}"]).


generates_correct_absolute_names_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required .m1.m2 z = 2;",
                               "  required e1     w = 3;",
                               "}",
                               "message m3 {",
                               "  required m1.m2 b = 1;",
                               "}"]),
    [{{msg,['.',m1]}, [{{msg,['.',m1,'.',m2]}, [#field{name=x}]},
                       {{enum,['.',m1,'.',e1]}, [_]},
                       #field{name=y, type={ref,['.',m1,'.',m2]}},
                       #field{name=z, type={ref,['.',m1,'.',m2]}},
                       #field{name=w, type={ref,['.',m1,'.',e1]}}]},
     {{msg,['.',m3]}, [#field{name=b, type={ref,['.',m1,'.',m2]}}]}] =
        lists:sort(absolutify_names(Elems)).

generates_correct_absolute_names_2_test() ->
    {ok, Elems} = parse_lines(["message m2 {",
                               "  message m4 { required uint32 x = 1; }",
                               "}",
                               "message m1 {",
                               "  message m2 {",
                               "    message m3 { required uint32 x = 1; }",
                               "  }",
                               "  required m1.m2 f1 = 1;", %% -> .m1.m2
                               "  required m2.m3 f2 = 2;", %% -> .m1.m2.m3
                               "  required m2.m4 f3 = 3;", %% -> .m2.m4 ???
                               "}"]),
    [{{msg,['.',m1]}, [{{msg,['.',m1,'.',m2]},
                        [{{msg,['.',m1,'.',m2,'.',m3]},_}]},
                       #field{name=f1,type={ref,['.',m1,'.',m2]}},
                       #field{name=f2,type={ref,['.',m1,'.',m2,'.',m3]}},
                       #field{name=f3,type={ref,['.',m2,'.',m4]}}]},
     {{msg,['.',m2]}, _}] =
        lists:sort(absolutify_names(Elems)).

flattens_absolutified_defs_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1;",
                               "               required uint32 y = 2; }",
                               "  required m2 z = 1;",
                               "  required m2 w = 2;",
                               "}"]),
    AElems = absolutify_names(Elems),
    [{{msg,['.',m1]},        [#field{name=z}, #field{name=w}]},
     {{msg,['.',m1,'.',m2]}, [#field{name=x}, #field{name=y}]}] =
        lists:sort(flatten_defs(AElems)).

reformat_names_defs_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required e1     z = 2;",
                               "  required uint32 w = 3;",
                               "}"]),
    [{{enum,m1_e1}, _},
     {{msg,m1},     [#field{name=y, type={ref,m1_m2}},
                     #field{name=z, type={ref,m1_e1}},
                     #field{name=w}]},
     {{msg,m1_m2},  [#field{name=x}]}] =
        lists:sort(reformat_names(flatten_defs(absolutify_names(Elems)))).

resolve_refs_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required e1     z = 2;",
                               "  required uint32 w = 3;",
                               "}",
                               "message m3 {",
                               "  required m1.m2 b = 1;",
                               "}"]),
    [{{enum,m1_e1}, _},
     {{msg,m1},     [#field{name=y, type={msg,m1_m2}},
                     #field{name=z, type={enum,m1_e1}},
                     #field{name=w}]},
     {{msg,m1_m2},  [#field{name=x}]},
     {{msg,m3},     [#field{name=b, type={msg,m1_m2}}]}] =
        lists:sort(
          resolve_refs(reformat_names(flatten_defs(absolutify_names(Elems))))).

%% helper
parse_lines(Lines) ->
    {ok, Tokens, _} = gpb_scan:string(
                        binary_to_list(
                          iolist_to_binary([[Line,"\n"] || Line <- Lines]))),
    ?MODULE:parse(Tokens++[{'$end',99}]).
