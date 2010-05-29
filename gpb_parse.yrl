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

-export([parse_lines/1]).

identifier_name({identifier, _Line, Name}) -> list_to_atom(Name).

literal_value({_TokenType, _Line, Value}) -> Value.

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


-record(a@b@c, {xyz}).
%% helper
parse_lines(Lines) ->
    {ok, Tokens, _} = gpb_scan:string(
                        binary_to_list(
                          iolist_to_binary([[Line,"\n"] || Line <- Lines]))),
    ?MODULE:parse(Tokens++[{'$end',99}]).
