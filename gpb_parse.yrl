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
        service_def rpc_defs rpc_def
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
        import
        option
        %% '(' and ')' for custom options
        extensions extend max to
        service rpc returns
        packed deprecated
        '.' ';' '(' ')' '{' '}' '[' ']' '=' ','
        .

Rootsymbol
        proto.

Endsymbol
        '$end'.


%% TODO: implement generation/formatting of records               -> .hrl
%% TODO: implement generation/formatting of msg+enum descriptions -> .erl
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
enum_fields -> enum_field:              ['$1'].

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
rpc_defs -> '$empty':                   [].

rpc_def -> rpc identifier '(' name ')' returns  '(' name ')' ';':
                                        {identifier_name('$2'), '$4', '$8'}.

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

parses_package_test() ->
    {ok, [{package,[p1,'.',p2]}, {{enum,e1}, _}]} =
        parse_lines(["package p1.p2;",
                     "enum e1 { a = 1; }"]).

parses_import_test() ->
    {ok, [{package,[p1,'.',p2]}, {import, "a/b/c.proto"}]} =
        parse_lines(["package p1.p2;",
                     "import \"a/b/c.proto\";"]).

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
    {ok, Elems} = parse_lines(["package p1;"
                               "import \"a/b/c.proto\";",
                               "message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 1;",
                               "  required e1     z = 2;",
                               "  required uint32 w = 3;",
                               "}",
                               "message m3 {",
                               "  required m1.m2 b = 1;",
                               "}"]),
    [{import, _},
     {package, p1},
     {{enum,m1_e1}, _},
     {{msg,m1},     [#field{name=y, type={msg,m1_m2}},
                     #field{name=z, type={enum,m1_e1}},
                     #field{name=w}]},
     {{msg,m1_m2},  [#field{name=x}]},
     {{msg,m3},     [#field{name=b, type={msg,m1_m2}}]}] =
        lists:sort(
          resolve_refs(reformat_names(flatten_defs(absolutify_names(Elems))))).

enumerates_msg_fields_test() ->
    {ok, Elems} = parse_lines(["message m1 {"
                               "  message m2 { required uint32 x = 1; }",
                               "  enum    e1 { a = 17; }",
                               "  required m2     y = 11;",
                               "  required e1     z = 12;",
                               "}"]),
    [{{enum,m1_e1}, _},
     {{msg,m1},     [#field{name=y, fnum=11, rnum=2},
                     #field{name=z, fnum=12, rnum=3}]},
     {{msg,m1_m2},  [#field{name=x, fnum=1, rnum=2}]}] =
        lists:sort(
          enumerate_msg_fields(
            resolve_refs(
              reformat_names(flatten_defs(absolutify_names(Elems)))))).

field_opt_normalization_test() ->
    {ok,Defs} = parse_lines(["message m1 {"
                             "  required uint32 f1=1 [packed=true,default=1];",
                             "  required uint32 f2=2 [packed=false];",
                             "  required uint32 f3=3 [packed,default=2];",
                             "  required uint32 f4=4 [deprecated=true];",
                             "  required uint32 f5=5 [deprecated=false];",
                             "  required uint32 f6=5 [deprecated];",
                             "  required bool   f7=7 [packed,default=true];",
                             "}"]),
    [{{msg,m1}, [#field{name=f1, opts=[packed, {default,1}]},
                 #field{name=f2, opts=[]},
                 #field{name=f3, opts=[packed, {default,2}]},
                 #field{name=f4, opts=[deprecated]},
                 #field{name=f5, opts=[]},
                 #field{name=f6, opts=[deprecated]},
                 #field{name=f7, opts=[packed, {default,true}]}]}] =
        normalize_msg_field_options(
          enumerate_msg_fields(
            resolve_refs(
              reformat_names(flatten_defs(absolutify_names(Defs)))))).


parses_empty_msg_field_options_test() ->
    {ok,Defs} = parse_lines(["message m1 { required uint32 f1=1 []; }"]),
    [{{msg,m1}, [#field{name=f1, opts=[]}]}] =
        normalize_msg_field_options(
          enumerate_msg_fields(
            resolve_refs(
              reformat_names(flatten_defs(absolutify_names(Defs)))))).

parses_and_ignores_enum_field_options_test() ->
    {ok,_Defs} = parse_lines(["enum e1 { a=1 [x=y]; }"]).

parses_and_ignores_empty_enum_field_options_test() ->
    {ok,_Defs} = parse_lines(["enum e1 { a=1 []; }"]).

parses_msg_extensions_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1;",
                             "  extensions 100 to 199, 300, 400 to max, 250;",
                             "  extensions 251, 252;",
                             "  message m2 {",
                             "    required uint32 f2=2;",
                             "    extensions 233;",
                             "  }",
                             "}"]),
    [{{extensions,m1},[{100,199},{250,250},{300,300},{400,max}]},
     {{extensions,m1},[{251,251},{252,252}]},
     {{extensions,m1_m2},[{233,233}]},
     {{msg,m1},    [#field{name=f1}]},
     {{msg,m1_m2}, [#field{name=f2}]}] =
        lists:sort(
          normalize_msg_field_options(
            enumerate_msg_fields(
              resolve_refs(
                reformat_names(
                  flatten_defs(
                    absolutify_names(Defs))))))).

parses_extending_msgs_test() ->
    {ok,Defs} = parse_lines(["message m1 {",
                             "  required uint32 f1=1 [default=17];",
                             "  extensions 200 to 299;",
                             "}",
                             "extend m1 {",
                             "  optional uint32 f2=2;",
                             "}"]),
    [{{extensions,m1},[{200,299}]},
     {{msg,m1},       [#field{name=f1, fnum=1, rnum=2, opts=[{default,17}],
                              occurrence=required},
                       #field{name=f2, fnum=2, rnum=3, opts=[],
                              occurrence=optional}]}] =
        lists:sort(
          normalize_msg_field_options(
            enumerate_msg_fields(
              extend_msgs(
                resolve_refs(
                  reformat_names(
                    flatten_defs(
                      absolutify_names(Defs)))))))).

parses_service_test() ->
    {ok,Defs} = parse_lines(["message m1 {required uint32 f1=1;}",
                             "message m2 {required uint32 f2=1;}",
                             "service s1 {",
                             "  rpc req(m1) returns (m2);",
                             "}"]),
    [{{msg,m1}, _},
     {{msg,m2}, _},
     {{service,s1},[{req,m1,m2}]}] =
        lists:sort(
          normalize_msg_field_options(
            enumerate_msg_fields(
              extend_msgs(
                resolve_refs(
                  reformat_names(
                    flatten_defs(
                      absolutify_names(Defs)))))))).



fetches_imports_test() ->
    {ok, Elems} = parse_lines(["package p1;"
                               "import \"a/b/c.proto\";",
                               "import 'd/e/f.proto';",
                               "message m1 { required uint32 x = 1; }",
                               "enum    e1 { a = 17; }"]),
    ["a/b/c.proto", "d/e/f.proto"] = fetch_imports(Elems).

%% helper
parse_lines(Lines) ->
    S = binary_to_list(iolist_to_binary([[L,"\n"] || L <- Lines])),
    case gpb_scan:string(S) of
        {ok, Tokens, _} ->
            case ?MODULE:parse(Tokens++[{'$end',length(Lines)+1}]) of
                {ok, Result} ->
                    {ok, Result};
                {error, {LNum,_Module,EMsg}=Reason} ->
                    io:format(user, "Parse error on line ~w:~n  ~p~n",
                              [LNum, {Tokens,EMsg}]),
                    erlang:error({parse_error,Lines,Reason})
            end;
        {error,Reason} ->
            io:format(user, "Scan error:~n  ~p~n", [Reason]),
            erlang:error({scan_error,Lines,Reason})
    end.
