%%% Copyright (C) 2017  Tomas Abrahamsson
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

%%% @doc Helper functions for the code-generator module
%%% @private

-module(gpb_lib).

-include("gpb_compile.hrl").

-export([mk_fn/2, mk_fn/3]).
-export([replace_term/2]).
-export([replace_tree/2]).
-export([splice_trees/2]).
-export([repeat_clauses/2]).

-export([msgs_or_groups/1]).
-export([msg_or_group_names/1]).
-export([msg_names/1]).
-export([get_field_name/1]).
-export([get_field_rnum/1]).
-export([get_field_occurrence/1]).
-export([unalias_enum/1]).

-export([mapping_match/3]).
-export([mapping_create/3]).
-export([mapping_update/4]).
-export([record_match/2]).
-export([record_create/2]).
-export([record_create_or_match/2]).
-export([record_update/3]).
-export([map_match/1]).
-export([map_create/1]).
-export([map_update/2]).
-export([map_set/2]).

-export([get_2tuples_or_maps_for_maptype_fields_by_opts/1]).
-export([get_records_or_maps_by_opts/1]).
-export([get_mapping_and_unset_by_opts/1]).
-export([get_strings_as_binaries_by_opts/1]).
-export([get_type_specs_by_opts/1]).
-export([get_gen_descriptor_by_opts/1]).
-export([get_field_format_by_opts/1]).
-export([mk_get_defs_as_maps_or_records_fn/1]).
-export([get_defs_as_maps_or_records/1]).
-export([is_target_major_version_at_least/2]).

-export([index_seq/1]).
-export([indent/2, indent_lines/2]).
-export([outdent_first/1]).
-export([split_indent_iolist/2]).
-export([linesplit_iolist/1]).
-export([iolist_to_utf8_or_escaped_binary/2]).

-include("../include/gpb.hrl").


mk_fn(Prefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Suffix])).

mk_fn(Prefix, Middlefix, Suffix) when is_integer(Middlefix) ->
    mk_fn(Prefix, list_to_atom(integer_to_list(Middlefix)), Suffix);
mk_fn(Prefix, Middlefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Middlefix, "_", Suffix])).

%% Helpers for gpb_codegen parse tree transform operations -----------
replace_term(Marker, NewTerm) when is_atom(Marker) ->
    {replace_term, Marker, NewTerm}.

replace_tree(Marker, NewTree) when is_atom(Marker) ->
    {replace_tree, Marker, NewTree}.

splice_trees(Marker, Trees) when is_atom(Marker) ->
    {splice_trees, Marker, Trees}.

repeat_clauses(Marker, RepetitionReplacements) ->
    {repeat_clauses, Marker, RepetitionReplacements}.

%% Various accessors -----

msgs_or_groups(Defs) ->
    [{Type,Name,Fields} || {{Type,Name},Fields} <- Defs,
                           Type =:= msg orelse Type =:= group].

msg_or_group_names(Defs) ->
    [Name || {_Type, Name, _Fields} <- msgs_or_groups(Defs)].

msg_names(Defs) ->
    [Name || {{msg, Name}, _Fields} <- Defs].

get_field_name(#?gpb_field{name=FName}) -> FName;
get_field_name(#gpb_oneof{name=FName})  -> FName.

get_field_rnum(#?gpb_field{rnum=RNum}) -> RNum;
get_field_rnum(#gpb_oneof{rnum=RNum})  -> RNum.

get_field_occurrence(#?gpb_field{occurrence=Occurrence}) -> Occurrence;
get_field_occurrence(#gpb_oneof{})                       -> optional.

%% The "option allow_alias = true;" inside an enum X { ... }
%% says it is ok to have multiple symbols that map to the same numeric value.
%% Appeared in protobuf 2.5.0.
unalias_enum([{_Sym,Value}=Enum | Rest]) ->
    [Enum | unalias_enum([E || {_,V}=E <- Rest, V /= Value])];
unalias_enum([{option,_Name,_Value} | Rest]) ->
    unalias_enum(Rest);
unalias_enum([]) ->
    [].

%% Record or map expr helpers --------

%% a mapping is either a record or a map
%%
%%
mapping_match(RName, Fields, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> record_match(RName, Fields);
        maps    -> map_match(Fields)
    end.

mapping_create(RName, Fields, Opts) when is_list(Opts) ->
    Fn = fun() -> get_records_or_maps_by_opts(Opts) end,
    mapping_create(RName, Fields, Fn);
mapping_create(RName, Fields, RecordsOrMaps) when is_function(RecordsOrMaps) ->
    case RecordsOrMaps() of
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



%% Option helpers ---------------

get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) ->
    Default = false,
    case proplists:get_value(mapfields_as_maps, Opts, Default) of
        true  -> maps;
        false -> '2tuples'
    end.

get_records_or_maps_by_opts(Opts) ->
    Default = false,
    case proplists:get_value(msgs_as_maps, Opts, Default) of
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

get_strings_as_binaries_by_opts(Opts) ->
    proplists:get_bool(strings_as_binaries, Opts).

get_type_specs_by_opts(Opts) ->
    Default = false,
    proplists:get_value(type_specs, Opts, Default).

get_gen_descriptor_by_opts(Opts) ->
    proplists:get_bool(descriptor, Opts).

get_field_format_by_opts(Opts) ->
    case proplists:get_bool(defs_as_proplists, proplists:unfold(Opts)) of
        false -> %% default
            case get_defs_as_maps_or_records(Opts) of
                records -> fields_as_records;
                maps    -> fields_as_maps
            end;
        true ->
            fields_as_proplists
    end.

mk_get_defs_as_maps_or_records_fn(Opts) ->
    fun() -> get_defs_as_maps_or_records(Opts) end.

get_defs_as_maps_or_records(Opts) ->
    Default = false,
    case proplists:get_value(defs_as_maps, Opts, Default) of
        false -> records;
        true  -> maps
    end.

is_target_major_version_at_least(VsnMin, Opts) ->
    case proplists:get_value(target_erlang_version, Opts, current) of
        current ->
            is_current_major_version_at_least(VsnMin);
        N when is_integer(N) ->
            N >= VsnMin
    end.

is_current_major_version_at_least(VsnMin) ->
    case erlang:system_info(otp_release) of
        "R"++Rest -> % R16 or ealier
            FirstChunkOfDigits = lists:takewhile(fun is_digit/1, Rest),
            list_to_integer(FirstChunkOfDigits) >= VsnMin;
        RelStr ->
            %% In Erlang 17 the leading "R" was dropped
            %% The exact format isn't super documented,
            %% so be prepared for some (future?) alternatives.
            try list_to_integer(RelStr) of
                N when is_integer(N) -> N >= VsnMin
            catch error:badarg ->
                    [NStr | _] = string:tokens(RelStr, ".-"),
                    try list_to_integer(NStr) of
                        N when is_integer(N) -> N >= VsnMin
                    catch error:badarg ->
                            false
                    end
            end
    end.

is_digit(C) when $0 =< C, C =< $9 -> true;
is_digit(_) -> false.

%% Misc ---

index_seq([]) -> [];
index_seq(L)  -> lists:zip(lists:seq(1,length(L)), L).

indent(Indent, Str) ->
    lists:duplicate(Indent, $\s) ++ Str.

outdent_first(IoList) ->
    lists:dropwhile(fun(C) -> C == $\s end,
                    binary_to_list(iolist_to_binary(IoList))).

indent_lines(Indent, Lines) ->
    [indent(Indent, Line) || Line <- Lines].

split_indent_iolist(Indent, IoList) ->
    [if Line == <<>> -> "\n"; %% don't indent empty lines
        true -> [indent(Indent, Line), "\n"]
     end
     || Line <- linesplit_iolist(IoList)].

linesplit_iolist(Iolist) ->
    re:split(Iolist, ["\n"], [trim, {return,binary}]).

iolist_to_utf8_or_escaped_binary(IoList, Opts) ->
    case understands_coding(Opts) of
        true  ->
            unicode:characters_to_binary(
              ["%% -*- coding: utf-8 -*-\n",
               IoList]);
        false ->
            %% What to do if on Erlang R15 or earlier?  We can't utf8-encode
            %% the file, because Erlang R15 will read it as latin1.
            %%
            %% For now, Assume such encodings are in strings only.
            %% So far, this is safe, since neither message names nor field
            %% names nor enum symbols are allowed to be non-ascii.
            %%
            %% This means only place for non-ascii is in comments and
            %% in default strings. Hope I haven't overlooked some
            %% important place...
            iolist_to_binary(esc_non_ascii(IoList))
    end.

understands_coding(Opts) ->
    %% version   coding: X             default source encoding
    %% R15:      ignores               latin1
    %% R16:      understands           latin1
    %% 17:       understands           utf-8
    is_target_major_version_at_least(16, Opts).

esc_non_ascii([H|T]) -> [esc_non_ascii(H) | esc_non_ascii(T)];
esc_non_ascii([])    -> [];
esc_non_ascii(B) when is_binary(B) -> B;
esc_non_ascii(C) when is_integer(C), C =< 127 -> C;
esc_non_ascii(C) when is_integer(C), C > 127  -> ?f("\\x{~.16b}", [C]).
