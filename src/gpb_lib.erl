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

-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").
-include("../include/gpb.hrl").

-export([mk_fn/2, mk_fn/3]).
-export([replace_term/2]).
-export([replace_tree/2]).
-export([splice_trees/2]).
-export([repeat_clauses/2]).

-export([msgs_or_groups/1]).
-export([msg_or_group_names/1]).
-export([msg_names/1]).
-export([enum_names/1]).
-export([contains_messages/1]).
-export([contains_groups/1]).
-export([get_field_name/1, get_field_names/1]).
-export([get_field_rnum/1]).
-export([get_field_occurrence/1]).
-export([map_type_to_msg_name/2]).
-export([unalias_enum/1]).
-export([zip_for_non_opt_fields/2]).
-export([any_field_is_sub_msg/1]).
-export([any_field_is_repeated/1]).
-export([any_enum_field_exists/1]).
-export([any_packed_field_exists/1]).
-export([at_least_one_submsg_with_size_not_known_at_compile_time_exists/1]).
-export([get_field_pass/2]).
-export([get_num_fields/2]).
-export([is_packed/1]).
-export([key_partition_on_optionality/2, key_partition_on_optionality/3]).
-export([classify_field_merge_action/1]).
-export([flatten_oneof_fields/1]).
-export([get_field_json_name/1]).
-export([is_field_for_unknowns/1]).
-export([defs_contains_fields_for_unknows/1]).

-export([fold_msg_fields/3]).
-export([fold_msg_or_group_fields_skip_field_for_unknowns/3]).
-export([fold_msg_or_group_fields/3]).
-export([fold_msg_or_group_fields_o/3]).
-export([fold_msgdef_fields/3]).
-export([fold_msgdef_fields_o/3]).
-export([mapfold_msg_or_group_fields_o/3]).
-export([mapfold_msgdef_fields_o/3]).
-export([map_msg_fields_o/2]).
-export([map_msgdef_fields_o/2]).

-export([mapping_match/3]).
-export([mapping_create/3]).
-export([mapping_create/4]).
-export([mapping_update/4]).
-export([record_match/2]).
-export([record_create/2]).
-export([record_update/3]).
-export([replace_map_key/3]).
-export([map_match/2]).
-export([map_create/2]).
-export([map_set/3]).
-export([term_mapping/3]).

-export([normalize_opts/1]).
-export([get_2tuples_or_maps_for_maptype_fields_by_opts/1]).
-export([get_mapping_by_opts/1]).
-export([get_mapping_and_unset_by_opts/1]).
-export([get_strings_as_binaries_by_opts/1]).
-export([get_type_specs_by_opts/1]).
-export([get_gen_descriptor_by_opts/1]).
-export([get_field_format_by_opts/1]).
-export([mk_get_defs_format_fn/1]).
-export([get_defs_format/1]).
-export([get_epb_functions_by_opts/1]).
-export([get_bypass_wrappers_by_opts/1]).
-export([get_enum_macros_by_opts/1]).
-export([is_target_major_version_at_least/2]).
-export([target_has_nifs_directive/1]).
-export([current_otp_release/0]).
-export([proto2_type_default/3]).
-export([proto3_type_default/3]).
-export([get_maps_key_type_by_opts/1]).
-export([json_by_opts/1]).
-export([json_object_format_by_opts/1]).
-export([json_key_format_by_opts/1]).
-export([json_array_format_by_opts/1]).
-export([json_string_format_by_opts/1]).
-export([json_null/1]).
-export([get_gen_mergers/1]).
-export([get_gen_introspect/1]).
-export([get_gen_verifiers/1]).
-export([get_gen_encoders/1]).
-export([get_gen_decoders/1]).
-export([possibly_adjust_proto_defs_version_opt/1]).

-export([var_f_n/1]).
-export([var_b_n/1]).
-export([var_j_n/1]).
-export([var_n/2]).
-export([var/2]).
-export([prefix_var/2]).
-export([assign_to_var/2]).
-export([match_bind_var/2]).
-export([varint_to_binary_fields/1]).
-export([do_exprs/3]).

-export([index_seq/1]).
-export([smember/2, smember_any/2]).
-export([indent/2, indent_lines/2]).
-export([outdent_first/1]).
-export([split_indent_iolist/2]).
-export([split_indent_butfirst_iolist/2]).
-export([cond_split_indent_iolist/3]).
-export([nowarn_unused_function/2]).
-export([nowarn_dialyzer_attr/2]).
-export([no_underspecs_dialyzer_attr/3]).

-export([drop_filename_ext/1]).
-export([copy_filename_ext/2]).
-export([basenameify_ish/1]).

-export([comma_join/1]).
-export([nl_join/1]).
-export([or_join/1]).
-export([dot_join/1]).
-export([string_join/2]).
-export([is_substr/2]).
-export([string_slice/2]).
-export([string_lexemes/2]).
-export([lowercase/1]).
-export([uppercase/1]).
-export([snake_case/1]).
-export([old_snake_case/1]).
-export([camel_case/1]).
-export([lower_camel_case/1]).

-export([ljoin/2]).

-export([maps_merge_with/3]).

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

enum_names(Defs) ->
    [Name || {{enum, Name}, _Syms} <- Defs].

contains_messages(Defs) ->
    lists:any(fun({{msg, _}, _}) -> true;
                 (_)             -> false
              end,
              Defs).

contains_groups(Defs) ->
    lists:any(fun({{group, _}, _}) -> true;
                 (_)               -> false
              end,
              Defs).

get_field_names(MsgDef) ->
    [get_field_name(Field) || Field <- MsgDef].

get_field_name(#?gpb_field{name=FName}) -> FName;
get_field_name(#gpb_oneof{name=FName})  -> FName.

get_field_rnum(#?gpb_field{rnum=RNum}) -> RNum;
get_field_rnum(#gpb_oneof{rnum=RNum})  -> RNum.

get_field_occurrence(#?gpb_field{occurrence=Occurrence}) -> Occurrence;
get_field_occurrence(#gpb_oneof{})                       -> optional.

map_type_to_msg_name(KeyType, {msg,MsgName}) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, MsgName]));
map_type_to_msg_name(KeyType, {enum,EnumName}) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, EnumName]));
map_type_to_msg_name(KeyType, ValueType) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, ValueType])).

%% The "option allow_alias = true;" inside an enum X { ... }
%% says it is ok to have multiple symbols that map to the same numeric value.
%% Appeared in protobuf 2.5.0.
unalias_enum([{_Sym, Value, _Opt}=Enum | Rest]) ->
    [Enum | unalias_enum([E || {_,V,_}=E <- Rest, V /= Value])];
unalias_enum([]) ->
    [].

zip_for_non_opt_fields([#?gpb_field{name=FName,
                                    occurrence=Occurrence} | FRest],
                       [Elem | ERest]) ->
    case Occurrence of
        optional -> zip_for_non_opt_fields(FRest, ERest);
        defaulty -> zip_for_non_opt_fields(FRest, ERest);
        required -> [{FName, Elem} | zip_for_non_opt_fields(FRest, ERest)];
        repeated -> zip_for_non_opt_fields(FRest, ERest)
    end;
zip_for_non_opt_fields([#gpb_oneof{} | FRest], [_Elem | ERest]) ->
    zip_for_non_opt_fields(FRest, ERest);
zip_for_non_opt_fields([], []) ->
    [].

any_field_is_sub_msg(Fields) ->
    lists:any(fun(#?gpb_field{type={msg,_}}) -> true;
                 (#?gpb_field{type={group,_}}) -> true;
                 (#?gpb_field{type={map,_,_}}) -> true;
                 (#gpb_oneof{fields=Fs}) -> any_field_is_sub_msg(Fs);
                 (_) -> false
              end,
              Fields).

any_field_is_repeated(Fields) ->
    lists:any(fun(#?gpb_field{occurrence=Occ}) -> Occ == repeated;
                 (#gpb_oneof{}) -> false
              end,
              Fields).

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
    IsMsgSizeUnknown = fun(Nm) -> maps:get(Nm, KnownSize) == undefined end,
    lists:any(IsMsgSizeUnknown, SubMsgNames) orelse
        lists:any(IsMsgSizeUnknown, MapMsgNames).

get_field_pass(MsgName, #anres{d_field_pass_method=M}) ->
    #{MsgName := FieldPass} = M,
    FieldPass.

get_num_fields(MsgName, #anres{num_fields=M}) ->
    #{MsgName := NumFields} = M,
    NumFields.

is_packed(#?gpb_field{type=Type, opts=Opts}=Field) ->
    case is_field_for_unknowns(Field) of
        false -> gpb:is_type_packable(Type) andalso lists:member(packed, Opts);
        true  -> false % these are never packed
    end.

is_maptype_field(#?gpb_field{type={map,_,_}}) -> true;
is_maptype_field(_) -> false.

%% -> {Optionals, NonOptionals}
key_partition_on_optionality(Key, Items) ->
    key_partition_on_optionality(Key, Items, []).
key_partition_on_optionality(Key, Items, Opts) ->
    lists:partition(
      fun(Item) ->
              Field = element(Key, Item),
              case get_field_occurrence(Field) of
                  optional -> true;
                  defaulty -> true;
                  required -> false;
                  repeated -> case mapfields_considered_required(Opts) of
                                  false -> true;
                                  true  -> not is_maptype_field(Field)
                              end
              end
      end,
      Items).

mapfields_considered_required(Opts) ->
    proplists:get_bool(mapfields_are_required, Opts).

classify_field_merge_action(FieldDef) ->
    case FieldDef of
        #?gpb_field{occurrence=required, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=optional, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=defaulty, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=required, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=optional, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=defaulty, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=required}                  -> overwrite;
        #?gpb_field{occurrence=optional}                  -> overwrite;
        #?gpb_field{occurrence=defaulty}                  -> overwrite;
        #?gpb_field{occurrence=repeated}                  -> seqadd
    end.

flatten_oneof_fields([#?gpb_field{}=F | Rest]) ->
    [F | flatten_oneof_fields(Rest)];
flatten_oneof_fields([#gpb_oneof{fields=OFields} | Rest]) ->
    OFields ++ flatten_oneof_fields(Rest);
flatten_oneof_fields([]) ->
    [].

get_field_json_name(#?gpb_field{name=FName, opts=Opts}) ->
    case proplists:get_value(json_name, Opts) of
        undefined ->
            lower_camel_case(atom_to_list(FName));
        Name when is_list(Name) ->
            Name
    end.

is_field_for_unknowns(#?gpb_field{type=unknown, occurrence=repeated}) ->
    true;
is_field_for_unknowns(_) ->
    false.

defs_contains_fields_for_unknows(Defs) ->
    {HasWithNoUnknowns, HasWithUnknowns} =
        lists:foldl(
          fun({{msg,_Name}, Fields}, {HasWithNoUnknowns, HasWithUnknonws}) ->
                  case lists:any(fun is_field_for_unknowns/1, Fields) of
                      true  -> {HasWithNoUnknowns, true};
                      false -> {true, HasWithUnknonws}
                  end;
             (_, Acc) ->
                  Acc
          end,
          {false, false},
          Defs),
    case {HasWithNoUnknowns, HasWithUnknowns} of
        {true,  true}  -> both_with_and_without_field_for_unknowns;
        {true,  false} -> all_are_without_field_for_unknowns;
        {false, true}  -> all_are_with_field_for_unknowns;
        {false, false} -> no_msgs
    end.

%% Msg iteration --------

%% Loop over all message fields, including oneof-fields
%% Call Fun for all #?gpb_fields{}, skip over non-msg defs
fold_msg_fields(Fun, InitAcc, Defs) ->
    fold_msg_fields_o(
      fun(MsgName, Field, _IsOneOf, Acc) -> Fun(MsgName, Field, Acc) end,
      InitAcc,
      Defs).

fold_msg_or_group_fields_skip_field_for_unknowns(F, InitAcc, Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(Type, Name, Field, Acc) ->
              case gpb_lib:is_field_for_unknowns(Field) of
                  true  -> Acc; % skip
                  false -> F(Type, Name, Field, Acc)
              end
      end,
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

%% MFFun takes 5 args:
%% MFFun(msg | group, MsgName, Field, IsOneof, Acc) -> {Field1, Acc1}
mapfold_msg_or_group_fields_o(MFFun, Acc0, Defs) ->
    lists:mapfoldl(
      fun({{Type, Name}, Fields}, Acc) when Type =:= msg;
                                            Type =:= group ->
              MFFun1 = fun(Field, IsOneof, FAcc) ->
                               MFFun(Type, Name, Field, IsOneof, FAcc)
                       end,
              {Fields1, AccOut} = mapfold_msgdef_fields_o(MFFun1, Acc, Fields),
              MsgOrGroup1 = {{Type, Name}, Fields1},
              {MsgOrGroup1, AccOut};
         (Other, Acc) ->
              {Other, Acc}
      end,
      Acc0,
      Defs).

%% MFFun takes 4 args:
%% MFFun(Field, IsOneof, Acc) -> {Field1, Acc1}
mapfold_msgdef_fields_o(MFFun, Acc0, Fields) ->
    lists:mapfoldl(
      fun(#?gpb_field{}=Field, Acc) ->
              MFFun(Field, false, Acc);
         (#gpb_oneof{name=CFName, fields=OFields}=Field, Acc)->
              {OFields1, AccOut} =
                  lists:mapfoldl(
                    fun(OField, Acc2) -> MFFun(OField, {true, CFName}, Acc2)
                    end,
                    Acc,
                    OFields),
              {Field#gpb_oneof{fields = OFields1}, AccOut}
      end,
      Acc0,
      Fields).

%% The fun takes 4 args: Fun(Msgname, #?gpb_field{}=F1, IsOneof) -> F2
map_msg_fields_o(Fun, Defs) ->
    lists:map(
      fun({{msg, MsgName}, Fields}) ->
              FFun = fun(Field, IsOneOf) ->
                             Fun(MsgName, Field, IsOneOf)
                     end,
              Fields1 = map_msgdef_fields_o(FFun, Fields),
              {{msg, MsgName}, Fields1};
         (OtherElem) ->
              OtherElem
      end,
      Defs).

map_msgdef_fields_o(FFun, Fields) ->
    lists:map(
      fun(#?gpb_field{}=Field) ->
              FFun(Field, false);
         (#gpb_oneof{name=CFName, fields=OFields}=Field)->
              OFields1 = lists:map(
                           fun(OField) -> FFun(OField, {true, CFName})
                           end,
                           OFields),
              Field#gpb_oneof{fields = OFields1}
      end,
      Fields).

%% Record or map expr helpers --------

%% a mapping is either a record or a map
%%
%%
mapping_match(RName, Fields, Opts) ->
    case get_mapping_by_opts(Opts) of
        records -> record_match(RName, Fields);
        natrecs  -> record_match(RName, Fields);
        maps    -> map_match(Fields, Opts)
    end.

mapping_create(RName, Fields, Opts) when is_list(Opts) ->
    Fn = fun() -> get_mapping_by_opts(Opts) end,
    mapping_create(RName, Fields, Fn, Opts).

mapping_create(RName, Fields, RecordsOrMaps, Opts)
  when is_function(RecordsOrMaps) ->
    case RecordsOrMaps() of
        records -> record_create(RName, Fields);
        natrecs  -> record_create(RName, Fields);
        maps    -> map_create(Fields, Opts)
    end.

mapping_update(Var, RName, FieldsValues, Opts) ->
    case get_mapping_by_opts(Opts) of
        records ->
            record_update(Var, RName, FieldsValues);
        natrecs ->
            record_update(Var, RName, FieldsValues);
        maps ->
            case get_mapping_and_unset_by_opts(Opts) of
                #maps{unset_optional=present_undefined} ->
                    map_update(Var, FieldsValues, Opts);
                #maps{unset_optional=omitted} ->
                    map_set(Var, FieldsValues, Opts)
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
replace_map_key(Marker, Key, Opts) when is_atom(Key) ->
    case get_maps_key_type_by_opts(Opts) of
        atom ->
            replace_term(Marker, Key);
        binary ->
            replace_tree(
              Marker,
              erl_syntax:binary(
                [erl_syntax:binary_field(
                   erl_syntax:string(atom_to_list(Key)))]))
    end;
replace_map_key(Marker, KeyExpr, Opts) ->
    case get_maps_key_type_by_opts(Opts) of
        atom ->
            replace_tree(Marker, KeyExpr);
        binary ->
            replace_tree(
              Marker,
              erl_syntax:binary(
                [erl_syntax:binary_field(
                   erl_syntax:application(erl_syntax:atom(erlang),
                                          erl_syntax:atom(atom_to_list),
                                          [KeyExpr]))]))
    end.

map_match(Fields, Opts) ->
    Literal = mapkey_literal_by_opts(Opts),
    erl_syntax:map_expr(
      [erl_syntax:map_field_exact(Literal(FName), Expr)
       || {FName, Expr} <- Fields]).

map_create(Fields, Opts) ->
    map_set(none, Fields, Opts).

map_update(Var, [], _Opts) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_update(Var, FieldsValueTrees, Opts) ->
    Literal = mapkey_literal_by_opts(Opts),
    erl_syntax:map_expr(
      Var,
      [erl_syntax:map_field_exact(Literal(FName), Expr)
       || {FName, Expr} <- FieldsValueTrees]).

map_set(Var, [], _Opts) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_set(Var, FieldsValueTrees, Opts) ->
    Literal = mapkey_literal_by_opts(Opts),
    ExprF = mapkey_expr_by_opts(Opts),
    erl_syntax:map_expr(
      Var,
      [if is_atom(FName) ->
               erl_syntax:map_field_assoc(Literal(FName), Expr);
          true -> % Key can be a variable or other type too.
               erl_syntax:map_field_assoc(ExprF(FName), Expr)
       end
       || {FName, Expr} <- FieldsValueTrees]).

mapkey_literal_by_opts(Opts) ->
    case get_maps_key_type_by_opts(Opts) of
        atom ->
            fun(Atom) -> erl_syntax:atom(Atom) end;
        binary ->
            fun(Atom) ->
                    Cs = atom_to_list(Atom),
                    erl_syntax:binary(
                           [erl_syntax:binary_field(
                              erl_syntax:integer(C)) || C <- Cs])
            end
    end.

mapkey_expr_by_opts(Opts) ->
    case get_maps_key_type_by_opts(Opts) of
        atom ->
            fun(Expr) -> Expr end;
        binary ->
            fun(Expr) ->
                    erl_syntax:application(erl_syntax:atom(erlang),
                                           erl_syntax:atom(atom_to_binary),
                                           [Expr, erl_syntax:atom(utf8)])
            end
    end.

%% Return an abstract syntax tree for the term, such that
%% when formatting it to text using erl_prettypr:format, records in `Records'
%% gets formatted as either record expressions, map expressions or proplists
%% according to Opts. In particular, they do not become tuple expressions.
%% This applies also recursively.
-spec term_mapping(term(), Records, gpb_compile:opts()) -> Result when
      Records :: #{RecordName::atom() => [FieldName::atom()]},
      Result  :: erl_syntax:syntaxTree().
term_mapping(List, Records, Opts) when is_list(List) ->
    erl_syntax:list(
      [term_mapping(Elem, Records, Opts) || Elem <- List]);
term_mapping(Record, Records, Opts)
  when is_tuple(Record),
       tuple_size(Record) >= 1,
       is_map_key(element(1, Record), Records) ->
    RName = element(1, Record),
    [RName | RValues] = tuple_to_list(Record),
    #{RName := FNames} = Records,
    FValues = [term_mapping(RValue, Records, Opts)
               || RValue <- RValues],
    FNamesValues = lists:zip(FNames, FValues),
    case get_field_format_by_opts(Opts) of
        fields_as_records ->
            mapping_create(RName, FNamesValues,
                           mk_get_defs_format_fn(Opts),
                           Opts);
        fields_as_maps ->
            mapping_create(RName, FNamesValues,
                           mk_get_defs_format_fn(Opts),
                           Opts);
        fields_as_proplists ->
            erl_syntax:list(
              [erl_syntax:tuple([erl_syntax:atom(FName), FValue])
               || {FName, FValue} <- FNamesValues])
    end;
term_mapping(Tuple, Records, Opts) when is_tuple(Tuple) ->
    erl_syntax:tuple(
      [term_mapping(Elem, Records, Opts)
       || Elem <- tuple_to_list(Tuple)]);
term_mapping(Map, _Records, _Opts) when is_map(Map) ->
    error({not_expected, Map}); % don't currently expect maps to be formatted
term_mapping(Noncompound, _Records, _Opts) ->
    erl_syntax:abstract(Noncompound).

%% Option helpers ---------------

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
                 fun norm_opt_msg_format/1,
                 fun norm_opt_mapfield_format/1,
                 fun norm_opt_defs_format/1,
                 fun norm_opt_any_translate/1,
                 fun norm_opt_json_format/1,
                 fun norm_opt_json_print_fields_with_no_presence/1,
                 fun norm_opt_gen_encoders/1,
                 fun norm_opt_gen_decoders/1,
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

norm_opt_msg_format(Opts) ->
    proplists:expand(
      [{msgs_as_maps,         [{msg_format, maps}]},
       {{msgs_as_maps, true}, [{msg_format, maps}]}],
      Opts).

norm_opt_mapfield_format(Opts) ->
    proplists:expand(
      [{mapfields_as_maps,          [{mapfield_format, maps}]},
       {{mapfields_as_maps, true},  [{mapfield_format, maps}]},
       {{mapfields_as_maps, false}, [{mapfield_format, '2tuples'}]}],
      Opts).

norm_opt_defs_format(Opts) ->
    proplists:expand(
      [{defs_as_maps,              [{defs_format, maps}]},
       {{defs_as_maps, true},      [{defs_format, maps}]},
       {defs_as_proplists,         [{defs_format, proplists}]},
       {{defs_as_proplists, true}, [{defs_format, proplists}]}],
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
      [{{json_format, maps},       [{json_object_format, map},
                                    {json_key_format, binary},
                                    {json_array_format, list},
                                    {json_string_format, binary},
                                    {json_null, null}]},
       {{json_format, jsx},        [{json_object_format, eep18},
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
                                    {json_null, null}]}],
      Opts).

norm_opt_json_print_fields_with_no_presence(Opts) ->
    proplists:substitute_aliases(
      [{json_always_print_primitive_fields,
        json_always_print_fields_with_no_presence}],
      Opts).

norm_opt_gen_encoders(Opts) ->
    proplists:expand(
      [{{gen_encoders, false}, [{gen_verifiers, false},
                                {gen_encoders, false}]}],
      Opts).

norm_opt_gen_decoders(Opts) ->
    proplists:expand(
      [{{gen_decoders, false}, [{gen_mergers, false},
                                {gen_decoders, false}]}],
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

get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) ->
    Default = default_mapfield_format_by_opts(Opts),
    proplists:get_value(mapfield_format, Opts, Default).

get_mapping_by_opts(Opts) ->
    Default = 'records',
    case proplists:get_value(msg_format, Opts, Default) of
        records        -> records;
        native_records -> natrecs;
        maps           -> maps
    end.

get_mapping_and_unset_by_opts(Opts) ->
    case get_mapping_by_opts(Opts) of
        records ->
            records;
        natrecs ->
            Undef = proplists:get_value(native_records_unset, Opts, undefined),
            #natrecs{unset_value=Undef};
        maps ->
            DefaultUnsetOptional = omitted,
            UnseOptional = proplists:get_value(maps_unset_optional, Opts,
                                               DefaultUnsetOptional),
            Oneof = proplists:get_value(maps_oneof, Opts, tuples),
            #maps{unset_optional=UnseOptional, oneof=Oneof}
    end.

get_strings_as_binaries_by_opts(Opts) ->
    proplists:get_bool(strings_as_binaries, Opts).

get_type_specs_by_opts(Opts) ->
    Default = true,
    proplists:get_value(type_specs, Opts, Default).

get_gen_descriptor_by_opts(Opts) ->
    proplists:get_bool(descriptor, Opts).

get_field_format_by_opts(Opts) ->
    Default = default_defs_format_by_opts(Opts),
    case proplists:get_value(defs_format, Opts, Default) of
        records   -> fields_as_records;
        maps      -> fields_as_maps;
        proplists -> fields_as_proplists
    end.

mk_get_defs_format_fn(Opts) ->
    fun() -> get_defs_format(Opts) end.

get_defs_format(Opts) ->
    Default = default_defs_format_by_opts(Opts),
    proplists:get_value(defs_format, Opts, Default).

default_mapfield_format_by_opts(NormalizedOpts) ->
    case get_mapping_by_opts(NormalizedOpts) of
        records -> '2tuples';
        natrecs -> maps;
        maps    -> maps
    end.

default_defs_format_by_opts(NormalizedOpts) ->
    case get_mapping_by_opts(NormalizedOpts) of
        records -> records;
        natrecs -> maps;
        maps    -> maps
    end.

get_epb_functions_by_opts(Opts) ->
    proplists:get_bool(epb_functions, Opts).

get_bypass_wrappers_by_opts(Opts) ->
    proplists:get_bool(bypass_wrappers, Opts).

get_enum_macros_by_opts(Opts) ->
    proplists:get_bool(gen_enum_macros, Opts).

is_target_major_version_at_least(VsnMin, Opts) ->
    case proplists:get_value(target_erlang_version, Opts, current) of
        current ->
            is_current_major_version_at_least(VsnMin);
        N when is_integer(N) ->
            N >= VsnMin
    end.

is_current_major_version_at_least(VsnMin) ->
    current_otp_release() >= VsnMin.

current_otp_release() ->
    RelStr = erlang:system_info(otp_release),
    try list_to_integer(RelStr)
    catch error:badarg ->
            FirstChunkOfDigits = lists:takewhile(fun is_digit/1, RelStr),
            list_to_integer(FirstChunkOfDigits)
    end.

is_digit(C) when $0 =< C, C =< $9 -> true;
is_digit(_) -> false.

%% In Erlang 25, declaring functions overridden as NIFs
%% in -nifs([fn1/1, fn2/1, ...]). allows for the compiler and loader
%% to do better.
target_has_nifs_directive(Opts) ->
    is_target_major_version_at_least(25, Opts).

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

get_maps_key_type_by_opts(Opts) ->
    proplists:get_value(maps_key_type, Opts, atom).

json_by_opts(Opts) ->
    proplists:get_bool(json, Opts).

json_object_format_by_opts(Opts) ->
    case proplists:get_value(json_object_format, Opts) of
        undefined ->
            map;
        eep18 ->
            eep18;
        {proplist} ->
            {proplist};
        {Atom, proplist} when is_atom(Atom) ->
            {Atom, proplist};
        map ->
            map
    end.

json_key_format_by_opts(Opts) ->
    case proplists:get_value(json_key_format, Opts, binary) of
        atom ->
            atom;
        binary ->
            binary;
        string ->
            string
    end.

json_array_format_by_opts(Opts) ->
    case proplists:get_value(json_array_format, Opts, list) of
        list ->
            list;
        {Atom, list} when is_atom(Atom) ->
            {Atom, list}
    end.

json_string_format_by_opts(Opts) ->
    case proplists:get_value(json_string_format, Opts, binary) of
        binary ->
            binary;
        list ->
            list
    end.

json_null(Opts) ->
    proplists:get_value(json_string_format, Opts, null).

get_gen_mergers(Opts) ->
    proplists:get_value(gen_mergers, Opts, true).

get_gen_introspect(Opts) ->
    proplists:get_value(gen_introspect, Opts, true).

get_gen_verifiers(Opts) ->
    proplists:get_value(gen_verifiers, Opts, true).

get_gen_encoders(Opts) ->
    proplists:get_value(gen_encoders, Opts, true).

get_gen_decoders(Opts) ->
    proplists:get_value(gen_decoders, Opts, true).

possibly_adjust_proto_defs_version_opt(Opts) ->
    case proplists:get_value(proto_defs_version, Opts) of
        undefined ->
            %% Default version is 1 for now
            [{proto_defs_version, 1} | Opts];
        _Vsn ->
            Opts
    end.

%% Syntax tree stuff ----

var_f_n(N) -> var_n("F", N).
var_b_n(N) -> var_n("B", N).
var_j_n(N) -> var_n("J", N).

var_n(S, N) ->
    var("~s~w", [S, N]).

var(Fmt, Args) ->
    erl_syntax:variable('replace_any_$_with_@'(?ff(Fmt, Args))).

'replace_any_$_with_@'(Str) ->
    %% The character `$' can occur in field names,
    %% Replace with `@' which can occur in variable names.
    %% The `@' can actually occur in atoms too, but I think
    %% `$unknown' looks slightly more erlangy than `@unknown'.
    lists:map(fun($$) -> $@;
                 (C)  -> C
              end,
              Str).

prefix_var(Prefix, Var) ->
    erl_syntax:variable(Prefix ++ erl_syntax:variable_literal(Var)).

match_bind_var(Pattern, Var) ->
    ?expr('Pattern' = 'Var',
          [replace_tree('Pattern', Pattern),
           replace_tree('Var', Var)]).

assign_to_var(Var, Expr) ->
    ?expr('<Var>' = '<Expr>',
          [replace_tree('<Var>', Var),
           replace_tree('<Expr>', Expr)]).

varint_to_binary_fields(IntValue) ->
    [erl_syntax:binary_field(?expr('<n>', [replace_term('<n>', N)]), [])
     || N <- binary_to_list(gpb:encode_varint(IntValue))].

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


%% File name related ---

drop_filename_ext(Path) ->
    [B | RRest] = lists:reverse(filename:split(Path)),
    BNoExt = filename:basename(B, filename:extension(B)),
    filename:join(lists:reverse(RRest, [BNoExt])).

copy_filename_ext(FilenameSansExt, FilenameToCopyFrom) ->
    FilenameSansExt ++ filename:extension(FilenameToCopyFrom).

%% @doc Compute filenames (paths) to basenames
%% but include last part(s) of the directories as necessary
%% to make the basenames unique.
%%
%% Example:
%% ```
%% basenameify_ish(["/home/u/a/b/c/f.proto",
%%                  "/home/u/a/d/c/f.proto",
%%                  "/home/u/x/y/z/f.proto",
%%                  "/home/u/x/y/z/g.proto"]) ->
%%   ["b/c/f.proto", "d/c/f.proto", "z/f.proto", "g.proto"]
%% '''
%% The order of the returned paths is the same as the order of the input
%% paths, so to create a mapping, one can use for example
%% `lists:zip(Paths, basenameify_ish(Paths))'
basenameify_ish(Paths) ->
    %% Given the example above, split into:
    %% [{"f.proto", ["c", "b", "a", "u", "home"]},
    %%  {"f.proto", ["c", "d", "a", "u", "home"]},
    %%  {"f.proto", ["z", "y", "x", "u", "home"]},
    %%  {"g.proto", ["z", "y", "x", "u", "home"]}]
    %% Then for all dups in the first element of these 2-tuples,
    %% "f.proto" in this case, prepend one more component from the
    %% RPath (the 2nd elem of the 2-tuples), then iterate until all first
    %% elements are unique.

    case find_dup_rpath_comps(Paths, []) of
        []   -> ok;
        Dups -> error({gpb_error, {multiply_defined_file_or_files, Dups}})
    end,

    RPathComps = [begin
                      [Basename | RPath] = lists:reverse(filename:split(P)),
                      {[Basename], RPath}
                  end
                  || P <- Paths],
    include_dir_comps_until_unique(RPathComps).

include_dir_comps_until_unique(RPathComps) ->
    case find_dup_rpath_comps([B || {B, _} <- RPathComps], []) of
        [] ->
            [filename:join(Baseish) || {Baseish, _} <- RPathComps];
        Dups ->
            %% Include one dir component for each elem in dup
            include_dir_comps_until_unique(
              [case {lists:member(Baseish, Dups), RPath} of
                   {true, [Comp | RestRPath]} ->
                       {[Comp |  Baseish], RestRPath};
                   {false, _} ->
                       Item
               end
               || {Baseish, RPath}=Item <- RPathComps])
    end.

find_dup_rpath_comps([X | Rest], Acc) ->
    case lists:member(X, Rest) of
        true  -> find_dup_rpath_comps([Y || Y <- Rest, Y =/= X], [X | Acc]);
        false -> find_dup_rpath_comps(Rest, Acc)
    end;
find_dup_rpath_comps([], Acc) ->
    Acc.

%% Misc ---

index_seq([]) -> [];
index_seq(L)  -> lists:zip(lists:seq(1,length(L)), L).

smember(Elem, Set) -> %% set-member
    sets:is_element(Elem, Set).

smember_any(Elems, Set) -> %% is any elem a member in the set
    lists:any(fun(Elem) -> smember(Elem, Set) end,
              Elems).

indent(Indent, Str) ->
    lists:duplicate(Indent, $\s) ++ Str.

outdent_first(IoList) ->
    lists:dropwhile(fun(C) -> C == $\s end,
                    binary_to_list(iolist_to_binary(IoList))).

indent_lines(Indent, Lines) ->
    [indent(Indent, Line) || Line <- Lines].

split_indent_butfirst_iolist(Indent, IoList) ->
    strip_left(iolist_to_binary(split_indent_iolist(Indent, IoList))).

strip_left(<<" ", Rest/binary>>) -> strip_left(Rest);
strip_left(Other)                -> Other.

cond_split_indent_iolist(Condition, Indent, IoList) ->
    B = iolist_to_binary(IoList),
    case Condition(B) of
        true  -> split_indent_iolist(Indent, IoList);
        false -> B
    end.

split_indent_iolist(Indent, IoList) ->
    [if Line == <<>> -> "\n"; %% don't indent empty lines
        true -> [indent(Indent, Line), "\n"]
     end
     || Line <- linesplit_iolist(IoList)].

linesplit_iolist(Iolist) ->
    re:split(Iolist, ["\n"], [trim, {return,binary}]).

nowarn_dialyzer_attr(FnName,Arity) ->
    %% Especially for the verifiers, dialyzer's success typing can
    %% think that some code paths in the verifiers can't be reached,
    %% and in a sense, it is right: the verifiers do much the same
    %% work as dialyzer. But I think their existence is still
    %% warranted because (a) they work-time rather than compile-time,
    %% and (b) provide for shorter turn-around times when dialyzer
    %% can take some time to analyze a non-trivial proto file.
    %%
    %% So mute dialyzer for the verifier functions.
    ?f("-dialyzer({nowarn_function,~p/~w}).~n", [FnName,Arity]).

no_underspecs_dialyzer_attr(FnName, Arity, Opts) ->
    %% Silence 'dialyzer -Wunderspecs' warnings about functions' specs
    %% being allowing more than the success typing. It can be difficult
    %% to generate very accurate specs.
    %%
    %% This dialyzer warning was added in Erlang 24, using it an earlier
    %% Erlang will result in a warning about an unknown dialyzer warning
    %% option.
    case can_do_no_underspecs_dialyzer_attr(Opts) of
        true ->
            [?f("-if(?OTP_RELEASE >= 24).~n"), % easy-support of slightly older
             ?f("-dialyzer({no_underspecs, ~p/~w}).~n", [FnName, Arity]),
             ?f("-endif.~n")];
        false ->
            ""
    end.

can_do_no_underspecs_dialyzer_attr(Opts) ->
    is_target_major_version_at_least(24, Opts).

nowarn_unused_function(FnName, Arity) ->
    ?f("-compile({nowarn_unused_function,~p/~w}).~n", [FnName,Arity]).

comma_join(Elements) ->
    lists:append(lists:join(", ", Elements)).

nl_join(Elements) ->
    lists:append(lists:join("\n", Elements)).

or_join(Alternatives) ->
    lists:append(lists:join(" | ", Alternatives)).

dot_join(Alternatives) ->
    lists:append(lists:join(".", Alternatives)).

string_join(Alternatives, Sep) ->
    lists:append(lists:join(Sep, Alternatives)).

is_substr(SearchPattern, String) ->
    string:find(String, SearchPattern) /= nomatch.

string_slice(String, Start) ->
    string:slice(String, Start).

string_lexemes(String, Separators) ->
    string:lexemes(String, Separators).

lowercase(Str) ->
    string:lowercase(Str).

uppercase(Str) ->
    string:uppercase(Str).


-define(is_lower(C), $a =< C, C =< $z).
-define(is_upper(C), $A =< C, C =< $Z).
-define(is_digit(C), $0 =< C, C =< $9).

snake_case(Str) ->
    sc_fsm(init, lists:reverse(Str), "").

sc_fsm(State, [C | Rest], Out) ->
    case {char_class(C), State} of
        {Class, init}  -> sc_fsm(Class, Rest, [low_c(C) | Out]);
        {upper, lower} -> sc_fsm(cap,   Rest, [low_c(C) | Out]);
        {$.,    cap}   -> sc_fsm(init,  Rest, [C | Out]);
        {$_,    cap}   -> sc_fsm(init,  Rest, [low_c(C) | Out]);
        {Class, cap}   -> sc_fsm(Class, Rest, [low_c(C), $_ | Out]);
        {$.,    digit} -> sc_fsm(init,  Rest, [C | Out]);
        {digit, digit} -> sc_fsm(digit, Rest, [C | Out]);
        {$_,    digit} -> sc_fsm(init,  Rest, [low_c(C) | Out]);
        {Class, digit} -> sc_fsm(Class, Rest, [low_c(C), $_ | Out]);
        {upper, upper} -> sc_fsm(upper, Rest, [low_c(C) | Out]);
        {$.,    upper} -> sc_fsm(init,  Rest, [C | Out]);
        {$_,    upper} -> sc_fsm(init,  Rest, [low_c(C) | Out]);
        {Class, upper} -> sc_fsm(Class, Rest, [low_c(C), $_ | Out]);
        {Class, _}     -> sc_fsm(Class, Rest, [low_c(C) | Out])
    end;
sc_fsm(_State, [], Out) ->
    Out.

char_class(C) when ?is_lower(C) -> lower;
char_class(C) when ?is_upper(C) -> upper;
char_class(C) when ?is_digit(C) -> digit;
char_class(C) -> C.

low_c(C) when ?is_upper(C) -> C + ($a - $A);
low_c(C) -> C.

%% Old snake case for bwd compat, but it is not idempotent
old_snake_case(Str) ->
    lowercase(
      lists:foldl(fun(RE, Snaking) ->
                          re:replace(Snaking, RE, "\\1_\\2", [{return, list},
                                                              global])
                  end, Str, [%% uppercase followed by lowercase
                             "([^.])([A-Z][a-z]+)",
                             %% any consecutive digits
                             "([^.])([0-9]+)",
                             %% uppercase with lowercase
                             %% or digit before it
                             "([a-z0-9])([A-Z])"])).

camel_case(Str) ->
    camel_case(Str, true).

%% Like camel case, but first letter is lower case
lower_camel_case(S) ->
    [C1 | Rest] = camel_case(S),
    [LC1] = lowercase([C1]),
    [LC1 | Rest].

camel_case([LC | Tl], CapNextLetter) when ?is_lower(LC) ->
    if CapNextLetter     -> [capitalize_letter(LC) | camel_case(Tl, false)];
       not CapNextLetter -> [LC | camel_case(Tl, false)]
    end;
camel_case([UC | Tl], _) when ?is_upper(UC) ->
    [UC | camel_case(Tl, false)];
camel_case([D | Tl], _) when ?is_digit(D) ->
    [D | camel_case(Tl, true)];
camel_case([_ | Tl], _) -> %% underscore and possibly more
    camel_case(Tl, true);
camel_case([], _) ->
    [].

capitalize_letter(C) ->
    C + ($A - $a).

ljoin(Sep, List) ->
    lists:join(Sep, List).

-ifndef(NO_HAVE_MAPS_MERGE_WITH_3).

maps_merge_with(Combiner, Map1, Map2) ->
    maps:merge_with(Combiner, Map1, Map2). % appeared in Erlang 24

-else. % NO_HAVE_MAPS_MERGE_WITH_3

maps_merge_with(Combiner, Map1, Map2) ->
    maps:fold(
      fun(K, V1, Acc) ->
              maps:update_with(K, fun(V2) -> Combiner(K, V1, V2) end, V1, Acc)
      end,
      Map2,
      Map1).

-endif. % NO_HAVE_MAPS_MERGE_WITH_3
