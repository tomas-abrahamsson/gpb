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

%%% @doc Generation of type specs and record definitinos.
%%% @private
-module(gpb_gen_types).

-export([format_msg_record/5]).
-export([format_maps_as_msgs_record_defs/1]).
-export([format_enum_typespec/2]).
-export([format_record_typespec/5]).
-export([format_export_types/3]).

-include("../include/gpb.hrl").
-include("gpb_compile.hrl").

format_msg_record(Msg, Fields, AnRes, Opts, Defs) ->
    Def = list_to_atom(gpb_lib:uppercase(lists:concat([Msg, "_PB_H"]))),
    [?f("-ifndef(~p).~n", [Def]),
     ?f("-define(~p, true).~n", [Def]),
     ?f("-record(~p,~n", [Msg]),
     ?f("        {"),
     gpb_lib:outdent_first(format_hfields(Msg, 8+1, Fields, AnRes, Opts, Defs)),
     "\n",
     ?f("        }).~n"),
     ?f("-endif.~n")].

format_maps_as_msgs_record_defs(MapsAsMsgs) ->
    [begin
         FNames = [atom_to_list(FName) || #?gpb_field{name=FName} <- Fields],
         ?f("-record(~p,{~s}).~n", [MsgName, gpb_lib:comma_join(FNames)])
     end
     || {{msg,MsgName},Fields} <- MapsAsMsgs].

format_export_types(Defs, AnRes, Opts) ->
    case gpb_lib:get_type_specs_by_opts(Opts) of
        false ->
            "";
        true ->
            iolist_to_binary(
              ["%% enumerated types\n",
               gpb_lib:nl_join([format_enum_typespec(Enum, Enumeration)
                                || {{enum, Enum}, Enumeration} <- Defs]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join(["'"++atom_to_list(Enum)++"'/0"
                                       || {{enum, Enum}, _} <- Defs])]),
               "\n\n",
               "%% message types\n",
               gpb_lib:nl_join(
                 [format_record_typespec(Name, Fields, Defs, AnRes, Opts)
                  || {_, Name, Fields} <- gpb_lib:msgs_or_groups(Defs)]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join(
                     ["'"++atom_to_list(Name)++"'/0"
                      || {_, Name, _} <- gpb_lib:msgs_or_groups(Defs)])]),
               "\n"])
    end.

format_enum_typespec(Enum, Enumeration) ->
  ?f("-type '~s'() :: ~s.", [Enum,
    gpb_lib:or_join(
      ["'"++atom_to_list(EName)++"'" || {EName, _} <- Enumeration])]).


format_record_typespec(Msg, Fields, Defs, AnRes, Opts) ->
    ElemPath = [Msg],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            ?f("-type ~p() :: ~s.", [Msg, TypeStr]);
        false ->
            case gpb_lib:get_records_or_maps_by_opts(Opts) of
                records ->
                    ?f("-type ~p() :: #~p{}.", [Msg, Msg]);
                maps ->
                    ?f("-type ~p() ::~n"
                       "      #{~s~n"
                       "       }.",
                       [Msg, gpb_lib:outdent_first(
                               format_hfields(
                                 Msg, 7 + 1,
                                 Fields, AnRes, Opts, Defs))])
            end
    end.

format_hfields(MsgName, Indent, Fields, AnRes, Opts, Defs) ->
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    TypeSpecs = gpb_lib:get_type_specs_by_opts(Opts),
    MapsOrRecords = gpb_lib:get_records_or_maps_by_opts(Opts),
    MappingAndUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    TypespecsCanIndicateMapItemPresence =
        gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts),
    Fields1 = case MappingAndUnset of
                  #maps{unset_optional=omitted, oneof=flat} ->
                      gpb_lib:flatten_oneof_fields(Fields);
                  _ ->
                      Fields
              end,
    LastIndex = case MappingAndUnset of
                    records ->
                        length(Fields1);
                    #maps{unset_optional=present_undefined} ->
                        length(Fields1);
                    #maps{unset_optional=omitted} ->
                        if TypespecsCanIndicateMapItemPresence ->
                                length(Fields1); % do typespecs for all fields
                           true ->
                                find_last_nonopt_field_index(Fields1)
                        end
                end,
    MapTypeFieldsRepr = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(
                          Opts),
    gpb_lib:nl_join(
      lists:map(
        fun({I, #?gpb_field{name=Name, fnum=FNum, opts=FOpts, type=Type,
                            occurrence=Occur}=Field}) ->
                TypeSpecifierSep = calc_field_type_sep(Field, Opts),
                LineLead = case MappingAndUnset of
                               #maps{unset_optional=omitted} when
                                     Occur == optional,
                                     not TypespecsCanIndicateMapItemPresence ->
                                   "%% ";
                              _ ->
                                   ""
                           end,
                DefaultStr =
                    case proplists:get_value(default, FOpts, '$no') of
                        '$no' ->
                            case {Type, Occur, MapsOrRecords} of
                                {{map,_,_}, repeated, records} ->
                                    case MapTypeFieldsRepr of
                                        maps ->
                                            ?f(" = #{}");
                                        '2tuples' ->
                                            ?f(" = []")
                                    end;
                                {_, repeated, records} ->
                                    ?f(" = []");
                                {_, _, records} ->
                                    case IsProto3 of
                                        true ->
                                            Default =
                                                gpb_lib:proto3_type_default(
                                                  Type,
                                                  Defs,
                                                  Opts),
                                            ?f(" = ~p", [Default]);
                                        false -> ""
                                    end;
                                _ ->
                                    ""
                            end;
                        Default ->
                            case MapsOrRecords of
                                records ->
                                    ?f(" = ~p", [Default]);
                                maps ->
                                    ""
                            end
                    end,
                TypeStr = type_to_typestr(MsgName, Field, Defs, AnRes, Opts),
                CommaSep = if I < LastIndex -> ",";
                              true          -> "" %% last entry
                           end,
                FieldTxt0 = ?f("~s~w~s", [LineLead, Name, DefaultStr]),
                FieldTxt1 = gpb_lib:indent(Indent, FieldTxt0),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                       TypeSpecifierSep,
                                                       TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    ?f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                                not TypeSpecs -> 40
                             end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                TypeComment = type_to_comment(MsgName, Field, TypeSpecs, AnRes),
                ?f("~s~s% = ~w~s~s",
                   [FieldTxt2, LineUpStr2, FNum,
                    [", " || TypeComment /= ""], TypeComment]);
           ({I, #gpb_oneof{name=Name}=Field}) ->
                TypeSpecifierSep = calc_field_type_sep(Field, Opts),
                LineLead = case MappingAndUnset of
                               #maps{unset_optional=omitted} when
                                     not TypespecsCanIndicateMapItemPresence->
                                   "%% ";
                               _ ->
                                   ""
                           end,
                TypeStr = type_to_typestr(MsgName, Field, Defs, AnRes, Opts),
                CommaSep = if I < LastIndex -> ",";
                              true          -> "" %% last entry
                           end,
                FieldTxt0 = ?f("~s~w", [LineLead, Name]),
                FieldTxt1 = gpb_lib:indent(Indent, FieldTxt0),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                       TypeSpecifierSep,
                                                       TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    ?f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                                not TypeSpecs -> 40
                             end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                TypeComment = type_to_comment(MsgName, Field, TypeSpecs, AnRes),
                ?f("~s~s% ~s",
                   [FieldTxt2, LineUpStr2, TypeComment])
        end,
        gpb_lib:index_seq(Fields1))).

find_last_nonopt_field_index(Fields) ->
    lists:foldl(fun({I, F}, Acc) ->
                        case gpb_lib:get_field_occurrence(F) of
                            required -> I;
                            repeated -> I;
                            optional -> Acc
                        end
                end,
                0,
                gpb_lib:index_seq(Fields)).

calc_field_type_sep(#?gpb_field{occurrence=Occurrence}, Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        records ->
            "::";
        #maps{unset_optional=present_undefined} ->
            mandatory_map_item_type_sep(Opts);
        #maps{unset_optional=omitted} ->
            case Occurrence of
                required -> mandatory_map_item_type_sep(Opts);
                repeated -> "=>";
                optional -> "=>"
            end
    end;
calc_field_type_sep(#gpb_oneof{}, Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        records   -> "::";
        #maps{} -> "=>"
    end.

mandatory_map_item_type_sep(Opts) ->
    %% With Erlang 19 we write #{n := integer()} to say that a
    %% map must contain a map item with key `n' and an integer value.
    %%
    %% With earlier Erlang versions, we can only write #{n => integer()}
    %% and we can never distinguish between map items that may or must
    %% be present.
    %%
    %% Ideally, we would want to know for which version of Erlang we're
    %% generating code.  For now, we assume the run-time version is the
    %% same as the compile-time version, which is not necessarily true.  For
    %% instance, we can generate code for maps even on pre-map Erlang R15.
    %%
    %% (At the time of this writing, the OTP_RELEASE pre-defined macro
    %% does not exist, but even if it had existed, it would have been of
    %% limited value because it would have linked the Erlang version at
    %% proto-encoding run-time with the Erlang version at compile-time
    %% of `gpb' not at compile-time of the .proto file.  In some
    %% scenario with a package manager, it might have a `gpb'
    %% pre-compiled with an old Erlang-version to be compatible with
    %% many environments.  Better to check version at run-time.)
    %%
    case gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts) of
        true  -> ":=";
        false -> "=>"
    end.

type_to_typestr(MsgName,
                #?gpb_field{name=FName, type=Type, occurrence=Occurrence},
                Defs, AnRes, Opts) ->
    OrUndefined = case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                      records ->
                          " | undefined";
                      #maps{unset_optional=present_undefined} ->
                          " | undefined";
                      #maps{unset_optional=omitted} ->
                          ""
                  end,
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            case Occurrence of
                required -> TypeStr;
                repeated -> TypeStr ++ OrUndefined;
                optional -> TypeStr ++ OrUndefined
            end;
        false ->
            TypeStr = type_to_typestr_2(Type, Defs, Opts),
            case Occurrence of
                required ->
                    TypeStr;
                repeated ->
                    case Type of
                        {map,_,_} ->
                            TypeStr;
                        _ ->
                            RElemPath = [MsgName, FName, []],
                            case gpb_gen_translators:has_type_spec_translation(
                                   RElemPath, AnRes) of
                                {true, RTs} ->
                                    "[" ++ RTs ++ "]";
                                false ->
                                    "[" ++ TypeStr ++ "]"
                            end
                    end
                        ++ OrUndefined;
                optional ->
                    TypeStr ++ OrUndefined
            end
    end;
type_to_typestr(MsgName,
                #gpb_oneof{name=FName, fields=OFields},
                Defs, AnRes, Opts) ->
    OrUndefinedElems = case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                           records ->
                               ["undefined"];
                           #maps{unset_optional=present_undefined} ->
                               ["undefined"];
                           #maps{unset_optional=omitted} ->
                               []
                       end,
    OrUndefinedStr = case OrUndefinedElems of
                         [] -> "";
                         [U] -> " | " ++ U
                     end,
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            TypeStr ++ OrUndefinedStr;
        false ->
            gpb_lib:or_join(
              [begin
                   OElemPath = [MsgName, FName, Name],
                   case gpb_gen_translators:has_type_spec_translation(
                          OElemPath, AnRes) of
                       {true, TypeStr} ->
                           TypeStr;
                       false ->
                           TypeStr = type_to_typestr_2(Type, Defs, Opts),
                           ?f("{~s, ~s}", [Name, TypeStr])
                   end
               end
               || #?gpb_field{name=Name, type=Type} <- OFields]
              ++ OrUndefinedElems)
    end.

type_to_typestr_2(sint32, _Defs, _Opts)   -> "integer()";
type_to_typestr_2(sint64, _Defs, _Opts)   -> "integer()";
type_to_typestr_2(int32, _Defs, _Opts)    -> "integer()";
type_to_typestr_2(int64, _Defs, _Opts)    -> "integer()";
type_to_typestr_2(uint32, _Defs, _Opts)   -> "non_neg_integer()";
type_to_typestr_2(uint64, _Defs, _Opts)   -> "non_neg_integer()";
type_to_typestr_2(bool, _Defs, _Opts)     -> "boolean() | 0 | 1";
type_to_typestr_2(fixed32, _Defs, _Opts)  -> "non_neg_integer()";
type_to_typestr_2(fixed64, _Defs, _Opts)  -> "non_neg_integer()";
type_to_typestr_2(sfixed32, _Defs, _Opts) -> "integer()";
type_to_typestr_2(sfixed64, _Defs, _Opts) -> "integer()";
type_to_typestr_2(float, _Defs, _Opts)    -> float_spec();
type_to_typestr_2(double, _Defs, _Opts)   -> float_spec();
type_to_typestr_2(string, _Defs, Opts)    ->
  string_to_typestr(gpb_lib:get_strings_as_binaries_by_opts(Opts));
type_to_typestr_2(bytes, _Defs, _Opts)    -> "iodata()";
type_to_typestr_2({enum,E}, Defs, Opts)   -> enum_typestr(E, Defs, Opts);
type_to_typestr_2({msg,M}, _Defs, Opts)   -> msg_to_typestr(M, Opts);
type_to_typestr_2({group,G}, _Defs, Opts) -> msg_to_typestr(G, Opts);
type_to_typestr_2({map,KT,VT}, Defs, Opts) ->
    KTStr = type_to_typestr_2(KT, Defs, Opts),
    VTStr = type_to_typestr_2(VT, Defs, Opts),
    MapSep = mandatory_map_item_type_sep(Opts),
    case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
        '2tuples' -> ?f("[{~s, ~s}]", [KTStr, VTStr]);
        maps      -> ?f("#{~s ~s ~s}", [KTStr, MapSep, VTStr])
    end.

float_spec() ->
    "float() | integer() | infinity | '-infinity' | nan".

msg_to_typestr(M, Opts) ->
  case gpb_lib:get_records_or_maps_by_opts(Opts) of
    records ->
      Mod = proplists:get_value(module, Opts),
      ?f("~p:~p()", [Mod, M]);
    maps -> ?f("~p()", [M])
  end.

%% when the strings_as_binaries option is requested the corresponding
%% typespec should be spec'ed
string_to_typestr(true) ->
  "iodata()";
string_to_typestr(false) ->
  "iolist()".

enum_typestr(E, Defs, Opts) ->
    UnknownEnums = case proplists:get_bool(nif, Opts) of
                       false -> " | integer()";
                       true  -> ""
                   end,
    {value, {{enum,E}, Enumerations}} = lists:keysearch({enum,E}, 1, Defs),
    gpb_lib:or_join(
      ["'"++atom_to_list(EName)++"'" || {EName, _} <- Enumerations])
        ++ UnknownEnums.

type_to_comment(MsgName, Field, TypeSpec, AnRes) ->
    ElemPath = [MsgName, gpb_lib:get_field_name(Field)],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, _} ->
            "";
        false ->
            type_to_comment_2(Field, TypeSpec)
    end.

type_to_comment_2(#?gpb_field{type=Type}, true=_TypeSpec) ->
    case Type of
        sint32   -> "32 bits";
        sint64   -> "32 bits";
        int32    -> "32 bits";
        int64    -> "32 bits";
        uint32   -> "32 bits";
        uint64   -> "32 bits";
        fixed32  -> "32 bits";
        fixed64  -> "32 bits";
        sfixed32 -> "32 bits";
        sfixed64 -> "32 bits";
        {enum,E} -> "enum "++atom_to_list(E);
        _        -> ""
    end;
type_to_comment_2(#?gpb_field{type=Type, occurrence=Occurrence}, false) ->
    case Occurrence of
        required -> ?f("~w", [Type]);
        repeated -> "[" ++ ?f("~w", [Type]) ++ "]";
        optional -> ?f("~w (optional)", [Type])
    end;
type_to_comment_2(#gpb_oneof{}, _) ->
    "oneof".


lineup(CurrentCol, TargetCol) when CurrentCol < TargetCol ->
    lists:duplicate(TargetCol - CurrentCol, $\s);
lineup(_, _) ->
    " ".
