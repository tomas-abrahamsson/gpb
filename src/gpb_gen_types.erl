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
-export([format_enum_typespec/3]).
-export([format_export_types/3]).

-include("../include/gpb.hrl").
-include("gpb_compile.hrl").

%% Collect some type spec info top-level wise instead
%% of needing check opts at many places.
-record(t_env,
        {type_specs :: boolean(),
         can_do_map_presence :: boolean(),
         mapping_and_unset :: records | #maps{},
         map_key_type :: atom | binary,
         map_type_fields :: '2tuples' | maps,
         module :: module(),
         nif :: boolean()}).

%% List of fields with some info/annotations to be rendered
%% either as record fields
%% or as map associations
-record(field_info, % order sort of from left to right
        {field :: #?gpb_field{} | #gpb_oneof{},
         out_comment :: boolean()  % if entire field must be out-commented
                      | undefined, % initially
         name :: string()   % field name, single-quoted if needed
               | undefined, % initially
         default :: string()   % for records
                  | undefined, % if no default
         type_sep :: string()   % "::" (records) or "=>" or ":=" (maps)
                   | undefined, % initially
         type_text :: string()
                    | undefined, % if no type specs
         comment_chunks :: [string()]}).

format_msg_record(Msg, Fields, AnRes, Opts, Defs) ->
    Def = list_to_atom(gpb_lib:uppercase(lists:concat([Msg, "_PB_H"]))),
    TEnv = t_env(Opts),
    [?f("-ifndef(~p).~n", [Def]),
     ?f("-define(~p, true).~n", [Def]),
     ?f("-record(~p,~n", [Msg]),
     ?f("        {"),
     gpb_lib:outdent_first(
       format_hfields(Msg, 8+1, Fields, AnRes, Opts, Defs, TEnv)),
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
    #t_env{type_specs=TypeSpecs} = TEnv = t_env(Opts),
    if not TypeSpecs ->
            "";
       TypeSpecs ->
            iolist_to_binary(
              ["%% enumerated types\n",
               gpb_lib:nl_join([format_enum_typespec(Enum, Enumeration, AnRes)
                                || {{enum, Enum}, Enumeration} <- Defs]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join([format_enum_export(Enum, AnRes)
                                       || {{enum, Enum}, _} <- Defs])]),
               "\n\n",
               "%% message types\n",
               gpb_lib:nl_join(
                 [format_record_typespec(Name, Fields, Defs, AnRes, Opts, TEnv)
                  || {_, Name, Fields} <- gpb_lib:msgs_or_groups(Defs)]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join(
                     ["'"++atom_to_list(Name)++"'/0"
                      || {_, Name, _} <- gpb_lib:msgs_or_groups(Defs)])]),
               "\n"])
    end.

format_enum_typespec(Enum, Enumeration, AnRes) ->
    Enum1 = rename_enum_type(Enum, AnRes),
    Enumerators = gpb_lib:or_join([?f("~p", [EName])
                                   || {EName, _} <- Enumeration]),
    ?f("-type ~p() :: ~s.", [Enum1, Enumerators]).

format_enum_export(Enum, AnRes) ->
    Enum1 = rename_enum_type(Enum, AnRes),
    ?f("~p/0", [Enum1]).

format_record_typespec(Msg, Fields, Defs, AnRes, Opts,
                       #t_env{mapping_and_unset=MappingAndUnset}=TEnv) ->
    MsgType = rename_msg_type(Msg, AnRes),
    case MappingAndUnset of
        records ->
            ?f("-type ~p() :: #~p{}.~n", [MsgType, Msg]);
        #maps{} ->
            HFields = format_hfields(Msg, 7 + 1, Fields,
                                     AnRes, Opts, Defs, TEnv),
            BType = calc_keytype_override(Fields, TEnv),
            if BType == no_override ->
                    ?f("-type ~p() ::~n"
                       "      #{~s~n"
                       "       }.~n",
                       [MsgType, gpb_lib:outdent_first(HFields)]);
               true ->
                    ?f("-type ~p() ::~n"
                       "      #{~s~n" % all fields gets rendered as comments
                       "        ~s~n"
                       "       }.~n",
                       [MsgType, gpb_lib:outdent_first(HFields), BType])
            end
    end.

t_env(Opts) ->
    TypeSpecs = gpb_lib:get_type_specs_by_opts(Opts),
    TypespecsCanIndicateMapItemPresence =
        gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts),
    MappingAndUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    KeyType = gpb_lib:get_maps_key_type_by_opts(Opts),
    MapTypeFieldsRepr = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(
                          Opts),
    Mod = proplists:get_value(module, Opts),
    Nif = proplists:get_bool(nif, Opts),
    #t_env{type_specs = TypeSpecs,
           can_do_map_presence = TypespecsCanIndicateMapItemPresence,
           mapping_and_unset = MappingAndUnset,
           map_key_type = KeyType,
           map_type_fields = MapTypeFieldsRepr,
           module = Mod,
           nif = Nif}.

calc_keytype_override([], _TEnv) ->
    no_override;
calc_keytype_override(Fields, TEnv) ->
    #t_env{map_key_type=KeyType,
           can_do_map_presence=TypespecsCanIndicateMapItemPresence}=TEnv,
    case KeyType of
        atom ->
            no_override;
        binary ->
            HaveMandatoryFields =
                lists:any(fun(F) ->
                                  gpb_lib:get_field_occurrence(F) /= optional
                          end,
                          Fields),
            if TypespecsCanIndicateMapItemPresence, HaveMandatoryFields ->
                    "binary() := _";
               TypespecsCanIndicateMapItemPresence, not HaveMandatoryFields ->
                    "binary() => _";
               true ->
                    "binary() => _"
            end
    end.

format_hfields(MsgName, Indent, Fields, AnRes, Opts, Defs, TEnv) ->
    FieldInfos = analyze_field_infos(MsgName, Fields, AnRes, Opts, Defs, TEnv),
    RenderedFieldTexts = render_field_infos(FieldInfos, Indent),
    Sep = "\n" ++ gpb_lib:indent(Indent, ""),
    gpb_lib:string_join(RenderedFieldTexts, Sep).

%% --------------------------------------------------------------
%% Collect info needed later for rendering
%%
analyze_field_infos(MsgName, Fields, AnRes, Opts, Defs, TEnv) ->
    FieldInfos0 = calc_zipped_fields_and_type_str_comments(
                    MsgName, Fields,
                    Defs, AnRes, TEnv),
    lists:foldl(
      fun(F, FieldInfos) -> F(FieldInfos) end,
      FieldInfos0,
      [fun(FIs) -> augment_field_name_strs(FIs) end,
       fun(FIs) -> augment_default_values(FIs, Opts, Defs, TEnv) end,
       fun(FIs) -> augment_type_sep(FIs, TEnv) end,
       fun(FIs) -> augment_out_commentation(FIs, TEnv) end,
       fun(FIs) -> augment_field_number(FIs) end]).

%% Step:
%% Set the 'name' field
augment_field_name_strs(FieldInfos) ->
    [FI#field_info{name = ?f("~p", [gpb_lib:get_field_name(Field)])}
     || #field_info{field=Field}=FI <- FieldInfos].

%% Step:
%% Set field default values (for use when generating records)
augment_default_values(FieldInfos, Opts, Defs, TEnv) ->
    #t_env{mapping_and_unset = MappingAndUnset} = TEnv,
    case MappingAndUnset of
        records ->
            [case Field of
                 #?gpb_field{}=Field ->
                     Default = record_field_default(Field, Opts, Defs, TEnv),
                     FI#field_info{default = Default};
                 #gpb_oneof{} ->
                     FI
             end
             || #field_info{field=Field}=FI <- FieldInfos];
        #maps{} ->
            FieldInfos
    end.

record_field_default(#?gpb_field{type=Type,
                                 occurrence=Occurence,
                                 opts=FOpts}=Field,
                     Opts, Defs, TEnv) ->
    #t_env{map_type_fields = MapTypeFieldsRepr} = TEnv,
    case proplists:get_value(default, FOpts, '$no') of
        '$no' ->
            IsMapTypeField = is_map_type_field(Field),
            if IsMapTypeField, MapTypeFieldsRepr == maps ->
                    "#{}";
               IsMapTypeField, MapTypeFieldsRepr == '2tuples' ->
                    "[]";
               Occurence == repeated ->
                    "[]";
               Occurence == defaulty ->
                    Default = gpb_lib:proto3_type_default(Type, Defs, Opts),
                    ?f("~p", [Default]);
               true ->
                    undefined
            end;
        Default ->
            ?f("~p", [Default])
    end.

%% Step:
%% Set the type separator to "::" (records) or "=>" or ":=" (for maps).
augment_type_sep(FieldInfos, TEnv) ->
    [FI#field_info{type_sep = calc_field_type_sep(Field, TEnv)}
     || #field_info{field=Field}=FI <- FieldInfos].

%% Step:
%% Set whether the field needs to be out-commented.
augment_out_commentation(FieldInfos, TEnv) ->
    #t_env{can_do_map_presence = TypespecsCanIndicateMapItemPresence,
           mapping_and_unset = MappingAndUnset,
           map_key_type = KeyType} = TEnv,
    case MappingAndUnset of
        records ->
            [FI#field_info{out_comment = false}
             || FI <- FieldInfos];
        #maps{} when KeyType == binary ->
            %% Unconveniently enough, this is not allowed:
            %%    #{<<"abc">> => some_type()}
            %% since <<"abc">> is not a valid type spec
            %% Only <<>>, <<_:M>>, <<_:_*N>> and a few more are.
            %% So out-comment entries
            [FI#field_info{out_comment = true}
             || FI <- FieldInfos];
        #maps{unset_optional=present_undefined} ->
            [FI#field_info{out_comment = false}
             || FI <- FieldInfos];
        #maps{unset_optional=omitted} ->
            [case gpb_lib:get_field_occurrence(Field) of
                 optional when not TypespecsCanIndicateMapItemPresence ->
                     FI#field_info{out_comment = true};
                 defaulty when not TypespecsCanIndicateMapItemPresence ->
                     FI#field_info{out_comment = true};
                 _ ->
                     FI#field_info{out_comment = false}
             end
             || #field_info{field=Field}=FI <- FieldInfos]
    end.

%% Step:
%% Prepend the field's number (from the .proto) as a comment
augment_field_number(FieldInfos) ->
    [case Field of
         #?gpb_field{fnum=FNum} ->
             Text = ?f("= ~p", [FNum]),
             FI#field_info{comment_chunks = [Text | Chunks]};
         #gpb_oneof{} ->
             FI#field_info{comment_chunks = Chunks}
     end
     || #field_info{field=Field, comment_chunks=Chunks}=FI <- FieldInfos].

%% --------------------------------------------------------------
%% Render the fields to text
%% Each line is later to be indented `Indent' spaces. (This amount
%% is here used for lineup calculations.)
%%
%% Return a list (one per field) of iolists.
%%
render_field_infos(FieldInfos, Indent) ->
    IndexedFieldInfos = gpb_lib:index_seq(FieldInfos),
    LastIndex = find_last_non_out_commented_field(IndexedFieldInfos),
    lists:map(
      fun({I, #field_info{out_comment=OutCommented,
                          name=FName,
                          default=Default,
                          type_sep=TypeSep,
                          type_text=Type,
                          comment_chunks=CommentChunks}}) ->
              LineLead = if OutCommented     -> "%% ";
                            not OutCommented -> ""
                         end,
              DefaultStr = if Default == undefined -> "";
                              true -> ?f(" = ~s", [Default])
                           end,
              CommaSep = if OutCommented -> "";
                            not OutCommented ->
                                 case has_next(I, LastIndex) of
                                     true  -> ",";
                                     false -> ""
                                 end
                         end,
              Comment = gpb_lib:comma_join(CommentChunks),
              FieldTxt1 = ?f("~s~s~s", [LineLead, FName, DefaultStr]),
              FieldTxt2 = if Type /= undefined ->
                                  %% with type specs
                                  LineUp = lineup(Indent, FieldTxt1, 32),
                                  ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                     TypeSep, Type, CommaSep]);
                             Type == undefined ->
                                  %% no type specs
                                  ?f("~s~s", [FieldTxt1, CommaSep])
                          end,
              LineUpCol2 = if Type /= undefined -> 52;
                              Type == undefined -> 40
                           end,
              LineUpStr2 = lineup(Indent, FieldTxt2, LineUpCol2),
              CommentStr = if Comment == "" -> "";
                              Comment /= "" -> "% " ++ Comment
                           end,
              ?f("~s~s~s", [FieldTxt2, LineUpStr2, CommentStr])
      end,
      IndexedFieldInfos).

find_last_non_out_commented_field(IndexedFieldInfos) ->
    lists:foldr(
      fun(_, Last) when is_integer(Last) ->
              Last;
         ({_, #field_info{out_comment=true}}, Acc) ->
              Acc;
         ({I, #field_info{out_comment=false}}, _Acc) ->
              I
      end,
      undefined,
      IndexedFieldInfos).

has_next(I, Last) ->
    if is_integer(Last) ->
            %% Index of last non-outcommented item
            I < Last;
       Last == undefined ->
            %% All items are out-com
            false
    end.

%% --------------------------------------------------------------
%% Helpers...

calc_zipped_fields_and_type_str_comments(MsgName, Fields, Defs, AnRes,
                                         TEnv) ->
    #t_env{type_specs=TypeSpecs,
           mapping_and_unset=MappingAndUnset} = TEnv,
    TypeInfos = [calc_type_str_infos(MsgName, Field, TypeSpecs,
                                     Defs, AnRes, TEnv)
                 || Field <- Fields],
    FieldsTypeInfos = lists:zip(Fields, TypeInfos),
    case MappingAndUnset of
        records ->
            [#field_info{field = Field,
                         type_text = if TypeSpecs -> type_str(TypeInfo);
                                        true -> undefined
                                     end,
                         comment_chunks = type_comments(TypeInfo)}
             || {Field, TypeInfo} <- FieldsTypeInfos];
        #maps{oneof=tuples} ->
            [#field_info{field = Field,
                         type_text = if TypeSpecs -> type_str(TypeInfo);
                                        true -> undefined
                                     end,
                         comment_chunks = type_comments(TypeInfo)}
             || {Field, TypeInfo} <- FieldsTypeInfos];
        #maps{oneof=flat} ->
            flatten_oneofs(FieldsTypeInfos, TEnv)
    end.

calc_field_type_sep(#?gpb_field{},
                    #t_env{mapping_and_unset=MappingAndUnset}=TEnv) ->
    case MappingAndUnset of
        records ->
            "::";
        #maps{unset_optional=present_undefined} ->
            mandatory_map_item_type_sep(TEnv);
        #maps{unset_optional=omitted} ->
            %% Even for required (proto2) fields, we use "=>", since we
            %% cannot guarantee that we will always decode to a map with
            %% all required fields always set, since it depends on the
            %% input binary.
            "=>"
    end;
calc_field_type_sep(#gpb_oneof{}, #t_env{mapping_and_unset=MappingAndUnset}) ->
    case MappingAndUnset of
        records -> "::";
        #maps{} -> "=>"
    end.

mandatory_map_item_type_sep(TEnv) ->
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
    #t_env{can_do_map_presence = TypespecsCanIndicateMapItemPresence} = TEnv,
    case TypespecsCanIndicateMapItemPresence of
        true  -> ":=";
        false -> "=>"
    end.

-record(field_type_str_info,
        {type_str :: string(),
         type_comment :: string()}).
-record(oneof_type_str_info,
        {type_strs :: [string()],
         type_comments :: [string()]}).

calc_type_str_infos(MsgName, #?gpb_field{}=Field, TypeSpecs,
                    Defs, AnRes, TEnv) ->
    #field_type_str_info{
       type_str = field_type_str(MsgName, Field, Defs, AnRes, TEnv),
       type_comment = field_type_comment(MsgName, Field, TypeSpecs, AnRes)};
calc_type_str_infos(MsgName, #gpb_oneof{}=Field, TypeSpecs,
                    Defs, AnRes, TEnv) ->
    #oneof_type_str_info{
       type_strs = oneof_type_strs(MsgName, Field, Defs, AnRes, TEnv),
       type_comments = oneof_type_comments(MsgName, Field, TypeSpecs, AnRes)}.

type_str(#field_type_str_info{type_str=Str})   -> Str;
type_str(#oneof_type_str_info{type_strs=Strs}) -> gpb_lib:or_join(Strs).

type_comments(#field_type_str_info{type_comment=Comment}) -> [Comment];
type_comments(#oneof_type_str_info{})                     -> ["oneof"].

flatten_oneofs([F | Rest], #t_env{type_specs=TypeSpecs}=TEnv) ->
    case F of
        {#?gpb_field{}=Field,
         #field_type_str_info{}=TypeInfo} ->
            TT = if TypeSpecs -> type_str(TypeInfo);
                    true -> undefined
                 end,
            FieldInfo = #field_info{field = Field,
                                    type_text = TT,
                                    comment_chunks = type_comments(TypeInfo)},
            [FieldInfo | flatten_oneofs(Rest, TEnv)];
        {#gpb_oneof{fields=OFields},
         #oneof_type_str_info{type_strs=TypeStrs,
                              type_comments=TypeComments}} ->
            %% TypeStrs can have a trailing "or undefined",
            %% so take as many TypeStrs elements as we have OFields
            TypeStrs1 = lists:sublist(TypeStrs, length(OFields)),
            FieldInfos =
                lists:map(
                  fun({Field, TypeStr, TypeComment}) ->
                          TT = if TypeSpecs -> TypeStr;
                                  true -> undefined
                               end,
                          #field_info{field = Field,
                                      type_text = TT,
                                      comment_chunks = [TypeComment]}
                  end,
                  lists:zip3(OFields, TypeStrs1, TypeComments)),
            FieldInfos ++ flatten_oneofs(Rest, TEnv)
    end;
flatten_oneofs([], _TEnv) ->
    [].

field_type_str(MsgName,
               #?gpb_field{name=FName, type=Type, occurrence=Occurrence},
               Defs, AnRes,
               #t_env{mapping_and_unset=MappingAndUnset}=TEnv) ->
    OrUndefined = case MappingAndUnset of
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
            %% Even if Occurrence == required, it can still end up undefined,
            %% for instance if decoding a proto2 message and the input binary
            %% does not contain it, even though it should.
            case Occurrence of
                required -> TypeStr ++ OrUndefined;
                repeated -> TypeStr ++ OrUndefined;
                optional -> TypeStr ++ OrUndefined;
                defaulty -> TypeStr ++ OrUndefined
            end;
        false ->
            TypeStr = type_to_typestr_2(Type, Defs, AnRes, TEnv),
            case Occurrence of
                required ->
                    TypeStr ++ OrUndefined;
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
                    TypeStr ++ OrUndefined;
                defaulty ->
                    TypeStr ++ OrUndefined
            end
    end.

oneof_type_strs(MsgName,
                #gpb_oneof{name=FName, fields=OFields},
                Defs, AnRes,
                #t_env{mapping_and_unset=MappingAndUnset}=TEnv) ->
    OrUndefinedElems = case MappingAndUnset of
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
    TagWrap = case MappingAndUnset of
                  #maps{oneof=flat} ->
                      fun(_Tag, TypeStr) -> TypeStr end;
                  _ ->
                      fun(Tag, TypeStr) -> ?f("{~p, ~s}", [Tag, TypeStr]) end
              end,
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            [TypeStr, OrUndefinedStr];
        false ->
            [begin
                 OElemPath = [MsgName, FName, Name],
                 case gpb_gen_translators:has_type_spec_translation(
                        OElemPath, AnRes) of
                     {true, TypeStr} ->
                         TagWrap(Name, TypeStr);
                     false ->
                         TypeStr = type_to_typestr_2(Type, Defs, AnRes, TEnv),
                         TagWrap(Name, TypeStr)
                 end
             end
             || #?gpb_field{name=Name, type=Type} <- OFields]
                ++ OrUndefinedElems
    end.

type_to_typestr_2(sint32, _Defs, _AnRes, _TEnv)   -> "integer()";
type_to_typestr_2(sint64, _Defs, _AnRes, _TEnv)   -> "integer()";
type_to_typestr_2(int32, _Defs, _AnRes, _TEnv)    -> "integer()";
type_to_typestr_2(int64, _Defs, _AnRes, _TEnv)    -> "integer()";
type_to_typestr_2(uint32, _Defs, _AnRes, _TEnv)   -> "non_neg_integer()";
type_to_typestr_2(uint64, _Defs, _AnRes, _TEnv)   -> "non_neg_integer()";
type_to_typestr_2(bool, _Defs, _AnRes, _TEnv)     -> "boolean() | 0 | 1";
type_to_typestr_2(fixed32, _Defs, _AnRes, _TEnv)  -> "non_neg_integer()";
type_to_typestr_2(fixed64, _Defs, _AnRes, _TEnv)  -> "non_neg_integer()";
type_to_typestr_2(sfixed32, _Defs, _AnRes, _TEnv) -> "integer()";
type_to_typestr_2(sfixed64, _Defs, _AnRes, _TEnv) -> "integer()";
type_to_typestr_2(float, _Defs, _AnRes, _TEnv)    -> float_spec();
type_to_typestr_2(double, _Defs, _AnRes, _TEnv)   -> float_spec();
type_to_typestr_2(string, _Defs, _AnRes, _TEnv)   -> "iodata()";
type_to_typestr_2(bytes, _Defs, _AnRes, _TEnv)    -> "iodata()";
type_to_typestr_2({enum,E}, Defs, _AnRes, TEnv) ->
    enum_typestr(E, Defs, TEnv);
type_to_typestr_2({msg,M}, _Defs, AnRes, TEnv) ->
    msg_to_typestr(M, AnRes, TEnv);
type_to_typestr_2({group,G}, _Defs, AnRes, TEnv) ->
    msg_to_typestr(G, AnRes, TEnv);
type_to_typestr_2({map,KT,VT}, Defs, AnRes, TEnv) ->
    #t_env{map_type_fields=MapTypeFieldsRepr} = TEnv,
    KTStr = type_to_typestr_2(KT, Defs, AnRes, TEnv),
    VTStr = type_to_typestr_2(VT, Defs, AnRes, TEnv),
    MapSep = mandatory_map_item_type_sep(TEnv),
    case MapTypeFieldsRepr of
        '2tuples' -> ?f("[{~s, ~s}]", [KTStr, VTStr]);
        maps      -> ?f("#{~s ~s ~s}", [KTStr, MapSep, VTStr])
    end.

float_spec() ->
    "float() | integer() | infinity | '-infinity' | nan".

msg_to_typestr(M, AnRes, TEnv) ->
    MsgType = rename_msg_type(M, AnRes),
    #t_env{mapping_and_unset=MappingAndUnset,
           module = Mod} = TEnv,
    case MappingAndUnset of
        records ->
            %% Prefix with module since records live in an hrl file
            ?f("~p:~p()", [Mod, MsgType]);
        #maps{} ->
            ?f("~p()", [MsgType])
    end.

enum_typestr(E, Defs, #t_env{nif=Nif}) ->
    UnknownEnums = if Nif  -> "";
                      true -> " | integer()"
                   end,
    {value, {{enum,E}, Enumerations}} = lists:keysearch({enum,E}, 1, Defs),
    gpb_lib:or_join(
      [?f("~p", [EName]) || {EName, _} <- Enumerations])
        ++ UnknownEnums.

field_type_comment(MsgName,
                   #?gpb_field{name=FName, occurrence=Occurrence}=Field,
                   TypeSpec, AnRes) ->
    PresenceIndication =
        case Occurrence of
            defaulty -> "optional";
            optional -> "optional";
            required -> "required";
            repeated -> "repeated"
        end,
    ElemPath = [MsgName, FName],
    TypeComment = field_type_comment_2(Field, ElemPath, TypeSpec, AnRes),
    if TypeComment == "" -> "(" ++ PresenceIndication ++ ")";
       TypeComment /= "" -> PresenceIndication ++ ": " ++ TypeComment
    end.

oneof_type_comments(MsgName, #gpb_oneof{name=FName, fields=OFields},
                    TypeSpec, AnRes) ->
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, _} ->
            "";
        false ->
            [begin
                 OElemPath = [MsgName, FName, OFName],
                 field_type_comment_2(OField, OElemPath, TypeSpec, AnRes)
             end
             || #?gpb_field{name=OFName}=OField <- OFields]
    end.

field_type_comment_2(Field, ElemPath, TypeSpec, AnRes) ->
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, _} ->
            "";
        false ->
            field_type_comment_3(Field, TypeSpec)
    end.

field_type_comment_3(#?gpb_field{type=Type}, true=_TypeSpec) ->
    case Type of
        sint32   -> "32 bits";
        sint64   -> "64 bits";
        int32    -> "32 bits";
        int64    -> "64 bits";
        uint32   -> "32 bits";
        uint64   -> "64 bits";
        fixed32  -> "32 bits";
        fixed64  -> "64 bits";
        sfixed32 -> "32 bits";
        sfixed64 -> "64 bits";
        {enum,E} -> "enum "++atom_to_list(E);
        _        -> ""
    end;
field_type_comment_3(#?gpb_field{type=Type, occurrence=Occurrence}, false) ->
    case Occurrence of
        required -> ?f("~w", [Type]);
        repeated -> "[" ++ ?f("~w", [Type]) ++ "]";
        optional -> ?f("~w (optional)", [Type]);
        defaulty -> ?f("~w (optional)", [Type])
    end.

lineup(BaseIndent, Text, TargetCol) ->
    CurrentCol = BaseIndent + iolist_size(Text),
    if CurrentCol < TargetCol ->
            lists:duplicate(TargetCol - CurrentCol, $\s);
       true ->
            " "
    end.

is_map_type_field(#?gpb_field{type={map,_,_}, occurrence=repeated}) -> true;
is_map_type_field(_) -> false.

rename_enum_type(Name, #anres{renamings=Renamings}) ->
    gpb_names:apply_enum_type_renaming(Name, Renamings).

rename_msg_type(Name, #anres{renamings=Renamings}) ->
    gpb_names:apply_msg_type_renaming(Name, Renamings).
