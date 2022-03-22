%%% Copyright (C) 2022  Tomas Abrahamsson
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

-module(gpb_parse_descr).

-export([defs_from_descriptors/1, defs_from_descriptors/2]).
-export([format_error/1]).

-export_type([opts/0, opt/0]).

-include_lib("eunit/include/eunit.hrl").

-include("gpb_descriptor.hrl").
-include("../include/gpb.hrl").

-type opts() :: [opt()].
-type opt() :: boolean_opt(use_package)
             | term().
-type boolean_opt(Opt) :: Opt | {Opt, boolean()}.

-record(env, {has_pkg,
              pkg,
              syntax,
              root_scope,
              use_packages}).

%% gpb_defs:post_process_all_files expects a 5-tuple (from the parser),
%% and not #?gpb_rpc{} which the gpb_defs:def() type says?!
%% Can it be changed bwd compatibly?
-define(tmp_rpc(RpcName, Arg, ArgIsStream, Ret, RetIsStream, Opts),
        {RpcName, {Arg, ArgIsStream}, {Ret, RetIsStream}, Opts}).

%% @equiv defs_from_descriptors(FileDescriptorSet, [])
-spec defs_from_descriptors(#'FileDescriptorSet'{} | binary()) ->
          {ok, gpb_defs:defs()} | {error, Reason::term()}.
defs_from_descriptors(FileDescriptorSet) ->
    defs_from_descriptors(FileDescriptorSet, []).

%% @doc Parse a #'FileDescriptorSet'{} (optionally encoded) to proto
%% defintions.
-spec defs_from_descriptors(#'FileDescriptorSet'{} | binary(), opts()) ->
          {ok, gpb_defs:defs()} | {error, Reason::term()}.
defs_from_descriptors(FileDescriptorSet, Opts)
  when is_binary(FileDescriptorSet) ->
    defs_from_descriptors(
      gpb_descriptor:decode_msg(FileDescriptorSet, 'FileDescriptorSet'),
      Opts);
defs_from_descriptors(#'FileDescriptorSet'{}=Set, Opts) ->
    defs_from_descr_set(Set, Opts).

format_error({error, Reason}) -> fmt_err(Reason);
format_error(Reason) -> fmt_err(Reason).

defs_from_descr_set(#'FileDescriptorSet'{file=Files}, Opts0) ->
    Opts1 = [Opt || Opt <- Opts0, not is_proto_defs_version_opt(Opt)],
    Opts2 = gpb_lib:possibly_adjust_proto_defs_version_opt(Opts0),
    FileChunks = [file_from_descr(PF, Opts1) || PF <- Files],
    Defs0 = lists:append(FileChunks),
    Defs1 = process_field_defaults(Defs0),
    case gpb_defs:post_process_all_files(Defs1, Opts2) of
        {ok, Defs2} ->
            Defs3 = process_map_msgs_and_mapfield_types(Defs2),
            Defs4 = process_groups_types(Defs3),
            TargetVsn = proplists:get_value(proto_defs_version, Opts2),
            case gpb_defs:convert_defs_from_latest_version(Defs4, TargetVsn) of
                {ok, Defs5} ->
                    {ok, Defs5};
                {error, Reason} ->
                    {error, {defs_version, Reason}}
            end;
        {error, Reasons} ->
            {error, {post_process, Reasons}}
    end.

process_map_msgs_and_mapfield_types(Defs) ->
    %% If we have the following Defs:
    %%
    %%     [{{msg,'M'},[?#gpb_field{name=f,
    %%                              type={ref,'MapFieldEntry_1_1'}
    %%                              occurrence=repeated}]},
    %%      {{msg,'MapFieldEntry_1_1'},
    %%       [#?gpb_field{type=int32},
    %%        #?gpb_field{type=string}]},
    %%      {{msg_options,'MapFieldEntry_1_1'},[{map_entry,true}]}]
    %%
    %% Then:
    %%
    %% - replace type of the field M.f with {map,int32,string}
    %% - and remove both the 'MapFieldEntry_1_1' msg and its msg_options
    %%   and also remove it from any msg_containment and proto3_msgs
    MapfieldMsgs = sets:from_list(
                     [MsgName || {{msg_options, MsgName}, MsgOpts} <- Defs,
                                 is_list(MsgOpts),
                                 lists:member({map_entry, true}, MsgOpts)]),
    Maps = lists:foldl(
             fun({{msg, MsgName}, [#?gpb_field{type=KType},
                                   #?gpb_field{type=VType}]},
                 D) ->
                     case sets:is_element(MsgName, MapfieldMsgs) of
                         true  -> dict:store(MsgName, {map, KType, VType}, D);
                         false -> D
                     end;
                (_Other, D) ->
                     D
             end,
             dict:new(),
             Defs),
    process_mapfield_msgs2(Defs, Maps).

process_mapfield_msgs2([Elem | Rest], Maps) ->
    case Elem of
        {{msg, MsgName}, Fields} ->
            case dict:is_key(MsgName, Maps) of
                true ->
                    process_mapfield_msgs2(Rest, Maps); % remove this
                false ->
                    Fields1 =
                        gpb_lib:map_msgdef_fields_o(
                          fun(#?gpb_field{type={msg,SubMsg}}=F, _IsOneof) ->
                                  case dict:find(SubMsg, Maps) of
                                      {ok, MapType} ->
                                          F#?gpb_field{type=MapType};
                                      error ->
                                          F
                                  end;
                             (OtherField, _IsOneof) ->
                                  OtherField
                          end,
                          Fields),
                    Elem1 = {{msg, MsgName}, Fields1},
                    [Elem1 | process_mapfield_msgs2(Rest, Maps)]
            end;
        {{msg_options, MsgName}, _} ->
            case dict:is_key(MsgName, Maps) of
                true ->
                    process_mapfield_msgs2(Rest, Maps); % remove this
                false ->
                    [Elem | process_mapfield_msgs2(Rest, Maps)]
            end;
        {{msg_containment, F}, MsgNames} ->
            MsgNames1 = [MsgName || MsgName <- MsgNames,
                                    not dict:is_key(MsgName, Maps)],
            Elem1 = {{msg_containment, F}, MsgNames1},
            [Elem1 | process_mapfield_msgs2(Rest, Maps)];
        {proto3_msgs, MsgNames} ->
            MsgNames1 = [MsgName || MsgName <- MsgNames,
                                    not dict:is_key(MsgName, Maps)],
            Elem1 = {proto3_msgs, MsgNames1},
            [Elem1 | process_mapfield_msgs2(Rest, Maps)];
        _Other ->
            [Elem | process_mapfield_msgs2(Rest, Maps)]
    end;
process_mapfield_msgs2([], _Maps) ->
    [].

process_groups_types(Defs) ->
    %% Find fields of type {msg,X} with synthetic option '$is_group_ref',
    %% - Remove that option and
    %% - Change definition {{msg,X},Fields} to {{group,X},Fields}
    %% - Drop the group X from msg_containment
    {Defs1, GroupNames} =
        gpb_lib:mapfold_msg_or_group_fields_o(
          fun(_msg, _MsgName, Field, _IsOneof, Acc) ->
                  case Field of
                      #?gpb_field{type={msg,MGName}, opts=FOpts}=F ->
                          case lists:keytake('$is_group_ref', 1, FOpts) of
                              {value, {'$is_group_ref', true}, RestFOpts} ->
                                  F1 = F#?gpb_field{type={group, MGName},
                                                    opts=RestFOpts},
                                  Acc1 = sets:add_element(MGName, Acc),
                                  {F1, Acc1};
                              false ->
                                  {F, Acc}
                          end;
                      F ->
                          {F, Acc}
                  end
          end,
          sets:new(),
          Defs),
    %% Now change {{msg -> group, Name}, Fields}
    %% and drop groups from msg_containment
    [case Elem of
         {{msg, Name}, Fields} ->
             case sets:is_element(Name, GroupNames) of
                 true -> {{group, Name}, Fields};
                 false -> Elem
             end;
         {{msg_containment, Proto}, MsgNames} ->
             MsgNames1 = [Name || Name <- MsgNames,
                                  not sets:is_element(Name, GroupNames)],
             {{msg_containment, Proto}, MsgNames1};
         _ ->
             Elem
     end
     || Elem <- Defs1].

file_from_descr(#'FileDescriptorProto'{name=FName,
                                       package=PkgStr,
                                       dependency=Deps,
                                       message_type=MsgTypes,
                                       enum_type=EnumTypes,
                                       service=Services,
                                       extension=Exts,
                                       %% %options=FileOpts,
                                       syntax=SyntaxStr},
                Opts) ->
    HasPkg = PkgStr /= undefined,
    Pkg = if not HasPkg -> undefined;
             HasPkg     -> pkgstr_to_dotted_atom_list(PkgStr)
          end,
    Syntax = if SyntaxStr == "proto2" -> "proto2";
                SyntaxStr == "proto3" -> "proto3";
                SyntaxStr == undefined -> "proto2";
                true -> error({unexpected_syntax, SyntaxStr, FName})
             end,
    UsePackages = proplists:get_bool(use_packages, Opts),
    Scope = if UsePackages, HasPkg -> ['.' | Pkg];
               HasPkg              -> ['.'];
               true                -> ['.']
            end,
    Env = #env{has_pkg = HasPkg,
               pkg = Pkg,
               syntax = Syntax,
               root_scope = Scope,
               use_packages = UsePackages},
    MsgElems = lists:append(
                 [msg_from_descr(MsgType, Scope, Env)
                  || MsgType <- MsgTypes]),
    ExtElems = ext_elems_from_descr(Exts, Scope, Env),
    EnumElems = lists:append(
                  [enum_from_descr(EnumType, Scope)
                   || EnumType <- EnumTypes]),
    ServiceElems = lists:append(
                     [service_from_descr(Service, Scope)
                      || Service <- Services]),
    MsgContainments = {{msg_containment, FName},
                       [MsgName || {{msg, MsgName}, _Fields} <- MsgElems]},
    EnumContainments = {{enum_containment, FName},
                        [EnumName
                         || {{enum, EnumName}, _} <- EnumElems ++ MsgElems]},
    ServiceNames = [ServiceName
                    || {{service, ServiceName}, _} <- ServiceElems],
    ServiceRpcNames = lists:append(
                        [[{ServiceName, RpcName}
                          || ?tmp_rpc(RpcName, _, _, _, _, _) <- Rpcs]
                         || {{service, ServiceName}, Rpcs} <- ServiceElems]),
    ServiceContainments = [{{service_containment, FName}, ServiceNames}
                           || ServiceNames /= []],
    RpcContainments = [{{rpc_containment, FName}, ServiceRpcNames}
                       || ServiceNames /= []],
    Proto3Msgs = if Syntax == "proto3" ->
                         MsgNames = [MsgName || {{msg,MsgName}, _} <- MsgElems],
                         [{proto3_msgs, MsgNames} || MsgNames /= []];
                    true ->
                         []
                 end,
    lists:append(
      [[{file, {FName, FName}}],
       [{{pkg_containment, FName}, Pkg} || HasPkg andalso UsePackages],
       [{package, Pkg} || HasPkg],
       [MsgContainments, EnumContainments],
       ServiceContainments, RpcContainments,
       [{syntax, Syntax}],
       [{import, Dep} || Dep <- Deps],
       Proto3Msgs,
       MsgElems,
       ExtElems,
       EnumElems,
       ServiceElems]).

msg_from_descr(#'DescriptorProto'{name=MsgNameStr,
                                  nested_type=NestedMsgs,
                                  field=Fields,
                                  enum_type=NestedEnums,
                                  extension=ExtFields,
                                  extension_range=ExtRanges,
                                  reserved_range=ReservedRanges,
                                  reserved_name=ReservedNames,
                                  oneof_decl=OneofDecls,
                                  options=MessageOptions},
               Scope,
               Env) ->
    MsgName = string_to_path_with_scope(MsgNameStr, Scope),
    Nested = lists:append(
               [msg_from_descr(NestedMsg, MsgName, Env)
                || NestedMsg <- NestedMsgs]),
    Enums = lists:append(
              [enum_from_descr(NestedEnum, MsgName)
               || NestedEnum <- NestedEnums]),
    ERs = [range_from_descr(R) || R <- ExtRanges],
    Exts = [{{extensions, MsgName}, ERs} || ERs /= []],
    RRs = [range_from_descr(R) || R <- ReservedRanges],
    ResNums = [{{reserved_numbers, MsgName}, RRs} || RRs /= []],
    ResNames = [{{reserved_names, MsgName}, ReservedNames}
                || ReservedNames /= []],
    OneofCounts = count_oneof_fields(Fields, OneofDecls),
    Fields1 = [field_from_descr(Field, OneofCounts, Env)
               || Field <- Fields],
    Fields2 = collect_oneof_fields(Fields1),
    ExtElems = ext_elems_from_descr(ExtFields, MsgName, Env),
    Msg = {{msg, MsgName}, Fields2},
    Opts = maybe_msg_options_from_descr(MessageOptions),
    MsgOptions = [{{msg_options, MsgName}, Opts} || Opts /= []],
    lists:append(
      [[Msg],
       ExtElems,
       Exts,
       ResNums,
       ResNames,
       Nested, % flatten
       Enums,
       MsgOptions]).

ext_elems_from_descr(DescrFields, Scope, Env) ->
    ext_elems_from_descr_aux(DescrFields, Scope, Env, []).

ext_elems_from_descr_aux([#'FieldDescriptorProto'{extendee=Extendee}=DF | Rest],
                         Scope, Env, Acc) ->
    NoOneofCounts = dict:new(), %% Extend cannot extend oneof elems
    Field = field_from_descr(DF, NoOneofCounts, Env),
    Acc1 = add_ext_field_to_acc(Field, Extendee, Acc),
    ext_elems_from_descr_aux(Rest, Scope, Env, Acc1);
ext_elems_from_descr_aux([], Scope, Env, Acc) ->
    if Acc == [] -> []; % common case; no computations needed
       true -> finalize_ext_field_acc(Acc, Scope, Env)
    end.

add_ext_field_to_acc(Field, Extendee, Acc) ->
    case Acc of
        [{Extendee, Fields} | RestAcc] ->
            [{Extendee, [Field | Fields]} | RestAcc];
        _ ->
            [{Extendee, [Field]} | Acc]
    end.

finalize_ext_field_acc(Acc, Scope, Env) ->
    Acc1 = lists:reverse([{Extendee, lists:reverse(Fields)}
                          || {Extendee, Fields} <- Acc]),
    lists:append(
      [begin
           Name = dotted_atom_list_exact(Extendee),
           %% ERef2 gets resolved in gpb_defs:post_process_all_files/2
           ERef2 = make_eref2(Name, Scope, Env),
           [{{extend, ERef2}, Fields},
            {{ext_origin, ERef2}, {Scope, Fields}}]
       end
       || {Extendee, Fields} <- Acc1]).

make_eref2(Name, Scope, Env) ->
    %% Google's protobuf produces rooted names (including pkg)
    %% but try to work with other impls too, if they behave slightly
    %% different. Correct behaviour is not very well specified.
    IsRooted = case Name of
                   ['.' | _] -> true;
                   _ -> false
               end,
    if IsRooted ->
            Name1 = maybe_unpackage_name(Name, Env),
            {eref2, Scope, Name1, [Name1]};
       true ->
            EmptyRoot = ['.'],
            Candidates =
                gpb_defs:rootward_names(Scope, Name) ++
                gpb_defs:rootward_names(EmptyRoot, Name),
            {eref2, Scope, Name, Candidates}
    end.

maybe_unpackage_name(Name, #env{has_pkg=false}) ->
    Name; % no unpackaging possible
maybe_unpackage_name(Name, #env{use_packages=true}) ->
    Name; % no unpackaging desired
maybe_unpackage_name(Name, #env{has_pkg=true, pkg=Pkg}) ->
    %% Expect package to be a prefix (but check), and drop it
    %% Name is normally with preceding dot (from the descr) but Pkg is not.
    drop_any_prefix(['.' | Pkg], Name).

drop_any_prefix(ProbablyPrefix, List) ->
    case lists:prefix(ProbablyPrefix, List) of
        true -> lists:nthtail(length(ProbablyPrefix), List);
        false -> List
    end.

collect_oneof_fields(Fields) ->
    %% First collect fields for each oneof
    Oneofs = lists:foldl(
               fun(#gpb_oneof{name=CFName, fields=[Field]}, D) ->
                       dict:append(CFName, Field, D);
                  (#?gpb_field{}, D) ->
                       D
               end,
               dict:new(),
               Fields),
    %% Then replace the first such occurrence for each oneof
    {Fields1R, _Empty} =
        lists:foldl(
          fun(#gpb_oneof{name=CFName}=OF, {AccFields, RemainingD}) ->
                  case dict:find(CFName, RemainingD) of
                      {ok, OFields} ->
                          %% First time for this oneof
                          F = OF#gpb_oneof{fields=OFields},
                          RemainingD1 = dict:erase(CFName, RemainingD),
                          {[F | AccFields], RemainingD1};
                      error ->
                          %% Already inserted
                          {AccFields, RemainingD}
                  end;
             (#?gpb_field{}=F, {AccFields, RemainingD}) ->
                  {[F | AccFields], RemainingD}
          end,
          {[], Oneofs},
          Fields),
    lists:reverse(Fields1R).

collect_oneof_fields_test() ->
    [#?gpb_field{name=f1},
     #gpb_oneof{name=c1, fields=[#?gpb_field{name=a1}, #?gpb_field{name=a2}]},
     #gpb_oneof{name=c2, fields=[#?gpb_field{name=b1}, #?gpb_field{name=b2}]},
     #?gpb_field{name=f2}] =
        collect_oneof_fields(
          [#?gpb_field{name=f1},
           #gpb_oneof{name=c1, fields=[#?gpb_field{name=a1}]},
           #gpb_oneof{name=c1, fields=[#?gpb_field{name=a2}]},
           #gpb_oneof{name=c2, fields=[#?gpb_field{name=b1}]},
           #gpb_oneof{name=c2, fields=[#?gpb_field{name=b2}]},
           #?gpb_field{name=f2}]).

count_oneof_fields(Fields, OneofDecls) ->
    %% Return a dict OneofDecl -> {OneofName, HowManyInTheOneof}
    lists:foldl(
      fun(#'FieldDescriptorProto'{oneof_index=OneofIndex}, D) ->
              if is_integer(OneofIndex) ->
                      CFName = find_oneof_name(OneofIndex, OneofDecls),
                      case dict:find(OneofIndex, D) of
                          {ok, {CFName, N}} ->
                              dict:store(OneofIndex, {CFName, N + 1}, D);
                          error ->
                              dict:store(OneofIndex, {CFName, 1}, D)
                      end;
                 OneofIndex == undefined ->
                      D
              end
      end,
      dict:new(),
      Fields).

find_oneof_name(OneofIndex, OneofDecls) ->
    if 0 =< OneofDecls, OneofIndex =< (length(OneofDecls) - 1) ->
            Pos = OneofIndex + 1,
            #'OneofDescriptorProto'{name=OName} = lists:nth(Pos, OneofDecls),
            string_to_atom(OName);
       true ->
            error({invalid_oneof_index,
                   OneofDecls,
                   {0, length(OneofDecls) - 1}})
    end.

field_from_descr(#'FieldDescriptorProto'{name=FName,
                                         number=FNum,
                                         label=Label,
                                         type=DType,
                                         type_name=TypeName,
                                         oneof_index=OneofIndex,
                                         default_value=Default,
                                         options=FieldOptions,
                                         json_name=JsonName,
                                         proto3_optional=IsP3Optional},
                 OneofCounts, #env{syntax=Syntax}) ->
    TypePath = if TypeName /= undefined -> dotted_atom_list_exact(TypeName);
                  TypeName == undefined -> undefined
               end,
    Type = field_type_from_descr_simple(DType, TypePath),
    Occurrence = case Label of
                     'LABEL_OPTIONAL' ->
                         case Syntax of
                             "proto3" ->
                                 case IsP3Optional of
                                     true      -> optional;
                                     false     -> defaulty;
                                     undefined -> defaulty
                                 end;
                             "proto2" ->
                                 optional
                         end;
                     'LABEL_REQUIRED' ->
                         required;
                     'LABEL_REPEATED' ->
                         repeated
                 end,
    FieldOpts = maybe_field_options_from_descr(FieldOptions, JsonName, Default,
                                               DType),
    Field = #?gpb_field{name = string_to_atom(FName),
                        fnum = FNum,
                        type = Type, % msg and enum types get resolved later
                        occurrence = Occurrence,
                        opts = FieldOpts},
    if is_integer(OneofIndex) ->
            case dict:fetch(OneofIndex, OneofCounts) of
                {_CFName, 1} when Syntax == "proto3",
                                  Occurrence == optional ->
                    %% This is a proto3 optional message wrapped in a
                    %% synthetic oneof for bwd compat with proto3 clients.
                    Field;
                {CFName, _N} ->
                    %% Other fields belonging to the same OName are
                    %% collected at an upper enclosing level
                    Field1 = Field#?gpb_field{occurrence = optional},
                    #gpb_oneof{name=CFName, fields=[Field1]}
            end;
       OneofIndex == undefined ->
            Field
    end.

field_type_from_descr_simple('TYPE_INT32', _TypeName)    -> int32;
field_type_from_descr_simple('TYPE_INT64', _TypeName)    -> int64;
field_type_from_descr_simple('TYPE_UINT32', _TypeName)   -> uint32;
field_type_from_descr_simple('TYPE_UINT64', _TypeName)   -> uint64;
field_type_from_descr_simple('TYPE_SINT32', _TypeName)   -> sint32;
field_type_from_descr_simple('TYPE_SINT64', _TypeName)   -> sint64;
field_type_from_descr_simple('TYPE_FIXED32', _TypeName)  -> fixed32;
field_type_from_descr_simple('TYPE_FIXED64', _TypeName)  -> fixed64;
field_type_from_descr_simple('TYPE_SFIXED32', _TypeName) -> sfixed32;
field_type_from_descr_simple('TYPE_SFIXED64', _TypeName) -> sfixed64;
field_type_from_descr_simple('TYPE_BOOL', _TypeName)     -> bool;
field_type_from_descr_simple('TYPE_FLOAT', _TypeName)    -> float;
field_type_from_descr_simple('TYPE_DOUBLE', _TypeName)   -> double;
field_type_from_descr_simple('TYPE_STRING', _TypeName)   -> string;
field_type_from_descr_simple('TYPE_BYTES', _TypeName)    -> bytes;
%% Refs will be translated later.
field_type_from_descr_simple('TYPE_ENUM', TypeName)      -> {ref, TypeName};
field_type_from_descr_simple('TYPE_MESSAGE', TypeName)   -> {ref, TypeName};
field_type_from_descr_simple('TYPE_GROUP', TypeName)     -> {ref, TypeName};
field_type_from_descr_simple(undefined, TypeName)        -> {ref, TypeName}.

process_field_defaults(Defs) ->
    gpb_lib:map_msg_fields_o(
      fun(_MsgName, #?gpb_field{type=Type, opts=Opts}=F, _IsOneof) ->
              Opts1 = [case Opt of
                           {default, D} -> process_field_default_2(D, Type);
                           _Other       -> Opt
                       end
                       || Opt <- Opts],
              F#?gpb_field{opts=Opts1}
      end,
      Defs).

process_field_default_2(Default, Type) ->
    case Type of
        bool ->
            case Default of
                "true" -> {default, true};
                "false" -> {default, true};
                _ -> false
            end;
        Int when Int == int32; Int == int64;
                 Int == uint32; Int == uint64;
                 Int == sint32; Int == sint64;
                 Int == fixed32; Int == fixed64;
                 Int == sfixed32; Int == sfixed64 ->
            case gpb_scan:read_number(Default) of
                {ok, N} when is_integer(N) -> {default, N};
                _ -> false
            end;
        Float when Float == float;
                   Float == double ->
            case gpb_scan:read_number(Default) of
                {ok, F} when is_number(F) -> {default, float(F)};
                {ok, infinity}            -> {default, infinity};
                {ok, '-infinity'}         -> {default, '-infinity'};
                {ok, nan}                 -> {default, nan};
                _ -> false
            end;
        string ->
            {default, Default};
        bytes ->
            {default, c_unescape(Default)};
        {ref, _EnumName} ->
            %% at this point it is still an unresolved ref
            {default, string_to_atom(Default)};
        _ ->
            error({default_value_for_unexpected_type, Type})
    end.

enum_from_descr(#'EnumDescriptorProto'{name=Name,
                                       value=EnumValues,
                                       options=EnumOptions},
                Scope) ->
    EName = string_to_path_with_scope(Name, Scope),
    Enums = enum_values_from_descr(EnumValues),
    Opts = maybe_enum_options_from_descr(EnumOptions),
    lists:append(
      [[{{enum, EName}, Enums}],
       [{{enum_options, EName}, Opts} || Opts /= []]]).

enum_values_from_descr(EnumValues) ->
    lists:map(
      fun(#'EnumValueDescriptorProto'{name=Sym, number=Num, options=EOpts}) ->
              O = maybe_enum_value_options_from_descr(EOpts),
              {string_to_atom(Sym), Num, O}
      end,
      EnumValues).

maybe_msg_options_from_descr(MessageOptions) ->
    FNames = record_info(fields, 'MessageOptions'),
    get_options(FNames, MessageOptions, #'MessageOptions'{}).

maybe_field_options_from_descr(FieldOptions, JsonName, DefaultStr, DType) ->
    FNames = record_info(fields, 'FieldOptions'),
    get_options(FNames, FieldOptions, #'FieldOptions'{}) ++
        [{json_name, JsonName} || JsonName /= undefined] ++
        [{default, DefaultStr} || DefaultStr /= undefined] ++ % processed later
        [{'$is_group_ref', true} || DType == 'TYPE_GROUP']. % processed later

maybe_enum_options_from_descr(EnumOptions) ->
    FNames = record_info(fields, 'EnumOptions'),
    get_options(FNames, EnumOptions, #'EnumOptions'{}).

maybe_enum_value_options_from_descr(EnumValueOptions) ->
    FNames = record_info(fields, 'EnumValueOptions'),
    get_options(FNames, EnumValueOptions, #'EnumValueOptions'{}).

get_options(_FNames, undefined, _XDefault) -> [];
get_options(FNames, XOptions, XDefault) ->
    KVDs = lists:zip3(FNames,
                      tl(tuple_to_list(XOptions)),
                      tl(tuple_to_list(XDefault))),
    lists:reverse(
      lists:foldl(
        fun({FName, Value, Default}, Acc) ->
                if FName == uninterpreted_option -> Acc;
                   Value == undefined -> Acc;
                   Value == Default   -> Acc;
                   Value /= Default   -> [{FName, Value} | Acc]
                end
        end,
        [],
        KVDs)).

range_from_descr(#'DescriptorProto.ExtensionRange'{start=Start, 'end'=End}) ->
    LowerIncl = Start,
    UpperIncl = if End == undefined -> max;
                   End == 16#20000000 -> max;
                   is_integer(End) -> End - 1
                end,
    {LowerIncl, UpperIncl};
range_from_descr(#'DescriptorProto.ReservedRange'{start=Start, 'end'=End}) ->
    LowerIncl = Start,
    UpperExcl = if End == undefined -> max;
                   is_integer(End) -> End
                end,
    if LowerIncl + 1 == UpperExcl -> LowerIncl;
       UpperExcl == 16#20000000   -> {LowerIncl, max};
       true                       -> {LowerIncl, UpperExcl - 1}
    end.

service_from_descr(#'ServiceDescriptorProto'{name=Name,
                                             method=Methods,
                                             options=Options},
                   Scope) ->
    SName = string_to_path_with_scope(Name, Scope),
    Opts = maybe_service_options_from_descr(Options),
    ServiceOpts = [{{service_options, SName}, Opts} || Opts /= []],
    [{{service, SName}, [method_from_descr(Method)
                         || Method <- Methods]}] ++
        ServiceOpts.

method_from_descr(#'MethodDescriptorProto'{name=Name,
                                           input_type=Input,
                                           output_type=Output,
                                           client_streaming=ClientStreaming,
                                           server_streaming=ServerStreaming,
                                           options=Options}) ->
    Arg = dotted_atom_list_exact(Input),
    Ret = dotted_atom_list_exact(Output),
    ArgIsStream = bool_from_descr(ClientStreaming, false),
    RetIsStream = bool_from_descr(ServerStreaming, false),
    MOpts1 = maybe_method_options_from_descr(Options),
    MOpts2 = [{option, K, V} || {K, V} <- MOpts1],
    ?tmp_rpc(string_to_atom(Name),
             Arg, ArgIsStream,
             Ret, RetIsStream,
             MOpts2).

maybe_service_options_from_descr(ServiceOptions) ->
    FNames = record_info(fields, 'ServiceOptions'),
    get_options(FNames, ServiceOptions, #'ServiceOptions'{}).

maybe_method_options_from_descr(MethodOptions) ->
    FNames = record_info(fields, 'MethodOptions'),
    get_options(FNames, MethodOptions, #'MethodOptions'{}).

%%

fmt_err({post_process, Reasons}) ->
    [gpb_defs:format_error(Reason) || Reason <- Reasons];
fmt_err({defs_version, Reason}) ->
    gpb_defs:format_error(Reason);
fmt_err(Other) ->
    io_lib:format("Unexpected error: ~p", [Other]).

is_proto_defs_version_opt({proto_defs_version, _}) -> true;
is_proto_defs_version_opt(_) -> false.

pkgstr_to_dotted_atom_list(PkgStr) ->
    %[list_to_atom(Elem) || Elem <- gpb_lib:string_lexemes(PkgStr, ".")].
    dotted_atom_list_exact(PkgStr).

string_to_path_with_scope(MsgNameStr, ['.']) ->
    ['.', string_to_atom(MsgNameStr)];
string_to_path_with_scope(MsgNameStr, Scope) ->
    Scope ++ ['.', string_to_atom(MsgNameStr)].

dotted_atom_list_exact(Str) ->
    dot_atoms(Str, none, []).

dot_atoms("." ++ Rest, CurrComp, Acc) ->
    Acc1 = ['.' | add_comp_to_acc(CurrComp, Acc)],
    dot_atoms(Rest, none, Acc1);
dot_atoms([C | Rest], CurrComp, Acc) ->
    CurrComp1 = add_to_curr_comp(C, CurrComp),
    dot_atoms(Rest, CurrComp1, Acc);
dot_atoms("", CurrComp, Acc) ->
    Acc1 = add_comp_to_acc(CurrComp, Acc),
    lists:reverse(Acc1).

add_comp_to_acc(none, Acc) -> Acc;
add_comp_to_acc(Comp, Acc) -> [string_to_atom(lists:reverse(Comp)) | Acc].

add_to_curr_comp(C, none) -> [C];
add_to_curr_comp(C, Comp) -> [C | Comp].

string_to_atom(S) ->
    list_to_atom(S). % need to do anything utf-8 related?

c_unescape(S) ->
    c_unescape2(S, <<>>).

c_unescape2("\\x" ++ Rest, Acc) ->
    {X, Rest2} = collect_chars(Rest, fun is_hex_digit/1, 2),
    N = list_to_integer(X, 16),
    c_unescape2(Rest2, <<Acc/binary, N>>);
c_unescape2("\\" ++ Rest, Acc) ->
    {O, Rest2} = collect_chars(Rest, fun is_oct_digit/1, 3),
    N = list_to_integer(O, 8),
    c_unescape2(Rest2, <<Acc/binary, N>>);
c_unescape2([C | Rest], Acc) ->
    c_unescape2(Rest, <<Acc/binary, C>>);
c_unescape2("", Acc) ->
    Acc.

collect_chars(Str, Pred, MaxNumChars) ->
    collect2(Str, Pred, MaxNumChars, "").

collect2([C | Rest], Pred, MaxNumChars, Acc) ->
    case Pred(C) of
        true when length(Acc) + 1 == MaxNumChars ->
            {lists:reverse([C | Acc]), Rest};
        true ->
            collect2(Rest, Pred, MaxNumChars, [C | Acc]);
        false ->
            {lists:reverse(Acc), [C | Rest]}
    end;
collect2("", _Pred, _MaxNumChars, Acc) ->
    {lists:reverse(Acc), ""}.

is_hex_digit(C) when $0 =< C, C =< $9 -> true;
is_hex_digit(C) when $a =< C, C =< $f -> true;
is_hex_digit(C) when $A =< C, C =< $F -> true;
is_hex_digit(_) -> false.

is_oct_digit(C) when $0 =< C, C =< $7 -> true;
is_oct_digit(_) -> false.

c_unescape_test() ->
    <<"abc", 255, 254>> = c_unescape("abc\\xff\\376").

bool_from_descr(undefined, Default) -> Default;
bool_from_descr(true,  _Default) -> true;
bool_from_descr(false, _Default) -> false.

%% -> [{index,element}] from a list, first index is 2
index2_seq([]) -> [];
index2_seq(L)  -> lists:zip(lists:seq(2, 1 + length(L)), L).

index2_seq_test() ->
    [] = index2_seq([]),
    [{2, a}] = index2_seq([a]),
    [{2, a}, {3, b}] = index2_seq([a, b]).
