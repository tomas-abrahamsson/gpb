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

%%% @doc Generation of introspection functions.
%%% @private

-module(gpb_gen_introspect).

-export([format_exports/3]).
-export([format_introspection/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2, repeat_clauses/2]).

format_exports(Defs, _AnRes, _Opts) ->
    [?f("-export([get_msg_defs/0]).~n"),
     ?f("-export([get_msg_names/0]).~n"),
     ?f("-export([get_group_names/0]).~n"),
     ?f("-export([get_msg_or_group_names/0]).~n"),
     ?f("-export([get_enum_names/0]).~n"),
     ?f("-export([find_msg_def/1, fetch_msg_def/1]).~n"),
     ?f("-export([find_enum_def/1, fetch_enum_def/1]).~n"),
     format_enum_value_symbol_converter_exports(Defs),
     ?f("-export([get_service_names/0]).~n"),
     ?f("-export([get_service_def/1]).~n"),
     ?f("-export([get_rpc_names/1]).~n"),
     ?f("-export([find_rpc_def/2, fetch_rpc_def/2]).~n"),
     ?f("-export([fqbin_to_service_name/1]).~n"),
     ?f("-export([service_name_to_fqbin/1]).~n"),
     ?f("-export([fqbins_to_service_and_rpc_name/2]).~n"),
     ?f("-export([service_and_rpc_name_to_fqbins/2]).~n"),
     ?f("-export([get_package_name/0]).~n"),
     ?f("-export([uses_packages/0]).~n")].

format_introspection(Defs, AnRes, Opts) ->
    Package = proplists:get_value(package, Defs, ''),
    MsgDefs  = [Item || {{msg, _}, _}=Item <- Defs],
    EnumDefs = [Item || {{enum, _}, _}=Item <- Defs],
    GroupDefs = [Item || {{group, _}, _}=Item <- Defs],
    ServiceDefs = [Item || {{service, _}, _}=Item <- Defs],
    ServiceInfos = compute_service_renaming_infos(ServiceDefs, Package,
                                                  AnRes, Opts),
    [gpb_codegen:format_fn(
       get_msg_defs, fun() -> '<Defs>' end,
       [replace_tree('<Defs>', msg_def_trees(EnumDefs, MsgDefs, GroupDefs,
                                             Opts))]),
     "\n",
     gpb_codegen:format_fn(
       get_msg_names, fun() -> '<Names>' end,
       [replace_term('<Names>', [MsgName || {{msg,MsgName}, _} <- Defs])]),
     "\n",
     gpb_codegen:format_fn(
       get_group_names, fun() -> '<Names>' end,
       [replace_term('<Names>', [Name || {{group,Name}, _} <- Defs])]),
     "\n",
     gpb_codegen:format_fn(
       get_msg_or_group_names, fun() -> '<Names>' end,
       [replace_term('<Names>', gpb_lib:msg_or_group_names(Defs))]),
     "\n",
     gpb_codegen:format_fn(
       get_enum_names, fun() -> '<Names>' end,
       [replace_term('<Names>', [EnumName || {{enum,EnumName}, _} <- Defs])]),
     "\n",
     format_fetch_msg_defs(gpb_lib:msgs_or_groups(Defs)),
     ?f("~n"),
     format_fetch_enum_defs(EnumDefs),
     ?f("~n"),
     format_find_msg_defs(Defs, Opts),
     ?f("~n"),
     format_find_enum_defs(EnumDefs),
     ?f("~n"),
     format_enum_value_symbol_converters(EnumDefs),
     ?f("~n"),
     format_get_service_names(ServiceDefs),
     ?f("~n"),
     format_get_service_defs(ServiceDefs, Opts),
     ?f("~n"),
     format_get_rpc_names(ServiceDefs),
     ?f("~n"),
     format_find_rpc_defs(ServiceDefs),
     ?f("~n"),
     [format_find_service_rpc_defs(ServiceName, Rpcs, Opts)
      || {{service, ServiceName}, Rpcs} <- ServiceDefs],
     ?f("~n"),
     format_fetch_rpc_defs(ServiceDefs, Opts),
     ?f("~n"),
     format_fqbin_to_service_name(ServiceInfos),
     ?f("~n"),
     format_service_name_to_fqbin(ServiceInfos),
     ?f("~n"),
     format_fqbins_to_service_and_rpc_name(ServiceInfos),
     ?f("~n"),
     format_service_and_rpc_name_to_fqbins(ServiceInfos),
     ?f("~n"),
     format_get_package_name(Defs),
     ?f("~n"),
     format_uses_packages(Opts)
    ].

msg_def_trees(EnumDefs, MsgDefs, GroupDefs, Opts) ->
    EnumDefTrees = [erl_parse:abstract(EnumDef) || EnumDef <- EnumDefs],
    MsgDefTrees = [msg_def_tree(MsgDef, Opts) || MsgDef <- MsgDefs],
    GroupDefTrees = [group_def_tree(GroupDef, Opts) || GroupDef <- GroupDefs],
    erl_syntax:list(EnumDefTrees ++ MsgDefTrees ++ GroupDefTrees).

msg_def_tree({{msg, MsgName}, Fields}, Opts) ->
    erl_syntax:tuple(
      [erl_syntax:tuple([erl_syntax:atom(msg), erl_syntax:atom(MsgName)]),
       fields_tree(Fields, Opts)]).

group_def_tree({{group, Name}, Fields}, Opts) ->
    erl_syntax:tuple(
      [erl_syntax:tuple([erl_syntax:atom(group), erl_syntax:atom(Name)]),
       fields_tree(Fields, Opts)]).

fields_tree(Fields, Opts) ->
    case gpb_lib:get_field_format_by_opts(Opts) of
        fields_as_records ->
            erl_syntax:list([field_tree(Field, Opts) || Field <- Fields]);
        fields_as_maps ->
            erl_syntax:list([field_tree(Field, Opts) || Field <- Fields]);
        fields_as_proplists ->
            erl_parse:abstract(gpb:field_records_to_proplists(Fields))
    end.

field_tree(#?gpb_field{}=F, Opts) ->
    [?gpb_field | FValues] = tuple_to_list(F),
    FNames = record_info(fields, ?gpb_field),
    gpb_lib:mapping_create(
      ?gpb_field,
      lists:zip(FNames,
                [erl_parse:abstract(FValue) || FValue <- FValues]),
      gpb_lib:mk_get_defs_as_maps_or_records_fn(Opts),
      Opts);
field_tree(#gpb_oneof{fields=OFields}=F, Opts) ->
    [gpb_oneof | FValues] = tuple_to_list(F),
    FNames = record_info(fields, gpb_oneof),
    gpb_lib:mapping_create(
      gpb_oneof,
      [if FName == fields -> {FName, fields_tree(OFields, Opts)};
          FName /= fields -> {FName, erl_parse:abstract(FValue)}
       end
       || {FName, FValue} <- lists:zip(FNames, FValues)],
      gpb_lib:mk_get_defs_as_maps_or_records_fn(Opts),
      Opts).

format_fetch_msg_defs([]) ->
    ["-spec fetch_msg_def(_) -> no_return().\n",
     gpb_codegen:format_fn(
       fetch_msg_def,
       fun(MsgName) -> erlang:error({no_such_msg, MsgName}) end)];
format_fetch_msg_defs(_MsgDefs) ->
    gpb_codegen:format_fn(
      fetch_msg_def,
      fun(MsgName) ->
              case find_msg_def(MsgName) of
                  Fs when is_list(Fs) -> Fs;
                  error               -> erlang:error({no_such_msg, MsgName})
              end
      end).

format_fetch_enum_defs([]) ->
    ["-spec fetch_enum_def(_) -> no_return().\n",
     gpb_codegen:format_fn(
       fetch_enum_def,
       fun(EnumName) -> erlang:error({no_such_enum, EnumName}) end)];
format_fetch_enum_defs(_EnumDefs) ->
    gpb_codegen:format_fn(
      fetch_enum_def,
      fun(EnumName) ->
              case find_enum_def(EnumName) of
                  Es when is_list(Es) -> Es;
                  error               -> erlang:error({no_such_enum, EnumName})
              end
      end).

format_find_msg_defs(Defs, Opts) ->
    gpb_codegen:format_fn(
      find_msg_def,
      fun('<Name>') -> '<Fields>';
         (_) -> error
      end,
      [repeat_clauses('<Name>',
                      [[replace_term('<Name>', Name),
                        replace_tree('<Fields>', fields_tree(Fields, Opts))]
                       || {_, Name, Fields} <- gpb_lib:msgs_or_groups(Defs)])]).

format_find_enum_defs(Enums) ->
    gpb_codegen:format_fn(
      find_enum_def,
      fun('<EnumName>') -> '<Values>';
         (_) -> error
      end,
      [repeat_clauses('<EnumName>',
                      [[replace_term('<EnumName>', EnumName),
                        replace_term('<Values>', Values)]
                       || {{enum, EnumName}, Values} <- Enums])]).


format_enum_value_symbol_converter_exports(Defs) ->
    [?f("-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).~n"),
     [begin
         ToSymFnName = gpb_lib:mk_fn(enum_symbol_by_value_, EnumName),
         ToValFnName = gpb_lib:mk_fn(enum_value_by_symbol_, EnumName),
         ?f("-export([~p/1, ~p/1]).~n", [ToSymFnName, ToValFnName])
     end
     || {{enum, EnumName}, _EnumDef} <- Defs]].

format_enum_value_symbol_converters(EnumDefs) when EnumDefs /= [] ->
    %% A difference between this function and `d_enum_X' as generated
    %% by `format_enum_decoders' is that this function generates
    %% value/symbol converters for all enums, not only for the ones
    %% that are used in messages.
    [gpb_codegen:format_fn(
       enum_symbol_by_value,
       fun('<EnumName>', Value) -> 'cvt'(Value) end,
       [repeat_clauses(
          '<EnumName>',
          [[replace_term('<EnumName>', EnumName),
            replace_term('cvt', gpb_lib:mk_fn(enum_symbol_by_value_, EnumName))]
           || {{enum, EnumName}, _EnumDef} <- EnumDefs])]),
     "\n",
     gpb_codegen:format_fn(
       enum_value_by_symbol,
       fun('<EnumName>', Sym) -> 'cvt'(Sym) end,
       [repeat_clauses(
          '<EnumName>',
          [[replace_term('<EnumName>', EnumName),
            replace_term('cvt', gpb_lib:mk_fn(enum_value_by_symbol_, EnumName))]
           || {{enum, EnumName}, _EnumDef} <- EnumDefs])]),
     "\n",
     [[gpb_codegen:format_fn(
         gpb_lib:mk_fn(enum_symbol_by_value_, EnumName),
         fun('<Value>') -> '<Sym>' end,
         [repeat_clauses('<Value>',
                         [[replace_term('<Value>', EnumValue),
                           replace_term('<Sym>', EnumSym)]
                          || {EnumSym, EnumValue} <- gpb_lib:unalias_enum(
                                                       EnumDef)])]),
       "\n",
       gpb_codegen:format_fn(
         gpb_lib:mk_fn(enum_value_by_symbol_, EnumName),
         fun('<Sym>') -> '<Value>' end,
         [repeat_clauses('<Sym>',
                         [[replace_term('<Value>', EnumValue),
                           replace_term('<Sym>', EnumSym)]
                          || {EnumSym, EnumValue} <- EnumDef])])]
      || {{enum, EnumName}, EnumDef} <- EnumDefs]];
format_enum_value_symbol_converters([]=_EnumDefs) ->
    ["-spec enum_symbol_by_value(_, _) -> no_return().\n",
     gpb_codegen:format_fn(
       enum_symbol_by_value,
       fun(E, V) -> erlang:error({no_enum_defs, E, V}) end),
     "\n",
     "-spec enum_value_by_symbol(_, _) -> no_return().\n",
     gpb_codegen:format_fn(
       enum_value_by_symbol,
       fun(E, V) -> erlang:error({no_enum_defs, E, V}) end),
     "\n"].

format_get_package_name(Defs) ->
    case lists:keyfind(package, 1, Defs) of
        false ->
            gpb_codegen:format_fn(
              get_package_name, fun() -> undefined end);
        {package, Package} ->
            gpb_codegen:format_fn(
              get_package_name, fun() -> '<Package>' end,
              [replace_term('<Package>', Package)])
    end.

format_uses_packages(Opts) ->
    ["%% Whether or not the message names\n"
     "%% are prepended with package name or not.\n",
     gpb_codegen:format_fn(
       uses_packages, fun() -> '<bool>' end,
       [replace_term('<bool>', proplists:get_bool(use_packages, Opts))])].

% --- service introspection methods

format_get_service_names(ServiceDefs) ->
    gpb_codegen:format_fn(
      get_service_names,
      fun() -> '<ServiceNames>' end,
      [replace_term(
         '<ServiceNames>',
         [ServiceName || {{service, ServiceName}, _Rpcs} <- ServiceDefs])]).

format_get_service_defs(ServiceDefs, Opts) ->
    gpb_codegen:format_fn(
      get_service_def,
      fun('<ServiceName>') -> '<ServiceDef>';
         (_) -> error
      end,
      [repeat_clauses(
         '<ServiceName>',
         [[replace_term('<ServiceName>', ServiceName),
           replace_tree('<ServiceDef>', service_def_tree(ServiceDef, Opts))]
          || {{service, ServiceName}, _Rpcs} = ServiceDef <- ServiceDefs])]).

format_get_rpc_names(ServiceDefs) ->
    gpb_codegen:format_fn(
      get_rpc_names,
      fun('<ServiceName>') -> '<ServiceRpcNames>';
         (_) -> error
      end,
      [repeat_clauses('<ServiceName>',
                      [[replace_term('<ServiceName>', ServiceName),
                        replace_term('<ServiceRpcNames>',
                                     [RpcName
                                      || #?gpb_rpc{name=RpcName} <- Rpcs])]
                       || {{service, ServiceName}, Rpcs} <- ServiceDefs])]).

format_find_rpc_defs(ServiceDefs) ->
    gpb_codegen:format_fn(
      find_rpc_def,
      fun('<ServiceName>', RpcName) -> '<ServiceFindRpcDef>'(RpcName);
         (_, _) -> error
      end,
      [repeat_clauses(
         '<ServiceName>',
         [[replace_term('<ServiceName>', ServiceName),
           replace_term('<ServiceFindRpcDef>',
                        gpb_lib:mk_fn(find_rpc_def_, ServiceName))]
          || {{service, ServiceName}, _} <- ServiceDefs])]).

format_find_service_rpc_defs(ServiceName, Rpcs, Opts) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(find_rpc_def_, ServiceName),
      fun('<RpcName>') -> '<RpcDef>';
         (_) -> error
      end,
      [repeat_clauses('<RpcName>',
                      [[replace_term('<RpcName>', RpcName),
                        replace_tree('<RpcDef>', rpc_def_tree(Rpc, Opts))]
                       || #?gpb_rpc{name=RpcName} = Rpc <- Rpcs])]).

format_fetch_rpc_defs([], _Opts) ->
    ["-spec fetch_rpc_def(_, _) -> no_return().\n",
     gpb_codegen:format_fn(
       fetch_rpc_def,
       fun(ServiceName, RpcName) ->
               erlang:error({no_such_rpc, ServiceName, RpcName})
       end)];
format_fetch_rpc_defs(_ServiceDefs, Opts) ->
    gpb_codegen:format_fn(
      fetch_rpc_def,
      fun(ServiceName, RpcName) ->
              case find_rpc_def(ServiceName, RpcName) of
                  Def when is_X(Def) -> Def;
                  error -> erlang:error({no_such_rpc, ServiceName, RpcName})
              end
      end,
      [replace_term(is_X,
                    case get_rpc_format_by_opts(Opts) of
                        rpcs_as_proplists ->
                            is_list;
                        rpcs_as_records ->
                            case gpb_lib:get_records_or_maps_by_opts(Opts) of
                                maps    -> is_map;
                                records -> is_tuple
                            end
                    end)]).

service_def_tree({{service, ServiceName}, Rpcs}, Opts) ->
    erl_syntax:tuple(
      [erl_syntax:tuple([erl_syntax:atom(service),
                         erl_syntax:atom(ServiceName)]),
       rpcs_def_tree(Rpcs, Opts)]).

get_rpc_format_by_opts(Opts) ->
    case proplists:get_bool(defs_as_proplists, proplists:unfold(Opts)) of
        false -> rpcs_as_records; %% default
        true  -> rpcs_as_proplists
    end.

rpc_record_def_tree(#?gpb_rpc{}=Rpc, Opts) ->
    [?gpb_rpc | RValues] = tuple_to_list(Rpc),
    RNames = record_info(fields, ?gpb_rpc),
    gpb_lib:mapping_create(
      ?gpb_rpc,
      lists:zip(RNames,
                [erl_parse:abstract(RValue) || RValue <- RValues]),
      gpb_lib:mk_get_defs_as_maps_or_records_fn(Opts),
      Opts).

rpcs_def_tree(Rpcs, Opts) ->
    case get_rpc_format_by_opts(Opts) of
        rpcs_as_records   ->
            erl_syntax:list([rpc_record_def_tree(Rpc, Opts) || Rpc <- Rpcs]);
        rpcs_as_proplists ->
            erl_parse:abstract(gpb:rpc_records_to_proplists(Rpcs))
    end.

rpc_def_tree(#?gpb_rpc{}=Rpc, Opts) ->
    case get_rpc_format_by_opts(Opts) of
        rpcs_as_records   ->
            rpc_record_def_tree(Rpc, Opts);
        rpcs_as_proplists ->
            erl_parse:abstract(gpb:rpc_record_to_proplist(Rpc))
    end.

compute_service_renaming_infos(ServiceDefs,
                               Package,
                               #anres{renamings = Renamings},
                               Opts) ->
    SvcRenamings = renamings_as_list(services, Renamings),
    RpcRenamings = renamings_as_list(rpcs, Renamings),
    UsesPackages = proplists:get_bool(use_packages, Opts),
    [begin
         {OrigSvc,SvcName} = find_orig_from_renamed(SvcName, SvcRenamings),
         FqOrigSvc =
             if UsesPackages ->
                     OrigSvc;
                not UsesPackages, Package /= '' ->
                     list_to_atom(lists:concat([Package, ".", OrigSvc]));
                not UsesPackages, Package == '' ->
                     OrigSvc
             end,
         RpcMapping = [find_orig_rpc_from_renamed(RpcName, RpcRenamings)
                       || #?gpb_rpc{name=RpcName} <- Rpcs],
         {{FqOrigSvc, SvcName}, RpcMapping}
     end
     || {{service,SvcName}, Rpcs} <- ServiceDefs].

renamings_as_list(_Key, no_renamings) ->
    no_renamings;
renamings_as_list(Key, Renamings) ->
    case proplists:get_value(Key, Renamings) of
        D when D /= undefined ->
            dict:to_list(D);
        undefined ->
            error({internal_error, no_key, Key, [K || {K, _} <- Renamings]})
    end.

find_orig_from_renamed(Name, no_renamings) ->
    {Name, Name};
find_orig_from_renamed(Renamed, Renamings) ->
    lists:keyfind(Renamed, 2, Renamings).

find_orig_rpc_from_renamed(Name, no_renamings) ->
    {Name, Name};
find_orig_rpc_from_renamed(Renamed, Renamings) ->
    {{_Svc, OrigRpc}, RenamedRpc} = lists:keyfind(Renamed, 2, Renamings),
    {OrigRpc, RenamedRpc}.

format_fqbin_to_service_name(ServiceInfos) ->
    ["%% Convert a a fully qualified (ie with package name) service name\n"
     "%% as a binary to a service name as an atom.\n",
     gpb_codegen:format_fn(
       fqbin_to_service_name,
       fun('<<"maybe.package.ServiceName">>') -> 'ServiceName';
          (X) -> error({gpb_error, {badservice, X}})
       end,
       [repeat_clauses(
          '<<"maybe.package.ServiceName">>',
          [[replace_tree('<<"maybe.package.ServiceName">>',
                         atom_to_binstr_stree(FqOrigServiceName)),
            replace_term('ServiceName', ServiceName)]
           || {{FqOrigServiceName, ServiceName}, _Rpcs} <- ServiceInfos])])].

format_service_name_to_fqbin(ServiceInfos) ->
    ["%% Convert a service name as an atom to a fully qualified\n\n"
     "%% (ie with package name) name as a binary.\n",
     gpb_codegen:format_fn(
       service_name_to_fqbin,
       fun('ServiceName') -> '<<"maybe.package.ServiceName">>';
          (X) -> error({gpb_error, {badservice, X}})
       end,
       [repeat_clauses(
          'ServiceName',
          [[replace_term('ServiceName', ServiceName),
            replace_tree('<<"maybe.package.ServiceName">>',
                         atom_to_binstr_stree(FqOrigServiceName))]
           || {{FqOrigServiceName, ServiceName}, _Rpcs} <- ServiceInfos])])].

format_fqbins_to_service_and_rpc_name(ServiceInfos) ->
    ["%% Convert a a fully qualified (ie with package name) service name\n"
     "%% and an rpc name, both as binaries to a service name and an rpc\n"
     "%% name, as atoms.\n",
     gpb_codegen:format_fn(
       fqbins_to_service_and_rpc_name,
       fun('<<"maybe.package.ServiceName">>', '<<"RpcName">>') ->
               {'ServiceName', 'RpcName'};
          (S, R) ->
               error({gpb_error, {badservice_or_rpc, {S, R}}})
       end,
       [repeat_clauses(
          '<<"maybe.package.ServiceName">>',
          [[replace_tree('<<"maybe.package.ServiceName">>',
                         atom_to_binstr_stree(FqOrigServiceName)),
            replace_tree('<<"RpcName">>',
                         atom_to_binstr_stree(OrigRpcName)),
            replace_term('ServiceName', ServiceName),
            replace_term('RpcName', RpcName)]
           || {{FqOrigServiceName, ServiceName}, Rpcs} <- ServiceInfos,
              {OrigRpcName, RpcName} <- Rpcs])])].

format_service_and_rpc_name_to_fqbins(ServiceInfos) ->
    ["%% Convert a service name and an rpc name, both as atoms,\n"
     "%% to a fully qualified (ie with package name) service name and\n"
     "%% an rpc name as binaries.\n",
     gpb_codegen:format_fn(
       service_and_rpc_name_to_fqbins,
       fun('ServiceName', 'RpcName') ->
               {'<<"maybe.package.ServiceName">>', '<<"RpcName">>'};
          (S, R) ->
               error({gpb_error, {badservice_or_rpc, {S, R}}})
       end,
       [repeat_clauses(
          'ServiceName',
          [[replace_term('ServiceName', ServiceName),
            replace_term('RpcName', RpcName),
            replace_tree('<<"maybe.package.ServiceName">>',
                         atom_to_binstr_stree(FqOrigServiceName)),
            replace_tree('<<"RpcName">>',
                         atom_to_binstr_stree(OrigRpcName))]
           || {{FqOrigServiceName, ServiceName}, Rpcs} <- ServiceInfos,
              {OrigRpcName, RpcName} <- Rpcs])])].

%% ---
atom_to_binstr_stree(A) ->
    %% When just using replace_term('a', <<"text">>)
    %% the result becomes <<116,101,120,116>>
    %% when the desired result is <<"text">>)
    %% so construct a textual tree node to get the desired format
    S = list_to_binary(atom_to_list(A)),
    erl_syntax:text(?f("<<\"~s\">>", [S])).
