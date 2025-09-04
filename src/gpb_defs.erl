%%% Copyright (C) 2019  Tomas Abrahamsson
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

%%% ------------------------------------------------------------------
%%% @doc Operations on definitions
%%% @end
%%% ------------------------------------------------------------------
-module(gpb_defs).

-export([supported_defs_versions/0]).
-export([earliest_supported_defs_version/0]).
-export([latest_defs_version/0]).
-export([convert_defs_to_latest_version/1]).
-export([convert_defs_from_latest_version/2]).
-export([extend_with_field_for_unknowns/1]).

-export([post_process_one_file/3]).
-export([post_process_all_files/2]).
-export([format_post_process_error/1]).
-export([format_error/1]).
-export([fetch_imports/1]).

-export_type([defs/0, def/0]).
-export_type([field/0]).
-export_type([version/0]).

-export([rootward_names/2]).

-include("../include/gpb.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(is_non_empty_string(Str), (is_list(Str) andalso is_integer(hd(Str)))).

-type defs() :: [def()] | map().
-type def() :: {proto_defs_version, version()} |
               {{msg, Name::atom()}, [field()]} |
               {{group, Name::atom()}, [field()]} |
               {{enum, Name::atom()},
                %% Defs format 2:
                [{Sym::atom(), Value::integer()} |
                 {option, Name::atom(), Val::term()}] |
                %% Defs format 3+:
                [{Sym::atom(), Value::integer(), [Opt::ee_option()]}]} |
               {{service, Name::atom()}, [#?gpb_rpc{}]} |
               {package, Name::atom()} |
               {syntax, string()} | % "proto2" | "proto3"
               {option, Name::atom(), Val::term()} |
               {{extensions, MsgName::atom()}, [field_number_extension()]} |
               {{extend, MsgName::atom()}, MoreFields::[field()]} |
               {{ext_origin,MsgName::atom()}, {atom(), MoreFields::[field()]}} |
               {proto3_msgs, [MsgName::atom()]} |
               {{reserved_numbers, MsgName::atom()}, [reserved_num()]} |
               {{reserved_names, MsgName::atom()}, [FieldName::atom()]} |
               {import, ProtoFile::string()} |
               {{msg_options, MsgName::atom()}, [msg_option()]} |
               {{enum_options, MsgName::atom()}, [enum_option()]} |
               {{service_options, MsgName::atom()}, [service_option()]} |
               {{msg_containment, ProtoName::string()}, [MsgName::atom()]} |
               {{pkg_containment, ProtoName::string()}, PkgName::atom()} |
               {{service_containment, ProtoName::string()},
                [ServiceName::atom()]} |
               {{rpc_containment, ProtoName::string()},
                [{ServiceName::atom(), RpcName::atom()}]} |
               {{enum_containment, ProtoName::string()}, [EnumName::atom()]} |
               {file, {BaseSansExt::string(), Base::string()}}.
-type field() :: #?gpb_field{} | #gpb_oneof{}.
-type field_number_extension() :: {Lower::integer(), Upper::integer() | max}.
-type reserved_num() :: integer() | {Lower::integer(), Upprt::integer() | max}.
-type msg_option() :: {[NameComponent::atom()], OptionValue::term()}.
-type enum_option() :: {atom() | [NameComponent::atom()], OptionValue::term()}.
-type ee_option() :: {atom() | [NameComponent::atom()], OptionValue::term()}.
-type service_option() :: {atom() | [NameComponent::atom()],
                           OptionValue::term()}.
-type version() :: integer().

%% @doc Return a list of supported versions of the definition format.
%%
%% The format is currently a list of tuples, most are 2-tuples.
%%
%% A new version means a non-backwards-compatible version change.
%%
%% Even when the version number has not changed, there may still
%% be differences, but only backwards compatible changes.
%%
%% See file `doc/dev-guide/proto-defs-versions.md' for more info.
%%
%% Format versions obey the Erlang term order and be compared like
%% normal Erlang terms with for instance `=<'.
-spec supported_defs_versions() -> [version()].
supported_defs_versions() ->
    [1, 2, 3, 4].

%% @doc Return the earliest supported proto defs version.
earliest_supported_defs_version() ->
    lists:min(supported_defs_versions()).

%% @doc Return the number of the latest proto defs version.
latest_defs_version() ->
    lists:max(supported_defs_versions()).

%% @doc Convert proto definitions from some format found in the definitions,
%% into the latest version for internal use within gpb.
-spec convert_defs_to_latest_version(Defs) ->
          {ok, Defs} |
          {error, term()} when
      Defs :: defs().
convert_defs_to_latest_version(Defs) ->
    FirstVsn = earliest_supported_defs_version(),
    LatestVsn = latest_defs_version(),
    %% The first version did not mandate a `proto_defs_version' marker,
    %% when it is missing, it is 1.
    case proplists:get_value(proto_defs_version, Defs, 1) of
        CurrentVsn when  FirstVsn =< CurrentVsn, CurrentVsn =< LatestVsn ->
            cvt_to_latest_aux(CurrentVsn, LatestVsn, Defs);
        X ->
            Supported = supported_defs_versions(),
            Reason = {convert_from_unsupported_proto_defs_version,
                      X, Supported},
            {error, Reason}
    end.

cvt_to_latest_aux(LatestVsn, LatestVsn, Defs) ->
    %% Add or replace a defs version marker.
    ensure_proto_defs_versionized(Defs, LatestVsn);
cvt_to_latest_aux(Vsn, LatestVsn, Defs) when Vsn < LatestVsn ->
    CvtRes = case Vsn of
                 1 -> cvt_defs_1_to_2(Defs);
                 2 -> cvt_defs_2_to_3(Defs);
                 3 -> cvt_defs_3_to_4(Defs)
             end,
    case CvtRes of
        {ok, Defs1} -> cvt_to_latest_aux(Vsn + 1, LatestVsn, Defs1)
    end.

%% @doc Convert proto definitions on the latest format to some earlier format.
-spec convert_defs_from_latest_version(Defs, version()) ->
          {ok, Defs} |
          {error, term()} when
      Defs :: defs().
convert_defs_from_latest_version(Defs, TargetVersion) ->
    case lists:member(TargetVersion, supported_defs_versions()) of
        true ->
            CurrentVsn = latest_defs_version(),
            cvt_from_latest_aux(CurrentVsn, TargetVersion, Defs);
        false ->
            Supported = supported_defs_versions(),
            Reason = {convert_to_unsupported_proto_defs_version,
                      TargetVersion, Supported},
            {error, Reason}
    end.

cvt_from_latest_aux(TargetVsn, TargetVsn, Defs) ->
    %% Add or replace a defs version marker.
    ensure_proto_defs_versionized(Defs, TargetVsn);
cvt_from_latest_aux(Vsn, TargetVsn, Defs) when Vsn > TargetVsn ->
    CvtRes = case Vsn of
                 4 -> cvt_defs_4_to_3(Defs);
                 3 -> cvt_defs_3_to_2(Defs);
                 2 -> cvt_defs_2_to_1(Defs)
             end,
    case CvtRes of
        {ok, Defs1} -> cvt_from_latest_aux(Vsn - 1, TargetVsn, Defs1);
        {error, Reason} -> {error, Reason}
    end.

%% @hidden
extend_with_field_for_unknowns(Defs) ->
    [case Elem of
         {{msg, _}, _} -> ensure_field_for_unknowns(Elem);
         {{group, _}, _} -> ensure_field_for_unknowns(Elem);
         _ -> Elem
     end
     || Elem <- Defs].

ensure_field_for_unknowns({{MsgOrGroup, MsgName}, Fields}=Item) ->
    case lists:any(fun gpb_lib:is_field_for_unknowns/1, Fields) of
        true ->
            Item;
        false ->
            FName = '$unknowns', % use an odd char so it won't collide
            Unknowns = #?gpb_field{name = FName,
                                   type = unknown,
                                   fnum = undefined,
                                   rnum = length(Fields) + 2,
                                   occurrence = repeated,
                                   opts = []},
            {{MsgOrGroup, MsgName}, Fields ++ [Unknowns]}
    end.

%% @hidden
%% @doc Post-process definitions from one file
post_process_one_file(FileName, Defs, Opts) ->
    case find_package_def(Defs, Opts) of
        {ok, Package} ->
            Defs1 = handle_proto_syntax_version_one_file(
                      join_any_elem_options(
                        convert_default_values(
                          flatten_qualify_defnames(Defs, Package)))),
            case tmp_fields_to_fields(Defs1) of
                {ok, Defs2} ->
                    MetaInfo = mk_meta_info(FileName, Defs2, Opts),
                    {ok, [{file, {FileName, FileName}} | MetaInfo] ++ Defs2};
                {error, Reasons} ->
                    {error, Reasons}
            end;
        {error, Reasons} ->
            {error, Reasons}
    end.

%% @hidden
%% @doc Post-process definitions once the file and all its imports
%% each have been parsed and processed.
post_process_all_files(Defs, Opts) ->
    case resolve_names(Defs) of
        {ok, Defs2} ->
            case verify_defs(Defs2, Opts) of
                ok ->
                    Defs3 = normalize_msg_field_options(
                              handle_proto_syntax_version_all_files(
                                enumerate_msg_fields(
                                  shorten_file_paths(
                                    reformat_names(
                                      extend_msgs(Defs2)))))),
                    Defs4 = versionize_defs(Defs3),
                    {ok, Defs4};
                {error, Reasons} ->
                    {error, Reasons}
            end;
        {error, Reasons} ->
            Reasons2 = possibly_hint_use_packages_opt(Reasons, Defs, Opts),
            {error, Reasons2}
    end.

%% -> {ok, Defs} | {error, [Reason]}
resolve_names(Defs) ->
    case resolve_refs(Defs) of
        {ok, ResolvedDefs} ->
            {ok, ResolvedDefs};
        {error, Reasons} ->
            {error, Reasons}
    end.

shorten_file_paths(Defs) ->
    Paths = [Path || {file, {Path, Path}} <- Defs],
    PathsNoExts = [gpb_lib:drop_filename_ext(P) || P <- Paths],
    Bases = try gpb_lib:basenameify_ish(PathsNoExts)
            catch error:{gpb_error, {multiply_defined_file_or_files, _}} ->
                    gpb_lib:basenameify_ish(Paths)
            end,
    Mapping = lists:zip(Paths, Bases),
    Defs2 = lists:map(
              fun({file, {P, P}}) ->
                      {P, BaseishSansExt} = lists:keyfind(P, 1, Mapping),
                      Baseish = gpb_lib:copy_filename_ext(BaseishSansExt, P),
                      {file, {BaseishSansExt, Baseish}};
                 ({file, _}=Def) ->
                      error({unexpected_file_def, Def});
                 (Other) ->
                      Other
              end,
              Defs),
    shorten_meta_info(Mapping, Defs2).

%% Find any package specifier. At most one such package specifier
%% may exist, and it can exist anywhere (top-level) in the proto file,
%% yet it still applies to the whole file.
find_package_def(Defs, Opts) ->
    case proplists:get_bool(use_packages, Opts) of
        true ->
            case [Pkg || {package, Pkg} <- Defs] of
                [] ->
                    {ok, empty_pkg_root()};
                [Pkg] ->
                    {ok, ['.' | Pkg]};
                Pkgs when length(Pkgs) >= 2 ->
                    PrettyPkgs = [reformat_name(Pkg) || Pkg <- Pkgs],
                    {error, [{multiple_pkg_specifiers, PrettyPkgs}]}
            end;
        false ->
            {ok, empty_pkg_root()}
    end.

empty_pkg_root() ->
    ['.'].

%% For nested message definitions such as
%% ```
%%    message m1 {
%%      required uint32 f1 = 1;
%%      message m2 { ... }
%%      enum e2 { ... }
%%    };",
%% '''
%% the parser will produce a nested structure, such as:
%% ```
%%   [{{msg,M1},[#field{},
%%               {{msg,M2}, [...]},
%%               {{enum,E2}, [...]}]}]
%% '''
%% Flattening means to lift the nested m2 and e2 definition to the top-level,
%% so the above turns into:
%% ```
%%   [{{msg,M1},[#field{}]},
%%    {{msg,M2}, [...]},
%%    {{enum,E2}, [...]}]
%% '''
%%
%% During this process, the message and enum names and similar get
%% fully qualified into absolute rooted name-paths. In the example
%% above, this applies to m1, m2 and e2. Note that at this stage,
%% nothing is done to resolve reference to names, such as message
%% types for fields. A name-path is a list of path components,
%% separated by the dot-atom, '.', and an absolute rooted name-path is
%% a path that begins with the dot-atom, '.', much like a slash or a
%% backslash in a file name path.
flatten_qualify_defnames(Defs, Root) ->
    lists:reverse(
      lists:foldl(
        fun({{msg,Name}, FieldsOrDefs}, Acc) ->
                FullName = prepend_path(Root, Name),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, FullName),
                [{{msg,FullName},Fields2} | Defs2] ++ Acc;
           ({{group,FullName}, FieldsOrDefs}, Acc) ->
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, FullName),
                [{{group,FullName},Fields2} | Defs2] ++ Acc;
           ({{enum,Name}, ENs}, Acc) ->
                FullName = prepend_path(Root, Name),
                {ENs2, Defs2} = flatten_enum_elems(ENs, FullName),
                [{{enum,FullName}, ENs2} | Defs2] ++ Acc;
           ({extensions,Exts}, Acc) ->
                [{{extensions,Root},Exts} | Acc];
           ({{extend,{eref1,Name}}, FieldsOrDefs}, Acc) ->
                FullNameCandidates =
                    rootward_names(Root, Name) ++
                    rootward_names(empty_pkg_root(), Name),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, Root),
                ERef2 = {eref2,Root,Name,FullNameCandidates},
                [{{extend,ERef2},Fields2},
                 {{ext_origin,ERef2}, {Root, Fields2}} % for descriptor
                 | Defs2] ++
                    Acc;
           ({{service, Name}, RPCs}, Acc) ->
                FullName = prepend_path(Root, Name),
                {RPCs2, Defs2} = flatten_service_elems(RPCs, FullName),
                [{{service,FullName}, RPCs2} | Defs2] ++ Acc;
           (OtherElem, Acc) ->
                [OtherElem | Acc]
        end,
        [],
        Defs)).

flatten_fields(FieldsOrDefs, FullName) ->
    {RFields2, Defs2} =
        lists:foldl(
          fun(#?gpb_field{}=F, {Fs,Ds}) ->
                  {[F | Fs], Ds};
             (#gpb_oneof{name=FName, fields=OFs}=O, {Fs,Ds}) ->
                  FullOneofName = prepend_path(FullName, FName),
                  {OFs2, ODs2} = flatten_fields(OFs, FullOneofName),
                  {[O#gpb_oneof{fields=OFs2} | Fs], ODs2++Ds};
             ({group1,TmpGName,GFields,MField}, {Fs,Ds}) ->
                  FullGroupName = prepend_path(FullName, TmpGName),
                  Group0 = {{group,FullGroupName}, GFields},
                  QDefs = flatten_qualify_defnames([Group0], FullGroupName),
                  MField1 = MField#?gpb_field{type={ref,FullGroupName}},
                  {[MField1 | Fs], QDefs++Ds};
             ({{extend, _Ref},_}=Def, {Fs,Ds}) ->
                  QDefs = flatten_qualify_defnames([Def], FullName),
                  {Fs, QDefs ++ Ds};
             ({reserved_numbers, Ns}, {Fs,Ds}) ->
                  Def = {{reserved_numbers,FullName}, Ns},
                  {Fs, [Def | Ds]};
             ({reserved_names, Ns}, {Fs,Ds}) ->
                  Def = {{reserved_names,FullName}, Ns},
                  {Fs, [Def | Ds]};
             ({{option,OptName,OptValue}}, {Fs,Ds}) ->
                  {Fs, [{{msg_option,FullName},{OptName,OptValue}} | Ds]};
             (Def, {Fs,Ds}) ->
                  QDefs = flatten_qualify_defnames([Def], FullName),
                  {Fs, QDefs++Ds}
          end,
          {[],[]},
          FieldsOrDefs),
    {lists:reverse(RFields2), Defs2}.

flatten_enum_elems(EnumElemsOrDefs, FullName) ->
    {EnumElems2, Defs2} =
        lists:foldl(
          fun({reserved_numbers, Ns}, {Es,Ds}) ->
                  Def = {{reserved_numbers,FullName}, Ns},
                  {Es, [Def | Ds]};
             ({reserved_names, Ns}, {Es,Ds}) ->
                  Def = {{reserved_names,FullName}, Ns},
                  {Es, [Def | Ds]};
             ({{option,OptName,OptValue}}, {Fs,Ds}) ->
                  {Fs, [{{enum_option,FullName},{OptName,OptValue}} | Ds]};
             (Other, {Es,Ds}) ->
                  {[Other | Es], Ds}
          end,
          {[],[]},
          EnumElemsOrDefs),
    {lists:reverse(EnumElems2), Defs2}.

flatten_service_elems(Elems, FullName) ->
    {Elems2, Defs2} =
        lists:foldl(
          fun({{option, OptName, OptValue}}, {Es, Ds}) ->
                  {Es, [{{service_option,FullName}, {OptName,OptValue}} | Ds]};
             (Other, {Es, Ds}) ->
                  {[Other | Es], Ds}
          end,
          {[],[]},
          Elems),
    {lists:reverse(Elems2), Defs2}.


%% Resolve any refs
resolve_refs(Defs) ->
    Root = ['.'],
    {ResolvedRefs, Reasons} =
        lists:mapfoldl(
          fun({{msg,FullName}, Fields}, Acc) ->
                  {NewFields, Acc2} =
                      resolve_field_refs(Fields, Defs, Root, FullName, Acc),
                  {{{msg,FullName}, NewFields}, Acc2};
             ({{group,FullName}, Fields}, Acc) ->
                  {NewFields, Acc2} =
                      resolve_field_refs(Fields, Defs, Root, FullName, Acc),
                  {{{group,FullName}, NewFields}, Acc2};
             ({{service,FullName}, Rpcs}, Acc) ->
                  {NewRPCs, Acc2} =
                      resolve_rpc_refs(Rpcs, Defs, Root, FullName, Acc),
                  {{{service,FullName}, NewRPCs}, Acc2};
             ({{extend,ExtendeeCandidates}, Fields}, Acc) ->
                  {Extendee, NewFields, Acc2} =
                      resolve_extend_refs(ExtendeeCandidates, Fields, Defs,
                                          Root, Acc),
                  {{{extend,Extendee}, NewFields}, Acc2};
             ({{ext_origin,ExtendeeCandidates}, {ERoot, Fields}}, Acc) ->
                  {Extendee, NewFields, Acc2} =
                      resolve_extend_refs(ExtendeeCandidates, Fields, Defs,
                                          Root, Acc),
                  {{{ext_origin,Extendee}, {ERoot, NewFields}}, Acc2};
             (OtherElem, Acc) ->
                  {OtherElem, Acc}
          end,
          [],
          Defs),
    if Reasons == [] -> {ok, ResolvedRefs};
       Reasons /= [] -> {error, lists:reverse(Reasons)}
    end.



resolve_field_refs(Fields, Defs, Root, FullName, Reasons) ->
    lists:mapfoldl(
      fun(#?gpb_field{name=FName, type={ref,Ref}}=Field, Acc) ->
              case resolve_ref(Defs, Ref, Root, FullName) of
                  {found, TypeName} ->
                      {Field#?gpb_field{type=TypeName}, Acc};
                  not_found ->
                      Reason = {ref_to_undefined_msg_or_enum,
                                {{FullName, FName}, Ref}},
                      {Field, [Reason | Acc]}
              end;
         (#?gpb_field{name=FName, type={map,KeyType,{ref,Ref}}}=Field, Acc) ->
              case resolve_ref(Defs, Ref, Root, FullName) of
                  {found, TypeName} ->
                      {Field#?gpb_field{type={map,KeyType,TypeName}}, Acc};
                  not_found ->
                      Reason = {ref_to_undefined_msg_or_enum,
                                {{FullName, FName}, Ref}},
                      {Field, [Reason | Acc]}
              end;
         (#?gpb_field{}=Field, Acc) ->
              {Field, Acc};
         (#gpb_oneof{fields=OFields1}=Oneof, Acc) ->
              {OFields2, Acc2} =
                  resolve_field_refs(OFields1, Defs, Root, FullName, Acc),
              {Oneof#gpb_oneof{fields=OFields2}, Acc2}
      end,
      Reasons,
      Fields).

resolve_rpc_refs(Rpcs, Defs, Root, FullName, Reasons) ->
    lists:mapfoldl(
      fun({RpcName, {Arg, ArgIsStream}, {Return, ReturnIsStream}, Opts}=Rpc,
          Acc) ->
              case resolve_ref(Defs, Arg, Root, FullName) of
                  {found, {msg, MArg}} ->
                      case resolve_ref(Defs, Return, Root, FullName) of
                          {found, {msg, MReturn}} ->
                              NewOpts = [{Name, Value}
                                         || {option,Name,Value} <- Opts],
                              NewRpc = #?gpb_rpc{name=RpcName,
                                                 input=MArg,
                                                 input_stream=ArgIsStream,
                                                 output=MReturn,
                                                 output_stream=ReturnIsStream,
                                                 opts=NewOpts},
                              {NewRpc, Acc};
                          {found, {BadType, MReturn}} ->
                              Reason = {rpc_return_ref_to_non_msg,
                                        {{FullName, RpcName, Return},
                                         BadType, MReturn}},
                              {Rpc, [Reason | Acc]};
                          not_found ->
                              Reason = {rpc_return_ref_to_undefined_msg,
                                        {{FullName, RpcName}, Return}},
                              {Rpc, [Reason | Acc]}
                      end;
                  {found, {BadType, MArg}} ->
                      Reason = {rpc_arg_ref_to_non_msg,
                                {{FullName, RpcName, Arg}, BadType, MArg}},
                      {Rpc, [Reason | Acc]};
                  not_found ->
                      Reason = {rpc_arg_ref_to_undefined_msg,
                                {{FullName, RpcName}, Arg}},
                      {Rpc, [Reason | Acc]}
              end
      end,
      Reasons,
      Rpcs).

resolve_extend_refs({eref2, Ctxt, _Name, ExtendeeCandidates}, Fields, Defs,
                    Root, Acc) ->
    case resolve_ref_candidates(Defs, ExtendeeCandidates) of
        {found, {msg,NewToBeExtended}} ->
            {NewFields, Acc2} =
                resolve_field_refs(Fields, Defs, Root, Ctxt, Acc),
            {NewToBeExtended, NewFields, Acc2};
        not_found ->
            Reason = {extend_ref_to_undefined_msg, hd(ExtendeeCandidates)},
            {hd(ExtendeeCandidates), Fields, [Reason | Acc]}
    end.

%% -> {found, {msg,FullName}|{enum,FullName}} | not_found
resolve_ref(Defs, Ref, Root, FullName) ->
    case is_absolute_ref(Ref) of
        true  ->
            FullRef = ensure_path_prepended(Root, Ref),
            find_typename(FullRef, Defs);
        false ->
            PossibleRoots = compute_roots(FullName),
            find_ref_rootwards(PossibleRoots, Ref, Defs)
    end.

resolve_ref_candidates(Defs, [Cand1 | Rest]) ->
    case find_typename(Cand1, Defs) of
        {found, TypeName} -> {found, TypeName};
        not_found -> resolve_ref_candidates(Defs, Rest)
    end;
resolve_ref_candidates(_Defs, []) ->
    not_found.

find_ref_rootwards([PossibleRoot | Rest], Ref, Defs) ->
    FullRef = ensure_path_prepended(PossibleRoot, Ref),
    case find_typename(FullRef, Defs) of
        {found, TypeName} -> {found, TypeName};
        not_found -> find_ref_rootwards(Rest, Ref, Defs)
    end;
find_ref_rootwards([], _Ref, _Defs) ->
    not_found.

is_absolute_ref(['.' | _]) -> true;
is_absolute_ref(_Other)    -> false.

find_typename(Name, [{{enum,Name}, _Values} | _])  -> {found, {enum,Name}};
find_typename(Name, [{{msg,Name}, _SubElems} | _]) -> {found, {msg,Name}};
find_typename(Name, [{{group,Name}, _Elems} | _])  -> {found, {group,Name}};
find_typename(Name, [_ | Rest])                    -> find_typename(Name, Rest);
find_typename(_Name,[])                            -> not_found.

%% Similar to compute_roots/1, but always keep `Name' last.
%% Example: rootward_names(['.',m1,'.',m2],  x) ->
%%            [['.',m1,'.',m2,'.',x],
%%             ['.',m1,'.',x]
%%             ['.',x]]
rootward_names(Path, Name) ->
    [prepend_path(R, Name) || R <- compute_roots(Path)].

%% Turn ['.',m1,'.',m2,'.',m3]
%% into [['.',m1,'.',m2,'.',m3],
%%       ['.',m1,'.',m2],
%%       ['.',m1],
%%       ['.']]
compute_roots(['.']) -> [['.']];
compute_roots(DeeperPath) ->
    [DeeperPath | compute_roots(drop_last_level(DeeperPath))].

drop_last_level(['.']) -> ['.'];
drop_last_level(['.', X]) when is_atom(X) -> ['.'];
drop_last_level(DeeperPath) when length(DeeperPath) >= 3 ->
    [_X, '.' | RestReversed] = lists:reverse(DeeperPath),
    lists:reverse(RestReversed).

prepend_path(['.'], Id) when is_atom(Id)           -> ['.', Id];
prepend_path(['.'], SubPath) when is_list(SubPath) -> ['.' | SubPath];
prepend_path(Path,  Id) when is_atom(Id)           -> Path ++ ['.', Id];
prepend_path(Path,  SubPath) when is_list(SubPath) -> Path ++ ['.' | SubPath].

ensure_path_prepended(Pkg, Path)   ->
    case lists:prefix(Pkg, Path) of
        false -> prepend_path(Pkg, Path);
        true ->  Path
    end.

convert_default_values(Defs) ->
    lists:map(
      fun({{msg,Name},Fields}) ->
              Fields2 = lists:map(fun convert_default_values_field/1, Fields),
              {{msg,Name},Fields2};
         ({{group,Name},Fields}) ->
              Fields2 = lists:map(fun convert_default_values_field/1, Fields),
              {{group,Name},Fields2};
         (Other) ->
              Other
      end,
      Defs).

convert_default_values_field(#?gpb_field{type=Type, opts=Opts}=Field) ->
    case {Type, lists:keyfind(default, 1, Opts)} of
        {bytes, {default, Default}} when is_list(Default) ->
            %% Default values for type bytes are written as a string
            Default2 = list_to_binary(Default),
            Opts2 = lists:keyreplace(default, 1, Opts, {default, Default2}),
            Field#?gpb_field{opts=Opts2};
        _ ->
            Field
    end;
convert_default_values_field(#gpb_oneof{fields=OFs}=Field) ->
    OFs2 = lists:map(fun convert_default_values_field/1, OFs),
    Field#gpb_oneof{fields=OFs2}.

-record(elem_dicts, {msg=dict:new(),
                     enum=dict:new(),
                     service=dict:new()}).
join_any_elem_options(Defs) ->
    {NonOptDefs, Dicts} =
        lists:foldl(
          fun({{msg_option,MsgName},Opt}, {Ds, #elem_dicts{msg=D0}=Dicts}) ->
                  D1 = dict:append(MsgName, Opt, D0),
                  Dicts1 = Dicts#elem_dicts{msg=D1},
                  {Ds, Dicts1};
             ({{enum_option,EName},Opt}, {Ds, #elem_dicts{enum=D0}=Dicts}) ->
                  D1 = dict:append(EName, Opt, D0),
                  Dicts1 = Dicts#elem_dicts{enum=D1},
                  {Ds, Dicts1};
             ({{service_option,SName},Opt},
              {Ds, #elem_dicts{service=D0}=Dicts}) ->
                  D1 = dict:append(SName, Opt, D0),
                  Dicts1 = Dicts#elem_dicts{service=D1},
                  {Ds, Dicts1};
             (OtherDef, {Ds, Dicts}) ->
                  {[OtherDef | Ds], Dicts}
          end,
          {[], #elem_dicts{}},
          Defs),
    #elem_dicts{msg=MsgOptsDict,
                enum=EnumOptsDict,
                service=ServiceOptsDict} = Dicts,
    MsgOpts = [{{msg_options, MsgName}, Opts}
               || {MsgName, Opts} <- dict:to_list(MsgOptsDict)],
    EnumOpts = [{{enum_options, EnumName}, Opts}
               || {EnumName, Opts} <- dict:to_list(EnumOptsDict)],
    ServiceOpts = [{{service_options, ServiceName}, Opts}
                   || {ServiceName, Opts} <- dict:to_list(ServiceOptsDict)],
    lists:reverse(NonOptDefs, EnumOpts++MsgOpts++ServiceOpts).

handle_proto_syntax_version_one_file(Defs) ->
    case proplists:get_value(syntax, Defs) of
        undefined -> handle_proto2_1(Defs);
        "proto2"  -> handle_proto2_1(Defs);
        "proto3"  -> handle_proto3_1(Defs)
    end.

handle_proto2_1(Defs) ->
    Defs.

handle_proto3_1(Defs) ->
    %% FIXME: Verify no 'extensions' or 'extend'
    %% FIXME: Verify no 'required' occurrences
    %% FIXME: Verify enums start with 0

    %% Remember which msgs were defined using proto3 syntax,
    %% so we can treat them differently later on.
    anno_msgs_proto3_origin(Defs).

anno_msgs_proto3_origin(Defs) ->
    anno_msgs_proto3_origin_2(Defs, []).

anno_msgs_proto3_origin_2([{{msg,Msg},_Fields}=Def | Rest], P3Msgs) ->
    [Def | anno_msgs_proto3_origin_2(Rest, [Msg | P3Msgs])];
anno_msgs_proto3_origin_2([Def | Rest], Acc) ->
    [Def | anno_msgs_proto3_origin_2(Rest, Acc)];
anno_msgs_proto3_origin_2([], Acc) ->
    [{proto3_msgs,lists:reverse(Acc)}].

handle_proto_syntax_version_all_files(Defs) ->
    P3Items = [X || {proto3_msgs,_}=X <- Defs],
    if P3Items == [] ->
            Defs;
       P3Items /= [] ->
            Proto3Msgs = lists:append([Msgs || {proto3_msgs,Msgs} <- P3Items]),
            Defs1 = Defs -- P3Items,
            Defs2 = Defs1 ++ [{proto3_msgs, lists:sort(Proto3Msgs)}],

            %% The protobuf language guide for proto3 says: "In proto3,
            %% repeated fields of scalar numeric types use packed encoding by
            %% default."
            default_repeated_to_packed(Defs2, Proto3Msgs)
    end.

default_repeated_to_packed(Defs, P3Msgs) ->
    lists:map(
      fun({{msg,MsgName},Fields}=MsgDef) ->
              case lists:member(MsgName, P3Msgs) of
                  true ->
                      Fields1 = default_repeated_fields_to_packed(Fields),
                      {{msg,MsgName}, Fields1};
                  false ->
                      MsgDef
              end;
         (Other) ->
              Other
      end,
      Defs).

default_repeated_fields_to_packed(Fields) ->
    lists:map(
      fun(#?gpb_field{occurrence=repeated, opts=Opts, type=Type}=F) ->
              case {proplists:get_value(packed, Opts),
                    is_scalar_numeric(Type)} of
                  {undefined, true} ->
                      NewOpts = [{packed, true} | Opts],
                      F#?gpb_field{opts=NewOpts};
                  _ ->
                      F
              end;
         (F) ->
              F
      end,
      Fields).

is_scalar_numeric(int32)    -> true;
is_scalar_numeric(int64)    -> true;
is_scalar_numeric(uint32)   -> true;
is_scalar_numeric(uint64)   -> true;
is_scalar_numeric(sint32)   -> true;
is_scalar_numeric(sint64)   -> true;
is_scalar_numeric(fixed32)  -> true;
is_scalar_numeric(fixed64)  -> true;
is_scalar_numeric(sfixed32) -> true;
is_scalar_numeric(sfixed64) -> true;
is_scalar_numeric(bool)     -> true;
is_scalar_numeric(float)    -> true;
is_scalar_numeric(double)   -> true;
is_scalar_numeric({enum,_}) -> true;
is_scalar_numeric(_)        -> false. % not: string | bytes | msg | map

%% Find inconsistencies
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and may or may not be reformatted.
verify_defs(Defs, Opts) ->
    DoJson = gpb_lib:json_by_opts(Opts),
    MsgVerifiers = lists:flatten(
                     [fun verify_field_defaults/2,
                      fun verify_field_names/2,
                      fun verify_field_numbers/2,
                      [fun verify_json_name_options/2 || DoJson],
                      [fun verify_json_field_names/2 || DoJson]]),
    collect_errors(Defs,
                   [{msg,     MsgVerifiers},
                    {group,   MsgVerifiers},
                    {enum,    [fun verify_at_least_one_member/2]},
                    {extend,  [fun verify_extend/2]},
                    {service, [fun verify_service_rpc_names/2]},
                    {all,     [fun verify_msg_names_unique/1,
                               fun verify_enum_names_unique/1,
                               fun verify_service_names_unique/1]}]).

collect_errors(Defs, VerifiersList) ->
    collect_errors(Defs, Defs, VerifiersList, ok).

collect_errors([{{ElemType,_},_}=Def | Rest], AllDefs, VerifiersList, Acc) ->
    Result = lists:foldl(
               fun(Verifier, A) -> add_acc(A, Verifier(Def, AllDefs)) end,
               Acc,
               find_verifiers(ElemType, VerifiersList)),
    collect_errors(Rest, AllDefs, VerifiersList, Result);
collect_errors([_OtherDef | Rest], AllDefs, VerifiersList, Acc) ->
    %% Example: import, package, ...
    collect_errors(Rest, AllDefs, VerifiersList, Acc);
collect_errors([], AllDefs, VerifiersList, Acc) ->
    Acc2 = lists:foldl(
             fun(Verifier, A) -> add_acc(A, Verifier(AllDefs)) end,
             Acc,
             find_verifiers(all, VerifiersList)),
    case Acc2 of
        ok                       -> ok;
        {error, ReasonsReversed} -> {error, lists:reverse(ReasonsReversed)}
    end.

add_acc(AnyPreviousResult, ok)         -> AnyPreviousResult;
add_acc(ok,                {error, R}) -> {error, add_reason([], R)};
add_acc({error, Reasons},  {error, R}) -> {error, add_reason(Reasons, R)}.

add_reason(Reasons, Reason) when not is_list(Reason) ->
    [Reason | Reasons];
add_reason(Reasons, MoreReasons) when is_list(MoreReasons) ->
    lists:reverse(MoreReasons, Reasons).

find_verifiers(Type,  [{Type, Verifiers} | _]) -> Verifiers;
find_verifiers(Type,  [_Other | Rest])         -> find_verifiers(Type, Rest);
find_verifiers(_Type, [])                      -> [].

verify_field_defaults({{msg,M}, Fields}, AllDefs) ->
    lists:foldl(fun(#?gpb_field{name=Name, type=Type, opts=FOpts}, Acc) ->
                        Res = case lists:keysearch(default, 1, FOpts) of
                                  {value, {default, Default}} ->
                                      verify_scalar_default_if_present(
                                        M, Name, Type, Default, AllDefs);
                                  false ->
                                      ok
                              end,
                        add_acc(Acc, Res);
                   (#gpb_oneof{fields=OFields}, Acc) ->
                        Res = verify_field_defaults({{msg,M},OFields}, AllDefs),
                        add_acc(Acc, Res)
                end,
                ok,
                Fields);
verify_field_defaults({{group,G}, Fields}, AllDefs) ->
    verify_field_defaults({{msg,G}, Fields}, AllDefs).


verify_scalar_default_if_present(MsgName, FieldName, Type, Default, AllDefs) ->
    case Type of
        {enum,Ref} ->
            case lists:keysearch({enum, Ref}, 1, AllDefs) of
                {value, {{enum,Ref}, Enumerators}} ->
                    case lists:keysearch(Default, 1, Enumerators) of
                        {value, {Default, _Value, _EeOpts}} ->
                            ok;
                        false ->
                            {error,
                             {{invalid_default_enum_value, Default},
                              {name_to_dstr(MsgName), atom_to_list(FieldName)}}}
                    end;
                false ->
                    ok %% caught by another verification step
            end;
        ScalarType when is_atom(ScalarType) ->
            case gpb:check_scalar(Default, ScalarType) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {Reason, {name_to_dstr(MsgName),
                                      atom_to_list(FieldName)}}}
            end
    end.

verify_field_names({{_msg_or_group, MsgName}, Fields}, _AllDefs) ->
    FNames = all_field_names(Fields),
    case FNames -- lists:usort(FNames) of
        [] ->
            ok;
        Dups ->
            {error,
             [{field_name_used_more_than_once, {name_to_dstr(MsgName), FName}}
              || FName <- Dups]}
    end.

verify_json_name_options({{_msg_or_group, MsgName}, Fields}, _AllDefs) ->
    BadFields = lists:reverse(
                  gpb_lib:fold_msgdef_fields(
                    fun(#?gpb_field{name=FName, opts=Opts}, Acc) ->
                            case proplists:get_value(json_name, Opts) of
                                undefined ->
                                    Acc;
                                Str when ?is_non_empty_string(Str) ->
                                    Acc;
                                X ->
                                    [{FName, X} | Acc]
                            end
                    end,
                    [],
                    Fields)),
    if BadFields == [] ->
            ok;
       true ->
            {error, [{json_name_must_be_string,
                      {name_to_dstr(MsgName), FName, InvalidValue}}
                     || {FName, InvalidValue} <- BadFields]}
    end.



verify_json_field_names({{_msg_or_group, MsgName}, Fields}, _AllDefs) ->
    %% Collect all fields, also those inside oneof
    AllFields = lists:reverse(
                  gpb_lib:fold_msgdef_fields(
                    fun(#?gpb_field{}=Field, Acc) ->
                            [Field | Acc]
                    end,
                    [],
                    Fields)),
    D = lists:foldl(
          fun(#?gpb_field{name=FName}=Field, D) ->
                  %% Store info both for collisions between json field names
                  %% (normally lowerCamelCase) and json field names and
                  %% ordinary field names, since decoding must accept both.
                  FNameStr = atom_to_list(FName),
                  D1 = dict:append(FNameStr, FName, D),
                  %% Take precautions not to crash on bad values
                  %% for the json_name option
                  try gpb_lib:get_field_json_name(Field) of
                      FNameStr  -> D1; % json name same as field name; ignore
                      JsonFName -> dict:append(JsonFName, FName, D1)
                  catch error:_ -> D1
                  end
          end,
          dict:new(),
          AllFields),
    %% Dict of field names that collide when converted to lowerCamelCase.
    D1 = dict:filter(fun(_K, FNames) -> length(FNames) >= 2 end, D),
    case dict:to_list(D1) of
        [] ->
            ok;
        Dups ->
            {error,
             [{json_lower_camel_case_field_name_collision,
               {name_to_dstr(MsgName), LowerCamelCasedFName, FNames}}
              || {LowerCamelCasedFName, FNames} <- Dups]}
    end.

all_field_names(Fields) ->
    lists:flatten(all_field_names2(Fields)).

all_field_names2([#?gpb_field{name=FName} | Rest]) ->
    [FName | all_field_names2(Rest)];
all_field_names2([#gpb_oneof{name=FName, fields=OFields} | Rest]) ->
    [FName, all_field_names2(OFields) | all_field_names(Rest)];
all_field_names2([]) ->
    [].

verify_field_numbers({{_msg_or_group, MsgName}, Fields}, _AllDefs) ->
    %% For each number, store the names associated to it
    D = gpb_lib:fold_msgdef_fields(
          fun(#?gpb_field{name=Name, fnum=Num}, D) ->
                  dict:append(Num, Name, D)
          end,
          dict:new(),
          Fields),
    %% Filter for numbers with more than one name
    D2 = dict:filter(fun(_Num, Names) -> length(Names) > 1 end, D),
    Errs2 = [{field_number_used_more_than_once,
              {name_to_dstr(MsgName), Num, FNames}}
             || {Num, FNames} <- dict:to_list(D2)],
    %% Check for field numbers not positive
    D3 = dict:filter(fun(Num, _Names) -> Num =< 0 end, D),
    Errs3 = [{field_number_must_be_positive,
              {name_to_dstr(MsgName), Num, FNames}}
             || {Num, FNames} <- dict:to_list(D3)],
    case Errs2 ++ Errs3 of
        [] -> ok;
        Errs -> {error, Errs}
    end.

verify_at_least_one_member({{enum,EnumName},Enums}, _AllDefs) ->
    case gpb_lib:unalias_enum(Enums) of
        [] ->
            {error,
             {enum_must_have_at_least_one_value, name_to_dstr(EnumName)}};
        _ ->
            ok
    end.

verify_extend(_, _AllDefs) ->
    %% FIXME
    ok.

verify_service_rpc_names({{service,ServiceName}, Rpcs}, _AllDefs) ->
    RpcNames = [RpcName || #?gpb_rpc{name=RpcName} <- Rpcs],
    case RpcNames -- lists:usort(RpcNames) of
        [] ->
            ok;
        Dups ->
            {error,
             [{rpc_multiply_defined, {name_to_dstr(ServiceName), RpcName}}
              || RpcName <- Dups]}
    end.

verify_msg_names_unique(AllDefs) ->
    MsgNames = [MsgName || {{msg, MsgName}, _Fields} <- AllDefs],
    case MsgNames -- lists:usort(MsgNames) of
        [] ->
            ok;
        Dups ->
            {error, [{msg_multiply_defined, name_to_dstr(MsgName)}
                     || MsgName <- Dups]}
    end.

verify_enum_names_unique(AllDefs) ->
    EnumNames = [EnumName || {{enum, EnumName}, _} <- AllDefs],
    case EnumNames -- lists:usort(EnumNames) of
        [] ->
            ok;
        Dups ->
            {error, [{enum_multiply_defined, name_to_dstr(EnumName)}
                     || EnumName <- Dups]}
    end.

verify_service_names_unique(AllDefs) ->
    SvcNames = [SvcName || {{service, SvcName}, _} <- AllDefs],
    case SvcNames -- lists:usort(SvcNames) of
        [] ->
            ok;
        Dups ->
            {error, [{service_multiply_defined, name_to_dstr(SvcName)}
                     || SvcName <- Dups]}
    end.

name_to_absdstr(['.' | Name]) -> "." ++ name_to_dstr(Name);
name_to_absdstr(Name) -> name_to_dstr(Name).

name_to_dstr(Name) when is_list(Name) ->
    gpb_lib:dot_join([atom_to_list(P) || P <- Name, P /= '.']);
name_to_dstr(Name) when is_atom(Name) ->
    atom_to_list(Name).

%% @hidden
%% Format error and reasons from the post processing stage.
format_post_process_error({error, Reasons}) ->
    lists:flatten([[fmt_err(Reason),"\n"] || Reason <- Reasons]).

format_error({error, Reason}) ->
    format_error(Reason);
format_error(Reason) ->
    lists:flatten([fmt_err(Reason),"\n"]).

-define(f(F, A), io_lib:format(F, A)).

fmt_err({multiple_pkg_specifiers, Pkgs}) ->
    ?f("package specified more than once: ~s~n",
       [gpb_lib:comma_join([atom_to_list(Pkg) || Pkg <- Pkgs])]);
fmt_err({hint,{{use_packages,option}, unresolved_references}}) ->
    ?f("hint: use the option use_packages (-pkgs) "
       "to use messages or enums in other packages", []);
fmt_err({ref_to_undefined_msg_or_enum, {{Msg, Field}, To}}) ->
    ?f("in msg ~s, field ~s: undefined reference  ~s",
       [name_to_dstr(Msg), name_to_dstr(Field), name_to_absdstr(To)]);
fmt_err({extend_ref_to_undefined_msg, Msg}) ->
    ?f("extend of unknown message ~s", [name_to_absdstr(Msg)]);
fmt_err({rpc_return_ref_to_non_msg,
         {{FullName, RpcName, Return}, BadType, MReturn}}) ->
    ?f("in service ~s, rpc ~s, the return type, ~s, refers to "
       " a ~p, ~s, instead of to a message",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Return),
        BadType, name_to_dstr(MReturn)]);
fmt_err({rpc_return_ref_to_undefined_msg, {{FullName, RpcName}, Ret}}) ->
    ?f("in service ~s, rpc ~s, return: undefined reference ~s",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Ret)]);
fmt_err({rpc_arg_ref_to_non_msg, {{FullName, RpcName, Arg}, BadType, MArg}}) ->
    ?f("in service ~s, rpc ~s, the arg type, ~s, refers to "
       " a ~p, ~s, instead of to a message",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Arg),
        BadType, name_to_dstr(MArg)]);
fmt_err({rpc_arg_ref_to_undefined_msg, {{FullName, RpcName}, Arg}}) ->
    ?f("in service ~s, rpc ~s, arg: undefined reference ~s",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Arg)]);
fmt_err({{invalid_default_enum_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: undefined enumerator in default value ~s",
       [Msg, Field, Default]);
fmt_err({{{value_out_of_range, Signedness, Bits}, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: default value ~p out of range for ~p ~p bit int",
       [Msg, Field, Default, Signedness, Bits]);
fmt_err({{{bad_integer_value, Signedness, Bits}, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for ~p ~p bit int",
       [Msg, Field, Default, Signedness, Bits]);
fmt_err({{bad_floating_point_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad floating point default value ~p",
       [Msg, Field, Default]);
fmt_err({{bad_boolean_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for boolean",
       [Msg, Field, Default]);
fmt_err({{bad_unicode_string, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for string",
       [Msg, Field, Default]);
fmt_err({{bad_binary_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for bytes",
       [Msg, Field, Default]);
fmt_err({field_name_used_more_than_once, {MsgName, FName}}) ->
    ?f("field ~s defined more than once in message ~s", [FName, MsgName]);
fmt_err({field_number_used_more_than_once, {MsgName, FNum, FNames}}) ->
    ?f("field number ~w used more than once in message ~s: for fields ~s",
       [FNum, MsgName, list_to_text(FNames)]);
fmt_err({field_number_must_be_positive, {MsgName, FNum, FNames}}) ->
    ?f("in message ~s, field number must be positive for ~s, but is ~w",
       [MsgName, list_to_text(FNames), FNum]);
fmt_err({json_lower_camel_case_field_name_collision,
               {MsgName, _LowerCamelCasedFName, FNames}}) ->
    ?f("with json, field names as lowerCamelCase collide in message ~s: ~s",
       [MsgName, list_to_text(FNames)]);
fmt_err({json_name_must_be_string,{MsgName, FName, InnvalidJsonNameValue}}) ->
    ?f("for field ~s in message ~s: json_name value must be string, found ~w",
       [MsgName, FName, InnvalidJsonNameValue]);
fmt_err({msg_multiply_defined, MsgName}) ->
    ?f("message name ~s defined more than once", [MsgName]);
fmt_err({enum_multiply_defined, EnumName}) ->
    ?f("enum ~s defined more than once", [EnumName]);
fmt_err({service_multiply_defined, ServiceName}) ->
    ?f("service ~s defined more than once", [ServiceName]);
fmt_err({rpc_multiply_defined, {ServiceName, RpcName}}) ->
    ?f("rpc ~s in service ~s defined more than once", [RpcName, ServiceName]);
fmt_err({enum_must_have_at_least_one_value, EnumName}) ->
    ?f("enum ~s must have at least one value", [EnumName]);
fmt_err({p3_unallowed_occurrence, MsgName, Field, Occurrence}) ->
    ?f("in msg ~s, field ~s: it is not allowed to specify ~s",
       [name_to_dstr(MsgName), Field, Occurrence]);
fmt_err({missing_occurrence, MsgName, Field}) ->
    ?f("in msg ~s, field ~s: missing 'optional' or 'required' or 'repeated'",
       [name_to_dstr(MsgName), Field]);
fmt_err({convert_from_unsupported_proto_defs_version, Found, Supported}) ->
    ?f("supplied proto_defs_version ~w is unsupported (supported: ~w)",
       [Found, Supported]);
fmt_err({convert_to_unsupported_proto_defs_version, TargetVsn, Supported}) ->
    ?f("specified proto_defs_version ~w is unsupported (supported: ~w)",
       [TargetVsn, Supported]);
fmt_err({defs_unrepresentable_in_fmt_1,p3_optional,MsgName,FName}) ->
    ?f("definitions unrepresentable in format 1 due to proto3 optional ~s.~s",
       [MsgName, FName]).

list_to_text([Item1, Item2]) ->
    ?f("~s and ~s", [Item1, Item2]);
list_to_text([Item | Rest]=L) when length(L) > 2->
    ?f("~s, ~s", [Item, list_to_text(Rest)]);
list_to_text([Item]) ->
    ?f("~s", [Item]).

%% Rewrites for instance ['.','m1','.',m2] into 'm1.m2'
%% Example: {{msg,['.','m1','.',m2]}, [#field{type={msg,['.','m1','.',m3]}}]}
%% becomes: {{msg,'m1.m2'},           [#field{type={msg,'m1.m3'}}]}
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and names and references
%% are expected to have been resolved
reformat_names(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{group,Name}, Fields}) ->
                      {{group,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{msg_containment, ProtoName}, Msgs}) ->
                      {{msg_containment,ProtoName},
                       [reformat_name(N) || N <- Msgs]};
                 ({{enum,Name}, ENs}) ->
                      {{enum,reformat_name(Name)}, reformat_enum_opt_names(ENs)};
                 ({{enum_containment, ProtoName}, EnumNames}) ->
                      {{enum_containment,ProtoName},
                       [reformat_name(EnumName) || EnumName <- EnumNames]};
                 ({{extensions,Name}, Exts}) ->
                      {{extensions,reformat_name(Name)}, Exts};
                 ({{extend,Name}, Fields}) ->
                      {{extend,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{ext_origin,Name}, {Root, Fields}}) ->
                      Root2 = reformat_name_or_dot(Root),
                      Fields2 = reformat_fields(Fields),
                      {{ext_origin,reformat_name(Name)}, {Root2, Fields2}};
                 ({{service,Name}, RPCs}) ->
                      {{service,reformat_name(Name)}, reformat_rpcs(RPCs)};
                 ({{service_containment, ProtoName}, ServiceNames}) ->
                      {{service_containment,ProtoName},
                       [reformat_name(Name) || Name <- ServiceNames]};
                 ({{rpc_containment, ProtoName}, RpcNames}) ->
                      {{rpc_containment,ProtoName},
                       [{reformat_name(ServiceName), RpcName}
                        || {ServiceName,RpcName} <- RpcNames]};
                 ({package, Name}) ->
                      {package, reformat_name(Name)};
                 ({{pkg_containment, ProtoName}, PkgName}) ->
                      {{pkg_containment,ProtoName}, reformat_name(PkgName)};
                 ({proto3_msgs,Names}) ->
                      {proto3_msgs,[reformat_name(Name) || Name <- Names]};
                 ({{reserved_numbers,Name}, Ns}) ->
                      {{reserved_numbers,reformat_name(Name)}, Ns};
                 ({{reserved_names,Name}, FieldNames}) ->
                      {{reserved_names,reformat_name(Name)}, FieldNames};
                 ({{msg_options,MsgName}, Opt}) ->
                      {{msg_options,reformat_name(MsgName)}, Opt};
                 ({{enum_options,EnumName}, Opt}) ->
                      {{enum_options,reformat_name(EnumName)}, Opt};
                 ({{service_options,ServiceName}, Opt}) ->
                      {{service_options,reformat_name(ServiceName)}, Opt};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

reformat_fields(Fields) ->
    lists:map(
      fun(#?gpb_field{type={T,Nm}}=F) ->
              F#?gpb_field{type={T,reformat_name(Nm)}};
         (#?gpb_field{type={map,KeyType,{T,Nm}}}=F) ->
              F#?gpb_field{type={map,KeyType,{T,reformat_name(Nm)}}};
         (#?gpb_field{}=F) ->
              F;
         (#gpb_oneof{fields=Fs}=O) ->
              O#gpb_oneof{fields=reformat_fields(Fs)}
      end,
      Fields).

%% `Defs' is expected to be parsed.
reformat_enum_opt_names(Def) ->
    [case Item of
         {option, Name, Value} ->
             {option, reformat_name(Name), Value};
         Other ->
             Other
     end
     || Item <- Def].

reformat_name(Name) when is_atom(Name) -> Name;
reformat_name(Name) when is_list(Name) -> % dotted name components:
    list_to_atom(gpb_lib:dot_join([atom_to_list(P) || P <- Name, P /= '.'])).

reformat_name_or_dot(['.']) -> '.';
reformat_name_or_dot(Name) -> reformat_name(Name).

reformat_rpcs(RPCs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}=R) ->
                      R#?gpb_rpc{name=RpcName,
                                 input=reformat_name(Arg),
                                 output=reformat_name(Return)}
              end,
              RPCs).

%% `Defs' is expected to be flattened and may or may not be reformatted
%% `Defs' is expected to be verified, to not extend missing messages
extend_msgs(Defs0) ->
    Extendings = [E || {{extend,_MsgToExtend},_MoreFields}=E <- Defs0],
    lists:foldl(fun possibly_extend_msg/2, Defs0, Extendings).


possibly_extend_msg({{extend,Msg}, MoreFields}=Extending, Defs) ->
    case lists:keyfind({msg,Msg}, 1, Defs) of
        {{msg,Msg}, OrigFields} ->
            NewDef = {{msg,Msg}, OrigFields ++ MoreFields},
            lists:keyreplace({msg,Msg}, 1, Defs, NewDef) -- [Extending];
        false ->
            Defs
    end.

%% `Defs' is expected to be flattened
enumerate_msg_fields(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, enumerate_fields(Fields)};
                 ({{group,Name}, Fields}) ->
                      {{group, Name}, enumerate_fields(Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

enumerate_fields(Fields) ->
    lists:map(fun({I, #?gpb_field{}=F}) ->
                      F#?gpb_field{rnum=I};
                 ({I, #gpb_oneof{fields=Fs}=O}) ->
                      NewFields = [F#?gpb_field{rnum=I} || F <- Fs],
                      O#gpb_oneof{rnum=I, fields=NewFields}
              end,
              index_seq(2, Fields)).

index_seq(_Start, []) -> [];
index_seq(Start, L)   -> lists:zip(lists:seq(Start, length(L) + Start - 1), L).

%% `Defs' is expected to be parsed.
normalize_msg_field_options(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, normalize_field_options(Fields)};
                 ({{group,Name}, Fields}) ->
                      {{group, Name}, normalize_field_options(Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

normalize_field_options(Fields) ->
    lists:map(fun(#?gpb_field{type={map,_KeyType,_ValueType}, opts=Opts}=F) ->
                      Opts1    = normalize_field_options_2(Opts),
                      Opts2    = Opts1 -- [packed],
                      F#?gpb_field{opts = Opts2};
                 (#?gpb_field{opts=Opts}=F) ->
                      Opts1    = normalize_field_options_2(Opts),
                      F#?gpb_field{opts = Opts1};
                 (#gpb_oneof{fields=Fs}=O) ->
                      O#gpb_oneof{fields=normalize_field_options(Fs)}
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

%% @doc Fetch the `import'ed files.
%% @end
%% `Defs' is expected to be parsed, but not necessarily post_processed.
-spec fetch_imports(defs()) -> [ProtoFile::string()].
fetch_imports(Defs) ->
    [Path || {import,Path} <- Defs].

mk_meta_info(FileName, Defs, Opts) ->
    meta_msg_containment(FileName, Defs)
        ++ meta_enum_containment(FileName, Defs)
        ++ meta_pkg_containment(FileName, Defs, Opts)
        ++ meta_service_and_rpc_containment(FileName, Defs).

meta_msg_containment(FileName, Defs) ->
    [{{msg_containment, FileName}, lists:sort(gpb_lib:msg_names(Defs))}].

meta_enum_containment(FileName, Defs) ->
    [{{enum_containment, FileName}, lists:sort(gpb_lib:enum_names(Defs))}].

meta_pkg_containment(FileName, Defs, Opts) ->
    case proplists:get_value(package, Defs, '$undefined') of
        '$undefined' ->
            [];
        Pkg ->
            case proplists:get_bool(use_packages, Opts) of
                false ->
                    [];
                true ->
                    [{{pkg_containment,FileName}, Pkg}]
            end
    end.

meta_service_and_rpc_containment(FileName, Defs) ->
    Services = [{Name,RPCs} || {{service,Name}, RPCs} <- Defs],
    if Services == [] ->
            [];
       true ->
            ServiceNames = [Name || {Name, _RPCs} <- Services],
            RpcNames = lists:append([[{SName, RName}
                                      || {RName, _In,_Out, _Opts} <- RPCs]
                                     || {SName, RPCs} <- Services]),
            [{{service_containment, FileName}, lists:sort(ServiceNames)},
             {{rpc_containment, FileName}, RpcNames}]
    end.

shorten_meta_info(Mapping, Defs) ->
    lists:map(
      fun({{msg_containment, FileName}, MsgNames}) ->
              {FileName, Baseish} = lists:keyfind(FileName, 1, Mapping),
              {{msg_containment, Baseish}, MsgNames};
         ({{enum_containment, FileName}, EnumNames}) ->
              {FileName, Baseish} = lists:keyfind(FileName, 1, Mapping),
              {{enum_containment, Baseish}, EnumNames};
         ({{service_containment, FileName}, Services}) ->
              {FileName, Baseish} = lists:keyfind(FileName, 1, Mapping),
              {{service_containment, Baseish}, Services};
         ({{rpc_containment, FileName}, Rpcs}) ->
              {FileName, Baseish} = lists:keyfind(FileName, 1, Mapping),
              {{rpc_containment, Baseish}, Rpcs};
         ({{pkg_containment, FileName}, PkgName}) ->
              {FileName, Baseish} = lists:keyfind(FileName, 1, Mapping),
              {{pkg_containment, Baseish}, PkgName};
         (Other) ->
              Other
      end,
      Defs).

versionize_defs(Defs) ->
    [{proto_defs_version, latest_defs_version()} | Defs].

possibly_hint_use_packages_opt(Reasons, Defs, Opts) ->
    UsePackagesOptPresent = case proplists:get_value(use_packages, Opts) of
                                undefined -> false;
                                _ -> true
                            end,
    UnresolvedRefs = lists:any(fun is_unresolved_ref_reason/1, Reasons),
    Imports = lists:any(fun is_import_item/1, Defs),
    DifferentPackages = length(lists:usort(find_pkgs(Defs))) =/= 1,
    if not UsePackagesOptPresent,
       UnresolvedRefs,
       Imports,
       DifferentPackages ->
            Hint = {hint, {{use_packages, option}, unresolved_references}},
            [Hint | Reasons];
       true ->
            Reasons
    end.

%% Check whether the the files in Defs are in different packages.
%% A {package, _} tuple indicates a package, but a file could also
%% be void of such an indicator.
find_pkgs(Defs) ->
    [proplists:get_value(package, FileChunk)
     || FileChunk <- file_chunks(Defs)].

%% Split to chunks separated by {file,_} items
file_chunks(Defs) ->
    %% Skip anything before first {file,_} item.
    %% There should not be any such chunks, but if there would be,
    %% they would not contain any package declaratins in any case.
    Defs1 = lists:dropwhile(fun is_not_file_item/1, Defs),
    file_chunks2(Defs1, []).

file_chunks2([{file, _}=FileItem | _]=Defs, Acc) ->
    {Chunk, Rest} = lists:splitwith(fun is_not_file_item/1, tl(Defs)),
    file_chunks2(Rest, [[FileItem | Chunk] | Acc]);
file_chunks2([], Acc) ->
    lists:reverse(Acc).

is_not_file_item(X) -> not is_file_item(X).

is_file_item({file, _}) -> true;
is_file_item(_) -> false.

is_import_item({import, _}) -> true;
is_import_item(_) -> false.

is_unresolved_ref_reason({ref_to_undefined_msg_or_enum,_}) -> true;
is_unresolved_ref_reason({rpc_arg_ref_to_undefined_msg, _}) -> true;
is_unresolved_ref_reason({rpc_return_ref_to_undefined_msg, _}) -> true;
is_unresolved_ref_reason({extend_ref_to_undefined_msg, _}) -> true;
is_unresolved_ref_reason(_) -> false.

tmp_fields_to_fields(Defs) -> % Defs are expected to be flattened
    Syntax = proplists:get_value(syntax, Defs, "proto2"),
    OccHandler = mk_occurrence_handler(Syntax),
    {Defs1, Errors} = to_fields_d2(Defs, OccHandler, _Errors=[]),
    if Errors == [] -> {ok, Defs1};
       Errors /= [] -> {error, lists:reverse(Errors)}
    end.

mk_occurrence_handler("proto2") ->
    fun(#?gpb_field{name=FName, occurrence=Occurrence}=F, MsgName, Errors) ->
            case Occurrence of
                required -> {F, Errors};
                repeated -> {F, Errors};
                optional -> {F, Errors};
                undefined ->
                    Err = {missing_occurrence, MsgName, FName},
                    {F#?gpb_field{occurrence=optional}, [Err | Errors]}
            end
    end;
mk_occurrence_handler("proto3") ->
    fun(#?gpb_field{name=FName, occurrence=Occurrence}=F, MsgName, Errors) ->
            case Occurrence of
                undefined -> {F#?gpb_field{occurrence=defaulty}, Errors};
                repeated -> {F, Errors};
                required ->
                    Err = {p3_unallowed_occurrence, MsgName, FName, Occurrence},
                    {F#?gpb_field{occurrence=defaulty}, [Err | Errors]};
                optional ->
                    {F, Errors}
            end
    end.

to_fields_d2(Defs, OccHandler, Errors0) ->
    lists:mapfoldl(
      fun({{msg,MsgName}, MsgElems}, Errors) ->
              {MsgElems1, Errors1} =
                  to_fields_msg2(MsgName, MsgElems, OccHandler, Errors),
              MsgDef1 = {{msg,MsgName}, MsgElems1},
              {MsgDef1, Errors1};
         ({{extend,{eref2,_Root,Name,_Cadidates}=ERef2}, Fields}, Errors) ->
              {Elems1, Errors1} =
                  to_fields_msg2(Name, Fields, OccHandler, Errors),
              Extend1 = {{extend,ERef2}, Elems1},
              {Extend1, Errors1};
         (Other, Errors) ->
              {Other, Errors}
      end,
      Errors0,
      Defs).

to_fields_msg2(MsgName, MsgElems, OccHandler, Errors0) ->
    lists:mapfoldl(
      fun(#?gpb_field{}=F, Errors) ->
              OccHandler(F, MsgName, Errors);
         (#gpb_oneof{}=F, Errors) ->
              {F, Errors}
      end,
      Errors0,
      MsgElems).

%%% -- Version and conversions ------------

%% NB:
%% Be quite explicit here about the structure.  Don't rely so much on
%% external helpers, such as gpb:is_msg_proto3/2, because helpers are
%% for the latest defs version, while this may (in some future) be
%% operating on a format that is not the latest any more.

ensure_proto_defs_versionized(Defs, Version) ->
    case proplists:get_value(proto_defs_version, Defs) of
        undefined ->
            {ok, [{proto_defs_version, Version} | Defs]};
        Version ->
            {ok, Defs};
        _OtherVsn ->
            Defs1 = lists:keydelete(proto_defs_version, 1, Defs),
            {ok, [{proto_defs_version, Version} | Defs1]}
    end.

%% --upgrade--

cvt_defs_1_to_2(Defs) ->
    %% Convert occurrence = optional -> defaulty for proto3 msg fields
    P3Msgs = proplists:get_value(proto3_msgs, Defs, []),
    {ok,
     [case Item of
          {{msg, MsgName}, Fields} ->
              case lists:member(MsgName, P3Msgs) of
                  true ->
                      Fields1 = [case F of
                                     #?gpb_field{occurrence=optional} ->
                                         F#?gpb_field{occurrence=defaulty};
                                     _ ->
                                         F
                                 end
                                 || F <- Fields],
                      {{msg,MsgName}, Fields1};
                  false ->
                      Item
              end;
          _ ->
              Item
      end
      || Item <- Defs]}.


cvt_defs_2_to_3(Defs) ->
    {ok, cvt_defs_2_to_3_aux(Defs)}.

cvt_defs_2_to_3_aux([{{enum,EName}, Elems} | Rest]) ->
    %% Extend enumerators with an empty options
    %% Lift any enumeration options to an {{enum_options, Name} Opts} entry.
    {Enumerators, EOptions} =
        lists:partition(
          fun({_Sym, _Value}) -> true;
             ({option,_OptName,_OptVal}) -> false
          end,
          Elems),
    Elems2 = [{Sym, Value, []} || {Sym, Value} <- Enumerators],
    EDef2 = {{enum, EName}, Elems2},
    if EOptions == [] ->
            [EDef2 | cvt_defs_2_to_3_aux(Rest)];
       EOptions /= [] ->
            EOptElems = [{OptName, OptValue}
                         || {option, OptName, OptValue} <- EOptions],
            EOptionsDef = {{enum_options, EName}, EOptElems},
            [EDef2, EOptionsDef | cvt_defs_2_to_3_aux(Rest)]
    end;
cvt_defs_2_to_3_aux([OtherDef | Rest]) ->
    [OtherDef | cvt_defs_2_to_3_aux(Rest)];
cvt_defs_2_to_3_aux([]) ->
    [].

cvt_defs_3_to_4(Defs) ->
    %% Should we attempt to consider packages to get more accurate
    %% name components?
    %% Eg if an option is [o,p,f,x] and there some other {package,'o,p'},
    %% in Defs, then a more accurate upgrade translation is [{o,p,f},x]
    %% Though, there are still cases we can't handle.
    %% And for now, we don't use the options.
    {ok, lists:map(
           fun({{service, _ServiceName}=Key, Rpcs3}) ->
                   Rpcs4 =
                       lists:map(
                         fun(#?gpb_rpc{opts=Opts}=R) ->
                                 R#?gpb_rpc{opts=rpc_opts_3_to_4(Opts)}
                         end,
                         Rpcs3),
                   {Key, Rpcs4};
              (Other) ->
                   Other
           end,
           Defs)}.

rpc_opts_3_to_4(Opts) ->
    lists:map(
      fun({Name, Value}) -> {rpc_opt_name_3_to_4(Name), Value};
         (Other)         -> Other % example: packed
      end,
      Opts).

rpc_opt_name_3_to_4(Name) ->
    case gpb_lib:string_lexemes(atom_to_list(Name), ".") of
        [_NotDotted] -> Name;
        Parts -> [list_to_atom(Part) || Part <- Parts]
    end.

%% --downgrade--

cvt_defs_4_to_3(Defs) ->
    {ok, lists:map(
           fun({{msg,_MsgName}=Key, Fields}) ->
                   {Key, field_opts_4_to_3(Fields)};
              ({{msg_options, _MsgName}=Key, Opts}) ->
                   {Key, opts_4_to_3(Opts)};
              ({{enum,_EnumName}=Key, EnumElems4}) ->
                   EnumElems3 =
                       lists:map(
                         fun({Sym, Val, Opts}) ->
                                 {Sym, Val, opts_4_to_3(Opts)}
                         end,
                         EnumElems4),
                   {Key, EnumElems3};
              ({{enum_options, _EnumName}=Key, Opts}) ->
                   {Key, opts_4_to_3(Opts)};
              ({{service, _ServiceName}=Key, Rpcs4}) ->
                   Rpcs3 =
                       lists:map(
                         fun(#?gpb_rpc{opts=Opts}=R) ->
                                 R#?gpb_rpc{opts=rpc_opts_4_to_3(Opts)}
                         end,
                         Rpcs4),
                   {Key, Rpcs3};
              ({{service_options, _ServiceName}=Key, Opts}) ->
                   {Key, opts_4_to_3(Opts)};
              (Other) ->
                   Other
           end,
           Defs)}.

field_opts_4_to_3(Fields) ->
    lists:map(
      fun(#?gpb_field{opts=Opts}=F) ->
              F#?gpb_field{opts = opts_4_to_3(Opts)};
         (#gpb_oneof{fields=Fs, opts=Opts}=F) ->
              F#gpb_oneof{fields = field_opts_4_to_3(Fs),
                          opts   = opts_4_to_3(Opts)}
      end,
      Fields).

opts_4_to_3(Opts) ->
    lists:map(
      fun({Name, Value}) when is_list(Name) -> {opt_name_4_to_3(Name), Value};
         ({OtherName, Value})               -> {OtherName, Value};
         (Other)                            -> Other % example: packed
      end,
      Opts).

%% Example: [{pkg,custom_opt},f] -> [pkg,'.',custom_opt,f]
opt_name_4_to_3([ExtComponents | Rest]) when is_tuple(ExtComponents) ->
    Dotted = case tuple_to_list(ExtComponents) of
                 ['.' | More] -> ['.' | gpb_lib:ljoin('.', More)];
                 Comps        -> gpb_lib:ljoin('.', Comps)
             end,
    Dotted ++ opt_name_4_to_3(Rest);
opt_name_4_to_3([NameComponent | Rest]) ->
    [NameComponent | opt_name_4_to_3(Rest)];
opt_name_4_to_3([]) ->
    [].

rpc_opts_4_to_3(Opts) ->
    lists:map(
      fun({Name, Value}) when is_list(Name) -> {rpc_opt_name_43(Name), Value};
         ({OtherName, Value})               -> {OtherName, Value};
         (Other)                            -> Other % example: packed
      end,
      Opts).

%% Example:[{pkg,a,custom_option},subfield] -> 'pkg.a.custom_option.subfield'
rpc_opt_name_43(OptName) ->
    Components = opt_name_4_to_3(OptName),
    list_to_atom(gpb_lib:dot_join([atom_to_list(P) || P <- Components])).

opt_name_4_to_3_test() ->
    [custom_opt] = opt_name_4_to_3([{custom_opt}]),         % (custom_opt)
    [pkg,'.',opt] = opt_name_4_to_3([{pkg,opt}]),           % (pkg.opt)
    [pkg,'.',opt,f] = opt_name_4_to_3([{pkg,opt},f]),       % (pkg.opt).f
    [opt,f,g] = opt_name_4_to_3([{opt},f,g]),               % (opt).f.g
    [p,f,g] = opt_name_4_to_3([{p},{f},g]),                 % (p).(f).g
    ['.',p,'.',f,g] = opt_name_4_to_3([{'.',p},{'.',f},g]), % (.p).(.f).g
    ok.

rpc_opt_name_43_test() ->
    custom_opt = rpc_opt_name_43([{custom_opt}]),       % (custom_opt)
    'pkg...opt' = rpc_opt_name_43([{pkg,opt}]),         % (pkg.opt)
    'pkg...opt.f' = rpc_opt_name_43([{pkg,opt},f]),     % (pkg.opt).f
    'opt.f.g' = rpc_opt_name_43([{opt},f,g]),           % (opt).f.g
    'p.f.g' = rpc_opt_name_43([{p},{f},g]),             % (p).(f).g
    '..p...f.g' = rpc_opt_name_43([{'.',p},{'.',f},g]), % (.p).(.f).g
    ok.

cvt_defs_3_to_2(Defs) ->
    %% For any {{enum_options, EName}, Opts}, insert the Opts
    %% into the corresponding Elems in {{enum, EName}, Elems}
    %%
    %% Also: For each {Sym, Name, _Opts} in Elems, drop _Opts

    {Defs2, EOptionsDefs} =
        lists:partition(
          fun({{enum_options, _EName}, _EOpts}) -> false;
             (_OtherElem) -> true
          end,
          Defs),
    {ok, cvt_defs_3_to_2_aux(Defs2, EOptionsDefs)}.

cvt_defs_3_to_2_aux([{{enum, EName}, Elems} | Rest], EOptionsDefs) ->
    Elems2 = [{Sym, Value} || {Sym, Value, _Opts} <- Elems],
    case lists:keyfind({enum_options, EName}, 1, EOptionsDefs) of
        {{enum_options, EName}, EOptions} ->
            OptElems = [{option, OptName, OptValue}
                        || {OptName, OptValue} <- EOptions],
            Def2 = {{enum, EName}, OptElems ++ Elems2},
            [Def2 | cvt_defs_3_to_2_aux(Rest, EOptionsDefs)];
        false ->
            Def2 = {{enum, EName}, Elems2},
            [Def2 | cvt_defs_3_to_2_aux(Rest, EOptionsDefs)]
    end;
cvt_defs_3_to_2_aux([OtherDef | Rest], EOptionsDefs) ->
    [OtherDef | cvt_defs_3_to_2_aux(Rest, EOptionsDefs)];
cvt_defs_3_to_2_aux([], _) ->
    [].


cvt_defs_2_to_1(Defs) ->
    %% Convert occurrence = defaulty -> optional for proto3 msg fields
    P3Msgs = proplists:get_value(proto3_msgs, Defs, []),
    try
        {ok,
         [case Item of
              {{msg, MsgName}, Fields} ->
                  case lists:member(MsgName, P3Msgs) of
                      true ->
                          Fields1 =
                              [case F of
                                   #?gpb_field{occurrence=defaulty} ->
                                       F#?gpb_field{occurrence=optional};
                                   #?gpb_field{name=FName,
                                               occurrence=optional} ->
                                       throw({p3_optional,MsgName,FName});
                                   _ ->
                                       F
                               end
                               || F <- Fields],
                          {{msg,MsgName}, Fields1};
                      false ->
                          Item
                  end;
              _ ->
                  Item
          end
          || Item <- Defs]}
    catch {p3_optional, MsgName, FName} ->
            {error, {defs_unrepresentable_in_fmt_1,p3_optional,MsgName,FName}}
    end.
