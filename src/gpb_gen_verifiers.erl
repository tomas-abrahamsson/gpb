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

%%% @doc Generation of message verifiers
%%% @private

-module(gpb_gen_verifiers).

-export([format_verifiers_top_function/2]).
-export([format_verifiers/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

format_verifiers_top_function(Defs, Opts) ->
    case {gpb_lib:contains_messages(Defs),
          gpb_lib:get_records_or_maps_by_opts(Opts)} of
        {false, records} -> format_verifiers_top_no_msgs_r();
        {false, maps}    -> format_verifiers_top_no_msgs_m();
        {true,  _}       -> format_verifiers_top_with_msgs(Defs, Opts)
    end.

format_verifiers_top_no_msgs_r() ->
    [?f("-spec verify_msg(_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg) -> call_self(Msg, []) end),
     ?f("-spec verify_msg(_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg,_Opts) -> mk_type_error(not_a_known_message, Msg, []) end),
     "\n"].

format_verifiers_top_no_msgs_m() ->
    [?f("-spec verify_msg(_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, MsgName) -> call_self(Msg, MsgName, []) end),
     ?f("-spec verify_msg(_,_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, _MsgName, _Opts) ->
               mk_type_error(not_a_known_message, Msg, [])
       end),
     "\n"].

format_verifiers_top_with_msgs(Defs, Opts) ->
    Mapping = gpb_lib:get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(MsgName)]
                  end,
    [gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, '<MsgName>') -> call_self(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, '<MsgName>', Opts) ->
               TrUserData = proplists:get_value(user_data, Opts),
               case '<MsgOrMsgName>' of
                   '<msg-match>' -> '<verify-msg>'(Msg, ['<MsgName>'],
                                                   TrUserData);
                   _ -> mk_type_error(not_a_known_message, Msg, [])
               end
       end,
       [repeat_clauses(
          '<msg-match>',
          [[replace_tree('<msg-match>',
                         case Mapping of
                             records -> gpb_lib:record_match(MsgName, []);
                             maps    -> erl_syntax:atom(MsgName)
                         end),
            replace_term('<verify-msg>', gpb_lib:mk_fn(v_msg_, MsgName)),
            replace_term('<MsgName>', MsgName)]
           || {{msg, MsgName}, _MsgDef} <- Defs]),
        replace_tree('<MsgOrMsgName>', case Mapping of
                                           records -> ?expr(Msg);
                                           maps    -> ?expr(MsgName)
                                       end),
        splice_trees('<MsgName>', MsgNameVars)])].

format_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifiers(Defs, AnRes, Opts),
     format_enum_verifiers(Defs, AnRes, Opts),
     format_type_verifiers(AnRes, Opts),
     format_map_verifiers(AnRes, Opts),
     format_verifier_auxiliaries(Defs, Opts)
    ].

format_msg_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifier(MsgName, MsgDef, AnRes, Opts)
     || {_Type, MsgName, MsgDef} <- gpb_lib:msgs_or_groups(Defs)].

format_msg_verifier(MsgName, MsgDef, AnRes, Opts) ->
    FNames = gpb_lib:get_field_names(MsgDef),
    FVars = [gpb_lib:var_f_n(I) || I <- lists:seq(1, length(FNames))],
    MsgVar = ?expr(M),
    {FieldMatching, NonOptKeys} =
        case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
            X when X == records;
                   X == {maps, present_undefined} ->
                {gpb_lib:mapping_match(MsgName, lists:zip(FNames, FVars), Opts),
                 FNames};
            {maps, omitted} ->
                FMap = gpb_lib:zip_for_non_opt_fields(MsgDef, FVars),
                {?expr('mapmatch' = 'M',
                       [replace_tree('mapmatch', gpb_lib:map_match(FMap)),
                        replace_tree('M', MsgVar)]),
                 [K || {K, _} <- FMap]}
        end,
    ExtraneousFieldsChecks =
        case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
            X2 when X2 == records;
                    X2 == {maps, present_undefined} ->
                [];
            {maps, omitted} ->
                [?expr(lists:foreach(
                         fun('<Key>') ->
                                 ok;
                            (OtherKey) ->
                                 mk_type_error({extraneous_key, OtherKey},
                                               'M', Path)
                         end,
                         maps:keys('M')),
                       [repeat_clauses('<Key>',
                                       [[replace_term('<Key>', Key)]
                                         || Key <- FNames]),
                        replace_tree('M', MsgVar)])]
        end,

    NeedsMatchOther = case gpb_lib:get_records_or_maps_by_opts(Opts) of
                          records -> can_occur_as_sub_msg(MsgName, AnRes);
                          maps    -> true
                      end,
    FnName = gpb_lib:mk_fn(v_msg_, MsgName),
    TrUserDataVar = ?expr(TrUserData),
    [gpb_lib:nowarn_dialyzer_attr(FnName,3,Opts),
     gpb_codegen:format_fn(
       FnName,
       fun('<msg-match>', '<Path>', 'MaybeTrUserData') ->
               '<verify-fields>',
               '<maybe-verify-no-extraneous-fields>',
               ok;
          ('<M>', Path, _TrUserData) when is_map('<M>') ->
               mk_type_error(
                 {missing_fields, 'NonOptKeys'--maps:keys('<M>'), '<MsgName>'},
                 '<M>', Path);
          ('<X>', Path, _TrUserData) ->
               mk_type_error({expected_msg,'<MsgName>'}, X, Path)
       end,
       [replace_tree('<msg-match>', FieldMatching),
        replace_tree('<Path>', if MsgDef == [], ExtraneousFieldsChecks == [] ->
                                       ?expr(_Path);
                                  true ->
                                       ?expr(Path)
                               end),
        replace_tree('MaybeTrUserData',
                     case gpb_lib:any_field_is_sub_msg(MsgDef)
                         orelse gpb_gen_translators:exists_tr_for_msg(MsgName,
                                                                      verify,
                                                                      AnRes) of
                         true  -> TrUserDataVar;
                         false -> ?expr(_)
                     end),
        splice_trees('<verify-fields>',
                     field_verifiers(MsgName, MsgDef, FVars, MsgVar,
                                     TrUserDataVar,
                                     AnRes, Opts)),
        splice_trees('<maybe-verify-no-extraneous-fields>',
                     ExtraneousFieldsChecks),
        repeat_clauses('<X>', case NeedsMatchOther of
                                  true  -> [[replace_tree('<X>', ?expr(X))]];
                                  false -> [] %% omit the else clause
                              end),
        repeat_clauses('<M>',
                       case gpb_lib:get_records_or_maps_by_opts(Opts) of
                           records ->
                               []; % omit this clause
                           maps ->
                               [[replace_tree('<M>', ?expr(M)),
                                 replace_term('NonOptKeys', NonOptKeys)]]
                       end),
        replace_term('<MsgName>', MsgName)])].

field_verifiers(MsgName, Fields, FVars, MsgVar, TrUserDataVar, AnRes, Opts) ->
    [field_verifier(MsgName, Field, FVar, MsgVar, TrUserDataVar, AnRes, Opts)
     || {Field, FVar} <- lists:zip(Fields, FVars)].

field_verifier(MsgName,
               #?gpb_field{name=FName, type=Type, occurrence=Occurrence}=Field,
               FVar, MsgVar, TrUserDataVar, AnRes, Opts) ->
    FVerifierFn =
        case Type of
            {msg,FMsgName}  -> gpb_lib:mk_fn(v_msg_, FMsgName);
            {group,GName}   -> gpb_lib:mk_fn(v_msg_, GName);
            {enum,EnumName} -> gpb_lib:mk_fn(v_enum_, EnumName);
            {map,KT,VT}     -> gpb_lib:mk_fn(v_, gpb_lib:map_type_to_msg_name(
                                                   KT,VT));
            Type            -> gpb_lib:mk_fn(v_type_, Type)
        end,
    ElemPath = gpb_gen_translators:mk_elempath_elem(MsgName, Field, false),
    FVerifierFn2 = gpb_gen_translators:find_translation(ElemPath, verify,
                                                        AnRes, FVerifierFn),
    Replacements = [replace_term('<verify-fn>', FVerifierFn2),
                    replace_tree('<F>', FVar),
                    replace_term('<FName>', FName),
                    replace_term('<Type>', Type),
                    replace_tree('TrUserData', TrUserDataVar),
                    splice_trees('MaybeTrUserData',
                                 gpb_gen_translators:maybe_userdata_param(
                                   Field,
                                   TrUserDataVar))],
    IsMapField = case Type of
                     {map,_,_} -> true;
                     _ -> false
                 end,
    case Occurrence of
        required ->
            %% FIXME: check especially for `undefined'
            %% and if found, error out with required_field_not_set
            %% specifying expected type
            ?expr('<verify-fn>'('<F>', ['<FName>' | Path], 'MaybeTrUserData'),
                  Replacements);
        repeated when not IsMapField ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if is_list('<F>') ->
                                 %% _ = [...] to avoid dialyzer error
                                 %% "Expression produces a value of type
                                 %% ['ok'], but this value is unmatched"
                                 %% with the -Wunmatched_returns flag.
                                 _ = ['<verify-fn>'(Elem, ['<FName>' | Path],
                                                    'MaybeTrUserData')
                                      || Elem <- '<F>'],
                                 ok;
                             true ->
                                 mk_type_error(
                                   {invalid_list_of, '<Type>'},
                                   '<F>',
                                   ['<FName>' | Path])
                          end,
                          Replacements);
                {maps, omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  if is_list('<F>') ->
                                         %% _ = [...] to avoid dialyzer error
                                         %% "Expression produces a value of type
                                         %% ['ok'], but this value is unmatched"
                                         %% with the -Wunmatched_returns flag.
                                         _ = ['<verify-fn>'(Elem, ['<FName>' | Path],
                                                            'MaybeTrUserData')
                                              || Elem <- '<F>'],
                                         ok;
                                     true ->
                                         mk_type_error(
                                           {invalid_list_of, '<Type>'},
                                           '<F>',
                                           ['<FName>' | Path])
                                  end;
                              _ -> ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}])),
                           replace_tree('M', MsgVar) | Replacements])
            end;
        repeated when IsMapField ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr('<verify-fn>'('<F>', ['<FName>' | Path],
                                        'TrUserData'),
                          Replacements);
                {maps, omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'TrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}])),
                           replace_tree('M', MsgVar) | Replacements])
            end;
        optional ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path],
                                                  'MaybeTrUserData')
                          end,
                          Replacements);
                {maps, omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'MaybeTrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}])),
                           replace_tree('M', MsgVar) | Replacements])
            end
    end;
field_verifier(MsgName, #gpb_oneof{name=FName, fields=OFields},
               FVar, MsgVar, TrUserDataVar, AnRes, Opts) ->
    IsOneof = {true, FName},
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            ?expr(
               case '<F>' of
                   undefined ->
                       ok;
                   '<oneof-pattern>' ->
                       '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path],
                                     'MaybeTrUserData');
                   _ ->
                       mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path])
               end,
               [replace_tree('<F>', FVar),
                replace_term('<FName>', FName),
                repeat_clauses(
                  '<oneof-pattern>',
                  [begin
                       FVerifierFn =
                           case Type of
                               {msg,FMsgName} ->
                                   gpb_lib:mk_fn(v_msg_, FMsgName);
                               {enum,EnumName} ->
                                   gpb_lib:mk_fn(v_enum_, EnumName);
                               Type ->
                                   gpb_lib:mk_fn(v_type_, Type)
                           end,
                       ElemPath = gpb_gen_translators:mk_elempath_elem(
                                    MsgName, F, IsOneof),
                       FVerifierFn2 = gpb_gen_translators:find_translation(
                                        ElemPath, verify, AnRes,
                                        FVerifierFn),
                       OFVar = gpb_lib:prefix_var("O", FVar),
                       [replace_tree('M', MsgVar),
                        replace_tree('<oneof-pattern>',
                                     ?expr({'<OFName>','<OFVar>'})),
                        replace_term('<verify-fn>', FVerifierFn2),
                        replace_tree('<OFVar>', OFVar),
                        replace_term('<OFName>', OFName),
                        splice_trees('MaybeTrUserData',
                                     gpb_gen_translators:maybe_userdata_param(
                                       F, TrUserDataVar))]
                   end
                   || #?gpb_field{name=OFName, type=Type}=F <- OFields])]);
        {maps, omitted} ->
            ?expr(
               case 'M' of
                   '<oneof-pattern>' ->
                       '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path],
                                     'MaybeTrUserData');
                   '#{<FName> := <F>}' ->
                       mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path]);
                   _ ->
                       ok
               end,
               [replace_tree('<F>', FVar),
                replace_term('<FName>', FName),
                replace_tree('M', MsgVar),
                replace_tree('#{<FName> := <F>}',
                             gpb_lib:map_match([{FName, FVar}])),
                repeat_clauses(
                  '<oneof-pattern>',
                  [begin
                       FVerifierFn =
                           case Type of
                               {msg,FMsgName} ->
                                   gpb_lib:mk_fn(v_msg_, FMsgName);
                               {enum,EnumName} ->
                                   gpb_lib:mk_fn(v_enum_, EnumName);
                               Type ->
                                   gpb_lib:mk_fn(v_type_, Type)
                           end,
                       ElemPath = gpb_gen_translators:mk_elempath_elem(
                                    MsgName, F, IsOneof),
                       FVerifierFn2 = gpb_gen_translators:find_translation(
                                        ElemPath, verify, AnRes,
                                        FVerifierFn),
                       OFVar = gpb_lib:prefix_var("O", FVar),
                       Trs1 = [replace_tree('<OFVar>', OFVar),
                               replace_term('<OFName>', OFName)],
                       OFPat = ?expr({'<OFName>','<OFVar>'}, Trs1),
                       [replace_tree('<oneof-pattern>',
                                     gpb_lib:map_match([{FName, OFPat}])),
                        replace_term('<verify-fn>', FVerifierFn2),
                        splice_trees('MaybeTrUserData',
                                     gpb_gen_translators:maybe_userdata_param(
                                       F, TrUserDataVar))
                        | Trs1]
                   end
                   || #?gpb_field{name=OFName, type=Type}=F <- OFields])])
    end.


can_occur_as_sub_msg(MsgName, #anres{used_types=UsedTypes}) ->
    sets:is_element({msg,MsgName}, UsedTypes)
        orelse sets:is_element({group,MsgName}, UsedTypes).

format_enum_verifiers(Defs, #anres{used_types=UsedTypes}, Opts) ->
    [format_enum_verifier(EnumName, Def, Opts)
     || {{enum,EnumName}, Def} <- Defs,
        gpb_lib:smember({enum, EnumName}, UsedTypes)].

format_enum_verifier(EnumName, EnumMembers, Opts) ->
    FnName = gpb_lib:mk_fn(v_enum_, EnumName),
    [gpb_lib:nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun('<sym>', _Path) -> ok;
          (V, Path) when is_integer(V) -> v_type_sint32(V, Path);
          (X, Path) -> mk_type_error({invalid_enum, '<EnumName>'}, X, Path)
       end,
       [repeat_clauses('<sym>', [[replace_term('<sym>', EnumSym)]
                                 || {EnumSym, _Value} <- EnumMembers]),
        replace_term('<EnumName>', EnumName)])].

format_type_verifiers(#anres{used_types=UsedTypes}, Opts) ->
    NeedSInt32 = (gpb_lib:smember(sint32, UsedTypes) orelse
                  gpb_lib:any_enum_field_exists(UsedTypes)),
    NeedBool   = gpb_lib:smember(bool, UsedTypes),
    NeedFloat  = gpb_lib:smember(float, UsedTypes),
    NeedDouble = gpb_lib:smember(double, UsedTypes),
    NeedString = gpb_lib:smember(string, UsedTypes),
    NeedBytes  = gpb_lib:smember(bytes, UsedTypes),
    [[format_int_verifier(sint32, signed, 32, Opts) || NeedSInt32],
     [format_int_verifier(Type, Signedness, Bits, Opts)
      || {Type, Signedness, Bits} <- [{sint64,   signed,   64},
                                      {int32,    signed,   32},
                                      {int64,    signed,   64},
                                      {uint32,   unsigned, 32},
                                      {uint64,   unsigned, 64},
                                      {fixed32,  unsigned, 32},
                                      {fixed64,  unsigned, 64},
                                      {sfixed32, signed,   32},
                                      {sfixed64, signed,   64}],
         gpb_lib:smember(Type, UsedTypes)],
     [format_bool_verifier(Opts)                || NeedBool],
     [format_float_verifier(float, Opts)        || NeedFloat],
     [format_float_verifier(double, Opts)       || NeedDouble],
     [format_string_verifier(Opts)              || NeedString],
     [format_bytes_verifier(Opts)               || NeedBytes]].

format_int_verifier(IntType, Signedness, NumBits, Opts) ->
    Min = case Signedness of
              unsigned -> 0;
              signed   -> -(1 bsl (NumBits-1))
          end,
    Max = case Signedness of
              unsigned -> 1 bsl NumBits - 1;
              signed   -> 1 bsl (NumBits-1) - 1
          end,
    FnName = gpb_lib:mk_fn(v_type_, IntType),
    [gpb_lib:nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(N, _Path) when '<Min>' =< N, N =< '<Max>' ->
               ok;
          (N, Path) when is_integer(N) ->
               mk_type_error({value_out_of_range, '<details>'}, N, Path);
          (X, Path) ->
               mk_type_error({bad_integer, '<details>'}, X, Path)
       end,
       [replace_term('<Min>', Min),
        replace_term('<Max>', Max),
        splice_trees('<details>', [erl_syntax:atom(IntType),
                                   erl_syntax:atom(Signedness),
                                   erl_syntax:integer(NumBits)])])].

format_bool_verifier(Opts) ->
    FnName = gpb_lib:mk_fn(v_type_, bool),
    [gpb_lib:nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(false, _Path) -> ok;
          (true, _Path)  -> ok;
          (0, _Path)  -> ok;
          (1, _Path)  -> ok;
          (X, Path) -> mk_type_error(bad_boolean_value, X, Path)
       end)].

format_float_verifier(FlType, Opts) ->
    BadTypeOfValue = list_to_atom(lists:concat(["bad_", FlType, "_value"])),
    FnName = gpb_lib:mk_fn(v_type_, FlType),
    [gpb_lib:nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(N, _Path) when is_float(N) -> ok;
          %% It seems a float for the corresponding integer value is
          %% indeed packed when doing <<Integer:32/little-float>>.
          %% So let verify accept integers too.
          %% When such a value is unpacked, we get a float.
          (N, _Path) when is_integer(N) -> ok;
          (infinity, _Path)    -> ok;
          ('-infinity', _Path) -> ok;
          (nan, _Path)         -> ok;
          (X, Path)            -> mk_type_error('<bad_x_value>', X, Path)
       end,
       [replace_term('<bad_x_value>', BadTypeOfValue)])].

format_string_verifier(Opts) ->
    FnName = gpb_lib:mk_fn(v_type_, string),
    [gpb_lib:nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(S, Path) when is_list(S); is_binary(S) ->
               try unicode:characters_to_binary(S) of
                   B when is_binary(B) ->
                       ok;
                   {error, _, _} -> %% a non-UTF-8 binary
                       mk_type_error(bad_unicode_string, S, Path)
               catch error:badarg ->
                       mk_type_error(bad_unicode_string, S, Path)
               end;
          (X, Path) ->
               mk_type_error(bad_unicode_string, X, Path)
       end)].

format_bytes_verifier(Opts) ->
    FnName = gpb_lib:mk_fn(v_type_, bytes),
    [gpb_lib:nowarn_dialyzer_attr(FnName, 2, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(B, _Path) when is_binary(B) ->
               ok;
          (B, _Path) when is_list(B) ->
               ok;
          (X, Path) ->
               mk_type_error(bad_binary_value, X, Path)
       end)].

format_map_verifiers(#anres{map_types=MapTypes}=AnRes, Opts) ->
    MapsOrTuples = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts),
    [format_map_verifier(KeyType, ValueType, MapsOrTuples, AnRes, Opts)
     || {KeyType,ValueType} <- sets:to_list(MapTypes)].

format_map_verifier(KeyType, ValueType, MapsOrTuples, AnRes, Opts) ->
    MsgName = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
    FnName = gpb_lib:mk_fn(v_, MsgName),
    KeyVerifierFn = gpb_lib:mk_fn(v_type_, KeyType),
    ValueVerifierFn1 = case ValueType of
                           {msg,FMsgName}  -> gpb_lib:mk_fn(v_msg_, FMsgName);
                           {enum,EnumName} -> gpb_lib:mk_fn(v_enum_, EnumName);
                           Type            -> gpb_lib:mk_fn(v_type_, Type)
                       end,
    ElemPath = [MsgName,value],
    ValueVerifierFn2 = gpb_gen_translators:find_translation(
                         ElemPath, verify, AnRes,
                         ValueVerifierFn1),

    TrUserDataVar = ?expr(TrUserData),
    TrUserDataReplacements =
        case {ValueType,{ValueVerifierFn1, ValueVerifierFn2}} of
            {{msg,_}, _} ->
                [replace_tree('MaybeTrUserDataArg', TrUserDataVar),
                 replace_tree('MaybeTrUserData', TrUserDataVar)];
            {_, {X,Y}} when X /= Y ->
                %% Translation exists
                [replace_tree('MaybeTrUserDataArg', TrUserDataVar),
                 replace_tree('MaybeTrUserData', TrUserDataVar)];
            _ ->
                [replace_tree('MaybeTrUserDataArg', ?expr(_)),
                 splice_trees('MaybeTrUserData', [])]
        end,
    [gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     case MapsOrTuples of
         '2tuples' ->
             gpb_codegen:format_fn(
               FnName,
               fun(KVs, Path, 'MaybeTrUserDataArg') when is_list(KVs) ->
                       [case X of
                            {Key, Value} ->
                                'VerifyKey'(Key, ['key' | Path]),
                                'VerifyValue'(Value, ['value' | Path],
                                             'MaybeTrUserData');
                            _ ->
                                mk_type_error(invalid_key_value_tuple, X, Path)
                        end
                        || X <- KVs],
                       ok;
                  (X, Path, _TrUserData) ->
                       mk_type_error(invalid_list_of_key_value_tuples, X, Path)
               end,
               [replace_term('VerifyKey', KeyVerifierFn),
                replace_term('VerifyValue', ValueVerifierFn2)]
               ++ TrUserDataReplacements);
         maps ->
             gpb_codegen:format_fn(
               FnName,
               fun(M, Path, 'MaybeTrUserDataArg') when is_map(M) ->
                       [begin
                            'VerifyKey'(Key, ['key' | Path]),
                            'VerifyValue'(Value, ['value' | Path],
                                         'MaybeTrUserData')
                        end
                        || {Key, Value} <- maps:to_list(M)],
                       ok;
                  (X, Path, _TrUserData) ->
                       mk_type_error(invalid_map, X, Path)
               end,
               [replace_term('VerifyKey', KeyVerifierFn),
                replace_term('VerifyValue', ValueVerifierFn2)]
               ++ TrUserDataReplacements)
     end].

format_verifier_auxiliaries(Defs, Opts) ->
    ["-spec mk_type_error(_, _, list()) -> no_return().\n",
     gpb_codegen:format_fn(
       mk_type_error,
       fun(Error, ValueSeen, Path) ->
               Path2 = prettify_path(Path),
               erlang:error({gpb_type_error,
                             {Error, [{value, ValueSeen},{path, Path2}]}})
       end),
     "\n",
     case gpb_lib:contains_messages(Defs) of
         false ->
             gpb_codegen:format_fn(
               prettify_path,
               fun([]) -> top_level end);
         true ->
             case gpb_lib:target_has_lists_join(Opts) of
                 true ->
                     format_prettify_path_with_lists_join();
                 false ->
                     format_prettify_path_with_string_join()
             end
     end].

format_prettify_path_with_lists_join() ->
    gpb_codegen:format_fn(
      prettify_path,
      fun([]) ->
              top_level;
         (PathR) ->
              list_to_atom(
                lists:append(
                  lists:join(".", lists:map(fun atom_to_list/1,
                                            lists:reverse(PathR)))))
      end).

format_prettify_path_with_string_join() ->
    gpb_codegen:format_fn(
      prettify_path,
      fun([]) ->
              top_level;
         (PathR) ->
              list_to_atom(string:join(lists:map(fun atom_to_list/1,
                                                 lists:reverse(PathR)),
                                       "."))
      end).
