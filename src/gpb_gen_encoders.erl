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

%%% @doc Generation of encoding functions.
%%% @private

-module(gpb_gen_encoders).

-export([format_encoders_top_function/2]).
-export([format_msg_encoders/4]).
-export([format_map_encoders/4]).
-export([format_aux_encoders/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

%% -- encoders -----------------------------------------------------

format_encoders_top_function(Defs, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_encoders_top_function_msgs(Defs, Opts);
        false -> format_encoders_top_function_no_msgs(Opts)
    end.

format_encoders_top_function_no_msgs(Opts) ->
    Mapping = gpb_lib:get_records_or_maps_by_opts(Opts),
    MsgNameVars = case Mapping of
                      records -> [];
                      maps    -> [?expr(_MsgName)]
                  end,
    SpecExtraArgs = case Mapping of
                        records -> "";
                        maps    -> ",_"
                    end,
    [?f("-spec encode_msg(_~s) -> no_return().~n", [SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>') -> encode_msg(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     ?f("-spec encode_msg(_,_~s) -> no_return().~n", [SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(_Msg, '<MsgName>', _Opts) ->
               erlang:error({gpb_error, no_messages})
       end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     [[?f("%% epb compatibility\n"),
       ?f("-spec encode(_) -> no_return().\n"),
       gpb_codegen:format_fn(
         encode,
         fun(_Msg) -> erlang:error({gpb_error, no_messages}) end)]
      || gpb_lib:get_epb_functions_by_opts(Opts)]].

format_encoders_top_function_msgs(Defs, Opts) ->
    Verify = proplists:get_value(verify, Opts, optionally),
    Mapping = gpb_lib:get_records_or_maps_by_opts(Opts),
    MsgNames = gpb_lib:msg_names(Defs),
    {MsgNameVars, MsgType, SpecExtraArgs} =
        case Mapping of
            records ->
                {[],
                 gpb_lib:or_join([?f("#~p{}", [M]) || M <- MsgNames]),
                 ""};
            maps ->
                MsgNameType = gpb_lib:or_join(
                                [?f("~p", [M]) || M <- MsgNames]),
                MsgMapType =
                    case gpb_lib:get_type_specs_by_opts(Opts) of
                        false ->
                            "map()";
                        true ->
                            gpb_lib:or_join([?f("~p()", [M]) || M <- MsgNames])
                    end,
                {[?expr(MsgName)],
                 MsgMapType,
                 "," ++ MsgNameType}
    end,
    DoNif    = proplists:get_bool(nif, Opts),
    [?f("-spec encode_msg(~s~s) -> binary().~n", [MsgType, SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>') -> encode_msg(Msg, '<MsgName>', []) end,
       [splice_trees('<MsgName>', MsgNameVars)]),
     "\n",
     ?f("-spec encode_msg(~s~s, list()) -> binary().~n",
        [MsgType, SpecExtraArgs]),
     gpb_codegen:format_fn(
       encode_msg,
       fun(Msg, '<MsgName>', '<Opts>') ->
               '<possibly-verify-msg>',
               'TrUserData = proplists:get_value(user_data, Opts)',
               case '<MsgOrMsgName>' of
                   '<msg-match>' -> 'encode'(Msg, 'TrUserData')
               end
       end,
       [replace_tree('<Opts>', case {DoNif, Verify} of
                                   {true,optionally} -> ?expr(Opts);
                                   {true,always}     -> ?expr(Opts);
                                   {true,never}      -> ?expr(_Opts);
                                   {false,_}         -> ?expr(Opts)
                               end),
        splice_trees('<possibly-verify-msg>',
                     case Verify of
                         optionally ->
                             [?expr(case proplists:get_bool(verify, Opts) of
                                        true  -> verify_msg(Msg, '<MsgName>',
                                                            Opts);
                                        false -> ok
                                    end)];
                         always ->
                             [?expr(verify_msg(Msg, '<MsgName>', Opts))];
                         never ->
                             []
                     end),
        splice_trees(
          'TrUserData = proplists:get_value(user_data, Opts)',
          if DoNif -> [];
             true  -> [?expr(TrUserData = proplists:get_value(user_data, Opts))]
          end),
        repeat_clauses('<msg-match>',
                       [[replace_tree(
                           '<msg-match>',
                           case Mapping of
                               records -> gpb_lib:record_match(MsgName, []);
                               maps    -> erl_syntax:atom(MsgName)
                           end),
                         replace_term('encode', gpb_lib:mk_fn(e_msg_, MsgName))]
                        || {{msg,MsgName}, _Fields} <- Defs]),
        replace_tree('<MsgOrMsgName>', case Mapping of
                                           records -> ?expr(Msg);
                                           maps    -> ?expr(MsgName)
                                       end),
        splice_trees('<MsgName>', MsgNameVars),
        splice_trees('TrUserData', if DoNif -> [];
                                      true  -> [?expr(TrUserData)]
                                   end)]),
     "\n",
     [[?f("%% epb compatibility\n"),
       ?f("-spec encode(_) -> binary().~n"),
       gpb_codegen:format_fn(
         encode,
         fun(Msg) -> encode_msg(Msg) end),
       [[?f("-spec ~p(_) -> binary().~n", [gpb_lib:mk_fn(encode_, MsgName)]),
         gpb_codegen:format_fn(
           gpb_lib:mk_fn(encode_, MsgName),
           fun(Msg) -> encode_msg(Msg) end)]
        || {{msg,MsgName}, _Fields} <- Defs]]
      || gpb_lib:get_epb_functions_by_opts(Opts)]].

format_aux_encoders(Defs, AnRes, _Opts) ->
    [format_enum_encoders(Defs, AnRes),
     format_type_encoders(AnRes),
     format_is_empty_string(AnRes)
    ].

format_enum_encoders(Defs, #anres{used_types=UsedTypes}) ->
    [gpb_codegen:format_fn(
       gpb_lib:mk_fn(e_enum_, EnumName),
       fun('<EnumSym>', Bin) -> <<Bin/binary, '<varint-bytes>'>>;
          (V, Bin) -> % integer (for yet unknown enums)
               e_varint(V, Bin)
       end,
       [repeat_clauses('<EnumSym>',
                       [begin
                            ViBytes = enum_to_binary_fields(EnumValue),
                            [replace_term('<EnumSym>', EnumSym),
                             splice_trees('<varint-bytes>', ViBytes)]
                        end
                        || {EnumSym, EnumValue} <- EnumDef])])
     || {{enum, EnumName}, EnumDef} <- Defs,
        gpb_lib:smember({enum,EnumName}, UsedTypes)].

format_map_encoders(Defs, AnRes, Opts0, IncludeStarter) ->
    Opts1 = case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts0)
            of
                '2tuples' -> [{msgs_as_maps, false} | Opts0];
                maps      -> [{msgs_as_maps, true} | Opts0]
            end,
    format_msg_encoders(Defs, AnRes, Opts1, IncludeStarter).

format_msg_encoders(Defs, AnRes, Opts, IncludeStarter) ->
    [[format_msg_encoder(MsgName, MsgDef, Defs,
                         AnRes, Opts,
                         if Type =:= group -> false;
                            true -> IncludeStarter
                         end)
      || {Type, MsgName, MsgDef} <- gpb_lib:msgs_or_groups(Defs)],
     format_special_field_encoders(Defs, AnRes)].

format_msg_encoder(MsgName, [], _Defs, _AnRes, _Opts, IncludeStarter) ->
    case IncludeStarter of
        true ->
            gpb_codegen:format_fn(
              gpb_lib:mk_fn(e_msg_, MsgName),
              fun(_Msg, _TrUserData) ->
                      <<>>
              end);
        false ->
            gpb_codegen:format_fn(
              gpb_lib:mk_fn(e_msg_, MsgName),
              fun(_Msg, Bin, _TrUserData) ->
                      Bin
              end)
    end;
format_msg_encoder(MsgName, MsgDef, Defs, AnRes, Opts, IncludeStarter) ->
    FNames = gpb_lib:get_field_names(MsgDef),
    FVars = [gpb_lib:var_f_n(I) || I <- lists:seq(1, length(FNames))],
    BVars = [gpb_lib:var_b_n(I) || I <- lists:seq(1, length(FNames)-1)] ++
        [last],
    MsgVar = ?expr(M),
    TrUserDataVar = ?expr(TrUserData),
    {EncodeExprs, _} =
        lists:mapfoldl(
          fun({NewBVar, Field, FVar}, PrevBVar) when NewBVar /= last ->
                  Tr = gpb_gen_translators:mk_find_tr_fn(MsgName, Field, AnRes),
                  EncExpr = field_encode_expr(MsgName, MsgVar, Field, FVar,
                                              PrevBVar, TrUserDataVar,
                                              Defs, Tr, AnRes, Opts),
                  E = ?expr('<NewB>' = '<encode-expr>',
                            [replace_tree('<NewB>', NewBVar),
                             replace_tree('<encode-expr>', EncExpr)]),
                  {E, NewBVar};
             ({last, Field, FVar}, PrevBVar) ->
                  Tr = gpb_gen_translators:mk_find_tr_fn(MsgName, Field, AnRes),
                  EncExpr = field_encode_expr(MsgName, MsgVar, Field, FVar,
                                              PrevBVar, TrUserDataVar,
                                              Defs, Tr, AnRes, Opts),
                  {EncExpr, dummy}
          end,
          ?expr(Bin),
          lists:zip3(BVars, MsgDef, FVars)),
    FnName = gpb_lib:mk_fn(e_msg_, MsgName),
    FieldMatching =
        case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
            X when X == records;
                   X == {maps, present_undefined} ->
                gpb_lib:mapping_match(MsgName, lists:zip(FNames, FVars), Opts);
            {maps, omitted} ->
                FMap = gpb_lib:zip_for_non_opt_fields(MsgDef, FVars),
                if length(FMap) == length(FNames) ->
                        gpb_lib:map_match(FMap);
                   length(FMap) < length(FNames) ->
                        ?expr('mapmatch' = 'M',
                              [replace_tree('mapmatch',
                                            gpb_lib:map_match(FMap)),
                               replace_tree('M', MsgVar)])
                end
        end,
    [[[gpb_codegen:format_fn(
         FnName,
         fun(Msg, TrUserData) ->
                 call_self(Msg, <<>>, TrUserData)
         end),
       "\n"] || IncludeStarter],
     gpb_codegen:format_fn(
       FnName,
       fun('<msg-matching>', Bin, TrUserData) ->
               '<encode-param-exprs>'
       end,
       [replace_tree('<msg-matching>', FieldMatching),
        splice_trees('<encode-param-exprs>', EncodeExprs)])].

field_encode_expr(MsgName, MsgVar, #?gpb_field{name=FName}=Field,
                  FVar, PrevBVar, TrUserDataVar, Defs, Tr, _AnRes, Opts)->
    FEncoder = mk_field_encode_fn_name(MsgName, Field),
    #?gpb_field{occurrence=Occurrence, type=Type, fnum=FNum, name=FName}=Field,
    TrFVar = gpb_lib:prefix_var("Tr", FVar),
    Transforms = [replace_term('fieldname', FName),
                  replace_tree('<F>', FVar),
                  replace_tree('TrF', TrFVar),
                  replace_term('Tr', Tr(encode)),
                  replace_tree('TrUserData', TrUserDataVar),
                  splice_trees('MaybeTrUserData',
                               gpb_gen_translators:maybe_userdata_param(
                                 Field, TrUserDataVar)),
                  replace_term('<enc>', FEncoder),
                  replace_tree('<Bin>', PrevBVar),
                  splice_trees('<Key>', key_to_binary_fields(FNum, Type))],
    case Occurrence of
        optional ->
            EncodeExpr =
                case gpb:is_msg_proto3(MsgName, Defs) of
                    false ->
                        ?expr(begin
                                  'TrF' = 'Tr'('<F>', 'TrUserData'),
                                  '<enc>'('TrF', <<'<Bin>'/binary, '<Key>'>>,
                                          'MaybeTrUserData')
                              end,
                              Transforms);
                    true when Type == string ->
                        ?expr(begin
                                  'TrF' = 'Tr'('<F>', 'TrUserData'),
                                  case is_empty_string('TrF') of
                                      true ->
                                          '<Bin>';
                                      false ->
                                          '<enc>'('TrF',
                                                  <<'<Bin>'/binary, '<Key>'>>,
                                                  'MaybeTrUserData')
                                  end
                              end,
                              Transforms);
                    true when Type == bytes ->
                        ?expr(begin
                                  'TrF' = 'Tr'('<F>', 'TrUserData'),
                                  case iolist_size('TrF') of
                                      0 ->
                                          '<Bin>';
                                      _ ->
                                          '<enc>'('TrF',
                                                  <<'<Bin>'/binary, '<Key>'>>,
                                                  'MaybeTrUserData')
                                  end
                              end,
                              Transforms);
                    true ->
                        TypeDefault = gpb:proto3_type_default(Type, Defs),
                        ?expr(
                           begin
                               'TrF' = 'Tr'('<F>', 'TrUserData'),
                               if 'TrF' =:= '<TypeDefault>' ->
                                       '<Bin>';
                                  true ->
                                       '<enc>'('TrF',
                                               <<'<Bin>'/binary, '<Key>'>>,
                                               'MaybeTrUserData')
                               end
                           end,
                           [replace_term('<TypeDefault>', TypeDefault)
                            | Transforms])
                end,
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(
                       if '<F>' == undefined ->
                               '<Bin>';
                          true ->
                               '<encodeit>'
                       end,
                       [replace_tree('<encodeit>', EncodeExpr) | Transforms]);
                {maps, omitted} ->
                    ?expr(
                       case 'M' of
                           '#{fieldname := <F>}' ->
                               '<encodeit>';
                           _ ->
                               '<Bin>'
                       end,
                       [replace_tree('M', MsgVar),
                        replace_tree('#{fieldname := <F>}',
                                     gpb_lib:map_match([{FName,FVar}])),
                        replace_tree('<encodeit>', EncodeExpr)
                        | Transforms])
            end;
        repeated ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                X when X == records;
                       X == {maps, present_undefined} ->
                    ?expr(
                       begin
                           'TrF' = 'Tr'('<F>', 'TrUserData'),
                           if 'TrF' == [] -> '<Bin>';
                              true -> '<enc>'('TrF', '<Bin>', 'TrUserData')
                           end
                       end,
                   Transforms);
                {maps, omitted} ->
                    ?expr(
                       case 'M' of
                           '#{fieldname := <F>}' ->
                               'TrF' = 'Tr'('<F>', 'TrUserData'),
                               if 'TrF' == [] -> '<Bin>';
                                  true -> '<enc>'('TrF', '<Bin>', 'TrUserData')
                               end;
                           _ ->
                               '<Bin>'
                       end,
                       [replace_tree('M', MsgVar),
                        replace_tree('#{fieldname := <F>}',
                                     gpb_lib:map_match([{FName,FVar}]))
                        | Transforms])
            end;
        required ->
            ?expr(
               begin
                   'TrF' = 'Tr'('<F>', 'TrUserData'),
                   '<enc>'('TrF', <<'<Bin>'/binary, '<Key>'>>,
                           'MaybeTrUserData')
               end,
               Transforms)
    end;
field_encode_expr(MsgName, MsgVar, #gpb_oneof{name=FName, fields=OFields},
                  FVar, PrevBVar, TrUserDataVar, Defs, Tr, AnRes, Opts) ->
    OFVar = gpb_lib:prefix_var("O", FVar),
    OneofClauseTransforms =
        [begin
             OFVal = ?expr({'<oneof-name>', '<OF>'},
                           [replace_term('<oneof-name>', Name),
                            replace_tree('<OF>', OFVar)]),
             MatchPattern =
                 case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                     X when X == records;
                            X == {maps, present_undefined} ->
                         OFVal;
                     {maps, omitted} ->
                         gpb_lib:map_match([{FName, OFVal}])
                 end,
             %% undefined is already handled, we have a match,
             %% the field occurs, as if it had been required
             OField2 = OField#?gpb_field{occurrence=required},
             Tr2 = Tr({update_elem_path,Name}),
             EncExpr = field_encode_expr(MsgName, MsgVar, OField2, OFVar,
                                         PrevBVar, TrUserDataVar,
                                         Defs, Tr2, AnRes, Opts),
             [replace_tree('<oneof...>', MatchPattern),
              replace_tree('<expr>', EncExpr)]
         end
         || #?gpb_field{name=Name}=OField <- OFields],
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            ?expr(case '<F>' of
                      undefined    -> '<Bin>';
                      '<oneof...>' -> '<expr>'
                  end,
                  [replace_tree('<F>', FVar),
                   replace_tree('<Bin>', PrevBVar),
                   repeat_clauses('<oneof...>', OneofClauseTransforms)]);
        {maps, omitted} ->
            ?expr(case 'M' of
                      '<oneof...>' -> '<expr>';
                      _ -> '<Bin>'
                  end,
                  [replace_term('fieldname', FName),
                   replace_tree('M', MsgVar),
                   replace_tree('<Bin>', PrevBVar),
                   repeat_clauses('<oneof...>', OneofClauseTransforms)])
    end.

mk_field_encode_fn_name(MsgName, #?gpb_field{occurrence=repeated, name=FName})->
    gpb_lib:mk_fn(e_field_, MsgName, FName);
mk_field_encode_fn_name(MsgName, #?gpb_field{type={msg,_Msg}, name=FName}) ->
    gpb_lib:mk_fn(e_mfield_, MsgName, FName);
mk_field_encode_fn_name(MsgName, #?gpb_field{type={group,_Nm}, name=FName}) ->
    gpb_lib:mk_fn(e_mfield_, MsgName, FName);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type={enum,EnumName}}) ->
    gpb_lib:mk_fn(e_enum_, EnumName);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=sint32}) ->
    gpb_lib:mk_fn(e_type_, sint);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=sint64}) ->
    gpb_lib:mk_fn(e_type_, sint);
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=uint32}) ->
    e_varint;
mk_field_encode_fn_name(_MsgName, #?gpb_field{type=uint64}) ->
    e_varint;
mk_field_encode_fn_name(MsgName,  #?gpb_field{type=Type}=F) ->
    case Type of
        {map,KeyType,ValueType} ->
            MapAsMsgMame = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
            F2 = F#?gpb_field{type = {msg,MapAsMsgMame}},
            mk_field_encode_fn_name(MsgName, F2);
        _ ->
            gpb_lib:mk_fn(e_type_, Type)
    end.

format_special_field_encoders(Defs, AnRes) ->
    lists:reverse( %% so generated auxiliary functions come in logical order
      gpb_lib:fold_msg_or_group_fields(
        fun(_Type, MsgName, #?gpb_field{occurrence=repeated}=FieldDef, Acc) ->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_Type, MsgName, #?gpb_field{type={msg,_}}=FieldDef, Acc)->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_Type, MsgName, #?gpb_field{type={group,_}}=FieldDef, Acc)->
                [format_field_encoder(MsgName, FieldDef, AnRes) | Acc];
           (_Type, _MsgName, #?gpb_field{}, Acc) ->
                Acc
        end,
        [],
        Defs)).

format_field_encoder(MsgName, FieldDef, AnRes) ->
    #?gpb_field{occurrence=Occurrence} = FieldDef,
    RFieldDef = FieldDef#?gpb_field{occurrence=required},
    [possibly_format_mfield_encoder(MsgName, RFieldDef, AnRes),
     case {Occurrence, gpb_lib:is_packed(FieldDef)} of
         {repeated, false} ->
             format_repeated_field_encoder2(MsgName, FieldDef, AnRes);
         {repeated, true} ->
             format_packed_field_encoder2(MsgName, FieldDef, AnRes);
         {optional, false} ->
             [];
         {required, false} ->
             []
     end].

possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={msg,SubMsg}}=FieldDef,
                               AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FieldDef),
    case is_msgsize_known_at_generationtime(SubMsg, AnRes) of
        no ->
            gpb_codegen:format_fn(
              FnName,
              fun(Msg, Bin, TrUserData) ->
                      SubBin = '<encode-msg>'(Msg, <<>>, TrUserData),
                      Bin2 = e_varint(byte_size(SubBin), Bin),
                      <<Bin2/binary, SubBin/binary>>
              end,
              [replace_term('<encode-msg>', gpb_lib:mk_fn(e_msg_, SubMsg))]);
        {yes, MsgSize} when MsgSize > 0 ->
            MsgSizeBytes = gpb_lib:varint_to_binary_fields(MsgSize),
            gpb_codegen:format_fn(
              FnName,
              fun(Msg, Bin, TrUserData) ->
                      Bin2 = <<Bin/binary, '<msg-size>'>>,
                      '<encode-msg>'(Msg, Bin2, TrUserData)
              end,
              [splice_trees('<msg-size>', MsgSizeBytes),
               replace_term('<encode-msg>', gpb_lib:mk_fn(e_msg_, SubMsg))]);
        {yes, 0} ->
            %% special case, there will not be any e_msg_<MsgName>/2 function
            %% generated, so don't call it.
            gpb_codegen:format_fn(
              FnName,
              fun(_Msg, Bin, _TrUserData) -> <<Bin/binary, 0>> end)
    end;
possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={map,KType,VType}}=FieldDef,
                               AnRes) ->
    MapAsMsgName = gpb_lib:map_type_to_msg_name(KType, VType),
    FieldDef2 = FieldDef#?gpb_field{type = {msg,MapAsMsgName}},
    possibly_format_mfield_encoder(MsgName, FieldDef2, AnRes);
possibly_format_mfield_encoder(MsgName,
                               #?gpb_field{type={group,GroupName},
                                           fnum=FNum}=FieldDef,
                               _AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FieldDef),
    EndTagBytes = key_to_binary_fields(FNum, group_end),
    gpb_codegen:format_fn(
      FnName,
      fun(Msg, Bin, TrUserData) ->
              GroupBin = '<encode-msg>'(Msg, <<>>, TrUserData),
              <<Bin/binary, GroupBin/binary, 'EndTagBytes'>>
      end,
      [replace_term('<encode-msg>', gpb_lib:mk_fn(e_msg_, GroupName)),
       splice_trees('EndTagBytes', EndTagBytes)]);
possibly_format_mfield_encoder(_MsgName, _FieldDef, _Defs) ->
    [].

is_msgsize_known_at_generationtime(MsgName, #anres{known_msg_size=MsgSizes}) ->
    case dict:fetch(MsgName, MsgSizes) of
        MsgSize when is_integer(MsgSize) ->
            {yes, MsgSize};
        undefined ->
            no
    end.

format_repeated_field_encoder2(MsgName, FDef, AnRes) ->
    #?gpb_field{fnum=FNum, type=Type, name=FName} = FDef,
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(
                      MsgName, FDef#?gpb_field{occurrence=required}),
    KeyBytes = key_to_binary_fields(FNum, Type),
    ElemPath = [MsgName,FName,[]],
    Transl = gpb_gen_translators:find_translation(ElemPath, encode, AnRes),
    gpb_codegen:format_fn(
      FnName,
      fun([Elem | Rest], Bin, TrUserData) ->
              Bin2 = <<Bin/binary, '<KeyBytes>'>>,
              Bin3 = '<encode-elem>'('Tr'(Elem, TrUserData), Bin2,
                                     'MaybeTrUserData'),
              call_self(Rest, Bin3, TrUserData);
         ([], Bin, _TrUserData) ->
              Bin
      end,
      [splice_trees('<KeyBytes>', KeyBytes),
       replace_term('<encode-elem>', ElemEncoderFn),
       replace_term('Tr', Transl),
       splice_trees('MaybeTrUserData',
                    gpb_gen_translators:maybe_userdata_param(
                      FDef, ?expr(TrUserData)))]).

format_packed_field_encoder2(MsgName, #?gpb_field{type=Type}=FDef, AnRes) ->
    case packed_byte_size_can_be_computed(Type) of
        {yes, BitLen, BitType} ->
            format_knownsize_packed_field_encoder2(MsgName, FDef,
                                                   BitLen, BitType);
        no ->
            format_unknownsize_packed_field_encoder2(MsgName, FDef, AnRes)
    end.

packed_byte_size_can_be_computed(fixed32)  -> {yes, 32, [little]};
packed_byte_size_can_be_computed(sfixed32) -> {yes, 32, [little,signed]};
packed_byte_size_can_be_computed(float)    -> {yes, 32, float};
packed_byte_size_can_be_computed(fixed64)  -> {yes, 64, [little]};
packed_byte_size_can_be_computed(sfixed64) -> {yes, 64, [little,signed]};
packed_byte_size_can_be_computed(double)   -> {yes, 64, double};
packed_byte_size_can_be_computed(_)        -> no.

format_knownsize_packed_field_encoder2(MsgName, #?gpb_field{name=FName,
                                                            fnum=FNum}=FDef,
                                       BitLen, BitType) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    KeyBytes = key_to_binary_fields(FNum, bytes),
    PackedFnName = gpb_lib:mk_fn(e_pfield_, MsgName, FName),
    [gpb_codegen:format_fn(
       FnName,
       fun(Elems, Bin, _TrUserData) when Elems =/= [] ->
               Bin2 = <<Bin/binary, '<KeyBytes>'>>,
               Bin3 = e_varint(length(Elems) * '<ElemLen>', Bin2),
               '<encode-packed>'(Elems, Bin3);
          ([], Bin, _TrUserData) ->
               Bin
       end,
       [splice_trees('<KeyBytes>', KeyBytes),
        replace_term('<ElemLen>', BitLen div 8),
        replace_term('<encode-packed>', PackedFnName)]),
     case BitType of
         float ->
             format_packed_float_encoder(PackedFnName);
         double ->
             format_packed_double_encoder(PackedFnName);
         _ ->
             gpb_codegen:format_fn(
               PackedFnName,
               fun([Value | Rest], Bin) ->
                       Bin2 = <<Bin/binary, Value:'<Size>'/'<BitType>'>>,
                       call_self(Rest, Bin2);
                  ([], Bin) ->
                       Bin
               end,
               [replace_term('<Size>', BitLen),
                splice_trees('<BitType>',
                             [erl_syntax:atom(T) || T <- BitType])])
     end].


format_unknownsize_packed_field_encoder2(MsgName,
                                         #?gpb_field{name=FName,
                                                     fnum=FNum}=FDef,
                                         AnRes) ->
    FnName = mk_field_encode_fn_name(MsgName, FDef),
    ElemEncoderFn = mk_field_encode_fn_name(
                      MsgName,
                      FDef#?gpb_field{occurrence=required}),
    KeyBytes = key_to_binary_fields(FNum, bytes),
    PackedFnName = gpb_lib:mk_fn(e_pfield_, MsgName, FName),
    ElemPath = [MsgName,FName,[]],
    Transl = gpb_gen_translators:find_translation(ElemPath, encode, AnRes),
    [gpb_codegen:format_fn(
       FnName,
       fun(Elems, Bin, TrUserData) when Elems =/= [] ->
               SubBin = '<encode-packed>'(Elems, <<>>, TrUserData),
               Bin2 = <<Bin/binary, '<KeyBytes>'>>,
               Bin3 = e_varint(byte_size(SubBin), Bin2),
               <<Bin3/binary, SubBin/binary>>;
          ([], Bin, _TrUserData) ->
               Bin
       end,
       [splice_trees('<KeyBytes>', KeyBytes),
        replace_term('<encode-packed>', PackedFnName)]),
     gpb_codegen:format_fn(
       PackedFnName,
       fun([Value | Rest], Bin, TrUserData) ->
               Bin2 = '<encode-elem>'('Tr'(Value, TrUserData), Bin,
                                      'MaybeTrUserData'),
               call_self(Rest, Bin2, TrUserData);
          ([], Bin, _TrUserData) ->
               Bin
       end,
       [replace_term('<encode-elem>', ElemEncoderFn),
        replace_term('Tr', Transl),
        splice_trees('MaybeTrUserData',
                     gpb_gen_translators:maybe_userdata_param(
                       FDef, ?expr(TrUserData)))])].

format_type_encoders(AnRes) ->
    [format_varlength_field_encoders(AnRes),
     format_fixlength_field_encoders(AnRes),
     [format_varint_encoder() || is_varint_encoder_needed(AnRes)]].

format_varlength_field_encoders(#anres{used_types=UsedTypes}) ->
    IsUsedSint32 = gpb_lib:smember(sint32, UsedTypes),
    IsUsedSint64 = gpb_lib:smember(sint64, UsedTypes),
    IsUsedInt32  = gpb_lib:smember(int32, UsedTypes),
    IsUsedInt64  = gpb_lib:smember(int64, UsedTypes),
    IsUsedBool   = gpb_lib:smember(bool, UsedTypes),
    IsUsedString = gpb_lib:smember(string, UsedTypes),
    IsUsedBytes  = gpb_lib:smember(bytes, UsedTypes),
    [[format_sint_encoder()         || IsUsedSint32 orelse IsUsedSint64],
     [format_int_encoder(int32, 32) || IsUsedInt32],
     [format_int_encoder(int64, 64) || IsUsedInt64],
     [format_bool_encoder()         || IsUsedBool],
     [format_string_encoder()       || IsUsedString],
     [format_bytes_encoder()        || IsUsedBytes]].

format_fixlength_field_encoders(AnRes) ->
    NeedsFixed32  = needs_f_enc(fixed32, AnRes),
    NeedsSFixed32 = needs_f_enc(sfixed32, AnRes),
    NeedsFloat    = needs_f_enc(float, AnRes),
    NeedsFixed64  = needs_f_enc(fixed64, AnRes),
    NeedsSFixed64 = needs_f_enc(sfixed64, AnRes),
    NeedsDouble   = needs_f_enc(double, AnRes),
    [[format_fixed_encoder(fixed32,  32, [little])        || NeedsFixed32],
     [format_fixed_encoder(sfixed32, 32, [little,signed]) || NeedsSFixed32],
     [format_fixed_encoder(fixed64,  64, [little])        || NeedsFixed64],
     [format_fixed_encoder(sfixed64, 64, [little,signed]) || NeedsSFixed64],
     [format_float_encoder(float) || NeedsFloat],
     [format_double_encoder(double) || NeedsDouble]].

needs_f_enc(FixedType, #anres{used_types=UsedTypes, fixlen_types=FTypes}) ->
    %% If a fixlength-type occurs _only_ as a packed repeated field,
    %% we need not generate a special encoder-function for it
    case [FT || FT <- sets:to_list(FTypes), FT#ft.type == FixedType] of
        [#ft{occurrence=repeated, is_packed=true}] ->
            false;
        _ ->
            gpb_lib:smember(FixedType, UsedTypes)
    end.

is_varint_encoder_needed(#anres{used_types=UsedTypes}=AnRes) ->
    TypesNeedingAVarintEncoder = [int32, int64, uint32, uint64, sint32, sint64,
                                  string, bytes],
    gpb_lib:smember_any(TypesNeedingAVarintEncoder, UsedTypes) orelse
        gpb_lib:any_enum_field_exists(UsedTypes) orelse
        gpb_lib:any_packed_field_exists(AnRes) orelse
        gpb_lib:at_least_one_submsg_with_size_not_known_at_compile_time_exists(
          AnRes).

format_sint_encoder() ->
    gpb_codegen:format_fn(
      e_type_sint,
      fun(Value, Bin) when Value >= 0 ->
              e_varint(Value * 2, Bin);
         (Value, Bin) ->
              e_varint(Value * -2 - 1, Bin)
      end).

format_int_encoder(Type, _BitLen) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(e_type_, Type),
      fun(Value, Bin) when 0 =< Value, Value =< 127 ->
              <<Bin/binary, Value>>; %% fast path
         (Value, Bin) ->
              %% Encode as a 64 bit value, for interop compatibility.
              %% Some implementations don't decode 32 bits properly,
              %% and Google's protobuf (C++) encodes as 64 bits
              <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
              e_varint(N, Bin)
      end,
      []).

format_bool_encoder() ->
    gpb_codegen:format_fn(
      e_type_bool,
      fun(true, Bin)  -> <<Bin/binary, 1>>;
         (false, Bin) -> <<Bin/binary, 0>>;
         (1, Bin) -> <<Bin/binary, 1>>;
         (0, Bin) -> <<Bin/binary, 0>>
      end).

format_fixed_encoder(Type, BitLen, BitType) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(e_type_, Type),
      fun(Value, Bin) ->
              <<Bin/binary, Value:'<Sz>'/'<T>'>>
      end,
      [replace_term('<Sz>', BitLen),
       splice_trees('<T>', [erl_syntax:atom(T) || T <- BitType])]).

format_packed_float_encoder(FnName) ->
    gpb_codegen:format_fn(
      FnName,
      fun([V | Rest], Bin) when is_number(V) ->
              call_self(Rest, <<Bin/binary, V:32/little-float>>);
         ([infinity | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:16,128,127>>);
         (['-infinity' | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:16,128,255>>);
         ([nan | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:16,192,127>>);
         ([], Bin) ->
              Bin
      end,
      []).

format_packed_double_encoder(FnName) ->
    gpb_codegen:format_fn(
      FnName,
      fun([V | Rest], Bin) when is_number(V) ->
              call_self(Rest, <<Bin/binary, V:64/float-little>>);
         ([infinity | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:48,240,127>>);
         (['-infinity' | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:48,240,255>>);
         ([nan | Rest], Bin) ->
              call_self(Rest, <<Bin/binary, 0:48,248,127>>);
         ([], Bin) ->
              Bin
      end,
      []).

format_float_encoder(Type) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(e_type_, Type),
      fun(V, Bin) when is_number(V) -> <<Bin/binary, V:32/little-float>>;
         (infinity, Bin)            -> <<Bin/binary, 0:16,128,127>>;
         ('-infinity', Bin)         -> <<Bin/binary, 0:16,128,255>>;
         (nan, Bin)                 -> <<Bin/binary, 0:16,192,127>>
      end,
      []).

format_double_encoder(Type) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(e_type_, Type),
      fun(V, Bin) when is_number(V) -> <<Bin/binary, V:64/little-float>>;
         (infinity, Bin)            -> <<Bin/binary, 0:48,240,127>>;
         ('-infinity', Bin)         -> <<Bin/binary, 0:48,240,255>>;
         (nan, Bin)                 -> <<Bin/binary, 0:48,248,127>>
      end,
      []).

format_string_encoder() ->
    gpb_codegen:format_fn(
      e_type_string,
      fun(S, Bin) ->
              Utf8 = unicode:characters_to_binary(S),
              Bin2 = e_varint(byte_size(Utf8), Bin),
              <<Bin2/binary, Utf8/binary>>
      end).

format_bytes_encoder() ->
    gpb_codegen:format_fn(
      e_type_bytes,
      fun(Bytes, Bin) when is_binary(Bytes) ->
              Bin2 = e_varint(byte_size(Bytes), Bin),
              <<Bin2/binary, Bytes/binary>>;
         (Bytes, Bin) when is_list(Bytes) ->
              BytesBin = iolist_to_binary(Bytes),
              Bin2 = e_varint(byte_size(BytesBin), Bin),
              <<Bin2/binary, BytesBin/binary>>
      end).

format_varint_encoder() ->
    gpb_codegen:format_fn(
      e_varint,
      fun(N, Bin) when N =< 127 ->
              <<Bin/binary, N>>;
         (N, Bin) ->
              Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
              call_self(N bsr 7, Bin2)
      end).

enum_to_binary_fields(Value) ->
    %% Encode as a 64 bit value, for interop compatibility.
    %% Some implementations don't decode 32 bits properly,
    %% and Google's protobuf (C++) encodes as 64 bits
    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
    gpb_lib:varint_to_binary_fields(N).

key_to_binary_fields(FNum, {group,_}) ->
    key_to_binary_fields(FNum, group_start);
key_to_binary_fields(FNum, Type) ->
    Key = (FNum bsl 3) bor gpb:encode_wiretype(Type),
    gpb_lib:varint_to_binary_fields(Key).

format_is_empty_string(#anres{has_p3_opt_strings=false}) ->
    "";
format_is_empty_string(#anres{has_p3_opt_strings=true}) ->
    [gpb_codegen:format_fn(
       is_empty_string,
       fun("") -> true;
          (<<>>) -> true;
          (L) when is_list(L) -> not string_has_chars(L);
          (B) when is_binary(B) -> false
       end),
     gpb_codegen:format_fn(
       string_has_chars,
       fun([C | _]) when is_integer(C) -> true; % common case
          ([H | T]) ->
               case string_has_chars(H) of
                   true  -> true;
                   false -> call_self(T)
               end;
          (B) when is_binary(B), byte_size(B) =/= 0 -> true;
          (C) when is_integer(C) -> true;
          (<<>>) -> false;
          ([]) -> false
       end)].
