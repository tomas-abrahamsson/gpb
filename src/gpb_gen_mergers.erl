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

%%% @doc Generation of msg-merging functions.
%%%
%%% Merging is an integral part of decoding: optional and required
%%% messages that occur multiple times on the wire are merged
%%% recursively. Scalar optional or required fields a merged by
%%% overwriting. Repeated fields are merged by appending.
%%%
%%% @private

-module(gpb_gen_mergers).

-export([format_msg_merge_code/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

format_msg_merge_code(Defs, AnRes, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_msg_merge_code_msgs(Defs, AnRes, Opts);
        false -> format_msg_merge_code_no_msgs(Opts)
    end.

format_msg_merge_code_no_msgs(Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            ["-spec merge_msgs(_, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New) ->
                       merge_msgs(Prev, New, [])
               end),
             "-spec merge_msgs(_, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _Opts) ->
                       erlang:error({gpb_error, no_messages})
               end)];
        maps ->
            ["-spec merge_msgs(_, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName) ->
                       merge_msgs(Prev, New, MsgName, [])
               end),
             "-spec merge_msgs(_, _, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _MsgName, _Opts) ->
                       erlang:error({gpb_error, no_messages})
               end)]
    end.

format_msg_merge_code_msgs(Defs, AnRes, Opts) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgDef} <- Defs],
    [format_merge_msgs_top_level(MsgNames, Opts),
     [case Type of
          msg ->
              format_msg_merger(Name, MsgDef, AnRes, Opts);
          group ->
              case is_repeated_group(Name, AnRes) of
                  true ->
                      %% merged with seq-add, not exported as top-level
                      %% ==> such groups are never merged recursively,
                      %%     thus never called
                      [];
                  false ->
                      format_msg_merger(Name, MsgDef, AnRes, Opts)
              end
      end
      || {Type, Name, MsgDef} <- gpb_lib:msgs_or_groups(Defs)]].

is_repeated_group(GroupName, #anres{group_occurrences=D}) ->
    dict:fetch(GroupName, D) == repeated.

format_merge_msgs_top_level(MsgNames, Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            [gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New) ->
                       merge_msgs(Prev, New, [])
               end),
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, Opts) when element(1, Prev) =:= element(1, New) ->
                       TrUserData = proplists:get_value(user_data, Opts),
                       case Prev of
                           '<msg-type>' -> '<merge-msg>'(Prev, New, TrUserData)
                       end
               end,
               [repeat_clauses(
                  '<msg-type>',
                  [[replace_tree('<msg-type>',
                                 gpb_lib:record_match(MsgName, [])),
                    replace_term('<merge-msg>',
                                 gpb_lib:mk_fn(merge_msg_, MsgName))]
                   || MsgName <- MsgNames])])];
        maps ->
            [gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName) ->
                       merge_msgs(Prev, New, MsgName, [])
               end),
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName, Opts) ->
                       TrUserData = proplists:get_value(user_data, Opts),
                       case MsgName of
                           '<msg-type>' -> '<merge-msg>'(Prev, New, TrUserData)
                       end
               end,
               [repeat_clauses(
                  '<msg-type>',
                  [[replace_tree('<msg-type>', erl_syntax:atom(MsgName)),
                    replace_term('<merge-msg>',
                                 gpb_lib:mk_fn(merge_msg_, MsgName))]
                   || MsgName <- MsgNames])])]
    end.

format_msg_merger(MsgName, [], _AnRes, _Opts) ->
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(merge_msg_, MsgName),
      fun(_Prev, New, _TrUserData) -> New end);
format_msg_merger(MsgName, MsgDef, AnRes, Opts) ->
    TrUserDataVar = ?expr(TrUserData),
    {PrevMatch, NewMatch, ExtraInfo} =
        format_msg_merger_fnclause_match(MsgName, MsgDef, Opts),
    {MandatoryMergings, OptMergings} = compute_msg_field_mergers(
                                         ExtraInfo, MsgName, AnRes),
    gpb_codegen:format_fn(
      gpb_lib:mk_fn(merge_msg_, MsgName),
      fun('Prev', 'New', 'MaybeTrUserData') ->
              '<merge-it>'
      end,
      [replace_tree('Prev',  PrevMatch),
       replace_tree('New',   NewMatch),
       splice_trees(
         '<merge-it>',
         gpb_lib:do_exprs(
           fun(Elem, Var) ->
                   render_omissible_merger(Elem, Var, TrUserDataVar)
           end,
           render_field_mergers(MsgName, MandatoryMergings,
                                TrUserDataVar, Opts),
                          OptMergings)),
       replace_tree('TrUserData', TrUserDataVar), % needed by some field mergers
       replace_tree('MaybeTrUserData',
                    case gpb_lib:any_field_is_sub_msg(MsgDef)
                        orelse gpb_lib:any_field_is_repeated(MsgDef)
                        orelse gpb_gen_translators:exists_tr_for_msg(MsgName,
                                                                     merge,
                                                                     AnRes) of
                        true  -> TrUserDataVar;
                        false -> ?expr(_)
                    end)]).

format_msg_merger_fnclause_match(_MsgName, [], _Opts) ->
    {?expr(PF), ?expr(_), no_fields};
format_msg_merger_fnclause_match(MsgName, MsgDef, Opts) ->
    FNames  = [gpb_lib:get_field_name(Field) || Field <- MsgDef],
    PFVars  = [case is_required_overwrite_merge(Field) of
                   true  -> none;
                   false -> gpb_lib:var("PF~s", [gpb_lib:get_field_name(Field)])
               end
               || Field <- MsgDef],
    NFVars  = [gpb_lib:var("NF~s", [FName]) || FName <- FNames],
    PFields = lists:zip(FNames, PFVars),
    NFields = lists:zip(FNames, NFVars),
    Infos = zip4(FNames, PFVars, NFVars, MsgDef),
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        X when X == records;
               X == {maps, present_undefined} ->
            PFields1 = [{FName,PFVar} || {FName,PFVar} <- PFields,
                                         PFVar /= none],
            P = gpb_lib:mapping_match(MsgName, PFields1, Opts),
            N = gpb_lib:mapping_match(MsgName, NFields, Opts),
            {P, N, {pr, Infos}};
        {maps, omitted} ->
            {OptInfos, MandInfos} = gpb_lib:key_partition_on_optionality(4,
                                                                         Infos),
            PMsg = ?expr(PMsg),
            NMsg = ?expr(NMsg),
            P = gpb_lib:map_match(
                  [{FName, PFVar} || {FName,PFVar,_,_} <- MandInfos,
                                     PFVar /= none]),
            N = gpb_lib:map_match(
                  [{FName, NFVar} || {FName,_,NFVar,_} <- MandInfos]),
            PB = gpb_lib:match_bind_var(P, PMsg),
            NB = gpb_lib:match_bind_var(N, NMsg),
            XInfo = {om, {MandInfos, OptInfos, PMsg, NMsg}},
            case {MandInfos, OptInfos} of
                {_, []} -> {P, N, XInfo};
                {[], _} -> {PMsg, NMsg, XInfo};
                {_,  _} -> {PB, NB, XInfo}
            end
    end.

is_required_overwrite_merge(#?gpb_field{occurrence=required}=Field) ->
    gpb_lib:classify_field_merge_action(Field) == overwrite;
is_required_overwrite_merge(_Field) ->
    false.

compute_msg_field_mergers({pr, XInfo}, MsgName, AnRes) ->
    Merges =
        [{FName, format_field_merge_expr(Field, PFVar, NFVar, MsgName, AnRes)}
         || {FName, PFVar, NFVar, Field} <- XInfo],
    {Merges, []};
compute_msg_field_mergers({om, {MandXInfo, OptXInfo, PMsg, NMsg}},
                          MsgName, AnRes) ->
    {MandMergs, []} = compute_msg_field_mergers({pr, MandXInfo},
                                                MsgName, AnRes),
    {OptMergs, []} = compute_msg_field_mergers({pr, OptXInfo},
                                               MsgName, AnRes),
    {MandMergs, reshape_cases_for_maps_find(OptMergs, PMsg, NMsg)}.

format_field_merge_expr(#?gpb_field{name=FName, occurrence=Occur}=Field,
                        PF, NF, MsgName, AnRes)->
    case gpb_lib:classify_field_merge_action(Field) of
        overwrite when Occur == required ->
            {required, {PF, NF}};
        overwrite ->
            {overwrite, {PF, NF}};
        seqadd ->
            ElemPath = [MsgName, FName],
            Append = gpb_gen_translators:find_translation(
                       ElemPath, merge, AnRes, 'erlang_++'),
            Tr = fun (_,_) -> Append end,
            {merge, {{PF, NF}, Tr, 'erlang_++'}};
        msgmerge ->
            Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(
                   MsgName, Field, false, AnRes),
            #?gpb_field{type={_msg_or_group,SubMsgName}}=Field,
            {merge, {{PF, NF}, Tr, gpb_lib:mk_fn(merge_msg_, SubMsgName)}}
    end;
format_field_merge_expr(#gpb_oneof{name=CFName, fields=OFields},
                        PF, NF, MsgName, AnRes) ->
    case [OField || #?gpb_field{type={msg,_}}=OField <- OFields] of
        [] ->
            {overwrite, {PF, NF}};
        MOFields ->
            {oneof,
             {{PF, NF},
              [begin
                   Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(
                          MsgName, F, {true, CFName}, AnRes),
                   {OFName, Tr, gpb_lib:mk_fn(merge_msg_, M2Name)}
               end
               || #?gpb_field{name=OFName, type={msg,M2Name}}=F <- MOFields]}}
    end.

reshape_cases_for_maps_find(Merges, PMsg, NMsg) ->
    [{FName, case Merge of
                 {overwrite, {_, _}} ->
                     {overwrite, {PMsg, NMsg}};
                 {merge, {{_, _}, Tr, MergeFn}} ->
                     {merge, {{PMsg, NMsg}, Tr, MergeFn}};
                 {oneof, {{_, _}, OFMerges}} ->
                     {oneof, {{PMsg, NMsg}, OFMerges}};
                 {expr, Expr} ->
                     {expr, Expr}
             end}
     || {FName, Merge} <- Merges].

render_field_mergers(MsgName, Mergings, TrUserDataVar, Opts) ->
    Fields = [{FName, render_field_merger(Merge, TrUserDataVar)}
              || {FName, Merge} <- Mergings],
    gpb_lib:mapping_create(MsgName, Fields, Opts).

render_field_merger({required, {none, NF}}, _TrUserDataVar) ->
    NF;
render_field_merger({overwrite, {PF, NF}}, _TrUserDataVar) ->
    ?expr(if 'NF' =:= undefined -> 'PF';
             true               -> 'NF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF)]);
render_field_merger({expr, Expr}, _TrUserDataVar) ->
    Expr;
render_field_merger({merge, {{PF, NF}, Tr, MergeFn}}, TrUserDataVar) ->
    ?expr(if 'PF' /= undefined, 'NF' /= undefined -> 'merge'('PF', 'NF',
                                                             'TrUserData');
             'PF' == undefined -> 'NF';
             'NF' == undefined -> 'PF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF),
           replace_term('merge', Tr(merge, MergeFn)),
           replace_tree('TrUserData', TrUserDataVar)]);
render_field_merger({oneof, {{PF, NF}, OFMerges}}, TrUserDataVar) ->
    Transforms = [replace_tree('PF', PF),
                  replace_tree('NF', NF),
                  replace_tree('OPF', gpb_lib:prefix_var("O", PF)),
                  replace_tree('ONF', gpb_lib:prefix_var("O", NF)),
                  replace_tree('TrUserData', TrUserDataVar)],
    ?expr(case {'PF', 'NF'} of
              '{{tag,OPF},{tag,ONF}}' -> {'tag', 'merge'('OPF','ONF',
                                                         'TrUserData')};
              {_, undefined}          -> 'PF';
              _                       -> 'NF'
          end,
          [repeat_clauses(
             '{{tag,OPF},{tag,ONF}}',
             [[replace_tree('{{tag,OPF},{tag,ONF}}',
                            ?expr({{'tag','OPF'},{'tag','ONF'}})),
               replace_term('tag', OFName),
               replace_term('merge', Tr(merge, OFMergeFn)) | Transforms]
              || {OFName, Tr, OFMergeFn} <- OFMerges])
           | Transforms]).

render_omissible_merger({FName, {overwrite, {PMsg, NMsg}}}, Var,
                        TrUserDataVar) ->
    ?expr(case {'PMsg', 'NMsg'} of
              {_, '#{fname := NF}'} -> 'Var#{fname=>NF}';
              {'#{fname := PF}', _} -> 'Var#{fname=>PF}';
              _                     -> 'Var'
          end,
          std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar));
render_omissible_merger({FName, {merge, {{PMsg, NMsg}, Tr, MergeFn}}}, Var,
                        TrUserDataVar) ->
    Trs = std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar),
    MergeCallTmpl = ?expr('merge'('PF','NF', 'TrUserData'), Trs),
    ?expr(case {'PMsg', 'NMsg'} of
              {'#{fname := PF}', '#{fname := NF}'} ->
                  'Var#{fname=>merge(PF,NF)}';
              {_, '#{fname := NF}'} ->
                  'Var#{fname=>NF}';
              {'#{fname := PF}', _} ->
                  'Var#{fname=>PF}';
              {_, _} ->
                  'Var'
          end,
          [replace_tree('Var#{fname=>merge(PF,NF)}',
                        gpb_lib:map_set(Var, [{FName,MergeCallTmpl}]))]
          ++ Trs
          ++ [replace_term('merge', Tr(merge, MergeFn))]);
render_omissible_merger({FName, {oneof, {{PMsg, NMsg}, OFMerges}}}, Var,
                       TrUserDataVar) ->
    OPF = gpb_lib:var("OPF~s", [FName]),
    ONF = gpb_lib:var("ONF~s", [FName]),
    OneofTransforms = [replace_tree('OPF', OPF),
                       replace_tree('ONF', ONF)],
    ?expr(case {'PMsg', 'NMsg'} of
              '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}' ->
                  'Var#{fname=>{tag,merge(OPF,ONF)}}';
              {_, '#{fname := NF}'} ->
                  'Var#{fname=>NF}';
              {'#{fname := PF}', _} ->
                  'Var#{fname=>PF}';
              {_, _} ->
                  'Var'
          end,
          [repeat_clauses(
             '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}',
             [begin
                  Trs2 = [replace_term('tag', OFName),
                          replace_term('merge', Tr(merge, OFMergeFn))]
                      ++ OneofTransforms,
                  MmO = gpb_lib:map_match(
                          [{FName, ?expr({'tag', 'OPF'}, Trs2)}]),
                  MmN = gpb_lib:map_match(
                          [{FName, ?expr({'tag', 'ONF'}, Trs2)}]),
                  MergeCall = ?expr({'tag','merge'('OPF','ONF', 'TrUserData')},
                                    Trs2),
                  [replace_tree(
                     '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}',
                     ?expr({'#{fname := {tag,OPF}}', '#{fname := {tag,ONF}}'},
                           [replace_tree('#{fname := {tag,OPF}}', MmO),
                            replace_tree('#{fname := {tag,ONF}}', MmN)])),
                   replace_tree('Var#{fname=>{tag,merge(OPF,ONF)}}',
                                gpb_lib:map_set(Var, [{FName,MergeCall}]))
                   | Trs2]
              end
              || {OFName, Tr, OFMergeFn} <- OFMerges])
           | std_omitable_merge_transforms(PMsg, NMsg, FName, Var,
                                           TrUserDataVar)]).

std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar) ->
    PF = gpb_lib:var("PF~s", [FName]),
    NF = gpb_lib:var("NF~s", [FName]),
    [replace_term('fname', FName),
     replace_tree('PMsg', PMsg),
     replace_tree('NMsg', NMsg),
     replace_tree('PF', PF),
     replace_tree('NF', NF),
     replace_tree('Var', Var),
     replace_tree('Var#{fname=>NF}', gpb_lib:map_set(Var, [{FName, NF}])),
     replace_tree('Var#{fname=>PF}', gpb_lib:map_set(Var, [{FName, PF}])),
     replace_tree('#{fname := NF}', gpb_lib:map_match([{FName, NF}])),
     replace_tree('#{fname := PF}', gpb_lib:map_match([{FName, PF}])),
     replace_tree('TrUserData', TrUserDataVar)].

zip4([A|T1], [B|T2], [C|T3], [D|T4]) -> [{A,B,C,D} | zip4(T1, T2, T3, T4)];
zip4([], [], [], [])                 -> [].
