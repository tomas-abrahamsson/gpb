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

-module(gpb_lib_tests).

-ifndef(NO_HAVE_PROPERTY_TESTER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

file_msg_format_opts_test() ->
    [?assertEqual(Expected,
                  gpb_lib:get_mapping_by_opts(
                    gpb_lib:normalize_opts(Opts)),
                  #{opts => Opts,
                    norm => gpb_lib:normalize_opts(Opts)})
     || {Expected, Opts} <- [{records, []},
                             {records, [{maps, false}]},
                             {maps, [maps]},
                             {maps, [{maps, true}]},
                             %% Overrides (first hit of 'maps'wins):
                             {records, [{maps, false}, {maps, true}]},
                             %% Already on normalized form, (also w/ overrides)
                             {records, [{msp_format, records}]},
                             {records, [{msg_format, records}, maps]},
                             {maps,    [{msg_format, maps}]},
                             {maps,    [{msg_format, maps}, {maps, false}]},
                             {natrecs, [{msg_format, native_records}]},
                             end_marker]],
    ok.

mapfields_opts_test() ->
    [?assertEqual(Expected,
                  gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(
                    gpb_lib:normalize_opts(Opts)),
                  #{opts => Opts,
                    norm => gpb_lib:normalize_opts(Opts)})
     || {Expected, Opts} <- [{'2tuples', []},
                             {maps,      [maps]},
                             {'2tuples', [{maps, false}, maps]},
                             {'2tuples', [{maps, false}, maps]},
                             %% mapfields may overrides:a
                             {'2tuples', [{mapfields_as_maps, false}, maps]},
                             {maps,      [{mapfields_as_maps, true}]},
                             {'2tuples', [{mapfields_as_maps, false}]},
                             {'2tuples', [{mapfields_as_maps, false}, maps]},
                             %% Already on normalized form, (also w/ overrides)
                             {'2tuples', [{mapfield_format, '2tuples'}]},
                             {'2tuples', [{mapfield_format, '2tuples'}, maps]},
                             {maps,      [{mapfield_format, maps}]},
                             {maps,      [{mapfield_format, maps},
                                          {maps, false}]},
                             end_marker]],
    ok.

defs_format_opts_test() ->
    %% Defs
    [?assertEqual(Expected,
                  gpb_lib:get_defs_format(
                    gpb_lib:normalize_opts(Opts)),
                  #{opts => Opts,
                    norm => gpb_lib:normalize_opts(Opts)})
     || {Expected, Opts} <- [{records, []},
                             {maps, [{maps, true}]},
                             {maps, [maps]},
                             {maps, [defs_as_maps]},
                             {maps, [{defs_as_maps, true}]},
                             {records, [{maps, false}, {maps, true}]},
                             %% Already on normalized form, (also w/ overrides)
                             {records, [{defs_format, records}]},
                             {records, [{defs_format, records}, maps]},
                             {maps,    [{defs_format, maps}]},
                             {maps,    [{defs_format, maps}, {maps, false}]},
                             end_marker]],
    %% Fields
    [?assertEqual(Expected,
                  gpb_lib:get_field_format_by_opts(
                    gpb_lib:normalize_opts(Opts)),
                  #{opts => Opts,
                    norm => gpb_lib:normalize_opts(Opts)})
     || {Expected, Opts}
            <- [{fields_as_records, []},
                {fields_as_records, [{maps, false}]},
                {fields_as_records, [{maps, false}, maps]},
                {fields_as_proplists, [defs_as_proplists]},
                {fields_as_proplists, [defs_as_proplists, maps]},
                %%
                %% This was oddly enough true previously, but no longer:
                %% {fields_as_proplists, [maps, defs_as_proplists]},
                %%
                {fields_as_maps, [maps]},
                {fields_as_maps, [{maps, true}]},
                {fields_as_maps, [{maps, true}, {maps, false}]},
                {fields_as_maps, [defs_as_maps]},
                {fields_as_maps, [{defs_as_maps, true}]},
                %% Already on normalized form, (also w/ overrides)
                {fields_as_records,   [{defs_format, records}]},
                {fields_as_records,   [{defs_format, records}, maps]},
                {fields_as_maps,      [{defs_format, maps}]},
                {fields_as_proplists, [{defs_format, proplists},
                                       {maps, false}]},
                {fields_as_proplists, [{defs_format, proplists}]},
                {fields_as_proplists, [{defs_format, proplists}, maps]},
                {fields_as_maps,      [{msg_format, native_records}]},
                {fields_as_maps,      [{msg_format, maps}]},
                {fields_as_records,   [{msg_format, records}]},
                end_marker]],
    ok.

term_mapping_test() ->
    KnownRecords = #{a => [f], b => []},
    S1 = erl_prettypr:format(
          gpb_lib:term_mapping([{x, {a, {b}}, {}}], KnownRecords, [])),
    assert_substring("#a{f", S1),
    assert_substring("#b{}", S1),
    %%
    ?assertEqual(
       [{x, #{f => #{}}, {}}],
       eval_formatted(
         erl_prettypr:format(
           gpb_lib:term_mapping([{x, {a, {b}}, {}}], KnownRecords,
                                [{defs_format, maps}])))),
    %%
    ?assertEqual(
       [{x, [{f, []}], {}}],
       eval_formatted(
         erl_prettypr:format(
           gpb_lib:term_mapping([{x, {a, {b}}, {}}], KnownRecords,
                                [{defs_format, proplists}])))),
    ok.

assert_substring(ExpectedSubstr, Str) ->
    case string:find(Str, ExpectedSubstr) of
        nomatch -> error({substr_not_present, #{expected => ExpectedSubstr,
                                                string => Str}});
        _ -> ok
    end.

eval_formatted(Str) ->
    {ok, Tokens, _End} = erl_scan:string(Str ++ ".", 1),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

snake_case_test() ->
    [?assertEqual(Expected, gpb_lib:snake_case(Input), {input_is, Input})
     || {Expected, Input} <- snake_casings()],
    ok.

snake_case_is_idempotent_1_test() ->
    [?assertEqual(Expected, gpb_lib:snake_case(gpb_lib:snake_case(Input)),
                  {input_is, Input})
     || {Expected, Input} <- snake_casings()],
    ok.

snake_casings() ->
    [{"winter_is_a_time_of_year", "WinterIsATimeOfYear"},
     {"winter_is_a_time_of_year", "winterIsATimeOfYear"},
     {"a_later_time", "ALaterTime"},
     {"a_later_time", "aLaterTime"},
     {"dotted.name_part", "Dotted.NamePart"},
     {"already_snake_case", "already_snake_case"},
     {"this_is_273_k", "ThisIs273K"},
     %% Some more:
     {"aa.aa", "Aa.Aa"},
     {"a.a_aa", "A.AAa"},
     {"a.aa_a", "A.AaA"},
     {"a.a_0_a", "A.A0A"},
     %% When the name already contains an underscore:
     {"abc_def", "Abc_Def"},
     {"x_097_def", "x_097_Def"},
     %% Some other cases
     {"abc", "Abc"},
     {"a_0_a", "A0A"},
     {"a_0", "A0"},
     {"_a_0", "_A0"},
     {"_a_0", "_a0"},
     {"a.a_0", "A.A0"},
     {"a._0", "A._0"},
     {"a.x_097_def", "A.x_097_Def"},
     'end-marker'
    ].

snake_case_is_idempotent_2_test() ->
    CamelCases =
        ["A.Bb",
         "BBcccA",
         "BaCAACab.ab.BA3acCA.CCCA88.ab8b8b.BCB.aC4aA.BABcb6A",
         "b1a99B.BbB8C",
         "C",
         "BCBcaABB4.baaABB.B3B2bc963a.b.c2b.A90.Ac8.BBCBbb.b2.aabb",
         "B6c85.b6.c066",
         "AC",
         "c79A0aAC.a8A5A.c8cbCB9b.AbbbbB",
         "B8c46aaAA.B25Aa.C.CBBAc06C",
         "bACA0c.CABA9cbcc4.baA.bBaAbaBb.a4ca.b2cCa.cb9.aAB35c",
         "accbCab8b"],
    [?assert(is_idempotent(X)) || X <- CamelCases],
    ok.

is_idempotent(S) -> % applying twice should result in same as once
    S2 = gpb_lib:snake_case(S),
    S3 = gpb_lib:snake_case(S2),
    S2 =:= S3.

old_snake_name_test() ->
    "winter_is_a_time_of_year" = gpb_lib:old_snake_case("WinterIsATimeOfYear"),
    "winter_is_a_time_of_year" = gpb_lib:old_snake_case("winterIsATimeOfYear"),
    "a_later_time" = gpb_lib:old_snake_case("ALaterTime"),
    "a_later_time" = gpb_lib:old_snake_case("aLaterTime"),
    "dotted.name_part" = gpb_lib:snake_case("Dotted.NamePart"),
    "already_snake_case" = gpb_lib:old_snake_case("already_snake_case"),
    "this_is_273_k" = gpb_lib:old_snake_case("ThisIs273K"),
    %% A couple of oddities by the old snake_case, verify preserved behaviour:
    "abc__def" = gpb_lib:old_snake_case("Abc_Def"),
    "x__097_def" = gpb_lib:old_snake_case("x_097_def"),
    ok.

basenameify_ish_test() ->
    ["b/c/f.proto", "d/c/f.proto", "z/f.proto", "g.proto"] =
        gpb_lib:basenameify_ish(["/home/u/a/b/c/f.proto",
                                 "/home/u/a/d/c/f.proto",
                                 "/home/u/x/y/z/f.proto",
                                 "/home/u/x/y/z/g.proto"]),
    ["f.proto", "g.proto", "h.proto", "i.proto"] =
        gpb_lib:basenameify_ish(["/home/u/a/b/c/f.proto",
                                 "/home/u/a/b/c/g.proto",
                                 "/home/u/a/b/c/h.proto",
                                 "/home/u/a/b/c/i.proto"]),
    ["c/f1.proto","f2.proto","z/f1.proto","f4.proto"] =
        gpb_lib:basenameify_ish(["a/b/c/f1.proto",
                                 "a/b/f2.proto",
                                 "x/y/z/f1.proto",
                                 "f4.proto"]),
    ?assertError({gpb_error, {multiply_defined_file_or_files, _}},
                 gpb_lib:basenameify_ish(["x", "x"])).

-ifndef(NO_HAVE_PROPERTY_TESTER).
idempotency_test_() ->
    {timeout, 10,
     fun() ->
             ?assert(proper:quickcheck(prop_is_idempotent()))
     end}.

prop_is_idempotent() ->
    ?FORALL(CamelCaseStr, camel_case_string(true),
            ?WHENFAIL(io:format("For ~p:~n"
                                "->  ~p~n"
                                "->> ~p~n",
                                [CamelCaseStr,
                                 gpb_lib:snake_case(CamelCaseStr),
                                 gpb_lib:snake_case(
                                   gpb_lib:snake_case(CamelCaseStr))]),
                      is_idempotent(CamelCaseStr))).

backwards_compat_once_test_() ->
    {timeout, 10,
     fun() ->
             ?assert(proper:quickcheck(prop_backwards_compat_once()))
     end}.

prop_backwards_compat_once() ->
    ?FORALL(CamelCaseStr, camel_case_string(false),
            ?WHENFAIL(io:format("For ~p:~n"
                                "  old: ~p~n"
                                "  new: ~p~n",
                                [CamelCaseStr,
                                 old_snake_case(CamelCaseStr),
                                 gpb_lib:snake_case(CamelCaseStr)]),
                      begin
                          Old = old_snake_case(CamelCaseStr),
                          New = gpb_lib:snake_case(CamelCaseStr),
                          Old =:= New
                      end)).

old_snake_case(Str) -> % the old implementation, not idempotent
    string:lowercase(
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

%% generators:
camel_case_string(U) ->  % U: whether or not to allow underscore as char
    ?LET({Segment1, RestSegments}, {camel_case_segment(U),
                                    list(camel_case_segment(U))},
         lists:flatten(lists:join(".", [Segment1 | RestSegments]))).

camel_case_segment(U) ->
    ?LET({C1, Rest}, {segment_first_char(U), list(ident_char(U))},
         [C1 | Rest]).

segment_first_char(U) -> % U: whether or not to allow underscore as char
    oneof(
      lists:append(
        [[integer($A, $Z)],
         [integer($a, $z)],
         [$_ || U]])).

ident_char(U) -> % U: whether or not to allow underscore as char
    oneof(
      lists:append(
        [[integer($A, $Z)],
         [integer($a, $z)],
         [integer($0, $9)],
         [$_ || U]])).
-endif. % -ifndef(NO_HAVE_PROPERTY_TESTER).
