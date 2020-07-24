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

%% Test gpb_scan.erl

-module(gpb_scan_tests).

-include_lib("eunit/include/eunit.hrl").

string_test() ->
    [{str_lit, "x"}] = ok_scan(<<"'x'">>),
    [{str_lit, "'x'"}] = ok_scan(<<"\"'x'\"">>),
    [{str_lit, "\"x\""}] = ok_scan(<<"'\"x\"'">>),
    %% \x: max 2 hex chars
    [{str_lit, [2]}]     = ok_scan(<<"'\\x2'">>),
    [{str_lit, " "}]     = ok_scan(<<"'\\x20'">>),
    [{str_lit, " 1"}]    = ok_scan(<<"'\\x201'">>),
    [{str_lit, [2, $z]}] = ok_scan(<<"'\\x2z'">>),
    %% \u: max 4 hex chars
    [{str_lit, [16#2]}]        = ok_scan(<<"'\\u2'">>),
    [{str_lit, [16#20]}]       = ok_scan(<<"'\\u20'">>),
    [{str_lit, [16#200]}]      = ok_scan(<<"'\\u200'">>),
    [{str_lit, [16#2000]}]     = ok_scan(<<"'\\u2000'">>),
    [{str_lit, [16#2000, $1]}] = ok_scan(<<"'\\u20001'">>),
    [{str_lit, [16#2, $z]}]    = ok_scan(<<"'\\u2z'">>),
    %% \U: max 8 hex chars
    [{str_lit, [16#2]}]            = ok_scan(<<"'\\U2'">>),
    [{str_lit, [16#20]}]           = ok_scan(<<"'\\U20'">>),
    [{str_lit, [16#200]}]          = ok_scan(<<"'\\U200'">>),
    [{str_lit, [16#2000]}]         = ok_scan(<<"'\\U2000'">>),
    [{str_lit, [16#20000]}]        = ok_scan(<<"'\\U20000'">>),
    [{str_lit, [16#000200]}]       = ok_scan(<<"'\\U000200'">>),
    [{str_lit, [16#0002000]}]      = ok_scan(<<"'\\U0002000'">>),
    [{str_lit, [16#00020000]}]     = ok_scan(<<"'\\U00020000'">>),
    [{str_lit, [16#00020000, $1]}] = ok_scan(<<"'\\U000200001'">>),
    [{str_lit, [16#10ffff]}]       = ok_scan(<<"'\\U10ffff'">>),
    [{str_lit, [16#2, $z]}]        = ok_scan(<<"'\\U2z'">>),
    ok.

dot_test() ->
    ['.'] = ok_scan(<<".">>),
    ok.

int_test() ->
    [{int_lit, {dec, 0}}] = ok_scan(<<"0">>).

float_test() ->
    [     {float_lit, 0.0}] = ok_scan(<<"0.">>),
    [     {float_lit, 125.0}] = ok_scan(<<"125.">>),
    [     {float_lit, 0.0}] = ok_scan(<<"0.0">>),
    [     {float_lit, 0.0}] = ok_scan(<<".0">>),
    [     {float_lit, 0.0}] = ok_scan(<<".0e1">>),
    ['+', {float_lit, 0.0}] = ok_scan(<<"+0.0">>),
    ['+', {float_lit, 0.0}] = ok_scan(<<"+.0">>),
    ['+', {float_lit, 0.0}] = ok_scan(<<"+.0e1">>),
    ['-', {float_lit, 0.0}] = ok_scan(<<"-0.0">>),
    ['-', {float_lit, 0.0}] = ok_scan(<<"-.0">>),
    ['-', {float_lit, 0.0}] = ok_scan(<<"-.0e1">>),
    [     {float_lit, 0.0}] = ok_scan(<<"0e1">>),
    ['+', {float_lit, 0.0}] = ok_scan(<<"+0e1">>),
    ['-', {float_lit, 0.0}] = ok_scan(<<"-0e1">>),
    [     {float_lit, 0.125}] = ok_scan(<<"0.125">>),
    ['-', {float_lit, 0.125}] = ok_scan(<<"-0.125">>),
    [     {float_lit, 0.125e3}] = ok_scan(<<"0.125e3">>),
    [     {float_lit, 0.125e3}] = ok_scan(<<"0.125e+3">>),
    [     {float_lit, 0.000125}] = ok_scan(<<"0.125e-3">>),
    [     {float_lit, 1.125}] = ok_scan(<<"1.125">>),
    [     {float_lit, 125.0e3}] = ok_scan(<<"125e3">>),
    [     {float_lit, 125.0e3}] = ok_scan(<<"125.e3">>),
    ok.

hex_test() ->
    [     {int_lit, {hex, 0}}]     = ok_scan(<<"0x0">>),
    ['+', {int_lit, {hex, 0}}]     = ok_scan(<<"+0x0">>),
    ['-', {int_lit, {hex, 0}}]     = ok_scan(<<"-0x0">>),
    [     {int_lit, {hex, 65535}}] = ok_scan(<<"0xffFF">>),
    [     {int_lit, {hex, 0}}]     = ok_scan(<<"0X0">>),
    ['+', {int_lit, {hex, 0}}]     = ok_scan(<<"+0X0">>),
    ['-', {int_lit, {hex, 0}}]     = ok_scan(<<"-0X0">>),
    [     {int_lit, {hex, 65535}}] = ok_scan(<<"0XffFF">>),
    ok.

oct_test() ->
    [     {int_lit, {oct, 10}}] = ok_scan(<<"012">>),
    ['+', {int_lit, {oct, 10}}] = ok_scan(<<"+012">>),
    ['-', {int_lit, {oct, 10}}] = ok_scan(<<"-012">>),
    ok.

parses_integer_test() -> %% tests inspired by tests in tokenizer_unittest.cc
    [     {int_lit, {dec, 123}}]        = ok_scan(<<"123">>),
    [     {int_lit, {hex, 2742}}]       = ok_scan(<<"0xab6">>),
    [     {int_lit, {hex, 2742}}]       = ok_scan(<<"0XAB6">>),
    [     {int_lit, {hex, 19088743}}]   = ok_scan(<<"0X1234567">>),
    [     {int_lit, {hex, 2309737967}}] = ok_scan(<<"0x89abcdef">>),
    [     {int_lit, {hex, 2309737967}}] = ok_scan(<<"0x89ABCDEF">>),
    [     {int_lit, {oct, 342391}}]     = ok_scan(<<"01234567">>),
    ['-', {int_lit, {dec, 123}}]        = ok_scan(<<"-123">>),
    ['-', {int_lit, {hex, 2742}}]       = ok_scan(<<"-0xab6">>),
    ['-', {int_lit, {hex, 2742}}]       = ok_scan(<<"-0XAB6">>),
    ['-', {int_lit, {hex, 19088743}}]   = ok_scan(<<"-0X1234567">>),
    ['-', {int_lit, {hex, 2309737967}}] = ok_scan(<<"-0x89abcdef">>),
    ['-', {int_lit, {hex, 2309737967}}] = ok_scan(<<"-0x89ABCDEF">>),
    ['-', {int_lit, {oct, 342391}}]     = ok_scan(<<"-01234567">>),
    ok.

parses_floats_test() -> %% tests inspired by tests in tokenizer_unittest.cc
    [{float_lit, 123.45}] = ok_scan(<<"123.45">>),
    [{float_lit, 1.0}]    = ok_scan(<<"1.">>),
    [{float_lit, 1.0e3}]  = ok_scan(<<"1e3">>),
    [{float_lit, 1.0e3}]  = ok_scan(<<"1E3">>),
    [{float_lit, 1.0e-3}] = ok_scan(<<"1e-3">>),
    [{float_lit, 1.0e3}]  = ok_scan(<<"1e+3">>),
    [{float_lit, 1.0e3}]  = ok_scan(<<"1.e3">>),
    [{float_lit, 1.2e3}]  = ok_scan(<<"1.2e3">>),
    [{float_lit, 0.1}]    = ok_scan(<<".1">>),
    [{float_lit, 0.1e3}]  = ok_scan(<<".1e3">>),
    [{float_lit, 0.1e-3}] = ok_scan(<<".1e-3">>),
    [{float_lit, 0.1e3}]  = ok_scan(<<".1e+3">>),
    ok.

skips_comments_test() ->
    [{int_lit, {dec,_}}] = ok_scan(<<"//abv\n12">>),
    %% no \n on last line:
    [{int_lit, {dec,_}}] = ok_scan(<<"12//def">>),
    ok.

skips_c_style_comments_test() ->
    %% not a comment:
    {error, _, _}            = gpb_scan:binary(<<"/*/12">>),
    %% smallest comment:
    [{int_lit, _}] = ok_scan(<<"/**/12">>),
    [{int_lit, _}] = ok_scan(<<"/* - */12">>),
    [{int_lit, _}] = ok_scan(<<"/*****/12">>),
    %% greedy test:
    [{int_lit, _}] = ok_scan(<<"/*****/12/****/">>),
    %% \n in comment:
    [{int_lit, _}] = ok_scan(<<"/**\n *\n*/12">>),
    %% comment in str:
    S = "x/* xyz */y",
    [{str_lit,S}] = ok_scan(list_to_binary("\""++S++"\"")),
    ok.

parses_strings_test() ->
    [{str_lit,""}]     = ok_scan(<<"\"\"">>),
    [{str_lit,""}]     = ok_scan(<<"''">>),
    [{str_lit,"abc"}]  = ok_scan(<<"\"abc\"">>),
    [{str_lit,"abc"}]  = ok_scan(<<"'abc'">>), %% single quotes
    [{str_lit,"a\"c"}] = ok_scan(<<"\"a\\\"c\"">>),
    [{str_lit,"abz"}]  = ok_scan(<<"\"a\\142z\"">>), % 0142=b
    [{str_lit,"abz"}]  = ok_scan(<<"\"a\\x62z\"">>), % 0x62=b
    [{str_lit,"a"}, {str_lit,"b"}] = ok_scan(<<"'a' 'b'">>),
    ok.

max_3_escaped_octdigits_test() ->
    [{str_lit,"ab3"}]  = ok_scan(<<"\"a\\1423\"">>).

max_2_escaped_hexdigits_test() ->
    [{str_lit,"ab3"}]  = ok_scan(<<"\"a\\x623\"">>).

parses_unicode_escapes_test() ->
    %% \u: exactly 4 following
    [{str_lit, [$a,16#1234,$b]}] =
        ok_scan(<<"\"a\\u1234b\"">>),
    %% \U: exactly 8 following hex digits
    [{str_lit, [$a,16#101234,$b]}] =
        ok_scan(<<"\"a\\U00101234b\"">>),
    ok.

ignores_whitespace_test() ->
    [{int_lit,_},{int_lit,_},{int_lit,_}] =
        ok_scan(<<"\n  12  13 \t 15">>),
    ok.

saves_orig_test() ->
    Strings = [{<<"'abcdef'">>,              single_quoted},
               {<<"\"abc\"">>,               double_quoted},
               {<<"'a\\u1234bc'">>,          up_to_4_hex_chars},
               {<<"'a\\U101234 zz'">>,       up_to_8_hex_chars},
               {<<"'a\\x10 zz'">>,           up_to_2_hex_chars},
               {<<"'a\\0377 zz'">>,          octal},
               {<<"'a\\n\\r\\v\\t\\b zz'">>, backslash_sequences}],
    [{{ok, [{{str_lit,_}, _, Orig}], _}, _} = {scan(Orig), What}
     || {Orig, What} <- Strings],

    Words = [{<<"message">>, message},
             {<<"max">>,     max},
             {<<"inf">>,     inf},
             {<<"m12_99">>,  m12_99}],
    [{{ok, [{Word, _, Word}], _}, _} = {scan(Word), What}
      || {Word, What} <- Words],

    IntNumbers = [{<<"0">>,          int_zero},
                  {<<"0x1234">>,     hex},
                  {<<"0x1234ffff">>, hex_2},
                  {<<"0X1234">>,     hex_3},
                  {<<"0377">>,       oct_255},
                  {<<"1234">>,       dec}],
    [{{ok, [{{int_lit,{_,_}}, _, Orig}], _}, _} = {scan(Orig), What}
     || {Orig, What} <- IntNumbers],

    FloatNumbers = [{<<".125">>,       leading_point},
                    {<<".125e3">>,     leading_point_exp},
                    {<<".125e-3">>,    leading_point_signed_exp},
                    {<<"0.125">>,      zero_point},
                    {<<"0.125e3">>,    zero_point_digits_exp},
                    {<<"0.125e-3">>,   zero_point_digits_signed_exp},
                    {<<"0.e3">>,       zero_point_exp},
                    {<<"0.e-3">>,      zero_point_signed_exp},
                    {<<"0e3">>,        zero_exp},
                    {<<"0e-3">>,       zero_signed_exp}],
    [{{ok, [{{float_lit,_}, _, Orig}], _}, _} = {scan(Orig), What}
     || {Orig, What} <- FloatNumbers],

    Punctuations = ".:;{}[]()=,<>-+",
    [begin
         B = <<C>>,
         T = list_to_atom([C]),
         {{ok, [{T, _, T}], _}, _} = {scan(B), [C]}
     end
     || C <- Punctuations],
    ok.

line_numbers_in_tokens_test() ->
    B = <<"'abc'\n"
          ".\n"
          "//\n"     % shortest possible
          "/**/\n"
          "/* ... */ // ...\n"
          "10\n"
          "1.25e3\n"
          "true">>,
    {ok, [{{str_lit,_}, 1, _},
          {'.', 2, _},
          %% C++ comment, one line line
          %% C comment, 2 lines
          %% C comment 1 line, then c++ comment same line
          {{int_lit, _}, 6, _},
          {{float_lit, _}, 7, _},
          {<<"true">>, 8, _}]=Tokens, 8 = _EndLine} =
        gpb_scan:binary(B),
    {ok, Tokens, 9} = gpb_scan:binary(<<B/binary, $\n>>),
    ok.

scan_error_test() ->
    %% Invalid lexemes
    err_scan(<<"&">>, []),
    %% Comments
    err_scan(<<"/*">>, ["unterminated", "comment"]),
    %% Strings
    err_scan(<<"'abc">>, ["unterminated", "string"]),
    err_scan(<<"'\\u">>, ["unterminated", "string"]),
    err_scan(<<"'\\U">>, ["unterminated", "string"]),
    err_scan(<<"'\\x">>, ["unterminated", "string"]),
    err_scan(<<"'\\">>, ["unterminated", "string"]),
    err_scan(<<"'", 255, "'">>, ["utf-8"]), % invalid utf8 octet
    err_scan(<<"'", 2#11100011, "'">>, ["utf-8"]), % incomplete utf8 char
    err_scan(<<"'\\UffffFFFF'">>, ["invalid", "code", "point"]), % too large
    err_scan(<<"'\\ud800'">>, ["invalid", "code", "point"]),
    err_scan(<<"'\\udc00'">>, ["invalid", "code", "point"]),
    %% Numbers
    err_scan(<<"0x">>, ["unterminated", "numeral"]),
    err_scan(<<"0xabz">>, ["need", "space"]),
    err_scan(<<"09">>, ["invalid", "octal"]),
    err_scan(<<"0z">>, ["need", "space"]),
    err_scan(<<"0.e">>, ["invalid", "number"]),
    err_scan(<<"0.e-">>, ["invalid", "number"]),
    err_scan(<<"15e1.2">>, ["invalid", "number"]),
    err_scan(<<"15e1z">>, ["need", "space"]),
    err_scan(<<"0.z">>, ["need", "space"]),
    err_scan(<<"0..">>, ["invalid", "number"]),
    ok.

ok_scan(B) when is_binary(B) ->
    {ok, TokensLines, _EndLine} = gpb_scan:binary(B),
    [Token || {Token, _Line, _Orig} <- TokensLines].

scan(B) when is_binary(B) ->
    gpb_scan:binary(B).

err_scan(B, ExpectedErrTextFragmentsLowerCase) when is_binary(B) ->
    {error, {Line, Mod, Reason}, _EndLine} = gpb_scan:binary(B),
    ?assert(is_integer(Line)),
    ErrText = Mod:format_error(Reason),
    ?assert(is_io_data(ErrText)),
    ErrStrLC = gpb_lib:lowercase(binary_to_list(iolist_to_binary(ErrText))),
    MissingErrTextFragments =
        [Fragment || Fragment <- ExpectedErrTextFragmentsLowerCase,
                     not gpb_lib:is_substr(Fragment, ErrStrLC)],
    if MissingErrTextFragments /= [] ->
            io:format("ErrStr=~s~n", [ErrStrLC]); % debug
       true -> ok
    end,
    ?assertEqual([], MissingErrTextFragments),
    ErrText.

is_io_data(X) ->
    try
        iolist_to_binary(X),
        true
    catch error:badarg ->
            false
    end.
