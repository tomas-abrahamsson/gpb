%%% Copyright (C) 2010-2011  Tomas Abrahamsson
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

-module(gpb_scan_tests).

-include_lib("eunit/include/eunit.hrl").


parses_integer_test() -> %% tests inspired by tests in tokenizer_unittest.cc
    {ok,[{dec_lit,_,123}],_}         = gpb_scan:string("123"),
    {ok,[{hex_lit,_,2742}],_}        = gpb_scan:string("0xab6"),
    {ok,[{hex_lit,_,2742}],_}        = gpb_scan:string("0XAB6"),
    {ok,[{hex_lit,_,19088743}],_}    = gpb_scan:string("0X1234567"),
    {ok,[{hex_lit,_,2309737967}],_}  = gpb_scan:string("0x89abcdef"),
    {ok,[{hex_lit,_,2309737967}],_}  = gpb_scan:string("0x89ABCDEF"),
    {ok,[{oct_lit,_,342391}],_}      = gpb_scan:string("01234567"),
    {ok,[{dec_lit,_,-123}],_}        = gpb_scan:string("-123"),
    {ok,[{hex_lit,_,-2742}],_}       = gpb_scan:string("-0xab6"),
    {ok,[{hex_lit,_,-2742}],_}       = gpb_scan:string("-0XAB6"),
    {ok,[{hex_lit,_,-19088743}],_}   = gpb_scan:string("-0X1234567"),
    {ok,[{hex_lit,_,-2309737967}],_} = gpb_scan:string("-0x89abcdef"),
    {ok,[{hex_lit,_,-2309737967}],_} = gpb_scan:string("-0x89ABCDEF"),
    {ok,[{oct_lit,_,-342391}],_}     = gpb_scan:string("-01234567"),
    ok.

parses_floats_test() -> %% tests inspired by tests in tokenizer_unittest.cc
    {ok,[{float_lit,_,123.45}],_} = gpb_scan:string("123.45"),
    {ok,[{float_lit,_,1.0}],_}    = gpb_scan:string("1."),
    {ok,[{float_lit,_,1.0e3}],_}  = gpb_scan:string("1e3"),
    {ok,[{float_lit,_,1.0e3}],_}  = gpb_scan:string("1E3"),
    {ok,[{float_lit,_,1.0e-3}],_} = gpb_scan:string("1e-3"),
    {ok,[{float_lit,_,1.0e3}],_}  = gpb_scan:string("1e+3"),
    {ok,[{float_lit,_,1.0e3}],_}  = gpb_scan:string("1.e3"),
    {ok,[{float_lit,_,1.2e3}],_}  = gpb_scan:string("1.2e3"),
    {ok,[{float_lit,_,0.1}],_}    = gpb_scan:string(".1"),
    {ok,[{float_lit,_,0.1e3}],_}  = gpb_scan:string(".1e3"),
    {ok,[{float_lit,_,0.1e-3}],_} = gpb_scan:string(".1e-3"),
    {ok,[{float_lit,_,0.1e3}],_}  = gpb_scan:string(".1e+3"),
    ok.

skips_comments_test() ->
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("//abv\n12"),
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("12//def"), %% no \n on last line
    ok.

skips_c_style_comments_test() ->
    {error, _, _}            = gpb_scan:string("/*/12"), % not a comment
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("/**/12"), % smallest comment
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("/* - */12"),
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("/*****/12"),
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("/*****/12/****/"), % greedy test
    {ok, [{dec_lit,_,_}], _} = gpb_scan:string("/**\n *\n*/12"), % \n in comment
    S = "x/* xyz */y",
    {ok, [{str_lit,_,S}], _} = gpb_scan:string("\""++S++"\""),% comment in str
    ok.

parses_strings_test() ->
    {ok, [{str_lit,_,""}], _}     = gpb_scan:string("\"\""),
    {ok, [{str_lit,_,""}], _}     = gpb_scan:string("''"),
    {ok, [{str_lit,_,"abc"}], _}  = gpb_scan:string("\"abc\""),
    {ok, [{str_lit,_,"abc"}], _}  = gpb_scan:string("'abc'"), %% single quotes
    {ok, [{str_lit,_,"a\"c"}], _} = gpb_scan:string("\"a\\\"c\""),
    {ok, [{str_lit,_,"abz"}], _}  = gpb_scan:string("\"a\\142z\""), % 0142=b
    {ok, [{str_lit,_,"abz"}], _}  = gpb_scan:string("\"a\\x62z\""), % 0x62=b
    {ok, [{str_lit,_,"a"},{str_lit,_,"b"}], _} = gpb_scan:string("'a' 'b'"),
    ok.

max_3_escaped_octdigits_test() ->
    {ok, [{str_lit,_,"ab3"}], _}  = gpb_scan:string("\"a\\1423\"").

max_2_escaped_hexdigits_test() ->
    {ok, [{str_lit,_,"ab3"}], _}  = gpb_scan:string("\"a\\x623\"").

parses_unicode_escapes_test() ->
    %% \u: exactly 4 following
    {ok, [{str_lit,_,[$a,16#1234,$b]}], _} =
        gpb_scan:string("\"a\\u1234b\""),
    %% \U: exactly 8 following hex digits
    {ok, [{str_lit,_,[$a,16#101234,$b]}], _} =
        gpb_scan:string("\"a\\U00101234b\"").

ignores_whitespace_test() ->
    {ok, [{dec_lit,_,_},{dec_lit,_,_},{dec_lit,_,_}], _} =
        gpb_scan:string("\n  12  13 \t 15"),
    ok.
