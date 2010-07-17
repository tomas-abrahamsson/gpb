%%% Copyright (C) 2010  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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

parses_strings_test() ->
    {ok, [{str_lit,_,""}], _}     = gpb_scan:string("\"\""),
    {ok, [{str_lit,_,""}], _}     = gpb_scan:string("''"),
    {ok, [{str_lit,_,"abc"}], _}  = gpb_scan:string("\"abc\""),
    {ok, [{str_lit,_,"abc"}], _}  = gpb_scan:string("'abc'"), %% single quotes
    {ok, [{str_lit,_,"a\"c"}], _} = gpb_scan:string("\"a\\\"c\""),
    {ok, [{str_lit,_,"abz"}], _}  = gpb_scan:string("\"a\\0142z\""), % 0142=b
    {ok, [{str_lit,_,"abz"}], _}  = gpb_scan:string("\"a\\x62z\""), % 0x62=b
    {ok, [{str_lit,_,"a"},{str_lit,_,"b"}], _} = gpb_scan:string("'a' 'b'"),
    ok.

ignores_whitespace_test() ->
    {ok, [{dec_lit,_,_},{dec_lit,_,_},{dec_lit,_,_}], _} =
        gpb_scan:string("\n  12  13 \t 15"),
    ok.
