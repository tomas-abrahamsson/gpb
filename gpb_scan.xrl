%% This line tells emacs to use -*- erlang -*- mode for this file 

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


Definitions.
D       = [0-9]
H       = [0-9a-fA-F]
WS      = [\000-\s]+

Rules.

required        : {token, {required,TokenLine}}.
optional        : {token, {optional,TokenLine}}.
repeated        : {token, {repeated,TokenLine}}.

double          : {token, {double,TokenLine}}.
float           : {token, {float,TokenLine}}.
int32           : {token, {int32,TokenLine}}.
int64           : {token, {int64,TokenLine}}.
uint32          : {token, {uint32,TokenLine}}.
uint64          : {token, {uint64,TokenLine}}.
sint32          : {token, {sint32,TokenLine}}.
sint64          : {token, {sint64,TokenLine}}.
fixed32         : {token, {fixed32,TokenLine}}.
sfixed32        : {token, {sfixed32,TokenLine}}.
sfixed64        : {token, {sfixed64,TokenLine}}.
bool            : {token, {bool,TokenLine}}.
string          : {token, {string,TokenLine}}.
bytes           : {token, {bytes,TokenLine}}.

package         : {token, {package,TokenLine}}.
message         : {token, {message,TokenLine}}.
enum            : {token, {enum,TokenLine}}.
option          : {token, {option,TokenLine}}.
import          : {token, {import,TokenLine}}.
default         : {token, {default,TokenLine}}.
packed          : {token, {packed,TokenLine}}.
deprecated      : {token, {deprecated,TokenLine}}.
exensions       : {token, {extensions,TokenLine}}.
extend          : {token, {extend,TokenLine}}.
to              : {token, {to,TokenLine}}.
max             : {token, {max,TokenLine}}.

service         : {token, {service,TokenLine}}.
rpc             : {token, {rpc,TokenLine}}.
returns         : {token, {returns,TokenLine}}.

true            : {token, {bool_lit,TokenLine,true}}.
false           : {token, {bool_lit,TokenLine,false}}.

syntax          : {token, {syntax,TokenLine}}.


"(\\x[0-9a-fA-F]|\\[0-7]|\\[abfnrtv?"'\\]|[^"\n])*" :
                  {token,{str_lit,TokenLine,string_value(TokenChars)}}.
'(\\x[0-9a-fA-F]|\\[0-7]|\\[abfnrtv?"'\\]|[^'\n])*' :
                   {token,{str_lit,TokenLine,string_value(TokenChars)}}.

(\+|-)?{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
                   {token,{float_lit,TokenLine,str_to_float_1(TokenChars)}}.
(\+|-)?\.{D}+((E|e)(\+|\-)?{D}+)? :
                   {token,{float_lit,TokenLine,str_to_float_2(TokenChars)}}.
(\+|-)?{D}+\.((E|e)(\+|\-)?{D}+)? :
                   {token,{float_lit,TokenLine,str_to_float_3(TokenChars)}}.
(\+|-)?{D}+(E|e)(\+|\-)?{D}+ :
                   {token,{float_lit,TokenLine,str_to_float_4(TokenChars)}}.

% Cannot create neither +-inf or nan in erlang...
% (\+|-)?inf :       {token,{float_lit,TokenLine,create_inf(TokenChars)}}.
% nan :              {token,{float_lit,TokenLine,create_nan()}}.

(\+|-)?0[xX]{H}+ : {token,{hex_lit,TokenLine,hexstr_to_integer(TokenChars)}}.
(\+|-)?0{D}+     : {token,{oct_lit,TokenLine,octstr_to_integer(TokenChars)}}.
(\+|-)?{D}+      : {token,{dec_lit,TokenLine,list_to_integer(TokenChars)}}.

[a-zA-Z_][A-Za-z0-9_]* : {token,{identifier,TokenLine,TokenChars}}.

\.              : {token, {'.', TokenLine}}.
;               : {token, {';', TokenLine}}.
\{              : {token, {'{', TokenLine}}.
\}              : {token, {'}', TokenLine}}.
\[              : {token, {'[', TokenLine}}.
\]              : {token, {']', TokenLine}}.
\(              : {token, {'(', TokenLine}}.
\)              : {token, {')', TokenLine}}.
=               : {token, {'=', TokenLine}}.
,               : {token, {',', TokenLine}}.



//.*\n		: skip_token. %% comment
//.*		: skip_token. %% comment
{WS}            : skip_token.


Erlang code.

-include_lib("eunit/include/eunit.hrl").

string_value(S) -> % S is with start+end quote
    %% Strip quotes.
    string_val_2(lists:sublist(S, 2, length(S) - 2)).

string_val_2([$\\|Cs]) -> string_escape(Cs);
string_val_2([C|Cs])   -> [C|string_val_2(Cs)];
string_val_2([])       -> [].

-define(is_octal_char(C), $0 =< C, C =< $7).

string_escape("x"++Rest) ->
    {HexChars, Rest2} = collect(fun is_hex_char/1, Rest),
    [hex_to_integer(HexChars) | string_val_2(Rest2)];
string_escape([Oct|Rest]) when ?is_octal_char(Oct) ->
    {OctChars, Rest2} = collect(fun is_oct_char/1, [Oct|Rest]),
    [oct_to_integer(OctChars) | string_val_2(Rest2)];
string_escape([C|Rest]) ->
    [escape_char(C) | string_val_2(Rest)].

escape_char($a) -> 7;                           %\a = BEL
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) -> $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char(C)  -> C.

collect(Pred, Str) -> lists:splitwith(Pred, Str).

is_oct_char(C) -> $0 =< C andalso C =< $7.
is_hex_char(C) -> ($0 =< C andalso C =< $9)
                      orelse ($a =< C andalso C =< $f)
                      orelse ($A =< C andalso C =< $F).

oct_to_integer(Str) -> erlang:list_to_integer(Str, 8).
hex_to_integer(Str) -> erlang:list_to_integer(Str, 16).

octstr_to_integer(Str) -> oct_to_integer(Str).

hexstr_to_integer("0x"++H)  -> hex_to_integer(H);
hexstr_to_integer("0X"++H)  -> hex_to_integer(H);
hexstr_to_integer("+0x"++H) -> hex_to_integer(H);
hexstr_to_integer("+0X"++H) -> hex_to_integer(H);
hexstr_to_integer("-0x"++H) -> -hex_to_integer(H);
hexstr_to_integer("-0X"++H) -> -hex_to_integer(H).

str_to_float_1(S)        -> list_to_float(S).

str_to_float_2("."++_=S) -> str_to_float_1("0"++S);
str_to_float_2("+."++T)  -> str_to_float_1("0."++T);
str_to_float_2("-."++T)  -> str_to_float_1("-0."++T).

str_to_float_3(S) -> %% No decimals after `.' Possiby e+-<n> following `.'
    {UpToDot, "."++Rest} = collect(fun isnt_dot/1, S),
    str_to_float_1(UpToDot++"."++"0"++Rest).

str_to_float_4(S) -> %% Integer preceeding e+-<n>
    {UpToDot, Rest} = collect(fun isnt_exp_e/1, S),
    str_to_float_1(UpToDot++"."++"0"++Rest).

isnt_dot(C) -> C /= $. .

isnt_exp_e($e) -> false;
isnt_exp_e($E) -> false;
isnt_exp_e(_)  -> true.

parses_integer_test() -> %% tests inspired by tests in tokenizer_unittest.cc
    {ok,[{dec_lit,_,123}],_}         = ?MODULE:string("123"),
    {ok,[{hex_lit,_,2742}],_}        = ?MODULE:string("0xab6"),
    {ok,[{hex_lit,_,2742}],_}        = ?MODULE:string("0XAB6"),
    {ok,[{hex_lit,_,19088743}],_}    = ?MODULE:string("0X1234567"),
    {ok,[{hex_lit,_,2309737967}],_}  = ?MODULE:string("0x89abcdef"),
    {ok,[{hex_lit,_,2309737967}],_}  = ?MODULE:string("0x89ABCDEF"),
    {ok,[{oct_lit,_,342391}],_}      = ?MODULE:string("01234567"),
    {ok,[{dec_lit,_,-123}],_}        = ?MODULE:string("-123"),
    {ok,[{hex_lit,_,-2742}],_}       = ?MODULE:string("-0xab6"),
    {ok,[{hex_lit,_,-2742}],_}       = ?MODULE:string("-0XAB6"),
    {ok,[{hex_lit,_,-19088743}],_}   = ?MODULE:string("-0X1234567"),
    {ok,[{hex_lit,_,-2309737967}],_} = ?MODULE:string("-0x89abcdef"),
    {ok,[{hex_lit,_,-2309737967}],_} = ?MODULE:string("-0x89ABCDEF"),
    {ok,[{oct_lit,_,-342391}],_}     = ?MODULE:string("-01234567"),
    ok.

parses_floats_test() -> %% tests inspired by tests in tokenizer_unittest.cc
    {ok,[{float_lit,_,123.45}],_} = ?MODULE:string("123.45"),
    {ok,[{float_lit,_,1.0}],_}    = ?MODULE:string("1."),
    {ok,[{float_lit,_,1.0e3}],_}  = ?MODULE:string("1e3"),
    {ok,[{float_lit,_,1.0e3}],_}  = ?MODULE:string("1E3"),
    {ok,[{float_lit,_,1.0e-3}],_} = ?MODULE:string("1e-3"),
    {ok,[{float_lit,_,1.0e3}],_}  = ?MODULE:string("1e+3"),
    {ok,[{float_lit,_,1.0e3}],_}  = ?MODULE:string("1.e3"),
    {ok,[{float_lit,_,1.2e3}],_}  = ?MODULE:string("1.2e3"),
    {ok,[{float_lit,_,0.1}],_}    = ?MODULE:string(".1"),
    {ok,[{float_lit,_,0.1e3}],_}  = ?MODULE:string(".1e3"),
    {ok,[{float_lit,_,0.1e-3}],_} = ?MODULE:string(".1e-3"),
    {ok,[{float_lit,_,0.1e3}],_}  = ?MODULE:string(".1e+3"),
    ok.

skips_comments_test() ->
    {ok, [{dec_lit,_,_}], _} = ?MODULE:string("//abv\n12"),
    {ok, [{dec_lit,_,_}], _} = ?MODULE:string("12//def"), %% no \n on last line
    ok.

parses_strings_test() ->
    {ok, [{str_lit,_,""}], _}     = ?MODULE:string("\"\""),
    {ok, [{str_lit,_,""}], _}     = ?MODULE:string("''"),
    {ok, [{str_lit,_,"abc"}], _}  = ?MODULE:string("\"abc\""),
    {ok, [{str_lit,_,"abc"}], _}  = ?MODULE:string("'abc'"), %% single quotes
    {ok, [{str_lit,_,"a\"c"}], _} = ?MODULE:string("\"a\\\"c\""),
    {ok, [{str_lit,_,"abz"}], _}  = ?MODULE:string("\"a\\0142z\""), % 0142=b
    {ok, [{str_lit,_,"abz"}], _}  = ?MODULE:string("\"a\\x62z\""), % 0x62=b
    {ok, [{str_lit,_,"a"},{str_lit,_,"b"}], _} = ?MODULE:string("'a' 'b'"),
    ok.

ignores_whitespace_test() ->
    {ok, [{dec_lit,_,_},{dec_lit,_,_},{dec_lit,_,_}], _} =
        ?MODULE:string("\n  12  13 \t 15"),
    ok.
