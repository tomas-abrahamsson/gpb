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

-module(gpb_tests).
%-compile(export_all).
-import(gpb, [decode_msg/3, encode_msg/2, merge_msgs/3, verify_msg/2]).

-include_lib("eunit/include/eunit.hrl").
-include("gpb.hrl").

-record(m1,{a}).

skipping_unknown_varint_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<32,150,1>>, %% field number 4 (not known), wire type = 0
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).

skipping_unknown_length_delimited_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<34,1,1>>, %% field number 4 (not known), wire type = 2
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).

skipping_unknown_64bit_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<33,0,0,0,0,0,0,0,0>>, %% field number 4, wire type = 1
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).
skipping_unknown_32bit_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<37,0,0,0,0>>, %% field number 4, wire type = 5
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]).

decode_msg_simple_occurrence_test() ->
    #m1{a = undefined} =
        decode_msg(<<>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=optional, opts=[]}]}]),
    #m1{a = 150} =
        decode_msg(<<8,150,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=required, opts=[]}]}]),
    #m1{a = [150, 151]} =
        decode_msg(<<8,150,1, 8,151,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[]}]}]),
    ok.

decode_msg_with_enum_field_test() ->
    #m1{a = v2} =
        decode_msg(<<8,150,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, 150}]}]).

decode_msg_with_negative_enum_value_test() ->
    #m1{a = v2} =
        decode_msg(<<8, 254,255,255,255,15>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, -2}]}]).

decode_msg_with_bool_field_test() ->
    #m1{a = true} =
        decode_msg(<<8,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]),
    #m1{a = false} =
        decode_msg(<<8,0>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]).

decoding_float_test() ->
    %% Stole idea from the python test in google-protobuf:
    %% 1.125 is perfectly representable as a float (no rounding error).
    #m1{a = 1.125} =
        decode_msg(<<13,0,0,144,63>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=float,
                                       occurrence=required, opts=[]}]}]).

decoding_double_test() ->
    #m1{a = 1.125} =
        decode_msg(<<9,0,0,0,0,0,0,242,63>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=double,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_string_field_test() ->
    #m1{a = "abc\345\344\366"++[1022]} =
        decode_msg(<<10,11,
                    $a,$b,$c,$\303,$\245,$\303,$\244,$\303,$\266,$\317,$\276>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=string,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_bytes_field_test() ->
    #m1{a = <<0,0,0,0>>} =
        decode_msg(<<10,4,0,0,0,0>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bytes,
                                       occurrence=required, opts=[]}]}]).

-record(m2, {b}).

decode_msg_with_sub_msg_field_test() ->
    #m1{a = #m2{b = 150}} =
        decode_msg(<<10,3, 8,150,1>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=required, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=required, opts=[]}]}]).

decode_msg_with_optional_nonpresent_sub_msg_field_test() ->
    #m1{a = undefined} =
        decode_msg(<<>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=optional, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=required, opts=[]}]}]).

decoding_zero_instances_of_packed_varints_test() ->
    %%    "A packed repeated field containing zero elements does not
    %%     appear in the encoded message."
    %%    -- http://code.google.com/apis/protocolbuffers/docs/encoding.html
    #m1{a = []} =
        decode_msg(<<>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[packed]}]}]).

decoding_one_packed_chunk_of_varints_test() ->
    #m1{a = [3, 270, 86942]} =
        decode_msg(<<16#22,                 % tag (field number 4, wire type 2)
                     16#06,                 % payload size (6 bytes)
                     16#03,                 % first element (varint 3)
                     16#8E, 16#02,          % second element (varint 270)
                     16#9E, 16#a7, 16#05>>, % third element (varint 86942)
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=4, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[packed]}]}]).

decoding_two_packed_chunks_of_varints_test() ->
    %%    "Note that although there's usually no reason to encode more
    %%     than one key-value pair for a packed repeated field, encoders
    %%     must be prepared to accept multiple key-value pairs. In this
    %%     case, the payloads should be concatenated. Each pair must
    %%     contain a whole number of elements."
    %%    -- http://code.google.com/apis/protocolbuffers/docs/encoding.html
    #m1{a = [3, 270, 86942, 4, 271, 86943]} =
        decode_msg(<<16#22, 16#06, 16#03, 16#8E, 16#02, 16#9E, 16#a7, 16#05,
                     16#22, 16#06, 16#04, 16#8F, 16#02, 16#9F, 16#a7, 16#05>>,
                   m1,
                   [{{msg,m1}, [#field{name=a, fnum=4, rnum=#m1.a, type=int32,
                                       occurrence=repeated, opts=[packed]}]}]),
    ok.

%% -------------------------------------------------------------

encode_required_varint_field_test() ->
    <<8,150,1>> =
        encode_msg(#m1{a=150},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=required, opts=[]}]}]).

encode_optional_varint_field_test() ->
    <<>> =
        encode_msg(#m1{a=undefined},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=optional, opts=[]}]}]).

encode_repeated_empty_field_test() ->
    <<>> =
        encode_msg(#m1{a=[]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[packed]}]}]),
    <<>> =
        encode_msg(#m1{a=[]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[]}]}]).

encode_repeated_nonempty_field_test() ->
    <<10,4, 150,1, 151,1>> =
        encode_msg(#m1{a=[150,151]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[packed]}]}]),
    <<8,150,1, 8,151,1>> =
        encode_msg(#m1{a=[150,151]},
                   [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a, type=int32,
                                      occurrence=repeated, opts=[]}]}]).

encode_msg_with_sub_msg_field_test() ->
    <<10,3, 8,150,1>> =
        encode_msg(#m1{a = #m2{b = 150}},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=required, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=required, opts=[]}]}]).

encode_msg_with_enum_field_test() ->
    <<8,150,1>> =
        encode_msg(#m1{a = v2},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, 150}]}]).

encode_msg_with_negative_enum_value_test() ->
    <<8, 254,255,255,255,15>> =
        encode_msg(#m1{a = v2},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a,
                                       type={enum,e},
                                       occurrence=required, opts=[]}]},
                    {{enum,e}, [{v1, 100},
                                {v2, -2}]}]).

encode_msg_with_bool_field_test() ->
    <<8,1>> =
        encode_msg(#m1{a = true},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]),
    <<8,0>> =
        encode_msg(#m1{a = false},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=bool,
                                       occurrence=required, opts=[]}]}]).

encode_float_test() ->
    %% Stole idea from the python test in google-protobuf:
    %% 1.125 is perfectly representable as a float (no rounding error).
    <<13,0,0,144,63>> =
        encode_msg(#m1{a = 1.125},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=float,
                                       occurrence=required, opts=[]}]}]).

encode_packed_repeated_bools_test() ->
    <<16#22,1,1>> =
        encode_msg(#m1{a=[true]},
                   [{{msg,m1},[#field{name=a,fnum=4,rnum=#m1.a, type=bool,
                                      occurrence=repeated, opts=[packed]}]}]).
decode_packed_repeated_bools_test() ->
    #m1{a=[true]} =
        decode_msg(<<16#22,1,1>>,
                   m1,
                   [{{msg,m1},[#field{name=a,fnum=4,rnum=#m1.a, type=bool,
                                      occurrence=repeated, opts=[packed]}]}]).



encode_double_test() ->
    <<9,0,0,0,0,0,0,242,63>> =
        encode_msg(#m1{a = 1.125},
                   [{{msg,m1}, [#field{name=a, fnum=1, rnum=#m1.a, type=double,
                                       occurrence=required, opts=[]}]}]).

%% -------------------------------------------------------------

merging_second_required_integer_overrides_first_test() ->
    #m1{a=20} = merge_msgs(#m1{a=10}, #m1{a=20},
                           [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                              occurrence=required,opts=[]}]}]).

merging_second_optional_integer_overrides_undefined_test() ->
    #m1{a=22} = merge_msgs(#m1{a=undefined}, #m1{a=22},
                           [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                              occurrence=optional,opts=[]}]}]).

merging_undefined_does_not_overrides_defined_integer_test() ->
    #m1{a=25} = merge_msgs(#m1{a=25}, #m1{a=undefined},
                           [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                              occurrence=optional,opts=[]}]}]).

merging_sequences_test() ->
    #m1{a=[11,12, 21,22]} =
        merge_msgs(#m1{a=[11,12]}, #m1{a=[21,22]},
                   [{{msg,m1},[#field{name=a,rnum=#m1.a,type=uint32,
                                      occurrence=repeated,opts=[]}]}]).

-record(m4, {x,y}).

merging_messages_recursively_test() ->
    #m1{a=#m4{x = 210,
              y = [111, 112, 211, 212]}} =
        merge_msgs(#m1{a = #m4{x = 110,
                               y = [111, 112]}},
                   #m1{a = #m4{x = 210,
                               y = [211, 212]}},
                   [{{msg,m1}, [#field{name=a,fnum=1, rnum=#m1.a,
                                       type={msg,m4},
                                       occurrence=required, opts=[]}]},
                    {{msg,m4}, [#field{name=x, fnum=1, rnum=#m4.x, type=uint32,
                                       occurrence=optional, opts=[]},
                                #field{name=y, fnum=2, rnum=#m4.y, type=uint32,
                                       occurrence=repeated, opts=[]}]}]).

merging_optional_messages_recursively1_test() ->
    #m1{a=#m2{b = 110}} =
        merge_msgs(#m1{a = #m2{b = 110}},
                   #m1{a = undefined},
                   [{{msg,m1}, [#field{name=a,fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=optional, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=optional, opts=[]}]}]).

merging_optional_messages_recursively2_test() ->
    #m1{a=#m2{b = 210}} =
        merge_msgs(#m1{a = undefined},
                   #m1{a = #m2{b = 210}},
                   [{{msg,m1}, [#field{name=a,fnum=1, rnum=#m1.a,
                                       type={msg,m2},
                                       occurrence=optional, opts=[]}]},
                    {{msg,m2}, [#field{name=b, fnum=1, rnum=#m2.b, type=uint32,
                                       occurrence=optional, opts=[]}]}]).


%% -------------------------------------------------------------

-define(verify_gpb_err(Expr), ?assertError({gpb_type_error, _}, Expr)).

verify_presetn_required_field_succeeds_test() ->
    ok = verify_msg(#m1{a=1},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                              occurrence=required}]}]).

verify_missing_required_field_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                                         occurrence=required}]}])).

verify_optional_undefined_field_is_ok_test() ->
    ok = verify_msg(#m1{},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                              occurrence=optional}]}]).

verify_optional_present_field_is_ok_test() ->
    ok = verify_msg(#m1{a=1},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                              occurrence=optional}]}]).

verify_valid_repeated_field_succeeds_test() ->
    ok = verify_msg(#m1{a=[1]},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                              occurrence=repeated}]}]).

verify_invalid_repeated_field_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{a=1},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                                         occurrence=repeated}]}])).

verify_valid_integer_succeeds_test() ->
    [ok = verify_msg(#m1{a=42},
                     [{{msg,m1},
                       [#field{name=a,fnum=1,rnum=#m1.a, type=IType,
                               occurrence=required}]}])
     || IType <- [int32, int64, uint32, uint64, sint32, sint64,
                  fixed32, fixed64, sfixed32, sfixed64]].

verify_integer_range_fails_test() ->
    [begin
         ok = verify_msg(#m1{a=int_min(IType)},
                         [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a,
                                            type=IType,
                                            occurrence=required}]}]),
         ok = verify_msg(#m1{a=int_max(IType)},
                         [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a,
                                            type=IType,
                                            occurrence=required}]}]),
         ?verify_gpb_err(verify_msg(#m1{a=int_min(IType)-1},
                                    [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a,
                                                       type=IType,
                                                       occurrence=required}]}])),
         ?verify_gpb_err(verify_msg(#m1{a=int_max(IType)+1},
                                    [{{msg,m1},[#field{name=a,fnum=1,rnum=#m1.a,
                                                       type=IType,
                                                       occurrence=required}]}]))
     end
     || IType <- [int32, int64, uint32, uint64, sint32, sint64,
                  fixed32, fixed64, sfixed32, sfixed64]].

int_min(int32)    -> int_min_by_descr(  signed, 32);
int_min(int64)    -> int_min_by_descr(  signed, 64);
int_min(uint32)   -> int_min_by_descr(unsigned, 32);
int_min(uint64)   -> int_min_by_descr(unsigned, 64);
int_min(sint32)   -> int_min_by_descr(  signed, 32);
int_min(sint64)   -> int_min_by_descr(  signed, 64);
int_min(fixed32)  -> int_min_by_descr(unsigned, 32);
int_min(fixed64)  -> int_min_by_descr(unsigned, 64);
int_min(sfixed32) -> int_min_by_descr(  signed, 32);
int_min(sfixed64) -> int_min_by_descr(  signed, 64).

int_min_by_descr(signed,   32) -> -16#80000000         = -(1 bsl 31);
int_min_by_descr(signed,   64) -> -16#8000000000000000 = -(1 bsl 63);
int_min_by_descr(unsigned, 32) -> 0;
int_min_by_descr(unsigned, 64) -> 0.

int_max(int32)    -> int_max_by_descr(  signed, 32);
int_max(int64)    -> int_max_by_descr(  signed, 64);
int_max(uint32)   -> int_max_by_descr(unsigned, 32);
int_max(uint64)   -> int_max_by_descr(unsigned, 64);
int_max(sint32)   -> int_max_by_descr(  signed, 32);
int_max(sint64)   -> int_max_by_descr(  signed, 64);
int_max(fixed32)  -> int_max_by_descr(unsigned, 32);
int_max(fixed64)  -> int_max_by_descr(unsigned, 64);
int_max(sfixed32) -> int_max_by_descr(  signed, 32);
int_max(sfixed64) -> int_max_by_descr(  signed, 64).

int_max_by_descr(signed,   32) -> 16#7fffFFFF         = (1 bsl 31) - 1;
int_max_by_descr(signed,   64) -> 16#7fffFFFFffffFFFF = (1 bsl 63) - 1;
int_max_by_descr(unsigned, 32) -> 16#ffffFFFF         = (1 bsl 32) - 1;
int_max_by_descr(unsigned, 64) -> 16#ffffFFFFffffFFFF = (1 bsl 64) - 1.


verify_bad_integer_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{a=true},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a, type=uint32,
                                         occurrence=required}]}])).

verify_valid_booleans_succeed_test() ->
    [ok = verify_msg(#m1{a=B},
                     [{{msg,m1},
                       [#field{name=a,fnum=1,rnum=#m1.a, type=bool,
                               occurrence=required}]}])
     || B <- [true, false]].

verify_bad_booleans_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{a=tomato},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a, type=bool,
                                         occurrence=required}]}])).

verify_valid_float_succeeds_test() ->
    [ok = verify_msg(#m1{a=1.2e3},
                     [{{msg,m1},
                       [#field{name=a,fnum=1,rnum=#m1.a, type=FloatType,
                               occurrence=required}]}])
     || FloatType <- [float, double]].


verify_bad_floats_fails_test() ->
    [?verify_gpb_err(verify_msg(#m1{a=tomato},
                                [{{msg,m1},
                                  [#field{name=a,fnum=1,rnum=#m1.a,
                                          type=FloatType,
                                          occurrence=required}]}]))
     || FloatType <- [float, double]].


verify_valid_string_succeeds_test() ->
    %% iolists are ok as strings
    %% strings are unicode
    ok = verify_msg(#m1{a=["abc", [16#449], <<"ff">>]},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a,
                              type=string,
                              occurrence=required}]}]).

verify_invalid_string_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{a=["abc", an_invalid_character]},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a,
                                         type=string,
                                         occurrence=required}]}])).

verify_valid_bytes_succeeds_test() ->
    ok = verify_msg(#m1{a = <<"ff">>},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a,
                              type=bytes,
                              occurrence=required}]}]).

verify_invalid_bytes_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{a=33},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a,
                                         type=bytes,
                                         occurrence=required}]}])).

verify_valid_enum_succeeds_test() ->
    ok = verify_msg(#m1{a = e1},
                    [{{msg,m1},
                      [#field{name=a,fnum=1,rnum=#m1.a,
                              type={enum,e},
                              occurrence=required}]},
                     {{enum,e},[{e1, 1}]}]).

verify_invalid_enum_fails_test() ->
    ?verify_gpb_err(verify_msg(#m1{a = exyz},
                               [{{msg,m1},
                                 [#field{name=a,fnum=1,rnum=#m1.a,
                                         type={enum,e},
                                         occurrence=required}]},
                                {{enum,e},[{e1, 1}]}])).

verify_valid_submsg_succeeds_test() ->
    ok = verify_msg(#m1{a = #m2{b = 1}},
                    [{{msg,m1}, [#field{name=a,fnum=1,rnum=#m1.a,
                                        type={msg,m2},
                                        occurrence=required}]},
                     {{msg,m2}, [#field{name=b,fnum=1,rnum=#m2.b,
                                        type=uint32,
                                        occurrence=required}]}]).

verify_invalid_submsg_fails_test() ->
    MsgDefs = [{{msg,m1}, [#field{name=a,fnum=1,rnum=#m1.a,
                                  type={msg,m2},
                                  occurrence=required}]},
               {{msg,m2}, [#field{name=b,fnum=1,rnum=#m2.b,
                                  type=uint32,
                                  occurrence=required}]}],
    ?verify_gpb_err(verify_msg(#m1{a = 1},    MsgDefs)),
    ?verify_gpb_err(verify_msg(#m1{a = {}},   MsgDefs)),
    ?verify_gpb_err(verify_msg(#m1{a = {m2}}, MsgDefs)).

verify_path_when_failure_test() ->
    MsgDefs = [{{msg,m1}, [#field{name=a,fnum=1,rnum=#m1.a,
                                  type={msg,m2},
                                  occurrence=required}]},
               {{msg,m2}, [#field{name=b,fnum=1,rnum=#m2.b,
                                  type=uint32,
                                  occurrence=required}]}],
    ?assertError({gpb_type_error, {_, [_, {path, top_level}]}},
                 verify_msg(bad_msg, MsgDefs)),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1'}]}},
                 verify_msg({m1}, MsgDefs)),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1.a'}]}},
                 verify_msg(#m1{a = bad_msg}, MsgDefs)),
    ?assertError({gpb_type_error, {_, [_, {path, 'm1.a.b'}]}},
                 verify_msg(#m1{a = #m2{b=x}}, MsgDefs)).
