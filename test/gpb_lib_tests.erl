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

-include_lib("eunit/include/eunit.hrl").

snake_case_test() ->
    "winter_is_a_time_of_year" = gpb_lib:snake_case("WinterIsATimeOfYear"),
    "winter_is_a_time_of_year" = gpb_lib:snake_case("winterIsATimeOfYear"),
    "a_later_time" = gpb_lib:snake_case("ALaterTime"),
    "a_later_time" = gpb_lib:snake_case("aLaterTime"),
    %"dotted.name_part" = gpb_lib:snake_case("Dotted.NamePart"),
    "already_snake_case" = gpb_lib:snake_case("already_snake_case"),
    "this_is_273_k" = gpb_lib:snake_case("ThisIs273K"),
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

