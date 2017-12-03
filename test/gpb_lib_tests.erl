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
