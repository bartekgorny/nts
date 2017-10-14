%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2017 22:41
%%%-------------------------------------------------------------------
-module(geo_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-define(PRECISION, 0.1).

all() ->
    [distance].

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

distance(_) ->
    % this is a regression in case we change the way of calculation
    % we don't need precision
    ok = check({51, 21}, {52, 21}, 111),
    ok = check({51, 21}, {51, 22}, 70),
    ok = check({51, 21}, {52, 22}, 130),
    ok.


%%%%%%%%%%%%%%%%

check(P1, P2, Expected) ->
    case abs(nts_utils:distance(P1, P2)) / Expected of
        X when X < (1 - ?PRECISION) -> too_low;
        X when X > (1 + ?PRECISION) -> too_high;
        _ -> ok
    end.
