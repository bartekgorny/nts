%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2017 17:20
%%%-------------------------------------------------------------------
-module(device_SUITE).
-author("bartekgorny").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-compile(export_all).
-define(DEVID, <<"01">>).

all() ->
    [simple_test].

init_per_suite(C) ->
    nts_helpers:set_config(C),
    application:ensure_all_started(nts),
    C.

simple_test(_) ->
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    RecDtm = fromnow(-10),
    Dtm = fromnow(-20),
    S = nts_device:getstate(Dev),
    ct:pal("S: ~p", [S]),
    D = maps:get(status, S#loc.data),
    ?assertMatch(#{last_signal := RecDtm, last_signal_dtm := Dtm }, D).

fromnow(Offset) ->
    N = os:timestamp(),
    Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(N)) + Offset,
    calendar:gregorian_seconds_to_datetime(Sec).

mkframe(RecOffset, Offset) ->
    #frame{type = location,
           received = fromnow(RecOffset),
           data = #{dtm => fromnow(Offset),
                    latitude => 1,
                    longitude => 2}}.