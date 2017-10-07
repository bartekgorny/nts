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
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    nts_helpers:clear_tables(["device"]),
    C.

simple_test(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    ok = nts_db:update_device(?DEVID, #{<<"cos">> => 99}),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    RecDtm = fromnow(-10),
    Dtm = fromnow(-20),
    S = nts_device:getstate(Dev),
    D = maps:get(status, S#loc.data),
    ?assertMatch(#{last_signal := RecDtm, last_signal_dtm := Dtm }, D),
    ?assertMatch({10, 20}, nts_location:coords(S)),
    has_error(false, Dev),
    nts_device:process_frame(Dev, mkframe(-8, -16)),
    check_coords({10, 20}, Dev), % not changed because of error
    has_error(true, Dev),
    nts_device:process_frame(Dev, mkframe(-7, -14)),
    check_coords({7, 14}, Dev),
    has_error(false, Dev),
    ok.

fromnow(Offset) ->
    N = os:timestamp(),
    Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(N)) + Offset,
    calendar:gregorian_seconds_to_datetime(Sec).

mkframe(RecOffset, Offset) ->
    #frame{type = location,
           received = fromnow(RecOffset),
           data = #{dtm => fromnow(Offset),
                    latitude => -RecOffset,
                    longitude => -Offset}}.

check_coords(Exp, Dev) ->
    S = nts_device:getstate(Dev),
    ?assertMatch(Exp, nts_location:coords(S)).

has_error(Bool, Dev) ->
    S = nts_device:getstate(Dev),
    E = nts_location:get(status, error, S),
    case Bool of
        true ->
            ?assertNotEqual(undefined, E);
        false ->
            ?assertEqual(undefined, E)
    end.

handler_set_coords(location, Frame, _OldLoc, NewLoc, StateData) ->
    Data = Frame#frame.data,
    Lat = maps:get(latitude, Data),
    Lon = maps:get(longitude, Data),
    {ok, nts_location:coords(Lat, Lon, NewLoc), StateData}.

handler_maybe_error(location, Frame, _OldLoc, NewLoc, StateData) ->
    case nts_frame:get(latitude, Frame) of
        8 -> throw(badmatch);
        _ -> ok
    end,
    {ok, NewLoc, StateData}.
