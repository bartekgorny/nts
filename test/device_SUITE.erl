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
    [simple_test, internal_state, failure].
%%    [simple_test].
%%    [internal_state].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    C.

init_per_testcase(_, C) ->
    nts_helpers:clear_tables(["device", "device_01"]),
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
    % but timestamps changed
    RecDtm2 = fromnow(-8),
    Dtm2 = fromnow(-16),
    S2 = nts_device:getstate(Dev),
    D2 = maps:get(status, S2#loc.data),
    ?assertMatch(#{last_signal := RecDtm2, last_signal_dtm := Dtm2 }, D2),
    has_error(true, Dev),
    nts_device:process_frame(Dev, mkframe(-7, -14)),
    check_coords({7, 14}, Dev),
    RecDtm3 = fromnow(-7),
    Dtm3 = fromnow(-14),
    S3 = nts_device:getstate(Dev),
    D3 = maps:get(status, S3#loc.data),
    ?assertMatch(#{last_signal := RecDtm3, last_signal_dtm := Dtm3 }, D3),
    has_error(false, Dev),
    ok.

internal_state(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    nts_device:process_frame(Dev, mkframe(-9, -18)),
    nts_device:process_frame(Dev, mkframe(-7, -14)),
    % hook handler accumulates data in internal state
    I = nts_device:getstate(Dev, internal),
    ?assertEqual([7, 9, 10], maps:get(trail, I)),
    nts_device:stop(Dev),
    timer:sleep(100),
    {ok, Dev1} = nts_device:start_link(?DEVID),
    Loc1 = nts_device:getstate(Dev1),
    ?assertEqual({7, 14}, nts_location:coords(Loc1)),
    I1 = nts_device:getstate(Dev1, internal),
    ?assertEqual([7, 9, 10], maps:get(<<"trail">>, I1)),
    % reset
    nts_device:reset(Dev1),
    I1a = nts_device:getstate(Dev1, internal),
    ?assertEqual(#{}, I1a),
    nts_device:stop(Dev1),
    {ok, Dev2} = nts_device:start_link(?DEVID),
    Loc2 = nts_device:getstate(Dev2),
    ?assertEqual({7, 14}, nts_location:coords(Loc2)),
    I2 = nts_device:getstate(Dev2, internal),
    ?assertEqual(#{}, I2),
    ok.

failure(_) ->
    % make sure error while saving loc kill the device
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    nts_device:process_frame(Dev, mkframe(-9, -18)),
    process_flag(trap_exit, true),
    try nts_device:process_frame(Dev, mkframe(-1, -2))
    catch _:_ -> ok end,
    process_flag(trap_exit, false),
    ?assertExit({noproc, _}, sys:get_state(Dev)),
    ok.

fromnow(Offset) ->
    N = os:timestamp(),
    Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(N)) + Offset,
    calendar:gregorian_seconds_to_datetime(Sec).

mkframe(RecOffset, Offset) ->
    #frame{type = location,
           device = ?DEVID,
           received = fromnow(RecOffset),
           values = #{dtm => fromnow(Offset),
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

handler_set_coords(location, Frame, _OldLoc, NewLoc, Internal, _State) ->
    Data = Frame#frame.values,
    Lat = maps:get(latitude, Data),
    Lon = maps:get(longitude, Data),
    {ok, nts_location:coords(Lat, Lon, NewLoc), Internal}.

handler_maybe_error(location, Frame, _OldLoc, NewLoc, Internal, _State) ->
    case nts_frame:get(latitude, Frame) of
        8 -> throw(badmatch);
        _ -> ok
    end,
    {ok, NewLoc, Internal}.

handler_trail(location, Frame, _OldLoc, NewLoc, Internal, _State) ->
    Trail = maps:get(trail, Internal, []),
    Lat = maps:get(latitude, Frame#frame.values),
    Internal1 = maps:put(trail, [Lat | Trail], Internal),
    {ok, NewLoc, Internal1}.

maybe_crash_while_saving(Acc, DevId, Loc, _Frame, _Internal) ->
    case Loc#loc.lat of
        1 -> Acc = DevId; % trigger badmatch
        _  -> ok
    end,
    {ok, Acc}.
