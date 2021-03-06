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

-import(nts_helpers, [fromnow/1]).

all() ->
%%    [event_frames].
%%all(a) ->
    [
        simple_test,
        internal_state,
        failure,
        startstop_events,
        idle_timeout,
        mapping,
        mapping_custom,
        state_recording,
        config,
        sensor_events,
        reprocessing,
        stabiliser,
        floatfilter,
        rewrite_buffered,
        invalid_frame,
        event_frames
    ].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    nts_helpers:get_priv_files(),
    C.

init_per_testcase(CaseName, C) ->
    lists:map(fun nts_helpers:rmdev/1, [<<"01">>, <<"02">>, <<"03">>]),
    nts_helpers:clear_tables(["events", "current"]),
    event_listener:start_link(),
    add_handlers(CaseName),
    C.

end_per_testcase(CaseName, _) ->
    event_listener:flush(),
    case global:whereis_name(?DEVID) of
        undefined -> ok;
        Dev ->
            nts_device:stop(Dev)
    end,
    remove_handlers(CaseName),
    ok.

end_per_suite(_Config) ->
    application:stop(nts).

handlers_for_testcase(simple_test) ->
    [{procloc, {generic, device_SUITE, handler_maybe_error, 30}}];
handlers_for_testcase(failure) ->
    [{save_state, {device_SUITE, maybe_crash_while_saving, 30}}];
handlers_for_testcase(internal_state) ->
    [{procloc, {generic, device_SUITE, handler_trail, 25}}];
handlers_for_testcase(stabiliser) ->
    [{procloc, {generic, mod_stabiliser, 25}}];
handlers_for_testcase(floatfilter) ->
    [{procloc, {generic, mod_floatfilter, 25}}];
handlers_for_testcase(_) ->
    [].

%%%===================================================================
%%% tests
%%%===================================================================


simple_test(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    ok = nts_db:update_device(?DEVID, #{cos => 99}),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    RecDtm = fromnow(-10),
    Dtm = fromnow(-20),
    S = nts_device:getstate(Dev),
    D = maps:get(status, S#loc.data),
    ?assertClose(RecDtm, maps:get(last_signal, D)),
    ?assertClose(Dtm, maps:get(last_signal_dtm, D)),
    ?assertMatch({10, 20}, nts_location:coords(S)),
    has_error(false, Dev),
    nts_device:process_frame(Dev, mkframe(-8, -16)),
    check_coords({10, 20}, Dev), % not changed because of error
    % but timestamps changed
    RecDtm2 = fromnow(-8),
    Dtm2 = fromnow(-16),
    S2 = nts_device:getstate(Dev),
    D2 = maps:get(status, S2#loc.data),
    ?assertClose(RecDtm2, maps:get(last_signal, D2)),
    ?assertClose(Dtm2, maps:get(last_signal_dtm, D2)),
    has_error(true, Dev),
    nts_device:process_frame(Dev, mkframe(-7, -14)),
    check_coords({7, 14}, Dev),
    RecDtm3 = fromnow(-7),
    Dtm3 = fromnow(-14),
    S3 = nts_device:getstate(Dev),
    D3 = maps:get(status, S3#loc.data),
    ?assertClose(RecDtm3, maps:get(last_signal, D3)),
    ?assertClose(Dtm3, maps:get(last_signal_dtm, D3)),
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
    ?assertEqual([7, 9, 10], maps:get(trail, I1)),
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
    catch exit:E ->
        ?assertMatch({error_saving_data, _}, E)
    end,
    process_flag(trap_exit, false),
    ?assertExit({noproc, _}, sys:get_state(Dev)),
    ok.

startstop_events(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    Now0 = fromnow(0),
    nts_device:process_frame(Dev, mkframe(0, -20)),
    timer:sleep(1000),
    nts_device:process_frame(Dev, mkframe(0, -10)),
    timer:sleep(1000),
    Now1 = fromnow(0),
    nts_device:stop(Dev),
    timer:sleep(1000),
    Res = nts_db:event_log(?DEVID, [device, activity], fromnow(-10), fromnow(0)),
    [E0, E1] = Res,
    ?assertClose(Now0, E0#event.dtm),
    ?assertClose(Now1, E1#event.dtm),
    % filter out current state
    FlushRes = [{EType, E} || {EType, #event{} = E} <- event_listener:flush()],
    [{P0Type, P0}, {P1Type, P1}] = FlushRes,
    ?assertClose(Now0, P0#event.dtm),
    ?assertClose(Now1, P1#event.dtm),
    ?assertEqual([device, activity, up], P0Type),
    ?assertEqual([device, activity, down], P1Type),
    ok.

idle_timeout(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(0, -20)),
    process_flag(trap_exit, true),
    timer:sleep(3000),
    process_flag(trap_exit, false),
    ?assertExit({noproc, _}, sys:get_state(Dev)),
    Res = nts_db:event_log(?DEVID, [device, activity], fromnow(-10), fromnow(0)),
    [E0, E1] = Res,
    ?assertEqual([device, activity, up], E0#event.type),
    ?assertEqual([device, activity, down], E1#event.type),
    ok.

mapping(_) ->
    ok = nts_db:create_device(?DEVID, formula, razdwatrzy),
    {ok, Dev} = nts_device:start_link(?DEVID),
    Sensors0 = #{sensor_a => 4,
                 sensor_b => 4,
                 sensor_c => 4},
    nts_device:process_frame(Dev, mkframe(-10, -20, Sensors0)),
    check_sensors(Dev, #{sensor_a => 4,
                         sensor_b => 4,
                         sensor_c => 4}),
    Sensors1 = #{sensor_a => undefined,
                 sensor_b => undefined,
                 sensor_c => undefined},
    nts_device:process_frame(Dev, mkframe(-9, -18, Sensors1)),
    check_sensors(Dev, #{sensor_a => 0,
                         sensor_b => 4,
                         sensor_c => 4}),
    Sensors2 = #{sensor_a => 0,
                 sensor_b => 0,
                 sensor_c => 0},
    nts_device:process_frame(Dev, mkframe(-8, -16, Sensors2)),
    check_sensors(Dev, #{sensor_a => 0,
                         sensor_b => 0,
                         sensor_c => 4}),
    ok.

mapping_custom(_) ->
    ok = nts_db:create_device(?DEVID, formula, razdwatrzy),
    Mappings = #{input_1 => #{name => sensor_a, type => 3},
                 input_2 => #{name => sensor_b, type => 2},
                 sensor_c => #{name => sensor_c, type => 1}},
    ok = nts_db:update_device(?DEVID, #{sensor_mapping => Mappings}),
    {ok, Dev} = nts_device:start_link(?DEVID),
    Sensors0 = #{input_1 => 4,
                 input_2 => 4,
                 sensor_c => 4},
    nts_device:process_frame(Dev, mkframe(-12, -22, Sensors0)),
    check_sensors(Dev, #{sensor_a => 4,
                         sensor_b => 4,
                         sensor_c => 4}),
    Sensors1 = #{input_1 => undefined,
                 input_2 => undefined,
                 sensor_c => undefined},
    nts_device:process_frame(Dev, mkframe(-11, -21, Sensors1)),
    check_sensors(Dev, #{sensor_a => 4,
                         sensor_b => 4,
                         sensor_c => 0}),
    Sensors2 = #{input_1 => 0,
                 input_2 => 0,
                 sensor_c => 0},
    nts_device:process_frame(Dev, mkframe(-10, -20, Sensors2)),
    check_sensors(Dev, #{sensor_a => 4,
                         sensor_b => 0,
                         sensor_c => 0}),
    ok.

state_recording(_) ->
    % check last loc from history
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    RecDtm = fromnow(-10),
    Dtm = fromnow(-20),
    LastLoc = nts_db:last_loc(?DEVID),
    ?assertMatch({10, 20}, nts_location:coords(LastLoc)),
    ?assertClose(RecDtm,
                 nts_utils:bin2time(nts_location:get(status, last_signal, LastLoc))),
    ?assertClose(Dtm,
                 nts_utils:bin2time(nts_location:get(status, last_signal_dtm, LastLoc))),
    CurLoc = nts_db:current_state(?DEVID),
    ?assertMatch({10, 20}, nts_location:coords(CurLoc)),
    ?assertClose(RecDtm,
                 nts_utils:bin2time(nts_location:get(status, last_signal, CurLoc))),
    ?assertClose(Dtm,
                 nts_utils:bin2time(nts_location:get(status, last_signal_dtm, CurLoc))),
    ?assertEqual(true, nts_location:get(status, up, CurLoc)),
    nts_device:stop(Dev),
    CurLoc2 = nts_db:current_state(?DEVID),
    ?assertEqual(false, nts_location:get(status, up, CurLoc2)),
    ok.

config(_) ->
    ok = nts_db:create_device(<<"01">>, xbox, <<"razdwatrzy">>),
    {ok, Dev1} = nts_device:start_link(<<"01">>),
    {normal, State1} = sys:get_state(Dev1),
    ?assertEqual(1, nts_device:get_config_param(testparam, State1)),
    ok = nts_db:create_device(<<"02">>, formula, <<"razdwatrzy">>),
    {ok, Dev2} = nts_device:start_link(<<"02">>),
    {normal, State2} = sys:get_state(Dev2),
    ?assertEqual(2, nts_device:get_config_param(testparam, State2)),
    ok = nts_db:create_device(<<"03">>, xbox, <<"razdwatrzy">>),
    ok = nts_db:update_device(<<"03">>, #{testparam => 3}),
    {ok, Dev3} = nts_device:start_link(<<"03">>),
    {normal, State3} = sys:get_state(Dev3),
    ?assertEqual(3, nts_device:get_config_param(testparam, State3)),
    nts_device:stop(Dev2),
    nts_device:stop(Dev3),
    ok.

sensor_events(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    nts_device:process_frame(Dev, mkframe(-9, -18, #{ignition => 0})),
    nts_device:process_frame(Dev, mkframe(-7, -14, #{ignition => 0})),
    Dtm1 = fromnow(-12),
    nts_device:process_frame(Dev, mkframe(-6, -12, #{ignition => 1})),
    nts_device:process_frame(Dev, mkframe(-5, -10, #{ignition => 1})),
    nts_device:process_frame(Dev, mkframe(-4, -8)),
    CurLoc = nts_device:getstate(Dev),
    ?assertEqual(1, nts_location:get(sensor, ignition, CurLoc)),
    CurLoc1 = nts_db:current_state(?DEVID),
    ?assertEqual(1, nts_location:get(sensor, ignition, CurLoc1)),
    CurLoc2 = nts_db:last_loc(?DEVID),
    ?assertEqual(1, nts_location:get(sensor, ignition, CurLoc2)),
    Dtm2 = fromnow(-6),
    nts_device:process_frame(Dev, mkframe(-3, -6, #{ignition => 0})),
    % now we should have two events
    Res = nts_db:event_log(?DEVID, [device, sensorchange, ignition],
                           fromnow(-20), fromnow(0)),
    [Eon, Eoff] = Res,
    ?assertClose(Dtm1, Eon#event.dtm),
    ?assertEqual(1, maps:get(value, Eon#event.data)),
    ?assertEqual(Dtm2, Eoff#event.dtm),
    ?assertEqual(0, maps:get(value, Eoff#event.data)),
    ok.

reprocessing(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-15, -50)),
    nts_device:process_frame(Dev, mkframe(-9, -18, #{ignition => 0})),
    nts_device:process_frame(Dev, mkframe(-7, -14, #{ignition => 0})),
    nts_device:process_frame(Dev, mkframe(-6, -12, #{ignition => 1})),
    nts_device:process_frame(Dev, mkframe(-5, -10, #{ignition => 1})),
    nts_device:process_frame(Dev, mkframe(-4, -8)),
    LocationHistory = nts_db:history(?DEVID, fromnow(-20), fromnow(0)),
    EventHistory = nts_db:event_log(?DEVID, [device],
                                    fromnow(-20), fromnow(0)),
    CurLoc1 = nts_device:getstate(Dev),
    CurLoc2 = nts_db:current_state(?DEVID),
    CurLoc3 = nts_db:last_loc(?DEVID),
    event_listener:flush(),
    % and now, ladies and gentlemen:
    nts_device:reprocess_data(Dev, fromnow(-30)),
    % since we didn't change config we should get exactly the same result
    compare_lh(LocationHistory, nts_db:history(?DEVID, fromnow(-20), fromnow(0))),
    compare_eh(EventHistory, nts_db:event_log(?DEVID, [device],
                                              fromnow(-20), fromnow(0))),
    compare_loc(CurLoc1, nts_device:getstate(Dev)),
    compare_loc(CurLoc2, nts_db:current_state(?DEVID)),
    compare_loc(CurLoc3, nts_db:last_loc(?DEVID)),
    % there should be only one event: new current state after reprocessing
    [{current_state, ?DEVID}] = event_listener:flush(),
    ok.

stabiliser(_) ->
    % just to see it works, details are tested in toolset_SUITE
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    send_and_check(Dev, ?DEVID, -10, {1, 1}, {1, 1}),
    send_and_check(Dev, ?DEVID, -8, {1.0001, 1.0001}, {1.0001, 1.0001}),
    send_and_check(Dev, ?DEVID, -6, {2, 2}, {1.0001, 1.0001}),
    send_and_check(Dev, ?DEVID, -4, {1.0002, 1.0002}, {1.0002, 1.0002}),
    ok.

floatfilter(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    send_and_check(Dev, ?DEVID, -20, {1, 1}, {1, 1}),
    send_and_check(Dev, ?DEVID, -19, {1, 2}, {1, 2}),
    send_and_check(Dev, ?DEVID, -18, {1, 2.0001}, {1, 2}), % are we moving?
    send_and_check(Dev, ?DEVID, -17, {1, 3}, {1, 3}), % yes
    send_and_check(Dev, ?DEVID, -16, {1, 4}, {1, 4}),
    send_and_check(Dev, ?DEVID, -15, {1, 4.00001}, {1, 4}),
    send_and_check(Dev, ?DEVID, -14, {1, 3.99999}, {1, 4}),
    send_and_check(Dev, ?DEVID, -13, {1, 4.00001}, {1, 4}),
    send_and_check(Dev, ?DEVID, -12, {1, 3.99999}, {1, 4}),
    send_and_check(Dev, ?DEVID, -11, {1, 4.00001}, {1, 4}),
    send_and_check(Dev, ?DEVID, -10, {1, 3.99999}, {1, 4}), % stopped
    send_and_check(Dev, ?DEVID, -9, {1, 4.00001}, {1, 4}),
    send_and_check(Dev, ?DEVID, -8, {1, 3.99999}, {1, 4}),
    send_and_check(Dev, ?DEVID, -7, {1, 5}, {1, 5}), % started moving
    send_and_check(Dev, ?DEVID, -6, {1, 6}, {1, 6}),
    LocationHistory = nts_db:history(?DEVID, fromnow(-25), fromnow(0)),
    ExpectedHistory = [{1, 1},
                       {1, 2},
                       {1, 2.0001}, % should have been fixed
                       {1, 3},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 4},
                       {1, 5},
                       {1, 6}],
    verify_history(LocationHistory, ExpectedHistory),
    Events = event_listener:flush(),
    Ev = [E || {[device, moving, _], E} <- Events],
    [Stopped, Moved] = Ev,
    ?assertEqual([device, moving, stopped], Stopped#event.type),
    ?assertEqual(fromnow(-10), Stopped#event.dtm),
    ?assertEqual(4, Stopped#event.lon),
    ?assertEqual([device, moving, started], Moved#event.type),
    ?assertEqual(fromnow(-7), Moved#event.dtm),
    ?assertEqual(5, Moved#event.lon),
    ok.

rewrite_buffered(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-9, -18, #{ignition => 0})),
    check_ign(?DEVID, Dev, 9, 0),
    nts_device:process_frame(Dev, mkframe(-7, -14)),
    nts_device:process_frame(Dev, mkframe(-6, -16, #{ignition => 1})),
    nts_device:process_frame(Dev, mkframe(-6, -20)),
    check_ign(?DEVID, Dev, 7, 0), % older frames were ignored
    [Ev] = flush_device_events(),
    check_event({9, 18, [device, activity, up]}, Ev),
    timer:sleep(1000),
    nts_device:process_frame(Dev, mkframe(-5, -10)),
    timer:sleep(1000),
    nts_device:process_frame(Dev, mkframe(-4, -8)),
    timer:sleep(1000),
    nts_device:process_frame(Dev, mkframe(-3, -6)),
    [] = flush_device_events(),
    check_ign(?DEVID, Dev, 3, 1), % rewrote history and took the '6, -16'
                                  % frame into account
    ExpHist = [{6, 20, false, undefined},
               {9, 18, true, 0},
               {6, 16, false, 1},
               {7, 14, true, 1},
               {5, 10, true, 1},
               {4, 8, true, 1},
               {3, 6, true, 1}],
    check_rewritten_history(ExpHist,
                            nts_db:full_history(?DEVID,
                                                fromnow(-30),
                                                fromnow(0))),
    [E1, E2] = nts_db:event_log(?DEVID, [device], fromnow(-30), fromnow(0)),
    check_event({6, 16, [device, sensorchange, ignition]}, E1),
    check_event({9, 18, [device, activity, up]}, E2),
    ok.

invalid_frame(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    nts_device:process_frame(Dev, mkframe(-8, {1, 3, 3})),
    check_coords({10, 20}, Dev), % not changed because of not enough sat
    % but timestamps changed
    RecDtm2 = fromnow(-8),
    Dtm2 = fromnow(-8),
    S2 = nts_device:getstate(Dev),
    D2 = maps:get(status, S2#loc.data),
    ?assertClose(RecDtm2, maps:get(last_signal, D2)),
    ?assertClose(Dtm2, maps:get(last_signal_dtm, D2)),
    has_error(false, Dev),
    ok.

event_frames(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    nts_device:process_frame(Dev, mkeventframe(-10, -12, <<"alert">>)),
    nts_device:process_frame(Dev, mkeventframe(-5, -7, <<"alert">>)),
    [Evt1, Evt2] = nts_db:event_log(?DEVID, [device, alert],
                           fromnow(-20), fromnow(0)),
    ?assertEqual([device, alert], Evt1#event.type),
    ?assertEqual([device, alert], Evt2#event.type),
    ?assertEqual(10, Evt2#event.lat),
    ok.

%%%===================================================================
%%% utils
%%%===================================================================

check_sensors(Dev, Exp) ->
    Loc = nts_device:getstate(Dev),
    lists:map(fun({K, V}) -> ?assertEqual(V, nts_location:get(sensor, K, Loc)) end,
        maps:to_list(Exp)).

mkframe(Offset, {Lat, Lon}) ->
    mkframe(Offset, Offset, {10, Lat, Lon}, #{});
mkframe(Offset, {Sat, Lat, Lon}) ->
    mkframe(Offset, Offset, {Sat, Lat, Lon}, #{});
mkframe(RecOffset, Offset) ->
    mkframe(RecOffset, Offset, #{}).

mkframe(RecOffset, Offset, {Sat, Lat, Lon}, Vals) ->
    Values = #{devid => ?DEVID,
               dtm => fromnow(Offset),
               latitude => Lat,
               longitude => Lon,
               sat => Sat,
               type => <<"location">>},
    V = maps:merge(Values, Vals),
    #frame{type = location,
           id = nts_frame:generate_frame_id(),
           device = ?DEVID,
           received = fromnow(RecOffset),
           values = V,
           data = nts_utils:json_encode_map(V)}.

mkframe(RecOffset, Offset, Vals) ->
    % this one is the most used
    mkframe(RecOffset, Offset, {10, -RecOffset, -Offset}, Vals).

mkeventframe(RecOffset, Offset, Type) ->
    mkeventframe(RecOffset, Offset, Type, #{}).

mkeventframe(RecOffset, Offset, Type, Vals) ->
    mkeventframe(RecOffset, Offset, event, Type, Vals).

mkeventframe(RecOffset, Offset, FrameType, Type, Vals) ->
    Values = #{devid => ?DEVID,
               dtm => fromnow(Offset),
               type => Type},
    V = maps:merge(Values, Vals),
    #frame{type = FrameType,
           id = nts_frame:generate_frame_id(),
           device = ?DEVID,
           received = fromnow(RecOffset),
           values = V,
           data = nts_utils:json_encode_map(V)}.

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

compare_loc(A, B) ->
    ?assertEqual(A#loc.lat, B#loc.lat),
    ?assertEqual(A#loc.lon, B#loc.lon),
    ?assertEqual(A#loc.dtm, B#loc.dtm),
    ?assertEqual(A#loc.dtm, B#loc.dtm),
    ?assertEqual(A#loc.data, B#loc.data).

compare_lh([], []) ->
    ok;
compare_lh([], _) ->
    throw(length_mismatch);
compare_lh(_, []) ->
    throw(length_mismatch);
compare_lh([A|Atail], [B|Btail]) ->
    compare_loc(A, B),
    compare_lh(Atail, Btail).

compare_event(A, B) ->
    ?assertEqual(A#event.device, B#event.device),
    ?assertEqual(A#event.dtm, B#event.dtm),
    ?assertEqual(A#event.lat, B#event.lat),
    ?assertEqual(A#event.lon, B#event.lon),
    ?assertEqual(A#event.data, B#event.data),
    ?assertEqual(A#event.type, B#event.type).

compare_eh([], []) ->
    ok;
compare_eh([], _) ->
    throw(length_mismatch);
compare_eh(_, []) ->
    throw(length_mismatch);
compare_eh([A|Atail], [B|Btail]) ->
    compare_event(A, B),
    compare_eh(Atail, Btail).

add_handlers(Case) ->
    Handlers = handlers_for_testcase(Case),
    lists:map(fun nts_helpers:add_handler/1, Handlers).

remove_handlers(Case) ->
    Handlers = handlers_for_testcase(Case),
    lists:map(fun nts_helpers:remove_handler/1, Handlers).

send_and_check(Dev, DevId, Offset, Loc, Exp) ->
    nts_device:process_frame(Dev, mkframe(Offset, Loc)),
    Res = nts_location:coords(nts_db:current_state(DevId)),
    ?assertEqual(Exp, Res).

verify_history([], []) ->
    ok;
verify_history([Loc|LocTail], [Exp|ExpTail]) ->
    ?assertEqual(Exp, nts_location:coords(Loc)),
    verify_history(LocTail, ExpTail).

check_ign(DevId, Dev, ExpLon, ExpIgn) ->
    CState = nts_device:getstate(Dev),
    DBState = nts_db:current_state(DevId),
    check_state(ExpLon, ExpIgn, CState),
    check_state(ExpLon, ExpIgn, DBState).

check_state(ExpLat, ExpIgn, St) ->
    {Lat, _} = nts_location:coords(St),
    ?assertEqual(ExpLat, Lat),
    ?assertEqual(ExpIgn, nts_location:get(sensor, ignition, St)),
    ok.


check_rewritten_history([], []) ->
    ok;
check_rewritten_history([], _) ->
    ct:fail("mismatched length");
check_rewritten_history(_, []) ->
    ct:fail("mismatched length");
check_rewritten_history([Exp|ETail], [Act|ActTail]) ->
    check_match(Exp, Act),
    check_rewritten_history(ETail, ActTail).

check_match({Lat, Lon, Up, Ign}, {_, Loc}) ->
    ?assertEqual({Lat, Lon}, nts_location:coords(Loc)),
    ?assertEqual(Up, nts_location:get(status, up, Loc)),
    ?assertEqual(Ign, nts_location:get(sensor, ignition, Loc)),
    ok.

check_event({Lat, Lon, EType}, Evt) ->
    ?assertEqual(Lat, Evt#event.lat),
    ?assertEqual(Lon, Evt#event.lon),
    ?assertEqual(EType, Evt#event.type),
    ok.

flush_device_events() ->
    Events = event_listener:flush(),
    [E || {[device|_], E} <- Events].

