%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2017 22:41
%%%-------------------------------------------------------------------
-module(db_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-define(DEVID, <<"01">>).

all() ->
%%    [current_state].
    [locs_and_frames, frames_and_updates, current_state, concurrency,
     errors, metrics, events].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:clear_tables(["device_01", "current", "events"]),
    C.

locs_and_frames(_) ->
    L1 = generate_location(-20),
    F1 = <<1, 99, 123>>,
    R1 = fromnow(-3),
    nts_db:save_loc(?DEVID, L1, #frame{hex = true, data = F1, received = R1}),
    L2 = generate_location(-18),
    Dtm2 = L2#loc.dtm,
    F2 = <<"Frame01">>,
    R2 = fromnow(-2),
    nts_db:save_loc(?DEVID, L2, #frame{hex = false, data = F2, received = R2}),
    % last location from history
    LastDirect = nts_db:last_loc(?DEVID, Dtm2),
    ?assertEqual(Dtm2, LastDirect#loc.dtm),
    ?assertEqual(18, LastDirect#loc.lat),
    LastIndirect = nts_db:last_loc(?DEVID, fromnow(-10)),
    ?assertEqual(Dtm2, LastIndirect#loc.dtm),
    ?assertEqual(18, LastIndirect#loc.lat),
    Null = nts_db:last_loc(?DEVID, fromnow(-22)),
    ?assertEqual(undefined, Null#loc.dtm),
    ?assertEqual(undefined, Null#loc.lat),
    % history
    Hist = nts_db:history(?DEVID, fromnow(-25), fromnow(-17)),
    [H1, H2] = Hist,
    ?assertEqual(20, H1#loc.lat),
    ?assertEqual(40, H1#loc.lon),
    D = H1#loc.data,
    ?assertEqual(-20, maps:get(<<"offset">>, D)),
    ?assertEqual(18, H2#loc.lat),
    ?assertEqual(36, H2#loc.lon),
    % frames
    Res = nts_db:frames(?DEVID, fromnow(-5), fromnow(0)),
    ?assertEqual(2, length(Res)),
    [A, B] = Res,
    ?assertEqual(true, A#frame.hex),
    ?assertEqual(<<1, 99, 123>>, A#frame.data),
    ?assertEqual(R1, A#frame.received),
    ?assertEqual(false, B#frame.hex),
    ?assertEqual(<<"Frame01">>, B#frame.data),
    ?assertEqual(R2, B#frame.received),
    ok.

frames_and_updates(_) ->
    F1 = #frame{hex = false, data = <<"aaa">>, received = fromnow(-30)},
    nts_db:save_frame(?DEVID, F1),
    [F] = nts_db:frames(?DEVID, fromnow(-32), fromnow(-28)),
    L = generate_location(-31),
    nts_db:update_loc(?DEVID, F#frame.id, L),
    [L1] = nts_db:history(?DEVID, fromnow(-33), fromnow(-30)),
    ?assertEqual(-31, maps:get(<<"offset">>, L1#loc.data)),
    ok.

current_state(_) ->
    L1 = generate_location(-10),
    nts_db:update_state(?DEVID, L1), % does insert
    S1 = nts_db:current_state(?DEVID),
    ?assertEqual(10, S1#loc.lat),
    ?assertEqual(20, S1#loc.lon),
    D = S1#loc.data,
    ?assertEqual(-10, maps:get(<<"offset">>, D)),
    [{?DEVID, S2}] = nts_db:current_state([?DEVID]),
    ?assertEqual(10, S2#loc.lat),
    ?assertEqual(20, S2#loc.lon),
    D2 = S2#loc.data,
    ?assertEqual(-10, maps:get(<<"offset">>, D2)),
    [{?DEVID, S2}] = nts_db:current_state([?DEVID, <<"asdf">>]),
    %%
    L3 = generate_location(-9),
    nts_db:update_state(?DEVID, L3), % does update
    S3 = nts_db:current_state(?DEVID),
    ?assertEqual(9, S3#loc.lat),
    ?assertEqual(18, S3#loc.lon),
    D3 = S3#loc.data,
    ?assertEqual(-9, maps:get(<<"offset">>, D3)),
    ok.

concurrency(_) ->
    % to prove that many concurrent processes can write at the same time
    L1 = generate_location(-1),
    F1 = #frame{hex = false, data = <<"abc">>, received = fromnow(-1)},
    F = fun() -> nts_db:save_loc(?DEVID, L1, F1) end,
    Count = 200,
    lists:map(fun(_) -> spawn(F) end, lists:seq(1, Count)),
    % a brief pause so that all processes can send their queries before we do
    timer:sleep(100),
    Res = nts_db:query("SELECT count(*) FROM device_01 WHERE frame='abc'"),
    {_, [{Br}]} = Res,
    ?assertEqual(Count, binary_to_integer(Br)),
    ok.

errors(_) ->
    {error, _} = nts_db:history(<<"00">>, fromnow(-25), fromnow(-17)),
    {error, _} = nts_db:frames(<<"00">>, fromnow(-25), fromnow(-17)),
    L1 = generate_location(-1),
    F1 = #frame{hex = false, data = <<"abc">>, received = fromnow(-1)},
    {error, _} = nts_db:save_loc(<<"00">>, L1, F1),
    {error, _} = nts_db:save_frame(<<"00">>, F1),
    undefined = nts_db:current_state(<<"00">>),
    {error, _} = nts_db:update_loc(?DEVID, -1, L1),
    {error, _} = nts_db:last_loc(<<"00">>, fromnow(-1)),
    ok.

metrics(_) ->
    Mets = [[db, ops], [db, failed_ops]],
    Start = get_metric_values(Mets),
    nts_db:history(?DEVID, fromnow(-25), fromnow(-17)),
    nts_db:history(<<"00">>, fromnow(-25), fromnow(-17)),
    Stop = get_metric_values(Mets),
    ?assertEqual([2, 2, 1, 1], metric_dif(Start, Stop)).

events(_) ->
    Loc = generate_location(-5),
    Evt = #event{dtm = Loc#loc.dtm, device = ?DEVID, type = [event, sample],
        lat = Loc#loc.lat, lon = Loc#loc.lon, data = Loc#loc.data},
    nts_db:save_event(Evt),
    [E1] = nts_db:event_log(?DEVID, [event, sample], fromnow(-10), fromnow(0)),
    ?assertEqual(?DEVID, E1#event.device),
    ?assertEqual(Loc#loc.dtm, E1#event.dtm),
    ?assertEqual(Loc#loc.lat, E1#event.lat),
    ?assertEqual(Loc#loc.lon, E1#event.lon),
    ?assertEqual(-5, maps:get(<<"offset">>, E1#event.data)),
    ?assertEqual([event, sample], E1#event.type),
    ok = nts_db:delete_events(?DEVID, fromnow(-10), fromnow(0)),
    [] = nts_db:event_log(?DEVID, [event, sample], fromnow(-10), fromnow(0)),
    ok.

%%%%%%%%%%%%%%%%

generate_location(Offset) ->
    D = fromnow(Offset),
    #loc{dtm = D, lat = -Offset, lon = -Offset * 2, data = #{offset => Offset}}.

fromnow(Offset) ->
    N = os:timestamp(),
    Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(N)) + Offset,
    calendar:gregorian_seconds_to_datetime(Sec).

get_metric_values(MList) ->
    lists:flatten(lists:map(fun(M) -> [nts_metrics:count(M), nts_metrics:one(M)] end, MList)).

metric_dif(Start, Stop) ->
    lists:map(fun({{ok, A}, {ok, B}}) -> B - A end, lists:zip(Start, Stop)).
