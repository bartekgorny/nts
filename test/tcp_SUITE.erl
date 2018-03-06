%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2017 17:20
%%%-------------------------------------------------------------------
-module(tcp_SUITE).
-author("bartekgorny").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-compile(export_all).
-define(DEVID, <<"00001">>).

-import(nts_helpers, [fromnow/1]).

all() ->
    [
        connect_and_disconnect,
%%        connect_and_terminate, % not if we use ranch
        connect_and_stop,
        buffering
    ].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    nts_helpers:get_priv_files(),
    C.

init_per_testcase(_CaseName, C) ->
    nts_helpers:clear_tables(["device", "device_00001", "events", "current"]),
    timer:sleep(200), % to make sure tcp starts
    C.

end_per_testcase(_CaseName, _) ->
    case global:whereis_name(?DEVID) of
        undefined -> ok;
        Dev ->
            nts_device:stop(Dev)
    end,
    ok.

end_per_suite(_Config) ->
    application:stop(nts).

%%%===================================================================
%%% tests
%%%===================================================================

connect_and_disconnect(_) ->
    % closing connection terminates device
    nts_helpers:trace_funcs([
        {nts_utils, rebuffer},
        {nts_frame, parse},
        {nts_device, termnate},
        {nts_tcp, maybe_process_frame},
        {nts_device, do_process_frame}
    ]),
    Socket = start_device(),
    Dev = global:whereis_name(?DEVID),
    State = nts_device:getstate(Dev),
    #loc{} = State,
    gen_tcp:close(Socket),
    timer:sleep(500),
    % there is a delay because the process is alive until its terminate returns
    undefined = global:whereis_name(?DEVID),
    check_event_log(),
    ok.

%%% ranch keeps connection open even if acceptors are down
%%connect_and_terminate(_) ->
%%    % shutting down listener closes connections and
%%    % acceptors and terminates devices
%%    Socket = start_device(),
%%    Dev = global:whereis_name(?DEVID),
%%    ?assertNotEqual(undefined, Dev),
%%     if we stop listener...
%%    nts_tcp_sup:terminate_listeners(),
%%    timer:sleep(500),
%%    undefined = global:whereis_name(?DEVID),
%%     then connection is closed
%%    {error, closed} = gen_tcp:send(Socket, <<"asfdd">>),
%%     and there is no acceptor
%%    {error, econnrefused} = gen_tcp:connect("localhost", 12345, []),
%%    check_event_log(),
%%    nts_tcp_sup:reload(),
%%    ok.

connect_and_stop(_) ->
    % stopping or exiting device terminates connection
    Socket = start_device(),
    Dev = global:whereis_name(?DEVID),
    exit(Dev, forced),
    timer:sleep(500),
    Frame1 = <<"a00001,20120307132629,F1,21.290000,52.290000,0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    {error, closed} = gen_tcp:send(Socket, Frame1),
    check_event_log(),
    ok.

buffering(_) ->
    Socket = start_device(),
    timer:sleep(100),
    Dev = global:whereis_name(?DEVID),
    F1 = nts_helpers:mkframe(formula, ?DEVID, -10, {10, 20}),
    ok = gen_tcp:send(Socket, F1),
    assert_lat(Dev, 10.0),
    % prepare half a frame, then the other half with part of the next, then rest
    Fa = nts_helpers:mkframe(formula, ?DEVID, -8, {8, 20}),
    Fb = nts_helpers:mkframe(formula, ?DEVID, -6, {6, 20}),
    M = <<$F>>,
    [Fa1, Fa2] = binary:split(Fa, M),
    [Fb1, Fb2] = binary:split(Fb, M),
    P1 = <<Fa1/binary, M/binary>>,
    P2 = <<Fa2/binary, Fb1/binary>>,
    P3 = <<M/binary, Fb2/binary>>,
    ok = gen_tcp:send(Socket, P1),
    timer:sleep(50),
    assert_lat(Dev, 10.0),
    ok = gen_tcp:send(Socket, P2),
    timer:sleep(50),
    assert_lat(Dev, 8.0),
    ok = gen_tcp:send(Socket, P3),
    timer:sleep(50),
    assert_lat(Dev, 6.0),
    ok.

%%%===================================================================
%%% helpers
%%%===================================================================

start_device() ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Socket} = gen_tcp:connect("localhost", 12345, []),
    Frame1 = <<"a00001,20120307132629,F1,21.290000,52.290000,0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0\n">>,
    ok = gen_tcp:send(Socket, Frame1),
    timer:sleep(100),
    Socket.

check_event_log() ->
    Res = nts_db:event_log(?DEVID, [device, activity], fromnow(-10), fromnow(10)),
    [E0, E1] = lists:sort(Res),
    ?assertEqual([device, activity, up], E0#event.type),
    ?assertEqual([device, activity, down], E1#event.type),
    ok.

assert_lat(Dev, Lat) ->
    timer:sleep(100),
    Loc = nts_device:getstate(Dev),
    {A, _} = nts_location:coords(Loc),
    ?assertEqual(Lat, A).
