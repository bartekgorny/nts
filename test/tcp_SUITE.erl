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
        connect_and_terminate,
        connect_and_stop
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
    Socket = start_device(),
    Dev = global:whereis_name(?DEVID),
    State = nts_device:getstate(Dev),
    #loc{} = State,
    gen_tcp:close(Socket),
    timer:sleep(100),
    undefined = global:whereis_name(?DEVID),
    check_event_log(),
    ok.

connect_and_terminate(_) ->
    % shutting down listener closes connections and
    % acceptors and terminates devices
    Socket = start_device(),
    Dev = global:whereis_name(?DEVID),
    ?assertNotEqual(undefined, Dev),
    %% if we stop listener...
    nts_tcp_sup:terminate_listeners(),
    timer:sleep(100),
    undefined = global:whereis_name(?DEVID),
    %% then connection is closed
    {error, closed} = gen_tcp:send(Socket, <<"asfdd">>),
    %% and there is no acceptor
    {error, econnrefused} = gen_tcp:connect("localhost", 12345, []),
    check_event_log(),
    nts_tcp_sup:reload(),
    ok.

connect_and_stop(_) ->
    % stopping or exiting device terminates connection
    Socket = start_device(),
    Dev = global:whereis_name(?DEVID),
    exit(Dev, forced),
    timer:sleep(100),
    Frame1 = <<"a00001,20120307132629,F1,21.290000,52.290000,0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    {error, closed} = gen_tcp:send(Socket, Frame1),
    check_event_log(),
    ok.

%%%===================================================================
%%% helpers
%%%===================================================================

start_device() ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Socket} = gen_tcp:connect("localhost", 12345, []),
    Frame1 = <<"a00001,20120307132629,F1,21.290000,52.290000,0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    Res = gen_tcp:send(Socket, Frame1),
    timer:sleep(200),
    Socket.

check_event_log() ->
    Res = nts_db:event_log(?DEVID, [device, activity], fromnow(-10), fromnow(10)),
    [E0, E1] = lists:sort(Res),
    ?assertEqual([device, activity, up], E0#event.type),
    ?assertEqual([device, activity, down], E1#event.type),
    ok.

