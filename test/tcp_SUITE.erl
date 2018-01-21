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
%%    [].
%%all(a) ->
    [
        connect
    ].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    nts_helpers:get_priv_files(),
    C.

init_per_testcase(_CaseName, C) ->
    nts_helpers:clear_tables(["device", "device_01", "events", "current"]),
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


connect(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Socket} = gen_tcp:connect("localhost", 12345, []),
    Frame1 = <<"a00001,20120307132629,F1,21.290000,52.290000,0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    gen_tcp:send(Socket, Frame1),
    timer:sleep(100),
    Dev = global:whereis_name(?DEVID),
    ct:pal("Dev: ~p", [{?DEVID, Dev}]),
    State = nts_device:getstate(Dev),
    #loc{} = State,
    ct:pal("State: ~p", [State]),
    gen_tcp:close(Socket),
    timer:sleep(100),
    undefined = global:whereis_name(?DEVID),
    ok.


