%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2017 17:20
%%%-------------------------------------------------------------------
-module(redis_SUITE).
-author("bartekgorny").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-compile(export_all).
-define(DEVID, <<"01">>).

-import(nts_helpers, [fromnow/1]).

all() ->
%%    [idle_timeout].
%%all(a) ->
    [
        redis_store
    ].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    nts_helpers:get_priv_files(),
    C.

init_per_testcase(CaseName, C) ->
    nts_helpers:clear_tables(["device", "device_01", "events", "current"]),
    add_handlers(CaseName),
    C.

end_per_testcase(CaseName, _) ->
    case global:whereis_name(?DEVID) of
        undefined -> ok;
        Dev ->
            nts_device:stop(Dev)
    end,
    remove_handlers(CaseName),
    ok.

end_per_suite(_Config) ->
    application:stop(nts).

handlers_for_testcase(_) ->
    [{publish_state, {mod_redis, handle_publishstate, 60}}].

%%%===================================================================
%%% tests
%%%===================================================================


redis_store(_) ->
    ok = nts_db:create_device(?DEVID, formula, <<"razdwatrzy">>),
    {ok, Dev} = nts_device:start_link(?DEVID),
    nts_device:process_frame(Dev, mkframe(-10, -20)),
    D = fromnow(-20),
    Conn = get_conn(),
    Rval = get_json_value(Conn, <<"device-state-01">>),
    #{dtm := Dtm, lat := Lat, data := Data} = Rval,
    ?assertEqual(D, nts_utils:bin2time(Dtm)),
    ?assertEqual(10, Lat),
    ?assertMatch(#{status := #{up := true}}, Data),
    ok.


%%%===================================================================
%%% utils
%%%===================================================================

mkframe(Offset, {Lat, Lon}) ->
    mkframe(Offset, {10, Lat, Lon});
mkframe(Offset, {Sat, Lat, Lon}) ->
    Values = #{devid => ?DEVID,
               dtm => fromnow(Offset),
               latitude => Lat,
               longitude => Lon,
               sat => Sat,
               type => <<"location">>},
    #frame{type = location,
           id = nts_frame:generate_frame_id(),
           device = ?DEVID,
           received = fromnow(Offset),
           values = Values,
           data = nts_utils:json_encode_map(Values)};
mkframe(RecOffset, Offset) ->
    mkframe(RecOffset, Offset, #{}).

mkframe(RecOffset, Offset, Vals) ->
    Values = #{dtm => fromnow(Offset),
               latitude => -RecOffset,
               type => <<"location">>,
               devid => ?DEVID,
               longitude => -Offset},
    V = maps:merge(Values, Vals),
    #frame{type = location,
           id = nts_frame:generate_frame_id(),
           device = ?DEVID,
           received = fromnow(RecOffset),
           values = V,
           data = nts_utils:json_encode_map(V)}.



add_handlers(Case) ->
    Handlers = handlers_for_testcase(Case),
    lists:map(fun nts_helpers:add_handler/1, Handlers).

remove_handlers(Case) ->
    Handlers = handlers_for_testcase(Case),
    lists:map(fun nts_helpers:remove_handler/1, Handlers).

get_conn() ->
    Conf = nts_config:get_value([modules, nts_redis]),
    Host = maps:get(host, Conf),
    Port = maps:get(port, Conf, 6379),
    Db = maps:get(db, Conf, 0),
    Password = maps:get(password, Conf, ""),
    Timeout = maps:get(timeout, Conf, 1000),
    {ok, P} = eredis:start_link(Host, Port, Db, Password, Timeout),
    P.

get_value(Conn, Key) ->
    {ok, Val} = eredis:q(Conn, ["GET", Key]),
    Val.

get_json_value(Conn, Key) ->
    nts_utils:json_decode_map(get_value(Conn, Key)).

