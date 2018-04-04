%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2017 20:40
%%%-------------------------------------------------------------------
-module(nts_helpers).
-author("bartekgorny").

%% API
-export([clear_tables/1, set_config/1, change_config/2, make_filename/2]).
-export([get_priv_files/0, fromnow/1]).
-export([compare_near_dates/2, compare_near_dates/3]).
-export([add_handler/1, remove_handler/1, add_handler/2, remove_handler/2]).
-export([trace_funcs/1]).
-export([mkframe/4, rmdev/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nts/src/nts.hrl").
clear_tables([]) -> ok;
clear_tables([T|Rest]) ->
    case nts_db:query("SELECT * FROM " ++ T) of
        {_, []} -> ok;
        _ ->
            nts_db:query("DELETE FROM " ++ T)
    end,
    clear_tables(Rest).

set_config(Config) ->
    ConfigPath = nts_helpers:make_filename(Config, "nts.cfg"),
    application:set_env(nts, config, ConfigPath),
    Config.

change_config(Config, Nf) ->
    ConfigPath = make_filename(Config, Nf),
    application:set_env(nts, config, ConfigPath),
    nts_config:reload().

make_filename(Config, F) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join([DataDir, F]).

get_priv_files() ->
    file:make_dir("priv"),
    file:copy("../../lib/nts/priv/pg_device.sql", "priv/pg_device.sql"),
    ok.

fromnow(Offset) ->
    N = os:timestamp(),
    Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(N)) + Offset,
    calendar:gregorian_seconds_to_datetime(Sec).

compare_near_dates(A, B) ->
    SA = calendar:datetime_to_gregorian_seconds(A),
    SB = calendar:datetime_to_gregorian_seconds(B),
    Dif = abs(SA - SB),
    Dif < 2.

compare_near_dates(neg, A, B) ->
    SA = calendar:datetime_to_gregorian_seconds(A),
    SB = calendar:datetime_to_gregorian_seconds(B),
    Dif = abs(SA - SB),
    Dif > 1.

add_handler(Hook, Handler) ->
    add_handler({Hook, Handler}).

add_handler({Hook, Handler}) ->
    nts_hooks:insert_handler(Hook, Handler).

remove_handler(Hook, Handler) ->
    remove_handler({Hook, Handler}).

remove_handler({Hook, Handler}) ->
    nts_hooks:remove_handler(Hook, Handler).

trace_funcs([]) -> ok;
trace_funcs(ToTrace) ->
    ModReload = lists:map(fun({M, _}) -> code:ensure_loaded(M) end, ToTrace),
    [module = M || {M, _} <- ModReload],
    CallSpec = [{M, F, [{'_', [], [{return_trace}]}]} || {M, F} <- ToTrace],
    {ok, Dev} = file:open("/tmp/trace", [append]),
    file:write(Dev, <<"\n\n">>),
    file:write(Dev, <<"========================================================================">>),
    file:write(Dev, <<"\n\n">>),
    R = recon_trace:calls(CallSpec,
                          100,
                          [{io_server, Dev}, {scope, local}]),
    ct:pal("Spec: ~p, tracing: ~p", [length(ToTrace), R]),
    ct:pal("Tracing: ~p", [ToTrace]),
    ok.

mkframe(formula, DevId, Offset, {Lat, Lon}) ->
    Trail = <<"0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    <<_:5/binary>> = DevId,
    BLat = format_coord(Lat),
    BLon = format_coord(Lon),
    {{Y, M, D}, {H, Mi, S}} = fromnow(Offset),
    Dtm = <<(integer_to_binary(Y))/binary,
            (pad(M))/binary,
            (pad(D))/binary,
            (pad(H))/binary,
            (pad(Mi))/binary,
            (pad(S))/binary>>,
    <<"a", DevId/binary, ",", Dtm/binary, ",F1", ",", BLon/binary, ",",
      BLat/binary, ",", Trail/binary, 10>>.

pad(I) ->
    list_to_binary(
        string:right(integer_to_list(I), 2, $0)).

format_coord(I) when is_integer(I) ->
    format_coord(I, 0);
format_coord(I) when is_float(I) ->
    format_coord(floor(I), I - floor(I)).

format_coord(I, F) ->
    In = string:right(integer_to_list(I), 2, $0),
    Fn = string:left(integer_to_list(F), 6, $0),
    <<(list_to_binary(In))/binary, ".", (list_to_binary(Fn))/binary>>.

rmdev(DevId) ->
    nts_db:delete_device(DevId),
    nts_db:purge_device(DevId),
    ok.
