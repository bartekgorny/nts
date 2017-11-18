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
