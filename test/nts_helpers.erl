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
-export([get_priv_files/0]).

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
