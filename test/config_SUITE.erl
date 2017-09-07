%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2017 22:41
%%%-------------------------------------------------------------------
-module(config_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").


all() ->
    [config, reload].

init_per_suite(Config) ->
    ConfigPath = make_filename(Config, "nts.cfg"),
    application:set_env(nts, config, ConfigPath),
    application:ensure_all_started(nts),
    Config.

config(_Config) ->
    ?assertEqual(123, nts_config:get_value(a)),
    ?assertEqual(undefined, nts_config:get_value(b)),
    ?assertEqual(1, nts_config:get_value([nested, first])),
    ?assertEqual(2, nts_config:get_value([nested, second])),
    ?assertEqual(undefined, nts_config:get_value([nested, brak])),
    ?assertEqual(31, nts_config:get_value([nested, third, one])),
    ?assertEqual(undefined, nts_config:get_value([nested, third, missing])),
    ?assertEqual(undefined, nts_config:get_value([nested, third, one, more])),
    ok.

reload(Config) ->
    ?assertEqual(123, nts_config:get_value(a)),
    ?assertEqual(undefined, nts_config:get_value(b)),
    change_config(Config, "nts_alt.cfg"),
    ?assertEqual(124, nts_config:get_value(a)),
    ?assertEqual("siedem", nts_config:get_value(b)),
    change_config(Config, "nts_bad.cfg"),
    ?assertEqual(124, nts_config:get_value(a)),
    ?assertEqual("siedem", nts_config:get_value(b)),
    change_config(Config, "does_not_exist.cfg"),
    ?assertEqual(124, nts_config:get_value(a)),
    ?assertEqual("siedem", nts_config:get_value(b)),
    ok.

change_config(Config, Nf) ->
    ConfigPath = make_filename(Config, Nf),
    application:set_env(nts, config, ConfigPath),
    nts_config:reload().

make_filename(Config, F) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join([DataDir, F]).

