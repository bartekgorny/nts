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
    application:stop(nts),
    application:load(nts),
    ConfigPath = nts_helpers:make_filename(Config, "nts.cfg"),
    application:set_env(nts, config, ConfigPath),
    application:ensure_all_started(nts),
    Config.

end_per_suite(_Config) ->
    application:stop(nts).

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
    nts_helpers:change_config(Config, "nts_alt.cfg"),
    ?assertEqual(124, nts_config:get_value(a)),
    ?assertEqual("siedem", nts_config:get_value(b)),
    nts_helpers:change_config(Config, "nts_bad.cfg"),
    ?assertEqual(124, nts_config:get_value(a)),
    ?assertEqual("siedem", nts_config:get_value(b)),
    nts_helpers:change_config(Config, "does_not_exist.cfg"),
    ?assertEqual(124, nts_config:get_value(a)),
    ?assertEqual("siedem", nts_config:get_value(b)),
    ok.


