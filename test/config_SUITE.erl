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
%%    [config, reload].
    [modules].

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

modules(Config) ->
    nts_helpers:change_config(Config, "nts.cfg"),
    Ch = supervisor:which_children(nts_modules_sup),
    [{_, P, _, _}] = Ch,
    #{param_a := 123} = gen_server:call(P, getconf),
    nts_helpers:change_config(Config, "nts_alt.cfg"),
    timer:sleep(100),
    Ch2 = supervisor:which_children(nts_modules_sup),
    [_, _] = Ch2,
    {_, P1, _, _} = proplists:lookup(test_mod, Ch2),
    {_, P2, _, _} = proplists:lookup(test_mod_2, Ch2),
    #{param_a := 321} = gen_server:call(P1, getconf),
    #{param_b := 111} = gen_server:call(P2, getconf),
    nts_helpers:change_config(Config, "nts.cfg"),
    timer:sleep(100),
    Ch3 = supervisor:which_children(nts_modules_sup),
    [{_, Px, _, _}] = Ch3,
    #{param_a := 123} = gen_server:call(Px, getconf),
    ok.


