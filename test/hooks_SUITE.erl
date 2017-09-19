%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2017 21:59
%%%-------------------------------------------------------------------
-module(hooks_SUITE).
-author("bartekgorny").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-compile(export_all).

all() ->
    [one_call].

init_per_suite(C) ->
    nts_helpers:set_config(C),
    application:ensure_all_started(nts),
    C.

one_call(_) ->
    Res = nts_hooks:run(dummydev, input_type, input_data, []),
    ?assertEqual([1, 2, 3], lists:reverse(Res)),
    Res2 = nts_hooks:run(formula, input_type, input_data, []),
    ?assertEqual([6, 1, 2, 4, 5], lists:reverse(Res2)),
    Res3 = nts_hooks:run(another, input_type, input_data, []),
    ?assertEqual([1, 2], lists:reverse(Res3)),
    Res4 = nts_hooks:run(crashing, input_type, input_data, []),
    ?assertEqual({error, crashed}, Res4),
    ok.

handle_input(_, _, List) ->
    {ok, [1 | List]}.

handler2(_, _, List) ->
    {ok, [2 | List]}.

handler3(_, _, List) ->
    {ok, [3 | List]}.

handler4(_, _, List) ->
    {ok, [4 | List]}.

handler5(_, _, List) ->
    {ok, [5 | List]}.

handler6(_, _, List) ->
    {ok, [6 | List]}.

stoper(_, _, List) ->
    {stop, List}.

crashit(_, _, _) ->
    crashed.


