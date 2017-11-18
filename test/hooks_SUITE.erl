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

-define(DEVID, <<"01">>).

% copied from nts_device
-record(state, {devid, device_type, label, loc = #loc{}, internaldata = #{},
                config = #{}, up = false}).

-compile(export_all).

all() ->
    [one_call, other_hooks, dynamic_procloc, dynamic_other].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    C.

end_per_suite(_C) ->
    application:stop(nts).

one_call(_) ->
    Internal = #{res => []},
    State = #state{devid = ?DEVID},
    {newloc, Res} = nts_hooks:run_procloc(dummydev, input_type, input_data, oldloc,
                                          newloc, Internal, State),
    ?assertEqual([1, 2, 3], lists:reverse(maps:get(res, Res))),
    {newloc, Res2} = nts_hooks:run_procloc(formula, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([6, 1, 2, 4, 5], lists:reverse(maps:get(res, Res2))),
    {newloc, Res3} = nts_hooks:run_procloc(another, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([1, 2], lists:reverse(maps:get(res, Res3))),
    Res4 = nts_hooks:run_procloc(crashing, input_type, input_data, oldloc,
                                 newloc, Internal, State),
    ?assertEqual({error, crashed}, Res4),
    ok.

other_hooks(_) ->
    Res = nts_hooks:run(something, [], [123, 4]),
    ?assertEqual([246, 123], Res).

dynamic_procloc(_) ->
    % it is a bit complicated: when we start/reload hooks every device type gets its own
    % dev-specific handlers merged with 'generic' handlers; if a device type does not have
    % its own handlers it uses generics.
    % however if we manipulate them directly we operate on those device-type-specific sets
    Internal = #{res => []},
    State = #state{devid = ?DEVID},
    % generic handlers for dev which has no own handlers
    {newloc, Res} = nts_hooks:run_procloc(dummydev, input_type, input_data, oldloc,
                                          newloc, Internal, State),
    ?assertEqual([1, 2, 3], lists:reverse(maps:get(res, Res))),
    nts_helpers:add_handler(procloc, {generic, hooks_SUITE, handler3, 55}),
    {newloc, Res1} = nts_hooks:run_procloc(dummydev, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([1, 3, 2, 3], lists:reverse(maps:get(res, Res1))),
    nts_helpers:remove_handler(procloc, {generic, hooks_SUITE, handler3, 55}),
    {newloc, Res2} = nts_hooks:run_procloc(dummydev, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([1, 2, 3], lists:reverse(maps:get(res, Res2))),
    nts_helpers:add_handler(procloc, {dummydev, hooks_SUITE, handler3, 55}),
    {newloc, Res3} = nts_hooks:run_procloc(dummydev, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    % !!! now dummydev has only ONE handler, because it does not use 'generic' handlers anymore
    ?assertEqual([3], lists:reverse(maps:get(res, Res3))),
    nts_helpers:remove_handler(procloc, {dummydev, hooks_SUITE, handler3, 55}),
    {newloc, Res4} = nts_hooks:run_procloc(dummydev, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([1, 2, 3], lists:reverse(maps:get(res, Res4))),
    % specific handlers for formula
    {newloc, Res5} = nts_hooks:run_procloc(formula, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([6, 1, 2, 4, 5], lists:reverse(maps:get(res, Res5))),
    nts_helpers:remove_handler(procloc, {formula, hooks_SUITE, handler4, 65}),
    {newloc, Res6} = nts_hooks:run_procloc(formula, input_type, input_data, oldloc,
                                           newloc, Internal, State),
    ?assertEqual([6, 1, 2, 5], lists:reverse(maps:get(res, Res6))),
    ok.

dynamic_other(_) ->
    Res = nts_hooks:run(something, [], [123, 4]),
    ?assertEqual([246, 123], Res),
    nts_helpers:add_handler(something, {hooks_SUITE, for_something, 55}),
    Res1 = nts_hooks:run(something, [], [12, 4]),
    ?assertEqual([24, 24, 12], Res1),
    nts_helpers:remove_handler(something, {hooks_SUITE, for_something_else, 50}),
    Res2 = nts_hooks:run(something, [], [12, 4]),
    ?assertEqual([24, 24], Res2),
    ok.

handle_input(_, _, _, NewLoc, St, _State) ->
    {ok, NewLoc, addtores(1, St)}.

handler2(_, _, _, NewLoc, St, _State) ->
    {ok, NewLoc, addtores(2, St)}.

handler3(_, _, _, NewLoc, St, _State) ->
    {ok, NewLoc, addtores(3, St)}.

handler4(_, _, _, NewLoc, St, _State) ->
    {ok, NewLoc, addtores(4, St)}.

handler5(_, _, _, NewLoc, St, _State) ->
    {ok, NewLoc, addtores(5, St)}.

handler6(_, _, _, NewLoc, St, _State) ->
    {ok, NewLoc, addtores(6, St)}.

stoper(_, _, _, NewLoc, St, _State) ->
    {stop, NewLoc, St}.

crashit(_, _, _, _, _, _State) ->
    crashed.

for_something(Acc, Int, _) ->
    {ok, [Int * 2| Acc]}.

for_something_else(Acc, Int, _) ->
    {ok, [Int | Acc]}.

addtores(I, R) ->
    L = maps:get(res, R),
    maps:put(res, [I | L], R).
