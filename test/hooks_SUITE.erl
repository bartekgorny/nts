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
    config = #{}}).

-compile(export_all).

all() ->
    [one_call, other_hooks].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    nts_helpers:change_config(C, "nts.cfg"),
    C.

end_per_suite(C) ->
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
    [Int * 2| Acc].

for_something_else(Acc, Int, _) ->
    [Int | Acc].

addtores(I, R) ->
    L = maps:get(res, R),
    maps:put(res, [I | L], R).
