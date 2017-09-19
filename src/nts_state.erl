%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Encapsulates device state which is a quite complex structure and is used
%%% by many modules
%%% @end
%%% Created : 15. Sep 2017 16:37
%%%-------------------------------------------------------------------
-module(nts_state).
-author("bartekgorny").
-include_lib("nts/src/nts.hrl").


-record(state, {devid, device_type, loc = #loc{}, internaldata = #{}}).
-type state() :: #state{devid :: devid(), device_type :: atom(),
                 loc :: loc() | undefined, internaldata :: map()}.

%% API
-export([init/2, devdata/1, getloc/1, setlocdata/3]).

-spec init(devid(), atom()) -> state().
init(DevId, DevType) ->
    #state{devid = DevId, device_type = DevType}.

-spec devdata(state()) -> {devid(), atom()}.
devdata(State) ->
    {State#state.devid, State#state.device_type}.

getloc(State) ->
    State#state.loc.

-spec setlocdata(atom(), term(), state()) -> state().
setlocdata(K, V, State) ->
    Loc = getloc(State),
    M = Loc#loc.data,
    M1 = maps:put(K, V, M),
    State#state{loc = Loc#loc{data = M1}}.


