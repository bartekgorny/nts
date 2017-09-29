%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Basic logic: this fsm receives new frames, usu. from a socket opened for
%%% a device. Upon receiving a frame it:
%%% * calls hooks to determine state changes
%%% * saves frame with the new state
%%% * calls location publishers
%%%
%%% If if receives a buffered frame (older than the most recent one) it
%%% enters a 'suspended' state - incoming frames are buffered locally.
%%% When buffered frames stop coming in the fsm it removes events from that
%%% period and reruns data processing (not publishing locations and events).
%%% When it is done it processes buffered frames (with publishing) and
%%% goes back to the normal state. (I think we need the third state, whereby
%%% it buffers incoming frames but publishes events).
%%% @end
%%% Created : 14. Sep 2017 22:41
%%%-------------------------------------------------------------------
-module(nts_device).
-author("bartekgorny").

-behaviour(gen_fsm).
-include_lib("nts/src/nts.hrl").

-record(state, {devid, device_type, loc = #loc{}, internaldata = #{}}).
-type state() :: #state{devid :: devid(), device_type :: atom(),
                        loc :: loc() | undefined, internaldata :: map()}.

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
         normal/2,
         normal/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([process_frame/2, getstate/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(devid()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(DevId) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [DevId], []).

process_frame(Pid, Frame) ->
    gen_fsm:sync_send_event(Pid, Frame).

getstate(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_state).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([DevId]) ->
    {_, DType, Label, Config} = nts_db:read_device(DevId),
    Internal = maps:put(<<"label">>, Label, Config),
    {ok, normal, #state{devid = DevId, device_type = DType,
                        internaldata = Internal}}.

normal(_Event, State) ->
    {next_state, normal, State}.

normal(Event, _From, State) ->
    case nts_hooks:run_procloc(State#state.device_type,
                               Event#frame.type,
                               Event,
                               State#state.loc,
                               nts_location:new(),
                               State#state.internaldata) of
        {error, _} ->
            % it has already been logged
            {reply, ok, normal, State};
        {NewLocation, NewStateData} ->
            % save and publish
            NewState = State#state{loc = NewLocation, internaldata = NewStateData},
            {reply, ok, normal, NewState}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    {reply, State#state.loc, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: state(), Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: state()}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
