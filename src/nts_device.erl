%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Basic logic: this statem receives new frames, usu. from a socket opened for
%%% a device. Upon receiving a frame it:
%%% * calls hooks to determine state changes
%%% * saves frame with the new state
%%% * calls location publishers
%%%
%%% If if receives a buffered frame (older than the most recent one) it
%%% enters a 'suspended' state - incoming frames are buffered locally.
%%% When buffered frames stop coming in the statem it removes events from that
%%% period and reruns data processing (not publishing locations and events).
%%% When it is done it processes buffered frames (with publishing) and
%%% goes back to the normal state. (I think we need the third state, whereby
%%% it buffers incoming frames but publishes events).
%%% @end
%%% Created : 14. Sep 2017 22:41
%%%-------------------------------------------------------------------
-module(nts_device).
-author("bartekgorny").

-behaviour(gen_statem).
-include_lib("nts/src/nts.hrl").

-record(state, {devid, device_type, label, loc = #loc{}, internaldata = #{},
                config = #{}, up = false}).
-type state() :: #state{devid :: devid(), device_type :: atom(), label :: binary(),
                        loc :: loc() | undefined, internaldata :: map(),
                        config :: map(), up :: boolean()}.

%% API
-export([start_link/1, stop/1]).

%% gen_statem callbacks
-export([init/1,
         normal/3,
         handle_event/4,
         callback_mode/0,
         terminate/3,
         code_change/4]).

-export([process_frame/2, getstate/1, getstate/2, devid/1, config/1, device_type/1]).
-export([get_config_param/2, add_event/2]).
-export([reset/1, reprocess_data/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(devid()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(DevId) ->
    gen_statem:start_link({global, DevId}, ?MODULE, [DevId], []).

stop(Pid) ->
    gen_statem:stop(Pid).

process_frame(Pid, Frame) ->
    gen_statem:call(Pid, Frame).

-spec getstate(pid()) -> loc().
getstate(Pid) ->
    gen_statem:call(Pid, get_state).

% for testing
getstate(Pid, internal) ->
    {_, S} = sys:get_state(Pid),
    S#state.internaldata.

% for troubleshooting
reset(Pid) ->
    gen_statem:call(Pid, reset_internal_state).

% state accessors (for hook handlers which get the whole state)

-spec devid(state()) -> devid().
devid(State) ->
    State#state.devid.

-spec config(state()) -> map().
config(State) ->
    State#state.config.

-spec device_type(state()) -> atom().
device_type(State) ->
    State#state.device_type.

%% @doc get configuration parameter from this device, or for this
%% device type, or default for any device
-spec get_config_param(atom(), state()) -> any().
get_config_param(ParamName, State) ->
    Conf = State#state.config,
    DType = State#state.device_type,
    case maps:get(ParamName, Conf, undefined) of
        undefined ->
            case nts_config:get_value([device, DType, ParamName]) of
                undefined ->
                    nts_config:get_value(ParamName);
                V -> V
            end;
        V -> V
    end.

-spec add_event(event(), internal()) -> internal().
add_event(Evt, Internal) ->
    EventQueue = maps:get(new_events, Internal, []),
    maps:put(new_events, [Evt|EventQueue], Internal).

-spec reprocess_data(pid(), datetime(), datetime()) -> ok.
reprocess_data(_Dev, _From, _To) ->
    % stub
    ok.

%%%===================================================================
%%% gen_statem
%%%===================================================================

init([DevId]) ->
    process_flag(trap_exit, true),
    {_, DType, Label, Config} = nts_db:read_device(DevId),
    {Loc, Internal} =  case nts_db:last_state(DevId) of
                           undefined -> {#loc{}, #{}};
                           R -> R
                       end,
    {ok, normal, #state{devid = DevId, device_type = DType, label = Label,
                        config = Config, loc = Loc, internaldata = Internal}}.

callback_mode() -> [state_functions].

normal(state_timeout, idle, _State) ->
    {stop, idle_timeout};
normal({call, From}, #frame{} = Frame, State) ->
    % clear previous error
    OldLocation = nts_location:remove(status, error, State#state.loc),
    NewLoc = set_timestamps(Frame, nts_location:new()),
    case nts_hooks:run_procloc(State#state.device_type,
                               Frame#frame.type,
                               Frame,
                               OldLocation,
                               NewLoc,
                               State#state.internaldata,
                               State) of
        {error, E} ->
            % it has already been logged
            % mark location as errored so that this info is published
            Es = nts_utils:format_error(E),
            NewLocation = nts_location:set(status, error, Es, OldLocation),
            % update timestamps, leave the rest unchanged
            NewLocation1 = set_timestamps(Frame, NewLocation),
            % do not change internal state as it might be corrupt
            NewState0 = State#state{loc = NewLocation1},
            NewState = maybe_emit_device_up(OldLocation, NewState0),
            % save frame & location and publish
            nts_hooks:run(save_state, [], [State#state.devid, NewLocation,
                Frame, State#state.internaldata]),
            nts_hooks:run(publish_state, [], [State#state.devid, NewLocation]),
            keep_state_with_timeout(NewState, From);
        {NewLocation, NewInternal} ->
            NewState0 = maybe_emit_device_up(NewLocation, State),
            % save and publish events created by handlers
            NewInternal1 = flush_events([save, publish], NewInternal),
            % save frame and location and publish state
            case nts_hooks:run(save_state, [],
                               [NewState0#state.devid, NewLocation,
                                Frame, NewInternal1]) of
                {error, _} -> exit(self(), error_saving_data);
                _ ->
                    NewState = NewState0#state{loc = NewLocation, internaldata = NewInternal1},
                    nts_hooks:run(publish_state, [], [State#state.devid, NewLocation]),
                    keep_state_with_timeout(NewState, From)
            end
    end;
normal(T, Event, State) ->
    handle_event(T, Event, normal, State).

handle_event({call, From}, get_state, _StateName, State) ->
    {keep_state_and_data, [{reply, From, State#state.loc}]};
handle_event({call, From}, reset_internal_state, _StateName, State) ->
    NState = State#state{internaldata = #{}},
    % here we will put some more telling 'frame'
    nts_hooks:run(save_state, [], [State#state.devid, State#state.loc,
                  nts_frame:empty(), #{}]),
    {keep_state, NState, [{reply, From, ok}]};
handle_event({call, From}, _Event, _StateName, _State) ->
    {keep_state_and_data, [{reply, From, ok}]};
handle_event(_, _Event, _StateName, _State) ->
    keep_state_and_data.

terminate(_Reason, _StateName, State) ->
    maybe_emit_device_down(State),
    update_current_down(State#state.devid, State#state.loc),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

keep_state_with_timeout(NewState, From) ->
    TVal = get_config(idle_timeout, NewState),
    {keep_state, NewState, [{reply, From, ok},
                            {state_timeout, TVal * 1000, idle}]}.

set_timestamps(Frame, NewLoc) ->
    NewLoc1 = nts_location:set(status, last_signal, Frame#frame.received, NewLoc),
    case maps:get(dtm, Frame#frame.values, undefined) of
        undefined -> % lame - should never happen, devices should set timestamps
            NewLoc1#loc{dtm = os:timestamp()};
        Dtm ->
            NewLoc2 = nts_location:set(status, last_signal_dtm, Dtm, NewLoc1),
            NewLoc2#loc{dtm = Dtm}
    end.

%% @doc we do it once, after receiving frame, using either new location (before saving in case
%% it crashes), or old one if loc processing errored out
maybe_emit_device_up(_, #state{up = true} = State) ->
    State;
maybe_emit_device_up(Loc, State) ->
    Evt = nts_event:create_event([device, activity, up],
                                 State#state.devid,
                                 Loc,
                                 nts_utils:dtm()),
    process_events([save, publish], [Evt]),
    State#state{up = true}.

maybe_emit_device_down(#state{up = false} = State) ->
    State;
maybe_emit_device_down(State) ->
    Evt = nts_event:create_event([device, activity, down],
                                 State#state.devid,
                                 State#state.loc,
                                 nts_utils:dtm()),
    process_events([save, publish], [Evt]),
    State#state{up = false}.

get_config(Name, State) ->
    case maps:get(Name, State#state.config, undefined) of
        undefined -> nts_config:get_value(Name);
        V -> V
    end.

update_current_down(DevId, Loc) ->
    Loc1 = nts_location:set(status, up, false, Loc),
    nts_db:update_state(DevId, Loc1),
    ok.

flush_events(Tasks, Internal) ->
    process_events(Tasks, maps:get(new_events, Internal, [])),
    maps:remove(new_events, Internal).

process_events([], _Q) ->
    ok;
process_events([H|T], Q) ->
    process_events(H, Q),
    process_events(T, Q);
process_events(save, Q) ->
    lists:map(fun nts_db:save_event/1, Q);
process_events(publish, Q) ->
    lists:map(fun(Evt) ->
        EType = Evt#event.type,
        nts_hooks:run(publish_event, [], [EType, Evt])
              end,
        Q).
