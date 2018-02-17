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
                config = #{}, up = false, reproc_timer = undefined}).
-type state() :: #state{devid :: devid(), device_type :: atom(), label :: binary(),
                        loc :: loc() | undefined, internaldata :: map(),
                        config :: map(), up :: boolean(), reproc_timer :: any()}.

%% API
-export([start_link/1, start_link/2, stop/1]).

%% gen_statem callbacks
-export([init/1,
         normal/3,
         handle_event/4,
         callback_mode/0,
         terminate/3,
         code_change/4]).

-export([process_frame/2, getstate/1, getstate/2, devid/1, config/1, device_type/1]).
-export([get_config_param/2, add_event/2]).
-export([reset/1, reprocess_data/2]).

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
    start_link(DevId, undefined).

start_link(DevId, Connector) ->
    gen_statem:start_link({global, DevId}, ?MODULE, [DevId, Connector], []).

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
            case nts_config:get_value([device_types, DType, ParamName]) of
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

-spec reprocess_data(pid(), datetime()) -> ok.
reprocess_data(Dev, From) ->
    gen_statem:call(Dev, {reprocess_data, From}).

%%%===================================================================
%%% gen_statem
%%%===================================================================

init([DevId, Connection]) ->
    case Connection of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            link(Pid) % to terminate if connector crashes
    end,
    process_flag(trap_exit, true),
    State = initialize_device(DevId),
    {ok, normal, State}.

callback_mode() -> [state_functions].

normal({call, From}, {reprocess_data, FromDtm}, State) ->
    NextState = do_reprocess_data(FromDtm, State),
    {keep_state, NextState, [{reply, From, ok}]};
normal(event_timeout, idle, _State) ->
    {stop, idle_timeout};
normal({call, From}, #frame{} = Frame, State) ->
    % here we handle fresh frame from a device
    FrameDtm = nts_frame:get(dtm, Frame),
    Loc = State#state.loc,
    LocDtm = Loc#loc.dtm,
    FrameSource = case {LocDtm, FrameDtm} of
                      {undefined, _} -> new;
                      {_, undefined} -> new;
                      {Ld, Fd} when Fd > Ld -> new;
                      _ -> buffer
                  end, 
    case do_process_frame(FrameSource, Frame, State) of
        {exit, E} ->
            {stop, E};
        NewState ->
            keep_state_with_timeout(NewState, From)
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
handle_event(info, {'EXIT', _Proc, Reason}, _StateName, _State) ->
    {stop, Reason};
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

initialize_device(DevId) ->
    initialize_device(DevId, nts_utils:dtm()).

initialize_device(DevId, Dtm) ->
    {_, DType, Label, Config} = nts_db:read_device(DevId),
    {Loc, Internal} =  case nts_db:last_state(DevId, Dtm) of
                           undefined -> {#loc{}, #{}};
                           R -> R
                       end,
    #state{devid = DevId, device_type = DType, label = Label,
           config = Config, loc = Loc, internaldata = Internal}.

keep_state_with_timeout(NewState, From) ->
    TVal = get_config(idle_timeout, NewState),
    {keep_state, NewState, [{reply, From, ok},
                            {event_timeout, TVal * 1000, idle}]}.

set_timestamps(Frame, NewLoc) ->
    NewLoc0 = nts_location:id(Frame#frame.id, NewLoc),
    NewLoc1 = nts_location:set(status, last_signal, Frame#frame.received, NewLoc0),
    case maps:get(dtm, Frame#frame.values, undefined) of
        undefined -> % lame - should never happen, devices should set timestamps
            NewLoc1#loc{dtm = Frame#frame.received};
        Dtm ->
            NewLoc2 = nts_location:set(status, last_signal_dtm, Dtm, NewLoc1),
            NewLoc2#loc{dtm = Dtm}
    end.

get_config(Name, State) ->
    case maps:get(Name, State#state.config, undefined) of
        undefined -> nts_config:get_value(Name);
        V -> V
    end.

update_current_down(DevId, Loc) ->
    case nts_location:get(status, up, Loc) of
        true ->
            Loc1 = nts_location:set(status, up, false, Loc),
            nts_db:update_state(DevId, Loc1);
        _ ->
            ok
    end.

flush_events(new, Internal) ->
    flush_events([save, publish], Internal);
flush_events(reproc, Internal) ->
    flush_events([save], Internal);

flush_events(Tasks, Internal) when is_list(Tasks) ->
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

-spec do_process_frame(new | reproc | buffer, frame(), state()) -> state() | {error, atom()}.
do_process_frame(buffer, Frame, State) ->
    DevId = devid(State),
    nts_db:save_frame(DevId, Frame),
    State1 = start_reproc_timer(nts_frame:get(dtm, Frame), State),
    State1;
do_process_frame(Origin, Frame, State) ->
    do_process_frame(Origin, undefined, Frame, State).

%% in reprocessing, OrigLoc is the one we got from dbase and may modify
%% for new loc it is undefined
-spec do_process_frame(new | reproc, loc() | undefined, frame(), state()) ->
    state() | {error, atom()}.
do_process_frame(Origin, OrigLoc, Frame, State) ->
    nts_hooks:run(preprocess, [], []),
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
            NewLocation0 = maybe_set_status_up(Origin, OrigLoc, NewLocation),
            % update timestamps, leave the rest unchanged
            NewLocation1 = set_timestamps(Frame, NewLocation0),
            % do not change internal state as it might be corrupt
            NewState0 = State#state{loc = NewLocation1},
            NewState = maybe_emit_device_up(Origin, Frame, OldLocation, NewState0),
            % save frame & location and publish
            nts_hooks:run(save_state, [], [State#state.devid, NewLocation,
                                           Frame, State#state.internaldata]),
            maybe_publish_state(Origin, State#state.devid, NewLocation1),
            NewState;
        {NewLocation0, NewInternal} ->
            NewLocation = maybe_set_status_up(Origin, OrigLoc, NewLocation0),
            NewState0 = maybe_emit_device_up(Origin, Frame, NewLocation, State),
            % save and possibly publish events created by handlers
            NewInternal1 = flush_events(Origin, NewInternal),
            % save frame and location and publish state
            case nts_hooks:run(save_state, [],
                               [NewState0#state.devid, NewLocation,
                                Frame, NewInternal1]) of
                {error, _} ->
                    {exit, error_saving_data};
                _ ->
                    NewState = NewState0#state{loc = NewLocation,
                                               internaldata = NewInternal1},
                    maybe_publish_state(Origin, State#state.devid, NewLocation),
                    NewState
            end
    end.

%% If it is new loc or buffer, up is true, for sure
%% if reprocess we preserve status as previously written
maybe_set_status_up(reproc, OrigLoc, Location) ->
    nts_location:set(status, up, get_up_status(OrigLoc), Location);
maybe_set_status_up(_, _OrigLoc, Location) ->
    nts_location:set(status, up, true, Location).

maybe_publish_state(new, DevId, Loc) ->
    nts_hooks:run(publish_state, [], [DevId, Loc]);
maybe_publish_state(_, _, _) ->
    ok.

%% @doc we do it once, after receiving frame, using either new location (before saving in case
%% it crashes), or old one if loc processing errored out
maybe_emit_device_up(_, _, _, #state{up = true} = State) ->
    State;
maybe_emit_device_up(reproc, _, _, State) ->
    State;
maybe_emit_device_up(_, Frame, Loc, State) ->
    Evt = nts_event:create_event([device, activity, up],
                                 State#state.devid,
                                 Loc,
                                 Frame#frame.received),
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

do_reprocess_data(FromDtm, State0) ->
    State = cancel_reproc_timer(State0),
    DevId = devid(State),
    StartState0 = initialize_device(DevId, FromDtm),
    % we know it is up since it started reprocessing
    StartState = StartState0#state{up = true},
    nts_db:clear_events(DevId, FromDtm),
    Hist = nts_db:full_history(DevId, FromDtm, nts_utils:dtm()),
    CurrentState = lists:foldl(fun reprocess_record/2, StartState, Hist),
    % when we're done update current table
    nts_hooks:run(publish_state, [], [State#state.devid,
                                      CurrentState#state.loc]),
    CurrentState.

reprocess_record({Fr, Loc}, State) ->
    Frame = nts_frame:parse(State#state.device_type, Fr#frame.id, Fr#frame.data,
                            Fr#frame.received),
    do_process_frame(reproc, Loc, Frame, State).

get_up_status(Loc) ->
    case nts_location:get(status, up, Loc) of
        undefined -> false;
        S -> S
    end.

start_reproc_timer(Dtm, #state{reproc_timer = undefined} = State) ->
    TVal = get_config(rewrite_history_timeout, State),
    {ok, Timer} = timer:apply_after(TVal * 1000, ?MODULE, reprocess_data, [self(), Dtm]),
    State#state{reproc_timer = Timer};
start_reproc_timer(Dtm, State) ->
    start_reproc_timer(Dtm, cancel_reproc_timer(State)).

cancel_reproc_timer(#state{reproc_timer = undefined} = State) ->
    State;
cancel_reproc_timer(#state{reproc_timer = Timer} = State) ->
    timer:cancel(Timer),
    State#state{reproc_timer = undefined}.
