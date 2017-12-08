%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2017 21:16
%%%-------------------------------------------------------------------
-module(mod_floatfilter).
-author("bartek").
-include_lib("nts/src/nts.hrl").
%% API
-export([handle_input/6]).

-define(MOVE_DISTANCE, 0.1).
-define(TRAIL_LEN, 5).


%% test API
-export([check_moving/2, newstate/0]).

-type floatmode() :: moving | uncertain | stopped.

-record(floatstate, {refloc, mode = moving, trail = []}).
%-type floatstate() :: #floatstate{refloc :: loc(),
                                  %mode :: floatmode(),
                                  %trail :: [loc()]}.
-type floatstate() :: map().

-spec handle_input(frametype(), frame(), loc(), loc(), internal(),
                   nts_device:state()) ->
    {ok, loc(), internal()}.
handle_input(location, _Frame, _OldLoc, NewLoc, Internal, _State) ->
    {ok, nts_location:set(status, up, true, NewLoc), Internal}.

newstate() -> #{refloc => undefined, mode => moving, trail => []}.

%% @doc Receives new location and floatfilter state; returns fixed location (meaning it may have
%% its coordinates reset to those of the reference location) and modified state.
-spec check_moving(loc(), floatstate()) -> {loc(), floatstate()}.
check_moving(NewLoc, #{refloc := undefined} = FState) ->
    {NewLoc, FState#{refloc => NewLoc}};
check_moving(NewLoc, FState) ->
    #{refloc := RefLoc, mode := Mode, trail := Trail} = FState,
    IsMove = is_move(NewLoc, RefLoc),
    {NewMode, FixedLoc, NewState} = check_moving(Mode, IsMove, Trail,
                                                 NewLoc, FState), % duplication for greater readability
    {FixedLoc, NewState#{mode => NewMode}}.

-spec check_moving(Mode :: floatmode(),
                   IsMove :: boolean(),
                   Trail :: [loc()],
                   NewLoc :: loc(),
                   FState :: floatstate()) ->
    {floatmode(), loc(), floatstate()}.
check_moving(moving, true, [], NewLoc, FState) ->
    {moving, NewLoc, FState#{refloc => NewLoc}};
check_moving(moving, false, [], NewLoc, FState) ->
    {uncertain, set_to_ref(NewLoc, FState), add_to_trail(NewLoc, FState)};
check_moving(uncertain, true, _Trail, NewLoc, FState) ->
    % turns out we are moving
    % re-save locations from Trail (they were saved with coords set to ref)
    {moving, NewLoc, FState#{trail => []}};
check_moving(uncertain, false, Trail, NewLoc, FState) ->
    case length(Trail) of
        ?TRAIL_LEN ->
            {stopped, set_to_ref(NewLoc, FState), FState#{trail => []}};
        _ ->
            {uncertain, set_to_ref(NewLoc, FState), add_to_trail(NewLoc, FState)}
    end;
check_moving(stopped, false, [], NewLoc, FState) ->
    {stopped, set_to_ref(NewLoc, FState), FState};
check_moving(stopped, true, [], NewLoc, FState) ->
    %% if needed we could add some logic to make sure he left for good (meaning: enter another
    %% state and collect some trail before changing to `moving`)
    {moving, NewLoc, FState};
check_moving(_, _, _, NewLoc, FState) ->
    % catch-all - reset state
    ?WARNING_MSG("no previous match, resetting state, ~p", {NewLoc, FState}),
    {moving, NewLoc, #{}}.

is_move(A, B) ->
    ct:pal("~p", [A]),
    ct:pal("~p", [B]),
    #{lat := LatA, lon := LonA} = A,
    #{lat := LatB, lon := LonB} = B,
    Dist = nts_utils:distance({LatA, LonA}, {LatB, LonB}),
    ct:pal("~p", [Dist]),
    Dist > ?MOVE_DISTANCE.

%% set location coords to those of reference location
set_to_ref(Loc, #{refloc := Ref}) ->
    #{lat := Lat, lon := Lon} = Ref,
    Loc#{lat => Lat, lon := Lon}.

add_to_trail(Loc, FState) ->
    Trail = maps:get(trail, FState),
    FState#{trail => [Loc | Trail]}.

%%check_moving(New, Prev, Trail) ->
%%    Moved = is_move(New, Prev),
%%    check_moving(Moved, New, Prev, Trail).
%%
%%check_moving(true, New, _Prev, []) ->
%%    {New, []};
%%check_moving(false, New, Prev, []) ->
%%    {Prev, [New]};

%%"""
%%        Filter out records if the device was not moving.
%%        Definition of "not moving" is the following:
%%            The device did not move more then a given distance
%%            from the start point for a given period of time.
%%            where:
%%                - start point - any point on the route
%%                - period of time - number of signals or time in seconds, or both\
%%                (if both we `and` both conditions)
%%     fix: only time, signals are stupid
%%        Filter Location objects or dicts which are cast into Locations
%%    """
%%"""
%%            if distance > radius go on
%%            else start collecting records
%%            while collecting, keep checking if we exceeded the count or seconds limit
%%            when we find next record which if farther then the radius
%%            then if we are within limit return all the records
%%            otherwise return this one and go on from there
%%        """


%% "
%% How preprocessor works, and why:
%%
%% Normal sequence is:
%% - receive frame
%% - determine current state of the device
%% - save frame and state to db
%% - publish state
%%
%% Sometimes, however, we can't fully determine current state, we can only calculate it retrospectively
%% later. Example: we don't know if device is really moving or it is just GPS jitter, we have to wait
%% a bit to find it out. If so, we have to keep position unchanged, and later update history.
%%
%% It is a difficult choice to make, because we have two options:
%% - refrain from publishing state until we find it out, then publish a series of states (for
%% clients which draw current path, so that they have it continuous)
%% - publish state with unchanged position, then we have a hop (for clients who want to know other
%% parameters and want to see changing last signal datetime, not to defer event generation etc)
%%
%% I'm in favour of the latter. Thus:
%% A handler may state that a certain parameter can not be determined yet. It sets it to
%% a temporary value (usu. the old one) and passes the location on, flagging it as
%% "enqueue for recheck" (this is because we want the loc to pass through all handlers).
%% When some future frame clears things up, the handler empties the queue, fixes
%% locations and store them into a db.

