%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% This is for detecting jumps in location
%%% @end
%%% Created : 09. Sep 2017 14:28
%%%-------------------------------------------------------------------
-module(mod_stabiliser).
-author("bartek").
-include_lib("nts/src/nts.hrl").

%%% this logic should be applied in the preprocessing state, BEFORE floatfilter, so that it does
%%% not treat GPS jumps as device movement!

%% API
-export([filter_loc/3]).

-define(IGNORE_TRESHOLD, 600).
-define(MIN_SAT, 4).
-define(SANE_SPEED, 300). % km/h, we don't support jet planes
% if after a supposed jump we receives so many good locations we accept
-define(STABLE_TRAIL_LEN, 5).

-record(state, {state = normal, last_good, trail = []}).
-type locdata() :: {datetime(), float(), float()}.
-type state() :: #state{state :: normal | checking,
                        last_good :: locdata() | undefined,
                        trail :: [locdata()]}.

-spec filter_loc(New :: locdata(), Sat :: integer(), State :: state() | undefined) ->
    {locdata() | undefined, state()}.
filter_loc(NewLoc, Sat, undefined) ->
    % init state
    filter_loc(NewLoc, Sat, #state{});
filter_loc(_, Sat, #state{last_good = undefined} = State) when Sat < 4 ->
    % corner case - we know nothing and loc is not valid
    {undefined, State};
filter_loc(_, Sat, State) when Sat < ?MIN_SAT ->
    % not enough sat - ignoring
    {State#state.last_good, State};
filter_loc(NewLoc, _, #state{last_good = undefined} = State) ->
    % no previous loc - take this one
    {NewLoc, State#state{last_good = NewLoc}};
filter_loc(NewLoc, _, #state{last_good = Last} = State) ->
    {Dtm, _, _} = NewLoc,
    {PDtm, _, _} = Last,
    case nts_utils:timediff(Dtm, PDtm) of
        X when X > ?IGNORE_TRESHOLD ->
            % long time since, we start anew
            % (this is arguable - maybe we should make this check against head of
            % the trail if present?)
            {NewLoc, State#state{last_good = NewLoc, state = normal, trail = []}};
        _ ->
            % now we are getting serious
            speed_check(State#state.state, NewLoc, State)
    end.

speed_check(normal, NewLoc, #state{last_good = Last} = State) ->
    case verify_speed(NewLoc, Last) of
        true ->
            % everything is fine
            {NewLoc, State#state{last_good = NewLoc}};
        false ->
            % speed is insane - we return last good position and wait to see
            {State#state.last_good,
             State#state{state = checking, trail = [NewLoc]}}
    end;
speed_check(checking, NewLoc, #state{last_good = LastGood} = State) ->
    Trail = State#state.trail,
    Last = hd(Trail),
    case verify_speed(NewLoc, Last) of
        false ->
            case verify_speed(NewLoc, LastGood) of
                true ->
                    % we are back on track
                    {NewLoc, State#state{state = normal,
                                         last_good = NewLoc,
                                         trail = []}};
                false ->
                    % GPS keeps kicking
                    {LastGood, State#state{trail = [NewLoc]}}
            end;
        true ->
            case length(Trail) of
                ?STABLE_TRAIL_LEN ->
                    % we received a few good locations, maybe gps is right anyway
                    % (rather rare case, I think)
                    {NewLoc, State#state{state = normal,
                                         last_good = NewLoc,
                                         trail = []}};
                _ ->
                    {LastGood, State#state{trail = [NewLoc | Trail]}}
            end
    end.



%%%%%%%%%%%%%%%%

verify_speed(NewLoc, OldLoc) ->
    case calc_speed(NewLoc, OldLoc) of
        X when X > ?SANE_SPEED -> false;
        _ -> true
    end.

calc_speed({Dtm, Lat, Lon}, {PDtm, PLat, PLon}) ->
    Dist = nts_utils:distance({Lat, Lon}, {PLat, PLon}),
    Seconds = nts_utils:timediff(Dtm, PDtm),
    3600 * Dist / Seconds.

