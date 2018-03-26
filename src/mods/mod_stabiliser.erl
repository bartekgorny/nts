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
-export([handle_input/6]).

-spec handle_input(frametype(), frame(), loc(), hookresult(), internal(),
                   nts_device:state()) ->
    {ok, hookresult(), internal()}.
handle_input(location, Frame, _OldLoc, HookRes, Internal, _State) ->
    #hookresult{newloc = NewLoc} = HookRes,
    Sat = nts_frame:get(sat, Frame),
    {Lat, Lon} = nts_location:coords(NewLoc),
    % extract key data so as not to store the whole loc in state
    % can't use records or tuples because they are not jsonisable, gah
    NewLocdata = #{dtm => nts_location:dtm(NewLoc), lat => Lat, lon => Lon},
    {FixedLocdata, StabState} = filter_loc(NewLocdata, Sat,
                                   maps:get(stabiliser_state, Internal, undefined)),
    FixedLoc = case FixedLocdata of
                   NewLocdata -> NewLoc;
                   _ ->
                       #{dtm := Ndtm, lat := Nlat, lon := Nlon} = FixedLocdata,
                       nts_location:coords(Nlat, Nlon,
                           nts_location:dtm(Ndtm, NewLoc))
               end,
    {ok, HookRes#hookresult{newloc = FixedLoc},
         maps:put(stabiliser_state, StabState, Internal)}.

-define(IGNORE_TRESHOLD, 600).
-define(MIN_SAT, 4).
-define(SANE_SPEED, 300). % km/h, we don't support jet planes
% if after a supposed jump we receives so many good locations we accept
-define(STABLE_TRAIL_LEN, 5).

-type locdata() :: map().
-type state() :: map().

-spec filter_loc(New :: locdata(), Sat :: integer(), State :: state() | undefined) ->
    {locdata() | undefined, state()}.
filter_loc(NewLoc, Sat, undefined) ->
    % init state
    filter_loc(NewLoc, Sat, #{state => normal, last_good => undefined, trail => []});
filter_loc(_, Sat, #{last_good := undefined} = State) when Sat < 4 ->
    % corner case - we know nothing and loc is not valid
    {undefined, State};
filter_loc(_, Sat, State) when Sat < ?MIN_SAT ->
    % not enough sat - ignoring
    {maps:get(last_good, State), State};
filter_loc(NewLoc, _, #{last_good := undefined} = State) ->
    % no previous loc - take this one
    {NewLoc, maps:put(last_good, NewLoc, State)};
filter_loc(NewLoc, _, #{last_good := Last} = State) ->
    Dtm = maps:get(dtm, NewLoc),
    PDtm = maps:get(dtm, Last),
    case nts_utils:timediff(Dtm, PDtm) of
        X when X > ?IGNORE_TRESHOLD ->
            % long time since, we start anew
            % (this is arguable - maybe we should make this check against head of
            % the trail if present?)
            {NewLoc, #{last_good => NewLoc, state => normal, trail => []}};
        _ ->
            % now we are getting serious
            speed_check(maps:get(state, State), NewLoc, State)
    end.

speed_check(normal, NewLoc, #{last_good := Last} = State) ->
    case verify_speed(NewLoc, Last) of
        true ->
            % everything is fine
            {NewLoc, maps:put(last_good, NewLoc, State)};
        false ->
            % speed is insane - we return last good position and wait to see
            {maps:get(last_good, State),
             maps:put(state, checking, maps:put(trail, [NewLoc], State))}
    end;
speed_check(checking, NewLoc, #{last_good := LastGood} = State) ->
    Trail = maps:get(trail, State),
    Last = hd(Trail),
    case verify_speed(NewLoc, Last) of
        false ->
            case verify_speed(NewLoc, LastGood) of
                true ->
                    % we are back on track
                    {NewLoc, #{state => normal,
                               last_good => NewLoc,
                               trail => []}};
                false ->
                    % GPS keeps kicking
                    {LastGood, maps:put(trail, [NewLoc], State)}
            end;
        true ->
            case length(Trail) of
                ?STABLE_TRAIL_LEN ->
                    % we received a few good locations, maybe gps is right anyway
                    % (rather rare case, I think)
                    {NewLoc, #{state => normal,
                               last_good => NewLoc,
                               trail => []}};
                _ ->
                    {LastGood, maps:put(trail, [NewLoc | Trail], State)}
            end
    end.



%%%%%%%%%%%%%%%%

verify_speed(NewLoc, OldLoc) ->
    case calc_speed(NewLoc, OldLoc) of
        X when X > ?SANE_SPEED -> false;
        _ -> true
    end.

calc_speed(#{dtm := Dtm, lat := Lat, lon := Lon},
           #{dtm := PDtm, lat := PLat, lon := PLon}) ->
    Dist = nts_utils:distance({Lat, Lon}, {PLat, PLon}),
    Seconds = nts_utils:timediff(Dtm, PDtm),
    Res = 3600 * Dist / Seconds,
    Res.

