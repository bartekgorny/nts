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

%% API
-export([filter_loc/2]).

-spec filter_loc({datetime(), float(), float(), integer()},
                 [{datetime(), float(), float()}]) ->
    {{float(), float()}, [{datetime(), float(), float()}]}.
filter_loc({_, _, _, Sat}, []) when Sat < 4 ->
    % corner case
    {undefined, []};
filter_loc({_, _, _, Sat}, Trail) when Sat < 4 ->
    {_, Lat, Lon} = hd(Trail),
    {{Lat, Lon}, Trail};
filter_loc({Dtm, Lat, Lon, Sat}, [{PDtm, PLat, PLon}|Trail]) ->
    ok.
%%    {{NextLat, NextLon}, NewTrail}.

