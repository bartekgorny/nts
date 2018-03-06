%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Copies basic data to new location
%%% @end
%%% Created : 21. Oct 2017 12:56
%%%-------------------------------------------------------------------
-module(mod_coords).
-author("bartek").
-include_lib("nts/src/nts.hrl").
%% API
-export([handle_input/6]).


handle_input(location, Frame, _OldLoc, NewLoc, Internal, _State) ->
    Data = Frame#frame.values,
    Lat = maps:get(latitude, Data),
    Lon = maps:get(longitude, Data),
    {ok, nts_location:coords(Lat, Lon, NewLoc), Internal};
handle_input(_, _Frame, _OldLoc, NewLoc, Internal, _State) ->
    {ok, NewLoc, Internal}.

