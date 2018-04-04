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


handle_input(location, Frame, OldLoc, #hookresult{newloc = NewLoc} = HookRes,
             Internal, _State) ->
    Data = Frame#frame.values,
    Sat = maps:get(sat, Data, undefined),
    {Lat, Lon} = case Sat of
                     undefined ->
                         {maps:get(latitude, Data), maps:get(longitude, Data)};
                     I when is_integer(I) andalso I > 3 ->
                         {maps:get(latitude, Data), maps:get(longitude, Data)};
                     _ -> nts_location:coords(OldLoc)
                 end,
    {ok,
     HookRes#hookresult{newloc = nts_location:coords(Lat, Lon, NewLoc)},
     Internal};
handle_input(_, _Frame, OldLoc, #hookresult{newloc = NewLoc} = HookRes,
             Internal, _State) ->
    {Lat, Lon} = nts_location:coords(OldLoc),
    {ok,
     HookRes#hookresult{newloc = nts_location:coords(Lat, Lon, NewLoc)},
     Internal}.

