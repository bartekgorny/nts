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
-export([]).
%%"""
%%        Filter out records if the device was not moving.
%%        Definition of "not moving" is the following:
%%            The device did not move more then a given distance
%%            from the start point for a given period of time.
%%            where:
%%                - start point - any point on the route
%%                - period of time - number of signals or time in seconds, or both\
%%                (if both we `and` both conditions)
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

next(ReferencePos, NewPos, Data) -> ok.