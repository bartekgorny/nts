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
%%
%% - a preprocessor analyses new state
%% - if state is known it returns everything as-is for further processing
%% - if state is uncertain then it
%%   - fixes appropriate parameters (like set coords to old values if we don't know if it is moving)
%%   - flags loc as uncertain (may be handy)
%%   - stores orig. data in a queue
%%   - returns it for further processing
%% - when state becomes known then
%%   - fixes data in the queue
%%   - flags them as 'update'
%%   - returns the whole queue plus the most recent one, not flagged
%%
%% Further stages process all msgs, they know not to publish them (since it is historical), but to
%% save it into db as update, not addition. Then it publishes the last one.
%%
%% The only drawback is that if device proc terminates in uncertain state then we should later run
%% data reprocessing, we might store such info somewhere if it is that important (which it probably
%% isn't).
%% "
%% All the above assumes we have only one preprocessor, which is most likely true. If we want
%% more we'd have to make preprocesor rerun the whole machinery (preprocessor included) but
%% first flag locations so as not to catch them again. Very complicated and not really necessary.