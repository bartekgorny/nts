%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% This is an extremely basic event handler; in practice it would have
%%% to handle lots of event types, varying per device type, some
%%% events will change sensor state etc. Will need a library.
%%% @end
%%% Created : 21. Oct 2017 12:56
%%%-------------------------------------------------------------------
-module(mod_events).
-author("bartek").
-include_lib("nts/src/nts.hrl").
%% API
-export([handle_input/6]).


handle_input(event, Frame, _OldLoc, HookRes, Internal, State) ->
    Data = Frame#frame.values,
    #hookresult{newloc = NewLoc} = HookRes,
    DevId = nts_device:devid(State),
    NHookRes = case maps:get(type, Data, undefined) of
                   undefined -> HookRes;
                   BType ->
                       Type = binary_to_existing_atom(BType, utf8),
                       Ev = nts_event:create_event([device, Type],
                                                   DevId,
                                                   NewLoc,
                                                   NewLoc#loc.dtm,
                                                   #{}),
                       nts_device:add_event(Ev, HookRes)
               end,
    {ok, NHookRes, Internal};
handle_input(_, _Frame, _OldLoc, HookRes, Internal, _State) ->
    {ok, HookRes, Internal}.

