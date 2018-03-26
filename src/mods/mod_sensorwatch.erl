%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2017 17:58
%%%-------------------------------------------------------------------
-module(mod_sensorwatch).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").
%% API

-export([handle_input/6]).

-spec handle_input(frametype(), frame(), loc(), hookresult(), internal(),
                   nts_device:state()) ->
    {ok, hookresult(), internal()}.
handle_input(location, _Frame, OldLoc, HookRes, Internal, State) ->
    #hookresult{newloc = NewLoc} = HookRes,
    ToWatch = nts_device:get_config_param(event_triggering_sensors,
                                          State),
    DevId = nts_device:devid(State),
    NewHookRes = lists:foldl(fun(SName, L) ->
                                  check_sensor(L, SName, DevId, OldLoc, NewLoc)
                              end,
                              HookRes,
                              ToWatch),
    {ok, NewHookRes, Internal}.

check_sensor(HookRes, SName, DevId, OldLoc, NewLoc) ->
    OldVal = nts_location:get(sensor, SName, OldLoc),
    NewVal = nts_location:get(sensor, SName, NewLoc),
    case has_changed(OldVal, NewVal) of
        true ->
            NEv = nts_event:create_event([device, sensorchange, SName],
                                         DevId,
                                         NewLoc,
                                         NewLoc#loc.dtm,
                                         #{value => NewVal}
                ),
            nts_device:add_event(NEv, HookRes);
        false ->
            HookRes
    end.

has_changed(undefined, _) ->
    false;
has_changed(_, undefined) ->
    false;
has_changed(V, V) ->
    false;
has_changed(_, _) ->
    true.
