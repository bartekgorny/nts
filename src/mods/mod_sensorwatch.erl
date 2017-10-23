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

-spec handle_input(frametype(), frame(), loc(), loc(), internal(),
    nts_device:state()) ->
    {ok, loc(), internal()}.
handle_input(location, _Frame, OldLoc, NewLoc, Internal, State) ->
    ToWatch = nts_device:get_config_param(event_triggering_sensors,
                                          State),
    DevId = nts_device:devid(State),
    lists:map(fun(SName) -> check_sensor(SName, DevId, OldLoc, NewLoc) end,
                ToWatch),
    {ok, NewLoc, Internal}.

check_sensor(SName, DevId, OldLoc, NewLoc) ->
    OldVal = nts_location:get(sensor, SName, OldLoc),
    NewVal = nts_location:get(sensor, SName, NewLoc),
    case has_changed(OldVal, NewVal) of
        true ->
            nts_event:emit_event([device, sensorchange, SName],
                DevId,
                NewLoc,
                NewLoc#loc.dtm,
                #{value => NewVal}
                );
        false ->
            ok
    end.

has_changed(undefined, _) ->
    false;
has_changed(_, undefined) ->
    false;
has_changed(V, V) ->
    false;
has_changed(_, _) ->
    true.
