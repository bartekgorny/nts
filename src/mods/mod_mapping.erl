%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Module for mapping and preprocessing sensor (device state attributes)
%%% like: ignition, voltage, doors open etc.
%%% There are sensor definitions and rules set per device to map data to sensors.
%%% Device can map a sensor onto itself to change its type.
%%% There are three types of sensors:
%%% Type 1 - undefined means zero
%%% Type 2 - undefined means no data, we carry forward last known value
%%% Type 3 - zero means no data, we carry forward last known value
%%% We treat every value as float, cast to bool when you want to.
%%% @end
%%% Created : 21. Oct 2017 13:03
%%%-------------------------------------------------------------------
-module(mod_mapping).
-author("bartek").
-include_lib("nts/src/nts.hrl").
%% API

-export([handle_input/6]).

%% test API
-export([map_sensor/5, sensor_defs/1]).

handle_input(location, Frame, OldLoc, NewLoc, Internal, State) ->
    ?ERROR_MSG("Frame: ~p~n", [Frame]),
    Mappings = maps:get(sensor_mapping, nts_device:config(State), #{}),
    SDefs = sensor_defs(nts_device:device_type(State)),
    NewLoc1 = lists:foldl(fun({BName, Val}, Acc) ->
                              map_sensor(Acc, BName, Val, OldLoc, SDefs, Mappings)
                          end,
                          NewLoc,
                          maps:to_list(Frame#frame.values)),
    {ok, NewLoc1, Internal}.


map_sensor(NewLoc, BName, Val, OldLoc, SDefs, Mappings) ->
    Name = case maps:get(BName, Mappings, undefined) of
               undefined ->
                   BName;
               CDef ->
                   maps:get(name, CDef)
           end,
    PVal = nts_location:get(sensor, Name, OldLoc),
    case map_sensor(BName, Val, PVal, SDefs, Mappings) of
        undefined -> NewLoc;
        {Name, NVal} -> nts_location:set(sensor, Name, NVal, NewLoc)
    end.

map_sensor(BName, Val, PVal, SDefs, Mappings) ->
    case maps:get(BName, Mappings, undefined) of
        undefined ->
            Name = to_atom(BName),
            case maps:get(Name, SDefs, undefined) of
                undefined -> undefined;
                Type -> {BName, proc_value(Type, Val, PVal)}
            end;
        CDef ->
            {maps:get(name, CDef), proc_value(maps:get(type, CDef), Val, PVal)}
    end.

proc_value(1, undefined, _) ->
    0;
proc_value(1, Val, _) ->
    Val;
proc_value(2, undefined, PVal) ->
    PVal;
proc_value(2, Val, _) ->
    Val;
proc_value(3, undefined, PVal) ->
    PVal;
proc_value(3, 0, PVal) ->
    PVal;
proc_value(3, Val, _) ->
    Val.

sensor_defs(DType) ->
    maps:merge(nts_config:get_value(sensors), nts_config:get_value([device, DType, sensors], #{})).

to_atom(S) when is_atom(S) -> S;
to_atom(S) when is_binary(S) ->binary_to_existing_atom(S, utf8).
