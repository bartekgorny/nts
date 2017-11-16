%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2017 22:52
%%%-------------------------------------------------------------------
-module(nts_location).
-author("bartekgorny").
-include_lib("nts/src/nts.hrl").

%% API
-export([new/0]).
-export([id/1, dtm/1, dtm/2, coords/1, coords/3, get/3, set/4, remove/3]).
-export([flag/3]).

-spec new() -> loc().
new() -> #loc{}.

-spec id(loc()) -> integer().
id(Loc) -> Loc#loc.id.

-spec dtm(loc()) -> datetime().
dtm(Loc) -> Loc#loc.dtm.

-spec dtm(datetime(), loc()) -> loc().
dtm(V, Loc) -> Loc#loc{dtm = V}.

-spec coords(loc()) -> {float(), float()}.
coords(Loc) -> {Loc#loc.lat, Loc#loc.lon}.

-spec coords(float(), float(), loc()) -> loc().
coords(Lat, Lon, Loc) ->
    Loc#loc{lat = Lat, lon = Lon}.

-spec get(datapart(), atom(), loc()) -> any().
get(logistic, Key, Loc) ->
    get_data(logistic, Key, Loc);
get(status, Key, Loc) ->
    get_data(status, Key, Loc);
get(sensor, Key, Loc) ->
    get_data(sensor, Key, Loc);
get(_, _, _) ->
    {error, invalid_section}.

-spec set(datapart(), atom(), any(), loc()) -> loc().
set(logistic, Key, Val, Loc) ->
    set_data(logistic, Key, Val, Loc);
set(status, Key, Val, Loc) ->
    set_data(status, Key, Val, Loc);
set(sensor, Key, Val, Loc) ->
    set_data(sensor, Key, Val, Loc);
set(_, _, _, _) ->
    {error, invalid_section}.

-spec remove(datapart(), atom(), loc()) -> loc().
remove(logistic, Key, Loc) ->
    remove_data(logistic, Key, Loc);
remove(status, Key, Loc) ->
    remove_data(status, Key, Loc);
remove(sensor, Key, Loc) ->
    remove_data(sensor, Key, Loc);
remove(_, _, _) ->
    {error, invalid_section}.

flag(get, Key, Loc) ->
    maps:is_key(Key, Loc#loc.flags);
flag(set, Key, Loc) ->
    Loc#loc{flags = maps:put(Key, true, Loc#loc.flags)};
flag(unset, Key, Loc) ->
    Loc#loc{flags = maps:remove(Key, Loc#loc.flags)}.

get_data(Part, Key, Loc) ->
    Data = Loc#loc.data,
    case maps:get(Part, Data, undefined) of
        undefined -> undefined;
        M when is_map(M) -> maps:get(Key, M, undefined)
    end.

set_data(Part, Key, Val, Loc) ->
    Data = Loc#loc.data,
    M = maps:get(Part, Data, #{}),
    Data1 = maps:put(Part, maps:put(Key, Val, M), Data),
    Loc#loc{data = Data1}.

remove_data(Part, Key, Loc) ->
    Data = Loc#loc.data,
    M = maps:get(Part, Data, #{}),
    Data1 = maps:put(Part, maps:remove(Key, M), Data),
    Loc#loc{data = Data1}.

