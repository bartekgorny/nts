%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Aug 2017 22:44
%%%-------------------------------------------------------------------
-module(nts_utils).
-author("bartekgorny").

%% API
-export([bin2hex/1, hex2bin/1]).
-export([json_encode_map/1, json_decode_map/1]).
-export([dtm/0, distance/2]).

-export([get_brackets/3, ew_table/0]).
-export([timediff/2]).

dtm() ->
    calendar:now_to_datetime(os:timestamp()).

timediff(A, B) ->
    abs(calendar:datetime_to_gregorian_seconds(A) -
        calendar:datetime_to_gregorian_seconds(B)).

bin2hex(B) ->
    lists:flatten([ i2h(I) || <<I>> <= B]).

i2h(I) when I > 255 ->
    {error, too_big};
i2h(I) ->
    maybe_add_zero(integer_to_list(I, 16)).

maybe_add_zero([C]) ->
    [$0, C];
maybe_add_zero([A, B]) ->
    [A, B];
maybe_add_zero(_) ->
    {error, too_many_chars}.

hex2bin(H) when is_binary(H) ->
    hex2bin(binary_to_list(H));
hex2bin(H) ->
    list_to_binary(lists:reverse(lists:flatten(hex2bin(H, [])))).

hex2bin([], Acc) ->
    Acc;
hex2bin([A, B | Tail], Acc) ->
    hex2bin(Tail, [h2i([A, B]) | Acc]).

h2i(H) ->
    list_to_integer(H, 16).

json_encode_map(Data) ->
    jiffy:encode(Data).

json_decode_map(Data) ->
    jiffy:decode(Data, [return_maps]).

%% @doc
%% if we wanted to be more precise we could implement Haversine formula
%% which doesn't make sense and is a waste of CPU power since it involves
%% a lot of trigonometric functions
%% we can use a rather simple approximation instead
-spec distance({float(), float()}, {float(), float()}) -> float().
distance({Lat1, Lon1}, {Lat2, Lon2}) ->
    % latitude is simple - one minute is one nautical mile
    NS = abs(Lat1 - Lat2) * 111.18,
    % find out which zone are we in
    {{Ka, Va}, {Kb, Vb}} = get_brackets(Lat1, [], ew_table()),
    P = (Lat1 - Ka)/(Kb - Ka),
    EW = Va * (1 - P) + Vb * P,
    Dew = abs(Lon1 - Lon2) * EW,
    Dns = abs(Lat1 - Lat2) * NS,
    math:sqrt(math:pow(Dew, 2) + math:pow(Dns, 2)).

get_brackets(V, [], [H|T]) ->
    get_brackets(V, [H], T);
get_brackets(V, [{Ka, Va}], [{Kh, Vh}|_]) when Kh > V ->
    {{Ka, Va}, {Kh, Vh}};
get_brackets(V, [A], [H|T]) ->
    get_brackets(V, [A, H], T);
get_brackets(V, [_, {Kb, Vb}], [{Kh, Vh}|_]) when Kh > V ->
    {{Kb, Vb}, {Kh, Vh}};
get_brackets(V, [_, B], [H|T]) ->
    get_brackets(V, [B, H], T);
get_brackets(_, _, []) -> {error, out_of_the_globe}.

ew_table() ->
    [{0, 111.320},
     {15, 107.551},
     {30, 96.486},
     {45, 78.847},
     {60, 55.800},
     {75, 28.902},
     {90, 0.000}].