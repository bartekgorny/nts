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

-include_lib("nts/src/nts.hrl").
%% API
-export([bin2hex/1, hex2bin/1]).
-export([json_encode_map/1, json_decode_map/1, format_error/1]).
-export([time2string/1, time2bin/1, bin2time/1]).
-export([dtm/0, distance/1, distance/2]).
-export([encode_location/1]).

-export([get_brackets/3, ew_table/0]).
-export([timediff/2]).
-export([insort/2]).

-export([rebuffer/2]).

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
    jiffy:encode(cookmap(Data)).

json_decode_map(null) ->
    #{};
json_decode_map(<<"">>) ->
    #{};
json_decode_map(Data) ->
    keys_to_atoms(jiffy:decode(Data, [return_maps])).

format_error({A, B}) ->
    list_to_binary(io_lib:format("~p:~p", [A, B])).

encode_location(Loc) ->
    M  = #{
        dtm => Loc#loc.dtm,
        lat => Loc#loc.lat,
        lon => Loc#loc.lon,
        data => Loc#loc.data
    },
    json_encode_map(M).

%% @doc
%% if we wanted to be more precise we could implement Haversine formula
%% which doesn't make sense and is a waste of CPU power since it involves
%% a lot of trigonometric functions
%% we can use a rather simple approximation instead
%% coords are in minutes
%% returns distance in km
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

-spec distance({loc(), loc()}) -> float().
distance({Loc1, Loc2}) ->
    distance({Loc1#loc.lat, Loc1#loc.lon},
             {Loc2#loc.lat, Loc2#loc.lon}).

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

-spec time2string(datetime() | undefined) -> string().
time2string(undefined) ->
    "";
time2string(T) ->
    {{Y, M, D}, {H, Mi, S}} = T,
    lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p", [Y, M, D, H, Mi, S])).

time2bin({{_, _, _}, {_, _, _}} = D) ->
    list_to_binary(io_lib:format("~p", [D])).

bin2time(B) ->
    B1 = binary:replace(B, <<"{">>, <<>>, [global]),
    B2 = binary:replace(B1, <<"}">>, <<>>, [global]),
    [Y, M, D, H, Mi, S] =
        lists:map(fun list_to_integer/1,
                  lists:map(fun binary_to_list/1,
                            binary:split(B2, <<",">>, [global]))),
    {{Y, M, D}, {H, Mi, S}}.


cookmap({{_, _, _}, {_, _, _}} = D) ->
    time2bin(D);
cookmap(M) when is_map(M) ->
    maps:map(fun(_, V) -> cookmap(V) end, M);
cookmap(M) when is_list(M) ->
    lists:map(fun cookmap/1, M);
cookmap(M) ->
    M.

keys_to_atoms(M) when is_map(M) ->
    maps:from_list(
        lists:map(fun({K, V}) -> {binary_to_existing_atom(K, utf8), keys_to_atoms(V)} end,
                  maps:to_list(M))
    );
keys_to_atoms(M) ->
    M.

insort(Obj, Lista) ->
    insort(Obj, [], Lista).

insort(Obj, Smaller, []) ->
    Smaller ++ [Obj];
insort(Obj, Smaller, [H|T]) when Obj > H ->
    insort(Obj, Smaller ++ [H], T);
insort(Obj, Smaller, T) ->
    Smaller ++ [Obj] ++ T.

-spec(rebuffer(binary(), binary()) -> {binary(), [binary()]}).
rebuffer(Buffer, <<>>) ->
    {Buffer, []};
rebuffer(Buffer, Data) ->
    DList = binary:split(Data, <<10>>, [global]),
    [Last | DList1] = lists:reverse(DList),
    case Last of
        <<>> ->
            rebuffer(Buffer, lists:reverse(DList1), true);
        _ ->
            rebuffer(Buffer, DList, false)
    end.

rebuffer(Buffer, [Data], false) ->
    {<<Buffer/binary, Data/binary>>, []};
rebuffer(Buffer, [Data], true) ->
    {<<>>, [<<Buffer/binary, Data/binary>>]};
rebuffer(Buffer, [H|Tail], true) ->
    {<<>>, [<<Buffer/binary, H/binary>> | Tail]};
rebuffer(Buffer, [H|Tail], false) ->
    [Last|NTailR] = lists:reverse(Tail),
    NTail = lists:reverse(NTailR),
    {Last, [<<Buffer/binary, H/binary>> | NTail]}.
