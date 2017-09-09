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
-export([dtm/0]).

dtm() ->
    calendar:now_to_datetime(os:timestamp()).

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