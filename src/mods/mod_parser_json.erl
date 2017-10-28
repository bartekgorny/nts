%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2017 16:04
%%%-------------------------------------------------------------------
-module(mod_parser_json).
-author("bartek").
-include_lib("nts/src/nts.hrl").
%% API
-export([parse_frame/1]).

parse_frame(Frame) ->
    Values = nts_utils:json_decode_map(Frame),
    Type = binary_to_existing_atom(maps:get(type, Values), utf8),
    Values1 = maps:put(type, Type, Values),
    V2 = case maps:get(dtm, Values1, undefined) of
             B when is_binary(B) -> maps:put(dtm, nts_utils:bin2time(B), Values1);
             _ -> Values1
         end,
    DevId = maps:get(devid, Values),
    #frame{type = Type, device = DevId, values = V2}.
