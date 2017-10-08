%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2017 23:31
%%%-------------------------------------------------------------------
-module(nts_frame).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").

%% API
-export([parse/2, parse/4, get/2]).

%% @doc parse fresh frame, received from socket
%% set id to 0 and datetime to now
-spec parse(atom() | list(), binary()) -> frame().
parse(Dtype, Frame) ->
    parse(Dtype, 0, Frame, nts_utils:dtm()).

%% @doc parse frame retrieved from database
%% we know it and when it was received
-spec parse(atom() | list(), integer(), binary(), datetime()) -> frame().
parse(Dtype, Id, Frame, Dtm) when is_atom(Dtype) ->
    case nts_config:get_value([device_types, Dtype]) of
        undefined -> {error, config_not_found};
        Settings -> parse(Settings, Id, Frame, Dtm)
    end;
parse(Settings, Id, Frame, Dtm) ->
    Pmod = proplists:get_value(parser_mod, Settings),
    F = Pmod:parse_frame(Frame),
    F#frame{id = Id, received = Dtm}.

get(Key, Frame) ->
    maps:get(Key, Frame#frame.values, undefined).
