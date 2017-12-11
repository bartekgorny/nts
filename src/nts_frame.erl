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
-export([parse/2, parse/4, get/2, empty/0]).

-export([generate_frame_id/0]).

%% @doc parse fresh frame, received from socket
%% set id to something guaranteed to be unique and datetime to now
-spec parse(atom() | list(), binary()) -> frame().
parse(Dtype, Frame) ->
    parse(Dtype, generate_frame_id(), Frame, nts_utils:dtm()).

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

empty() ->
    #frame{id = generate_frame_id(), received = nts_utils:dtm()}.

% timestamp in microseconds, plus two random digits to be 100% sure it is unique
generate_frame_id() ->
    {A, B, C} = os:timestamp(),
    Mln = 1000000,
    M = (A * Mln + B) * Mln + C,
    T1 = rand:uniform(9),
    T2 = rand:uniform(9),
    M * 100 + T1 * 10 + T2.

