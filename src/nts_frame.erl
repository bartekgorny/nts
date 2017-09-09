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
-export([parse/2, parse/3]).

-spec parse(atom() | list(), binary()) -> frame().
parse(Dtype, Frame) ->
    parse(Dtype, 0, Frame).

-spec parse(atom() | list(), integer(), binary()) -> frame().
parse(Dtype, Id, Frame) when is_atom(Dtype) ->
    case nts_config:get_value([device_types, Dtype]) of
        undefined -> {error, config_not_found};
        Settings -> parse(Settings, Id, Frame)
    end;
parse(Settings, Id, Frame) when is_list(Settings) ->
    Pmod = proplists:get_value(parser_mod, Settings),
    F = Pmod:parse_frame(Frame),
    F#frame{id = Id, received = nts_utils:dtm()}.

