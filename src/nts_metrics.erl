%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Sep 2017 22:54
%%%-------------------------------------------------------------------
-module(nts_metrics).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").

%% API
-export([create_metrics/0, up/1, up/2, count/1, one/1]).

create_metrics() ->
    lists:foreach(fun(M) -> ensure_metric(M, spiral) end, ?METRICS_SPIRAL).

up(M) ->
    up(M, 1).

up(M, I) ->
    exometer:update(M, I).

count(M) ->
    case exometer:get_value(M, count) of
        {ok, [{count, I}]} -> {ok, I};
        E -> {error, E}
    end.

one(M) ->
    case exometer:get_value(M, one) of
        {ok, [{one, I}]} -> {ok, I};
        E -> {error, E}
    end.

ensure_metric(M, Type) ->
    case exometer:info(M, type) of
        Type -> ok;
        undefined -> exometer:new(M, Type)
    end.
