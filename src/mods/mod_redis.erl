%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Feb 2018 20:56
%%%-------------------------------------------------------------------
-module(mod_redis).
-author("bartekgorny").

%% API
-export([handle_publishstate/3]).

handle_publishstate(Acc, DevId, Loc) ->
    ct:pal("Loc: ~p", [Loc]),
    {ok, Acc}.


