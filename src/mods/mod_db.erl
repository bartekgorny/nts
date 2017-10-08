%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2017 21:00
%%%-------------------------------------------------------------------
-module(mod_db).
-author("bartekgorny").

%% API
-export([handle_newstate/4]).

handle_newstate(Acc, DevId, Loc, Frame) ->
    ok = nts_db:save_loc(DevId, Loc, Frame),
    Acc.
