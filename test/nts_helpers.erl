%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2017 20:40
%%%-------------------------------------------------------------------
-module(nts_helpers).
-author("bartekgorny").

%% API
-export([clear_tables/1]).

clear_tables([]) -> ok;
clear_tables([T|Rest]) ->
    nts_db:query("DELETE FROM " ++ T),
    clear_tables(Rest).
