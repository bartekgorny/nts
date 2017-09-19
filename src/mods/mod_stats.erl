%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Sets all "last_*" values.
%%% @end
%%% Created : 15. Sep 2017 17:36
%%%-------------------------------------------------------------------
-module(mod_stats).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").

%% API
-export([handle_input/3]).

handle_input(location, Frame, State) ->
    S1 = nts_state:setlocdata(last_signal, Frame#frame.received, State),
    case maps:get(dtm, Frame#frame.data, undefined) of
        undefined -> {ok, S1};
        Dtm -> {ok, nts_state:setlocdata(last_signal_dtm, Dtm, S1)}
    end.

