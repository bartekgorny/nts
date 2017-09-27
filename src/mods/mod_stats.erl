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
-export([handle_input/5]).

handle_input(location, Frame, _OldLoc, NewLoc, StateData) ->
    NewLoc1 = nts_location:set(status, last_signal, Frame#frame.received, NewLoc),
    case maps:get(dtm, Frame#frame.data, undefined) of
        undefined -> {ok,
                      NewLoc1,
                      StateData};
        Dtm -> {ok,
                nts_location:set(status, last_signal_dtm, Dtm, NewLoc1),
                StateData}
    end.

