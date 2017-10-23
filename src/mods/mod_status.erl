%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2017 16:45
%%%-------------------------------------------------------------------
-module(mod_status).
-author("bartekgorny").
-include_lib("nts/src/nts.hrl").

%% API
-export([handle_input/6]).

-spec handle_input(frametype(), frame(), loc(), loc(), internal(),
                   nts_device:state()) ->
    {ok, loc(), internal()}.
handle_input(location, _Frame, _OldLoc, NewLoc, Internal, _State) ->
    {ok, nts_location:set(status, up, true, NewLoc), Internal}.
