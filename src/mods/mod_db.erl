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

-include_lib("nts/src/nts.hrl").

%% API
-export([handle_savestate/5, handle_publishstate/3]).

handle_savestate(Acc, DevId, Loc, Frame, Internal) ->
    save_or_update(Frame#frame.id, DevId, Loc, Frame, Internal),
    {ok, Acc}.

handle_publishstate(Acc, DevId, Loc) ->
    ok = nts_db:update_state(DevId, Loc),
    {ok, Acc}.


save_or_update(_, DevId, Loc, Frame, Internal) ->
    ok = nts_db:save_loc(DevId, Loc, Frame, Internal).
%save_or_update(Id, DevId, Loc, _Frame, Internal) ->
    %ok = nts_db:update_loc(DevId, Id, Loc, Internal).
