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

-include_lib("nts/src/nts.hrl").

%% API
-export([handle_publishstate/3]).

handle_publishstate(Acc, DevId, Loc) ->
    Key = <<"device-state-", DevId/binary>>,
    J = nts_utils:encode_location(Loc),
    nts_redis:q(["SET", Key, J]),
    nts_redis:q(["PUBLISH", Key, "new-location"]),
    nts_redis:q(["PUBLISH", <<"device-state">>, DevId]),
    {ok, Acc}.


