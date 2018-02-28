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
    ?ERROR_MSG("Key:~n~p~n~n", [Key]),
    Conn = nts_redis:get_connection(),
    J = nts_utils:encode_location(Loc),
    eredis:q(Conn, ["SET", Key, J]),
    {ok, Acc}.


