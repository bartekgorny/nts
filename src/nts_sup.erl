%%%-------------------------------------------------------------------
%% @doc nts top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    DbConnector = #{id => db_connector,
                    start => {nts_db_conn, start_link, []}},
    Children = [DbConnector],
    {ok, { {one_for_one, 2, 20}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
