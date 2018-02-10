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
    SysBus = #{id => system_bus,
               start => {gen_event, start_link, [{local, system_bus}]}},
    DbConnector = #{id => db_connector,
                    start => {nts_db_conn, start_link, []}},
    Config = #{id => config_manager,
               start => {nts_config, start_link,[]}},
    Hooks = #{id => hook_manager,
              start => {nts_hooks, start_link,[]}},
    Tcp = #{id => tcp_listener,
            type => supervisor,
            start => {nts_tcp_sup, start_link, []}},
    Children = [SysBus, Config, DbConnector, Hooks, Tcp],
    {ok, { {one_for_one, 2, 20}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
