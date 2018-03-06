%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2018 23:08
%%%-------------------------------------------------------------------
-module(nts_tcp_sup).
-author("bartekgorny").

-behaviour(supervisor).
-include_lib("nts/src/nts.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([reload/0]).

%%% for tests
-export([terminate_listeners/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

reload() ->
    Listeners = nts_config:get_value(listen),
    % FIXME it should be more intelligent
    setup_listeners(Listeners),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Listeners = nts_config:get_value(listen),
    setup_listeners(Listeners),
    gen_event:add_sup_handler(system_bus, nts_tcp_eh, []),
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
        permanent, 5000, supervisor, [ranch_sup]},

    {ok, {SupFlags, [RanchSupSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_listeners(undefined) ->
    ok;
setup_listeners(Listeners) ->
    ?ERROR_MSG("Listeners reloaded:~n~p~n~n", [Listeners]),
    terminate_listeners(),
    lists:map(fun start_listener/1, Listeners),
    ok.

terminate_listeners() ->
    lists:map(fun(Id) ->
                  supervisor:terminate_child(?SERVER, Id),
                  supervisor:delete_child(?SERVER, Id)
              end,
              [Pid || {_, Pid, _, _} <- supervisor:which_children(?SERVER)]).

start_listener({DType, _, Port}) ->
    ListenerSpec = ranch:child_spec(DType,
                                    100,
                                    ranch_tcp,
                                    [{port, Port}],
                                    nts_tcp,
                                    []
                                   ),
    supervisor:start_child(?SERVER, ListenerSpec).
