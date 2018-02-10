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
    setup_listeners(Listeners),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    Listeners = nts_config:get_value(listen),
    setup_listeners(Listeners),
    gen_event:add_sup_handler(system_bus, nts_tcp_eh, []),
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Child = #{id => listener, start => {nts_tcp, start_link, []}},

    {ok, {SupFlags, [Child]}}.

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
    lists:map(fun(Id) -> supervisor:terminate_child(?SERVER, Id) end,
              [Pid || {_, Pid, _, _} <- supervisor:which_children(?SERVER)]).

start_listener({DType, _, Port}) ->
    supervisor:start_child(?SERVER, [DType, Port]).
