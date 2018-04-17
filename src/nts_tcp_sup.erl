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
%%-export([terminate_listeners/0]).

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

init([]) ->
%%    Listeners = nts_config:get_value(listen),
    ets:new(tcp_listeners, [named_table, public, bag]),
%%    setup_listeners(Listeners),
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
    Running = lists:map(fun hd/1, ets:match(tcp_listeners, '$1')),
    Rs = sets:from_list(Running),
    Ls = sets:from_list(Listeners),
    lists:map(fun stop_listener/1, sets:to_list(sets:subtract(Rs, Ls))),
    lists:map(fun start_listener/1, sets:to_list(sets:subtract(Ls, Rs))),
    ok.

stop_listener(Lspec) ->
    Id = {ranch_listener_sup, Lspec},
    supervisor:terminate_child(?SERVER, Id),
    supervisor:delete_child(?SERVER, Id).

start_listener(Lspec) ->
    {DType, Proto, Port} = Lspec,
    ListenerSpec = ranch:child_spec(Lspec,
                                    ranch_tcp,
                                    [{port, Port}, {num_acceptors, 100}],
                                    nts_tcp,
                                    []
                                   ),
    case supervisor:start_child(?SERVER, ListenerSpec) of
        {error, E} -> 
            {error, E};
        Ret ->
            ets:insert(tcp_listeners, {DType, Proto, Port}),
            Ret
    end.
