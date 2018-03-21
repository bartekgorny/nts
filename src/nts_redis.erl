%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Feb 2018 18:16
%%%-------------------------------------------------------------------
-module(nts_redis).
-author("bartekgorny").

-behaviour(gen_server).

-include_lib("nts/src/nts.hrl").

%% API
-export([start_link/1]).
-export([q/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection}).

%%%===================================================================
%%% API
%%%===================================================================

%%I am not actually sure we need gen_server, traping exit and terminate
%%here, maybe we could get by with just a start function (like
%%in nts_db)

-spec(start_link(map()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Conf) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Conf], []).

q(Q) ->
    wpool:cast(redis_pool, Q, random_worker).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Conf]) ->
    process_flag(trap_exit, true),
    wpool:start(),
    PoolOpts = [
        {workers, 10},
        {worker, {nts_redis_worker, Conf}}
    ],
    wpool:start_sup_pool(redis_pool, PoolOpts),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    wpool:stop_pool(redis_pool),
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
