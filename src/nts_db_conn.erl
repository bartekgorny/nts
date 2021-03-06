%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% an abstraction layer over dbase connection
%%% current implementation is very rudimentary - it uses only one connection which
%%% internally (in epgsql) queues all requests, so there is no concurrency
%%% @end
%%% Created : 02. Sep 2017 12:41
%%%-------------------------------------------------------------------
-module(nts_db_conn).
-author("bartek").
-include("nts.hrl").

-behaviour(gen_server).

%% API

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(CONN_TIMEOUT, 5000).

-record(state, {connection}).

%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(_) ->
    process_flag(trap_exit, true), % so that it calls terminate
    Conn = connect(),
    {ok, #state{connection = Conn}}.

handle_call({query, Q}, _From, #state{connection = Conn} = State) ->
    Res = nts_db:query(Conn, Q, fun nts_db:or_error/2),
    {reply, Res, State};
handle_call({transaction, F}, _From, #state{connection = Conn} = State) ->
    Res = epgsql:with_transaction(Conn, F),
    {reply, Res, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({stop, Reason}, _From, State) ->
    {stop,  Reason, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({stop, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
    ?ERROR_MSG("Terminating ~p", [self()]),
    cleanup(State),
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cleanup(#state{connection = Conn}) ->
    disconnect(Conn).

connect() ->
    {ok, ConnData} = application:get_env(nts, postgres_conn_data),
    {ok, Conn} = epgsql:connect(ConnData),
    Conn.

disconnect(undefined) ->
    ok;
disconnect(Conn) ->
    ?ERROR_MSG("Disconnecting ~p", [self()]),
    epgsql:close(Conn).

