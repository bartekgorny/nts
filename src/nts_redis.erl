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
-export([get_connection/0]).

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

-spec(start_link(map()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Conf) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Conf], []).

-spec(get_connection() -> pid()).
get_connection() -> gen_server:call(?SERVER, get_connection).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Conf]) ->
    Host = maps:get(host, Conf),
    Port = maps:get(port, Conf, 6379),
    Db = maps:get(db, Conf, 0),
    Password = maps:get(password, Conf, ""),
    Timeout = maps:get(timeout, Conf, 1000),
    {ok, P} = eredis:start_link(Host, Port, Db, Password, Timeout),
    {ok, #state{connection = P}}.

handle_call(get_connection, _From, State) ->
    {reply, State#state.connection, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
