%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2018 22:42
%%%-------------------------------------------------------------------
-module(nts_redis_worker).
-author("bartekgorny").

-behaviour(gen_server).

-include_lib("nts/src/nts.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection}).

start_link(Conf) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Conf], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Conf) ->
    Host = maps:get(host, Conf),
    Port = maps:get(port, Conf, 6379),
    Db = maps:get(db, Conf, 0),
    Password = maps:get(password, Conf, ""),
    Timeout = maps:get(timeout, Conf, 1000),
    {ok, P} = eredis:start_link(Host, Port, Db, Password, Timeout),
    {ok, #state{connection = P}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Request, #state{connection = Conn} = State) ->
    eredis:q(Conn, Request),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
