%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 21:57
%%%-------------------------------------------------------------------
-module(event_listener).
-author("bartekgorny").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([send/1, flush/0]).

% hook handler
-export([publish_event/3]).

-define(SERVER, ?MODULE).

-record(state, {events = []}).

flush() ->
    gen_server:call(?MODULE, flush).

send(Evt) ->
    gen_server:call(?MODULE, Evt).

publish_event(Acc, EType, Evt) ->
    send({EType, Evt}),
    {ok, Acc}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    {ok, #state{}}.

handle_call(flush, _From, State) ->
    {reply, lists:reverse(State#state.events), State#state{events = []}};
handle_call(Evt, _From, State) ->
    Q = State#state.events,
    NState = State#state{events = [Evt | Q]},
    {reply, ok, NState}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

