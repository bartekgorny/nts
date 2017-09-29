%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2017 22:01
%%%-------------------------------------------------------------------
-module(nts_config).
-author("bartekgorny").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([get_value/1, get_value/2, reload/0]).

-include("nts.hrl").
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc supports nesting, but doesn't allow mixing values with sublists
get_value([H|Tail]) ->
    case get_value(H) of
        undefined -> undefined;
        V -> get_nested(V, Tail)
    end;
get_value(Name) ->
    case ets:lookup(runtime_config, Name) of
        [{Name, Val}] -> Val;
        [] -> undefined
    end.

get_value(Name, Default) ->
    case get_value(Name) of
        undefined -> Default;
        Val -> Val
    end.

reload() ->
    gen_server:call(?SERVER, reload).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ets:new(runtime_config, [named_table, {read_concurrency, true}]),
    reload_config(),
    {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(reload, _From, State) ->
    reload_config(),
    {reply, ok, State};
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

reload_config() ->
    ets:new(runtime_config_backup, [named_table, private]),
    lists:map(fun([H]) -> ets:insert(runtime_config_backup, H) end,
              ets:match(runtime_config, '$1')),
    try
        do_reload_config()
    catch
        E:F ->
            ?ERROR_MSG("Failed to reload config, caught ~p:~p", [E, F]),
            ets:delete_all_objects(runtime_config),
            lists:map(fun([H]) -> ets:insert(runtime_config, H) end,
                      ets:match(runtime_config_backup, '$1'))
    end,
    ets:delete(runtime_config_backup).

do_reload_config() ->
    ets:delete_all_objects(runtime_config),
    {ok, Cf} = application:get_env(nts, config),
    ?INFO_MSG("Loading config from file: ~p", [Cf]),
    {ok, Data} = file:consult(Cf),
    lists:map(fun(H) -> ets:insert(runtime_config, H) end, Data),
    ok.

get_nested(undefined, _) -> undefined;
get_nested(V, []) -> V;
get_nested(V, _) when not is_list(V) -> undefined;
get_nested(V, [H|T]) ->
    get_nested(proplists:get_value(H, V, undefined), T).

