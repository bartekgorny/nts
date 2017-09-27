%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Hook management - we want it to be a gen_server so that there is a
%%% process which might subscribe to a config reload event
%%% @end
%%% Created : 13. Sep 2017 22:55
%%%-------------------------------------------------------------------
-module(nts_hooks).
-author("bartekgorny").

-behaviour(gen_server).
-include_lib("nts/src/nts.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([run/6]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec run(DeviceType :: atom(),
          InputType :: atom | [atom()],
          InputData :: frame(),
          OldLoc :: loc(),
          NewLoc :: loc(),
          StateData :: map()) -> {loc(), map()} | {error, atom()}.
run(DeviceType, InputType, InputData, OldLoc, NewLoc, StateData) ->
    Res = case ets:lookup(hooks, DeviceType) of
              [] -> ets:lookup(hooks, global);
              HList -> HList
          end,
    [{_, Handlers}] = Res,
    run_handlers(Handlers, InputType, InputData, OldLoc, NewLoc, StateData).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    init_hook_table(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
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

init_hook_table() ->
    ets:new(hooks, [named_table]),
    ets:delete_all_objects(hooks),
    HookList = nts_config:get_value(hooks),
    Entries = create_entries(HookList),
    ets:insert(hooks, Entries),
    ok.

create_entries(HookList) ->
    Map = lists:foldl(fun addhook/2, #{}, HookList),
    lists:map(fun(K) -> {K, gen_sublist(K, Map)} end, maps:keys(Map)).

addhook({DType, Mod, Seq}, Acc) ->
    addhook({DType, Mod, handle_input, Seq}, Acc);
addhook({DType, Mod, Fun, Seq}, Acc) ->
    DMap = maps:get(DType, Acc, #{}),
    maps:put(DType, maps:put(Seq, {Mod, Fun}, DMap), Acc).

gen_sublist(K, Map) ->
    M1 = maps:merge(maps:get(global, Map), maps:get(K, Map)),
    Plist = maps:to_list(M1),
    [{M, F} || {_, {M, F}} <- lists:usort(Plist)].

run_handlers([], _, _, _, NewLoc, StateData) ->
    {NewLoc, StateData};
run_handlers([H|Handlers], InputType, InputData, OldLoc, NewLoc, StateData) ->
    {Mod, Fun} = H,
    try Mod:Fun(InputType, InputData, OldLoc, NewLoc, StateData) of
        {ok, NewLoc1, NewStateData} ->
            run_handlers(Handlers, InputType, InputData, OldLoc, NewLoc1, NewStateData);
        {stop, NewLoc1, NewState} ->
            {NewLoc1, NewState};
        E ->
            ?ERROR_MSG("Error - handler ~p:~p returned ~p", [Mod, Fun, E]),
            {error, E}
    catch Etype:Eval ->
        ?ERROR_MSG("Error - handler ~p:~p threw ~p:~p", [Mod, Fun, Etype, Eval]),
        {error, {Etype, Eval}}
    end.
