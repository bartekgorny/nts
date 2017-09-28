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

-export([run_procloc/6, run/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec run_procloc(DeviceType :: atom(),
                  InputType :: atom() | [atom()],
                  InputData :: frame(),
                  OldLoc :: loc(),
                  NewLoc :: loc(),
                  StateData :: map()) -> {loc(), map()} | {error, atom()}.
run_procloc(DeviceType, InputType, InputData, OldLoc, NewLoc, StateData) ->
    Section = case ets:lookup(hooks, procloc) of
                  [] -> [];
                  [{_, HSection}] -> HSection
              end,
    Handlers = case proplists:get_value(DeviceType, Section) of
              undefined -> proplists:get_value(generic, Section);
              Lst -> Lst
          end,
    run_loc_handlers(Handlers, InputType, InputData, OldLoc, NewLoc, StateData).

-spec run(Section :: atom(), Acc :: any(), Args :: [any()]) -> any().
run(Hook, Acc, Args) ->
    Handlers = case ets:lookup(hooks, Hook) of
                  [] -> [];
                  [{_, HList}] -> HList
              end,
    run_handlers(Handlers, Acc, Args).

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
    lists:map(fun create_section/1, HookList),
    ok.

create_section({Hook, HandlerList}) ->
    Entries = create_entries(Hook, HandlerList),
    ets:insert(hooks, {Hook, Entries}).

create_entries(procloc, HandlerList) ->
    Map = lists:foldl(fun addhandler/2, #{}, HandlerList),
    lists:map(fun(K) -> {K, gen_sublist(K, Map)} end, maps:keys(Map));
create_entries(_, HandlerList) ->
    Lst = lists:map(fun addotherhandler/1, HandlerList),
    Lst1 = lists:sort(Lst),
    lists:map(fun({_, M, F}) -> {M, F} end, Lst1).

addotherhandler({Mod, Seq}) ->
    addotherhandler({Mod, handle_input, Seq});
addotherhandler({Mod, Fun, Seq}) ->
    {Seq, Mod, Fun}.

addhandler({DType, Mod, Seq}, Acc) ->
    addhandler({DType, Mod, handle_input, Seq}, Acc);
addhandler({DType, Mod, Fun, Seq}, Acc) ->
    DMap = maps:get(DType, Acc, #{}),
    maps:put(DType, maps:put(Seq, {Mod, Fun}, DMap), Acc).

gen_sublist(K, Map) ->
    M1 = maps:merge(maps:get(generic, Map), maps:get(K, Map)),
    Plist = maps:to_list(M1),
    [{M, F} || {_, {M, F}} <- lists:usort(Plist)].

run_loc_handlers([], _, _, _, NewLoc, StateData) ->
    {NewLoc, StateData};
run_loc_handlers([H|Handlers], InputType, InputData, OldLoc, NewLoc, StateData) ->
    {Mod, Fun} = H,
    try Mod:Fun(InputType, InputData, OldLoc, NewLoc, StateData) of
        {ok, NewLoc1, NewStateData} ->
            run_loc_handlers(Handlers, InputType, InputData, OldLoc, NewLoc1, NewStateData);
        {stop, NewLoc1, NewState} ->
            {NewLoc1, NewState};
        E ->
            ?ERROR_MSG("Error - handler ~p:~p returned ~p", [Mod, Fun, E]),
            {error, E}
    catch Etype:Eval ->
        ?ERROR_MSG("Error - handler ~p:~p threw ~p:~p", [Mod, Fun, Etype, Eval]),
        {error, {Etype, Eval}}
    end.

run_handlers([], Acc, _) ->
    Acc;
run_handlers([H|Tail], Acc, Args) ->
    {Mod, Fun} = H,
    Acc1 = apply(Mod, Fun, [Acc | [Args]]),
    run_handlers(Tail, Acc1, Args).
