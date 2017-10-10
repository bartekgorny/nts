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

-export([run_procloc/7, run/3, reload/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec run_procloc(DeviceType :: atom(),
                  InputType :: atom() | [atom()],
                  InputData :: frame(),
                  OldLoc :: loc(),
                  NewLoc :: loc(),
                  Internal :: map(),
                  State :: term()) -> {loc(), map()} | {error, atom()}.
run_procloc(DeviceType, InputType, InputData, OldLoc, NewLoc, Internal, State) ->
    Section = case ets:lookup(hooks, procloc) of
                  [] -> [];
                  [{_, HSection}] -> HSection
              end,
    Handlers = case proplists:get_value(DeviceType, Section) of
                   undefined -> proplists:get_value(generic, Section);
                   Lst -> Lst
               end,
    run_loc_handlers(Handlers, InputType, InputData, OldLoc, NewLoc, 
                     Internal, State).

-spec run(Section :: atom(), Acc :: any(), Args :: [any()]) -> any().
run(Hook, Acc, Args) ->
    Handlers = case ets:lookup(hooks, Hook) of
                  [] -> [];
                  [{_, HList}] -> HList
              end,
    run_handlers(Handlers, Acc, Args).

-spec reload() -> ok.
reload() ->
    gen_server:call(?SERVER, reload),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    init_hook_table(),
    gen_event:add_sup_handler(system_bus, nts_hooks_eh, []),
    {ok, #state{}}.

handle_call(reload, _From, State) ->
    init_hook_table(),
    {reply, ok, State};
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
    case ets:info(hooks) of
        undefined ->
            ets:new(hooks, [named_table]);
        _ ->
            ok
    end,
    ets:delete_all_objects(hooks),
    HookList = nts_config:get_value(hooks, []),
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

run_loc_handlers([], _, _, _, NewLoc, Internal, _State) ->
    {NewLoc, Internal};
run_loc_handlers([H|Handlers], InputType, InputData, OldLoc, NewLoc, Internal, State) ->
    {Mod, Fun} = H,
    DevId = nts_device:devid(State),
    try Mod:Fun(InputType, InputData, OldLoc, NewLoc, Internal, State) of
        {ok, NewLoc1, NewInternal} ->
            run_loc_handlers(Handlers, InputType, InputData, OldLoc, NewLoc1,
                             NewInternal, State);
        {stop, NewLoc1, NewInternal} ->
            {NewLoc1, NewInternal};
        E ->
            ?ERROR_MSG("Error in ~p - handler ~p:~p returned ~p",
                       [DevId, Mod, Fun, E]),
            {error, E}
    catch Etype:Eval ->
        ?ERROR_MSG("Error in ~p - handler ~p:~p threw ~p:~p",
                   [DevId, Mod, Fun, Etype, Eval]),
        {error, {Etype, Eval}}
    end.

run_handlers([], Acc, _) ->
    Acc;
run_handlers([H|Tail], Acc, Args) ->
    {Mod, Fun} = H,
    Acc1 = apply(Mod, Fun, [Acc | Args]),
    run_handlers(Tail, Acc1, Args).
