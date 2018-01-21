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
-export([insert_handler/2, remove_handler/2]). % for dynamically manipulating handlers (tests only)

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
                  State :: term()) -> {loc(), map()} | {error, term()}.
run_procloc(DeviceType, InputType, InputData, OldLoc, NewLoc, Internal, State) ->
    Section = case ets:lookup(hooks, procloc) of
                  [] -> [];
                  [{_, HSection}] -> HSection
              end,
    Handlers = case proplists:get_value(DeviceType, Section) of
                   undefined -> proplists:get_value(generic, Section, []);
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

-spec insert_handler(atom(), term()) -> ok | {error, term()}.
insert_handler(Hook, Handler) ->
    gen_server:call(?SERVER, {insert_handler, Hook, Handler}).

-spec remove_handler(atom(), term()) -> ok | {error, term()}.
remove_handler(Hook, Handler) ->
    gen_server:call(?SERVER, {remove_handler, Hook, Handler}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    init_hook_table(),
    gen_event:add_sup_handler(system_bus, nts_hooks_eh, []),
    {ok, #state{}}.

handle_call({insert_handler, Hook, Handler}, _From, State) ->
    Resp = do_insert_handler(Hook, Handler),
    {reply, Resp, State};
handle_call({remove_handler, Hook, Handler}, _From, State) ->
    Resp = do_remove_handler(Hook, Handler),
    {reply, Resp, State};
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
    lists:sort(Lst).

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
    [{Seq, M, F} || {Seq, {M, F}} <- lists:usort(Plist)].

run_loc_handlers([], _, _, _, NewLoc, Internal, _State) ->
    {NewLoc, Internal};
run_loc_handlers([H|Handlers], InputType, InputData, OldLoc, NewLoc, Internal, State) ->
    {_, Mod, Fun} = H,
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
    {_, Mod, Fun} = H,
    try apply(Mod, Fun, [Acc | Args]) of
        {ok, Acc1} ->
            run_handlers(Tail, Acc1, Args);
        {stop, Acc1} ->
            Acc1;
        E ->
            ?ERROR_MSG("Error - handler ~p:~p returned ~p",
                       [Mod, Fun, E]),
            {error, E}
    catch Etype:Eval ->
        ?ERROR_MSG("Error - handler ~p:~p threw ~p:~p",
            [Mod, Fun, Etype, Eval]),
        {error, {Etype, Eval}}
    end.

do_insert_handler(procloc, Handler) ->
    Modifier = fun({Dev, Mod, Fun, Seq}, Handlers) ->
                   DevHandlers = proplists:get_value(Dev, Handlers, []),
                   NDevHandlers = nts_utils:insort({Seq, Mod, Fun}, DevHandlers),
                   [{Dev, NDevHandlers} | proplists:delete(Dev, Handlers)]
               end,
    do_modify_handlers(procloc, Handler, Modifier);
do_insert_handler(Hook, Handler) ->
    Modifier = fun({Mod, Fun, Seq}, Handlers) ->
                   nts_utils:insort({Seq, Mod, Fun}, Handlers)
               end,
    do_modify_handlers(Hook, Handler, Modifier).

do_remove_handler(procloc, Handler) ->
    Modifier = fun({Dev, Mod, Fun, Seq}, Handlers) ->
        DevHandlers = proplists:get_value(Dev, Handlers, []),
        case lists:delete({Seq, Mod, Fun}, DevHandlers) of
            [] -> proplists:delete(Dev, Handlers);
            NDevHandlers -> [{Dev, NDevHandlers} | proplists:delete(Dev, Handlers)]
        end
               end,
    do_modify_handlers(procloc, Handler, Modifier);
do_remove_handler(Hook, Handler) ->
    Modifier = fun({Mod, Fun, Seq}, Handlers) ->
                   lists:delete({Seq, Mod, Fun}, Handlers)
               end,
    do_modify_handlers(Hook, Handler, Modifier).

do_modify_handlers(Hook, Handler, Modifier) ->
    Handlers = case ets:lookup(hooks, Hook) of
                   [] -> [];
                   [{_, HList}] -> HList
               end,
    Handler1 = fix_handler(Hook, Handler),
    NHandlers = Modifier(Handler1, Handlers),
    ets:insert(hooks, {Hook, NHandlers}),
    ok.

fix_handler(procloc, {Dev, Module, Seq}) ->
    {Dev, Module, handle_input, Seq};
fix_handler(procloc, H) ->
    H;
fix_handler(_, {Module, Seq}) ->
    {Module, handle_input, Seq};
fix_handler(_, H) ->
    H.

