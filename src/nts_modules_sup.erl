%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Feb 2018 18:59
%%%-------------------------------------------------------------------
-module(nts_modules_sup).
-author("bartekgorny").

-behaviour(supervisor).

-include_lib("nts/src/nts.hrl").

%% API
-export([start_link/0]).
-export([reload/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_event:add_sup_handler(system_bus, nts_modules_eh, []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

reload() ->
    Mods = case nts_config:get_value(modules) of
               undefined -> [];
               ModuleList -> ModuleList
           end,
    ToRun = sets:from_list([M || {M, _} <- Mods]),
    Children = supervisor:which_children(?MODULE),
    Running = sets:from_list([M || {M, _, _, _} <- Children]),
    % stop modules if config changed
    lists:map(fun(Mod) ->
                  Conf = proplists:get_value(Mod, Mods),
                  [{Mod, RunConf}] = ets:lookup(running_modules, Mod),
                  case {Conf, RunConf} of
                      {RunConf, Conf} -> ok;
                      _ ->
                          supervisor:terminate_child(?MODULE, Mod),
                          supervisor:delete_child(?MODULE, Mod)
                  end
              end,
              sets:to_list(sets:intersection(ToRun, Running))),
    % stop redundant modules
    lists:map(fun(Mod) ->
                  supervisor:terminate_child(?MODULE, Mod),
                  supervisor:delete_child(?MODULE, Mod)
              end,
              sets:to_list(sets:subtract(Running, ToRun))),
    % now start
    Children2 = supervisor:which_children(?MODULE),
    Running2 = sets:from_list([M || {M, _, _, _} <- Children2]),
    lists:map(fun(Mod) ->
                  Conf = proplists:get_value(Mod, Mods),
                  AChild = #{id => Mod,
                             start => {Mod, start_link, [Conf]},
                             restart => permanent,
                             shutdown => 2000,
                             type => worker},
                  supervisor:start_child(?MODULE, AChild),
                  ets:insert(running_modules, {Mod, Conf})
              end,
              sets:to_list(sets:subtract(ToRun, Running2))),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    ets:new(running_modules, [named_table, public]),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},


    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

