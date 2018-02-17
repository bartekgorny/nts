%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2018 22:52
%%%-------------------------------------------------------------------
-module(nts_tcp).
-author("bartekgorny").

-behaviour(gen_server).

-include_lib("nts/src/nts.hrl").
%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {dtype, port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(atom(), integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(DType, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DType, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([DType, Port]) ->
    timer:sleep(100), % reduce restart intensity (lame)
    ok = start_listener(DType, Port),
    {ok, #state{dtype = DType, port = Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_listener(DType, Port) ->
    Opts = [{active, true}, binary, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            S = self(),
            spawn(fun() -> accept(ListenSocket, DType, S) end),
            ok;
        E ->
            E
    end.

accept(LS, DType, Listener) ->
    monitor(process, Listener),
    case gen_tcp:accept(LS) of
        {ok, S} ->
            spawn(fun() -> accept(LS, DType, Listener) end),
            server(S, DType);
        Other ->
            ?ERROR_MSG("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

server(Socket, DType) ->
    server(Socket, DType, <<"">>, undefined, undefined).

server(Socket, DType, Buffer, DevId, Dev) ->
    receive
        {tcp, _, Data} ->
            ct:pal("Data: ~p", [Data]),
            Frame = nts_frame:parse(DType, Data),
            {DeviceId, Device} = get_device(DevId, Frame#frame.device, Dev),
            maybe_process_frame(Device, Frame),
            server(Socket, DType, Buffer, DeviceId, Device);
        {tcp_closed, _} ->
            stop_device(Dev),
            ok;
        {'DOWN', A, B, C, normal} ->
            ct:pal("{stopping, self()}: ~p", [{stopping, self(), A, B, C}]),
            ok;
        E ->
            ?ERROR_MSG("TCP connector terminated:~n~p~n~n", [E]),
            exit(unexpected_tcp_termination)

    end.

get_device(undefined, undefined, undefined) ->
    {undefined, undefined};
get_device(undefined, DevId, undefined) ->
    {ok, Dev} = nts_device:start_link(DevId, self()),
    ct:pal("{started, DevId, Dev}: ~p", [{started, DevId, Dev}]),
    {DevId, Dev};
get_device(DevId, _, Dev) when is_pid(Dev) ->
    {DevId, Dev}.

maybe_process_frame(undefined, _) ->
    ok;
maybe_process_frame(_, undefined) ->
    ok;
maybe_process_frame(Device, #frame{} = Frame) when is_pid(Device) ->
    nts_device:process_frame(Device, Frame).

stop_device(undefined) ->
    ok;
stop_device(Dev) when is_pid(Dev) ->
    nts_device:stop(Dev).
