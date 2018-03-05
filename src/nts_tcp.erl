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

start_link(DType, Port) ->
    gen_server:start_link(?MODULE, [DType, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([DType, Port]) ->
    timer:sleep(100), % reduce restart intensity (lame)
    ok = start_listener(DType, Port),
    {ok, #state{dtype = DType, port = Port}}.

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
        {error, closed} ->
            ok;
        Other ->
            ?ERROR_MSG("accept returned ~w",[Other]),
            ok
    end.

server(Socket, DType) ->
    server(Socket, DType, <<"">>, undefined, undefined).

server(Socket, DType, Buffer, DevId, Dev) ->
    process_flag(trap_exit, true),
    receive
        {tcp, _, Data} ->
            Frame = nts_frame:parse(DType, Data),
            {DeviceId, Device} = get_device(DevId, Frame#frame.device, Dev),
            maybe_process_frame(Device, Frame),
            server(Socket, DType, Buffer, DeviceId, Device);
        {tcp_closed, _} ->
            % connection terminated - this is normal
            stop_device(Dev),
            ok;
        {'DOWN', _, _, _, normal} ->
            % listener being shut down
            gen_tcp:close(Socket),
            ok;
        {'EXIT', Dev, Reason} ->
            gen_tcp:close(Socket),
            ?ERROR_MSG("TCP terminated because ~p exited with '~p'", [DevId, Reason]),
            ok;
        E ->
            gen_tcp:close(Socket),
            ?ERROR_MSG("TCP connector terminated:~n~p~n~n", [E]),
            exit(unexpected_tcp_termination)

    end.

get_device(undefined, undefined, undefined) ->
    {undefined, undefined};
get_device(undefined, DevId, undefined) ->
    {ok, Dev} = nts_device:start_link(DevId, self()),
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
