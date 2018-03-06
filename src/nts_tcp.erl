%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2018 00:01
%%%-------------------------------------------------------------------
-module(nts_tcp).
-behaviour(ranch_protocol).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    Transport:setopts(Socket, [{active, true}]),
    ok = ranch:accept_ack(Ref),
    server(Socket, Ref, Transport).

server(Socket, DType, Transport) ->
    server(Socket, DType, Transport, <<"">>, undefined, undefined).

server(Socket, DType, Transport, Buffer, DevId, Dev) ->
    process_flag(trap_exit, true),
    receive
        {tcp, _, Data} ->
            {NBuffer, DataList} = nts_utils:rebuffer(Buffer, Data),
            ProcFun = fun(D, {DevId0, Dev0}) ->
                          Frame = nts_frame:parse(DType, D),
                          {DeviceId, Device} = get_device(DevId0,
                                                          Frame#frame.device,
                                                          Dev0),
                          maybe_process_frame(Device, Frame),
                          {DeviceId, Device}
                      end,
            {DeviceId, Device} = lists:foldl(ProcFun, {DevId, Dev}, DataList),
            server(Socket, DType, Transport, NBuffer, DeviceId, Device);
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
