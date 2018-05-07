%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2018 23:10
%%%-------------------------------------------------------------------
-module(test_device).
-author("bartekgorny").

-behaviour(gen_server).

-include_lib("nts/src/nts.hrl").

%% API
-export([start/1, start_link/1, stop/1, set/3, printstate/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {devid, lat = 52, lon = 21, speed = 0, direction = 0,
                socket, timer}).

%%%===================================================================
%%% API
%%%===================================================================

start(DevId) ->
    gen_server:start(?MODULE, [DevId], []).

start_link(DevId) ->
    gen_server:start_link(?MODULE, [DevId], []).

stop(Pid) ->
    gen_server:stop(Pid).

-spec set(pid(), speed | dir, integer()) -> ok.
set(Dev, What, Value) ->
    gen_server:cast(Dev, {set, What, Value}),
    ok.

printstate(Dev) ->
    gen_server:cast(Dev, printstate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([DevId]) ->
    State = case nts_db:current_state(DevId) of
                undefined ->
                    #state{devid = DevId};
                #loc{lat = Lat, lon = Lon} ->
                    #state{devid = DevId, lat = Lat, lon = Lon}
            end,

    {ok, Timer} = timer:send_interval(5000, new_location),
    {ok, Socket} = gen_tcp:connect("localhost", 12345, []),
    {ok, State#state{timer = Timer, socket = Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set, speed, V}, State) ->
    {noreply, State#state{speed = V}};
handle_cast({set, dir, V}, State) ->
    {noreply, State#state{direction = V}};
handle_cast(printstate, State) ->
    ?ERROR_MSG("Current state: ~p", [State]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(new_location, State) ->
    NState = send_new_location(State),
    {noreply, NState};
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

send_new_location(#state{lat = Lat, lon = Lon, speed = Speed} = State) ->
    {Lat1, Lon1} = maybe_move(Lat, Lon, Speed),
    ?ERROR_MSG("{Lat1, Lon1, Speed}:~n~p~n~n", [{Lat1, Lon1, Speed}]),
    Frame = mkframe(formula, State#state.devid, 0, {Lat1, Lon1}),
    ?ERROR_MSG("Frame:~n~p~n~n", [Frame]),
    gen_tcp:send(State#state.socket, Frame),
    State#state{lat = Lat1, lon = Lon1}.

maybe_move(Lat, Lon, 0) ->
    {Lat, Lon};
maybe_move(Lat, Lon, _) ->
    {Lat + 0.001, Lon + 0.001}.

mkframe(formula, DevId, Offset, {Lat, Lon}) ->
    Trail = <<"0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    <<_:5/binary>> = DevId,
    BLat = format_coord(Lat),
    BLon = format_coord(Lon),
    {{Y, M, D}, {H, Mi, S}} = fromnow(Offset),
    Dtm = <<(integer_to_binary(Y))/binary,
        (pad(M))/binary,
        (pad(D))/binary,
        (pad(H))/binary,
        (pad(Mi))/binary,
        (pad(S))/binary>>,
    <<"a", DevId/binary, ",", Dtm/binary, ",F1", ",", BLon/binary, ",",
        BLat/binary, ",", Trail/binary, 10>>.

pad(I) ->
    list_to_binary(
        string:right(integer_to_list(I), 2, $0)).

format_coord(I) when is_integer(I) ->
    format_coord(I, 0);
format_coord(I) when is_float(I) ->
    format_coord(floor(I), floor(1000000 * (I - floor(I)))).

format_coord(I, F) ->
    In = string:right(integer_to_list(I), 2, $0),
    Fn = string:left(integer_to_list(F), 6, $0),
    <<(list_to_binary(In))/binary, ".", (list_to_binary(Fn))/binary>>.

fromnow(Offset) ->
    N = os:timestamp(),
    Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(N)) + Offset,
    calendar:gregorian_seconds_to_datetime(Sec).

