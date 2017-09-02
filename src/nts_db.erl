%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2017 22:35
%%%-------------------------------------------------------------------
-module(nts_db).
-author("bartekgorny").
-include("nts.hrl").

%% API
-export([query/1, history/1, history/3, save_loc/3]).
-export([frames/3, save_frame/2, update_loc/3, update_state/2]).
-export([current_state/1, last_loc/2]).


query(Q) ->
    log_query(Q),
    Conn = nts_db_conn:get_connection(),
    Ret = case epgsql:squery(Conn, Q) of
              {ok, Types, Values} -> {Types, Values};
              {ok, _} -> ok
          end,
    nts_db_conn:free_connection(Conn),
    Ret.

-spec history(devid()) -> [loc()].
history(DevId) ->
    history(DevId, {{2000, 1, 1}, {0, 0, 0}}, {{2100, 1, 1}, {0, 0, 0}}).

-spec history(devid(), datetime(), datetime()) -> [loc()].
history(DevId, Start, Stop) ->
    Q = "SELECT id, dtm, coords, data FROM device_" ++
        binary_to_list(DevId) ++
        " WHERE dtm > '" ++
        time2string(Start) ++
        "' AND dtm < '" ++
        time2string(Stop) ++
        "' ORDER BY dtm",
    {_, Vals} = query(Q),
    lists:map(fun parse_loc/1, Vals).

-spec frames(devid(), datetime(), datetime()) -> [frame()].
frames(DevId, Start, Stop) ->
    Q = "SELECT id, hex, frame, received FROM device_" ++
        binary_to_list(DevId) ++
        " WHERE received > '" ++
        time2string(Start) ++
        "' AND received < '" ++
        time2string(Stop) ++
        "' ORDER BY dtm",
    {_, Vals} = query(Q),
    lists:map(fun convert_frame/1, Vals).

%% @doc if we receive new info we calculate new state of device and save it here, together
%% with the incoming frame, whatever it may be
-spec save_loc(devid(), loc(), frame()) -> ok.
save_loc(DevId, Loc, Frame) ->
    Q = "INSERT INTO device_" ++
        binary_to_list(DevId) ++
        "(dtm, coords, data, hex, frame, received) values (" ++
        quote(time2string((Loc#loc.dtm))) ++
        "," ++
        io_lib:format("'(~p, ~p)'", [Loc#loc.lat, Loc#loc.lon]) ++
        "," ++
        quote(to_json(Loc#loc.data)) ++
        "," ++
        quote(Frame#frame.hex) ++
        "," ++
        quote(prepare_frame(Frame)) ++
        "," ++
        quote(time2string((Frame#frame.received))) ++
        ")",
    query(Q),
    ok.

%% @doc for storing frames only if received from buffer (are older then current state)
%% to be used later for reprocessing
-spec save_frame(devid(), frame()) -> ok.
save_frame(DevId, Frame) ->
    Q = "INSERT INTO device_" ++
    binary_to_list(DevId) ++
    "(hex, frame, received) values (" ++
    quote(Frame#frame.hex) ++
    "," ++
    quote(prepare_frame(Frame)) ++
    "," ++
    quote(time2string((Frame#frame.received))) ++
    ")",
    query(Q),
    ok.

%% @doc if we reprocess data we use frames to recalculate state and then frame id to update
-spec update_loc(devid(), integer(), loc()) -> ok.
update_loc(DevId, Id, Loc) ->
    Q = "UPDATE device_" ++ binary_to_list(DevId) ++
        " SET dtm=" ++ quote(time2string((Loc#loc.dtm))) ++
        ", coords=" ++ io_lib:format("'(~p, ~p)'", [Loc#loc.lat, Loc#loc.lon]) ++
        ", data=" ++ quote(to_json(Loc#loc.data)) ++
        " WHERE id=" ++ integer_to_list(Id),
    query(Q),
    ok.


-spec update_state(devid(), loc()) -> ok.
update_state(DevId, Loc) ->
    Q = "INSERT INTO current " ++
        "(device, dtm, coords, data) values (" ++
        quote(DevId) ++
        "," ++
        quote(time2string((Loc#loc.dtm))) ++
        "," ++
        io_lib:format("'(~p, ~p)'", [Loc#loc.lat, Loc#loc.lon]) ++
        "," ++
        quote(to_json(Loc#loc.data)) ++
        ") ON CONFLICT ( device ) DO UPDATE SET " ++
        " dtm=EXCLUDED.dtm, coords=EXCLUDED.coords, data=EXCLUDED.data",
    query(Q),
    ok.

-spec current_state(devid() | [devid()]) -> [loc()] | [{devid(), loc()}].
current_state(DevId) when is_binary(DevId) ->
    Q = "SELECT * FROM current WHERE device=" ++ quote(DevId),
    {_, Vals} = query(Q),
    case Vals of
        [] -> undefined;
        [R] ->
            {_, L} = parse_state(R),
            L
    end;
current_state(DevIds) when is_list(DevIds) ->
    Dids = lists:map(fun binary_to_list/1, DevIds),
    H = quote(hd(Dids)),
    Qs = lists:foldl(fun(B, Acc) -> Acc ++ "," ++ quote(B) end, [H], tl(Dids)),
    Q = "SELECT * FROM current WHERE device IN (" ++ Qs ++ ")",
    {_, Res} = query(Q),
    lists:map(fun parse_state/1, Res).

-spec last_loc(devid(), datetime()) -> loc().
last_loc(DevId, Dtm) ->
    % in nearly all cases we have a loc at this point
    Qdirect = "SELECT id, dtm, coords, data FROM device_" ++ binary_to_list(DevId) ++
              " WHERE dtm=" ++ quote(time2string(Dtm)),
    {_, Res} = query(Qdirect),
    case Res of
        [R] -> parse_loc(R);
        [] ->
            Qindirect = "SELECT id, dtm, coords, data FROM device_" ++ binary_to_list(DevId) ++
                        " WHERE dtm<" ++ quote(time2string(Dtm)) ++
                        " ORDER BY dtm DESC LIMIT 1",
            {_, Res1} = query(Qindirect),
            case Res1 of
                [R1] -> parse_loc(R1);
                [] -> #loc{}
            end
    end.

%% helpers

quote(S) when is_binary(S) ->
    quote(binary_to_list(S));
quote(S) ->
    io_lib:format("'~s'", [S]).

prepare_frame(#frame{hex = false} = Frame) ->
    binary_to_list(Frame#frame.data);
prepare_frame(#frame{hex = true} = Frame) ->
    nts_utils:bin2hex(Frame#frame.data).

to_json(Data) ->
    binary_to_list(nts_utils:json_encode_map(Data)).

parse_loc({BId, D, Coords, BData}) ->
    Id = binary_to_integer(BId),
    Dt = parse_binary_datetime(D),
    Data = nts_utils:json_decode_map(BData),
    {Lat, Lon} = parse_binary_coords(Coords),
    #loc{id = Id, dtm = Dt, lat = Lat, lon = Lon, data = Data}.

parse_state({DevId, D, Coords, BData}) ->
    {DevId, parse_loc({<<"0">>, D, Coords, BData})}.

-spec parse_binary_coords(binary()) -> {float(), float()}.
parse_binary_coords(Coords) ->
    [A, O] = binary:split(Coords, <<$,>>),
    Lat = list_to_arith(tl(binary_to_list(A))),
    Lon = list_to_arith(
        lists:reverse(
            tl(
                lists:reverse(
                    binary_to_list(O)
                )
            )
        )
    ),
    {Lat, Lon}.

-spec parse_binary_datetime(binary()) -> datetime().
parse_binary_datetime(R) ->
    L = binary:bin_to_list(R),
    Y = extract_int(L, 1, 4),
    M = extract_int(L, 6, 2),
    D = extract_int(L, 9, 2),
    H = extract_int(L, 12, 2),
    Mi = extract_int(L, 15, 2),
    S = extract_int(L, 18, 2),
    {{Y, M, D}, {H, Mi, S}}.

-spec extract_int(string(), integer(), integer()) -> integer().
extract_int(L, Start, Len) ->
    list_to_integer(lists:sublist(L, Start, Len)).

-spec time2string(datetime()) -> string().
time2string(T) ->
    {{Y, M, D}, {H, Mi, S}} = T,
    lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p", [Y, M, D, H, Mi, S])).

list_to_arith(L) ->
    case string:chr(L, $.) of
        0 -> list_to_integer(L);
        _ -> list_to_float(L)
    end.

convert_frame(R) ->
    {BId, BHex, Frame, BRec} = R,
    Id = binary_to_integer(BId),
    Hex = bin2bool(BHex),
    Rec = parse_binary_datetime(BRec),
    Data = case Hex of
               true -> nts_utils:hex2bin(Frame);
               false -> Frame
           end,
    #frame{id = Id, hex = Hex, data = Data, received = Rec}.


bin2bool(<<"t">>) -> true;
bin2bool(<<"f">>) -> false.

log_query(Q) ->
    case application:get_env(postgres, log_queries) of
        {ok, true} ->
            ?INFO_MSG("QUERY: ~s", [Q]);
        _ ->
            ok
    end.

