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
-include_lib("epgsql/include/epgsql.hrl").

%% API
-export([query/1, history/1, history/3, save_loc/3]).
-export([frames/3, save_frame/2, update_loc/3, update_state/2]).
-export([current_state/1, last_loc/2]).
-export([save_event/1, event_log/4, delete_events/3]).


query(Q) ->
    log_query(Q),
    Conn = nts_db_conn:get_connection(),
    nts_metrics:up([db, ops]),
    Ret = case epgsql:squery(Conn, Q) of
              {ok, Types, Values} -> {Types, Values};
              {ok, 0} ->
                  nts_metrics:up([db, failed_ops]),
                  {error, op_failed};
              {ok, _} -> ok;
              {error, #error{codename = EName}} ->
                  nts_metrics:up([db, failed_ops]),
                  {error, EName}
          end,
    nts_db_conn:free_connection(Conn),
    Ret.

-spec history(devid()) -> [loc()] | {error, atom()}.
history(DevId) ->
    history(DevId, {{2000, 1, 1}, {0, 0, 0}}, {{2100, 1, 1}, {0, 0, 0}}).

-spec history(devid(), datetime(), datetime()) -> [loc()] | {error, atom()}.
history(DevId, Start, Stop) ->
    Q = "SELECT id, dtm, coords, data FROM device_" ++
        binary_to_list(DevId) ++
        " WHERE dtm > '" ++
        time2string(Start) ++
        "' AND dtm < '" ++
        time2string(Stop) ++
        "' ORDER BY dtm",
    case query(Q) of
        {error, E} -> {error, E};
        {_, Vals} ->
            lists:map(fun parse_loc/1, Vals)
    end.

-spec frames(devid(), datetime(), datetime()) -> [frame()] | {error, any()}.
frames(DevId, Start, Stop) ->
    Q = "SELECT id, hex, frame, received FROM device_" ++
        binary_to_list(DevId) ++
        " WHERE received > '" ++
        time2string(Start) ++
        "' AND received < '" ++
        time2string(Stop) ++
        "' ORDER BY dtm",
    case query(Q) of
        {error, E} -> {error, E};
        {_, Vals} ->
            lists:map(fun(F) -> convert_frame(DevId, F) end, Vals)
    end.

%% @doc if we receive new info we calculate new state of device and save it here, together
%% with the incoming frame, whatever it may be
-spec save_loc(devid(), loc(), frame()) -> ok | {error, atom()}.
save_loc(DevId, Loc, Frame) ->
    Q = "INSERT INTO device_" ++
        binary_to_list(DevId) ++
        "(dtm, coords, data, hex, frame, received) values (" ++
        quote(time2string((Loc#loc.dtm))) ++
        "," ++
        quote(encode_coords(Loc#loc.lat, Loc#loc.lon)) ++
        "," ++
        quote(to_json(Loc#loc.data)) ++
        "," ++
        quote(Frame#frame.hex) ++
        "," ++
        quote(prepare_frame(Frame)) ++
        "," ++
        quote(time2string((Frame#frame.received))) ++
        ")",
    query(Q).

%% @doc for storing frames only if received from buffer (are older then current state)
%% to be used later for reprocessing
-spec save_frame(devid(), frame()) -> ok | {error, atom()}.
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
    query(Q).

%% @doc if we reprocess data we use frames to recalculate state and then frame id to update
-spec update_loc(devid(), integer(), loc()) -> ok | {error, atom()}.
update_loc(DevId, Id, Loc) ->
    Q = "UPDATE device_" ++ binary_to_list(DevId) ++
        " SET dtm=" ++ quote(time2string((Loc#loc.dtm))) ++
        ", coords=" ++ io_lib:format("'(~p, ~p)'", [Loc#loc.lat, Loc#loc.lon]) ++
        ", data=" ++ quote(to_json(Loc#loc.data)) ++
        " WHERE id=" ++ integer_to_list(Id),
    query(Q).


-spec update_state(devid(), loc()) -> ok.
update_state(DevId, Loc) ->
    Q = "INSERT INTO current " ++
        "(device, dtm, coords, data) values (" ++
        quote(DevId) ++
        "," ++
        quote(time2string((Loc#loc.dtm))) ++
        "," ++
        quote(encode_coords(Loc#loc.lat, Loc#loc.lon)) ++
        "," ++
        quote(to_json(Loc#loc.data)) ++
        ") ON CONFLICT ( device ) DO UPDATE SET " ++
        " dtm=EXCLUDED.dtm, coords=EXCLUDED.coords, data=EXCLUDED.data",
    query(Q).

-spec current_state(devid() | [devid()]) -> loc() | undefined | [{devid(), loc()}].
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

-spec last_loc(devid(), datetime()) -> loc() | {error, atom()}.
last_loc(DevId, Dtm) ->
    % in nearly all cases we have a loc at this point
    Qdirect = "SELECT id, dtm, coords, data FROM device_" ++ binary_to_list(DevId) ++
              " WHERE dtm=" ++ quote(time2string(Dtm)),
    case query(Qdirect) of
        {error, E} -> {error, E};
        {_, [R]} -> parse_loc(R);
        {_, []} ->
            Qindirect = "SELECT id, dtm, coords, data FROM device_" ++ binary_to_list(DevId) ++
                        " WHERE dtm<" ++ quote(time2string(Dtm)) ++
                        " ORDER BY dtm DESC LIMIT 1",
            case query(Qindirect) of
                {error, E} -> {error, E};
                {_, [R1]} -> parse_loc(R1);
                {_, []} -> #loc{}
            end
    end.

-spec save_event(event()) -> ok | {error, atom()}.
save_event(#event{} = E) ->
    Q = "INSERT INTO events" ++
        "(device, dtm, coords, type, data) values (" ++
        quote(binary_to_list(E#event.device)) ++
        "," ++
        quote(time2string((E#event.dtm))) ++
        "," ++
        quote(encode_coords(E#event.lat, E#event.lon)) ++
        "," ++
        quote(encode_event_type(E#event.type)) ++
        "," ++
        quote(to_json(E#event.data)) ++
        ")",
    query(Q).

-spec event_log(devid(), eventtype(), datetime(), datetime()) -> [event()] | {error, atom()}.
event_log(DevId, EType, Start, Stop) ->
    Q = "SELECT id, device, dtm, coords, type, data FROM events" ++
        " WHERE dtm > " ++
        quote(time2string(Start)) ++
        " AND dtm < " ++
        quote(time2string(Stop)) ++
        " AND device = " ++ quote(DevId) ++
        " AND type LIKE '" ++ encode_event_type(EType) ++ "%'" ++
        " ORDER BY dtm",
    case query(Q) of
        {error, E} -> {error, E};
        {_, Vals} ->
            lists:map(fun parse_event/1, Vals)
    end.

-spec delete_events(devid(), datetime(), datetime()) -> ok | {error, atom()}.
delete_events(DevId, Start, Stop) ->
    Q = "DELETE FROM events" ++
        " WHERE dtm > " ++
        quote(time2string(Start)) ++
        " AND dtm < " ++
        quote(time2string(Stop)) ++
        " AND device = " ++ quote(DevId),
    query(Q).

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

encode_coords(Lat, Lon) ->
    io_lib:format("(~p, ~p)", [Lat, Lon]).

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

convert_frame(DevId, R) ->
    {BId, BHex, Frame, BRec} = R,
    Id = binary_to_integer(BId),
    Hex = bin2bool(BHex),
    Rec = parse_binary_datetime(BRec),
    Data = case Hex of
               true -> nts_utils:hex2bin(Frame);
               false -> Frame
           end,
    #frame{id = Id, device = DevId, hex = Hex, data = Data, received = Rec}.


bin2bool(<<"t">>) -> true;
bin2bool(<<"f">>) -> false.

log_query(Q) ->
    case application:get_env(postgres, log_queries) of
        {ok, true} ->
            ?INFO_MSG("QUERY: ~s", [Q]);
        _ ->
            ok
    end.

encode_event_type(Tags) ->
    H = atom_to_list(hd(Tags)),
    Qs = lists:foldl(fun(B, Acc) -> Acc ++ ":" ++ atom_to_list(B) end, [H], tl(Tags)),
    lists:flatten(Qs).

decode_event_type(BType) ->
    lists:map(fun(B) -> binary_to_existing_atom(B, utf8) end,
              binary:split(BType, <<":">>)).

parse_event({BId, DevId, BDtm, BCoords, BType, BData}) ->
    Id = binary_to_integer(BId),
    Dtm = parse_binary_datetime(BDtm),
    {Lat, Lon} = parse_binary_coords(BCoords),
    Type = decode_event_type(BType),
    Data = nts_utils:json_decode_map(BData),
    #event{id = Id, device = DevId, dtm = Dtm, lat = Lat, lon = Lon, type = Type, data = Data}.



