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
-export([query/1, history/1, history/3, save_loc/4]).
-export([query/2, transaction/1]).
-export([frames/3, save_frame/2, update_loc/4, update_state/2, update_coords/4]).
-export([full_history/3]).
-export([current_state/1, last_loc/2, last_loc/1, last_state/1, last_state/2]).
-export([save_event/1, event_log/4, delete_events/3, clear_events/2]).
-export([create_device/3, read_device/1, update_device/2, delete_device/1]).
-export([initialise_device/1, purge_device/1]).
-export([table_exists/1]). % needed for tests


query(Q) ->
    log_query(Q),
    QType = hd(Q),
    Conn = nts_db_conn:get_connection(),
    nts_metrics:up([db, ops]),
    Ret = case {QType, epgsql:squery(Conn, Q)} of
              {$D, {ok, 0}} -> ok; % deleted 0 rows
              {_, {ok, Types, Values}} -> {Types, Values};
              {_, {ok, 0}} ->
                  nts_metrics:up([db, failed_ops]),
                  {error, op_failed};
              {_, {ok, _}} -> ok;
              {_, {error, #error{codename = EName}}} ->
                  nts_metrics:up([db, failed_ops]),
                  {error, EName}
          end,
    case Ret of
        {error, E} ->
            ?ERROR_MSG("Error running query:~n~p:~n~p~n~n", [Q, E]);
        _ ->
            ok
    end,
    nts_db_conn:free_connection(Conn),
    Ret.

query(Conn, Q) ->
    log_query(Q),
    nts_metrics:up([db, ops]),
    Ret = case epgsql:squery(Conn, Q) of
              {$D, {ok, 0}} -> ok; % deleted 0 rows
              {ok, Types, Values} -> {Types, Values};
              {ok, 0} ->
                  nts_metrics:up([db, failed_ops]),
                  {error, op_failed};
              {ok, _} -> ok;
              {error, #error{codename = EName}} ->
                  nts_metrics:up([db, failed_ops]),
                  {error, EName}
          end,
    case Ret of
        {error, _} ->
            throw(stop_that_transaction);
        _ ->
            ok
    end,
    nts_db_conn:free_connection(Conn),
    Ret.

transaction(F) ->
    Conn = nts_db_conn:get_connection(),
    Ret = epgsql:with_transaction(Conn, F),
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
        nts_utils:time2string(Start) ++
        "' AND dtm < '" ++
        nts_utils:time2string(Stop) ++
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
        nts_utils:time2string(Start) ++
        "' AND received < '" ++
        nts_utils:time2string(Stop) ++
        "' ORDER BY dtm",
    case query(Q) of
        {error, E} -> {error, E};
        {_, Vals} ->
            lists:map(fun(F) -> parse_frame(DevId, F) end, Vals)
    end.

-spec full_history(devid(), datetime(), datetime()) -> [{frame(), loc()}] | {error, any()}.
full_history(DevId, Start, Stop) ->
    Q = "SELECT id, hex, frame, received, dtm, coords, data FROM device_" ++
        binary_to_list(DevId) ++
        " WHERE received > '" ++
        nts_utils:time2string(Start) ++
        "' AND received < '" ++
        nts_utils:time2string(Stop) ++
        "' ORDER BY dtm",
    case query(Q) of
        {error, E} -> {error, E};
        {_, Vals} ->
            lists:map(fun(F) -> convert_both(DevId, F) end, Vals)
    end.

%% @doc if we receive new info we calculate new state of device and save it here, together
%% with the incoming frame, whatever it may be, and internal state data
%% ids are auto-generated
-spec save_loc(devid(), loc(), frame(), map()) -> ok | {error, atom()}.
save_loc(DevId, Loc, Frame, Internal) ->
    Q = "INSERT INTO device_" ++
        binary_to_list(DevId) ++
        "(id, dtm, coords, data, hex, frame, received, internal) values (" ++
        integer_to_list(Frame#frame.id) ++
        "," ++
        quote(nts_utils:time2string((Loc#loc.dtm))) ++
        "," ++
        quote(encode_coords(Loc#loc.lat, Loc#loc.lon)) ++
        "," ++
        quote(to_json(Loc#loc.data)) ++
        "," ++
        quote(Frame#frame.hex) ++
        "," ++
        quote(prepare_frame(Frame)) ++
        "," ++
        quote(nts_utils:time2string((Frame#frame.received))) ++
        "," ++
        quote(to_json(Internal)) ++
        ") ON CONFLICT (id) DO UPDATE" ++
        " SET dtm=" ++ quote(nts_utils:time2string((Loc#loc.dtm))) ++
        ", coords=" ++ io_lib:format("'(~p, ~p)'", [Loc#loc.lat, Loc#loc.lon]) ++
        ", data=" ++ quote(to_json(Loc#loc.data)) ++
        ", internal=" ++ quote(to_json(Internal)),
    query(Q).


%% @doc for storing frames only if received from buffer (are older then current state)
%% to be used later for reprocessing
-spec save_frame(devid(), frame()) -> ok | {error, atom()}.
save_frame(DevId, Frame) ->
    Q = "INSERT INTO device_" ++
    binary_to_list(DevId) ++
    "(id, hex, frame, received, dtm) values (" ++
    integer_to_list(Frame#frame.id) ++
    "," ++
    quote(Frame#frame.hex) ++
    "," ++
    quote(prepare_frame(Frame)) ++
    "," ++
    quote(nts_utils:time2string((Frame#frame.received))) ++
    "," ++
    quote(nts_utils:time2string(nts_frame:get(dtm, Frame))) ++
    ")",
    query(Q).

%% @doc if we reprocess data we use frames to recalculate state and then frame id to update
-spec update_loc(devid(), integer(), loc(), map()) -> ok | {error, atom()}.
update_loc(DevId, Id, Loc, Internal) ->
    Q = "UPDATE device_" ++ binary_to_list(DevId) ++
        " SET dtm=" ++ quote(nts_utils:time2string((Loc#loc.dtm))) ++
        ", coords=" ++ io_lib:format("'(~p, ~p)'", [Loc#loc.lat, Loc#loc.lon]) ++
        ", data=" ++ quote(to_json(Loc#loc.data)) ++
        ", internal=" ++ quote(to_json(Internal)) ++
        " WHERE id=" ++ integer_to_list(Id),
    query(Q).


-spec update_coords(devid(), integer(), float(), float()) -> ok | {error, atom()}.
update_coords(DevId, Id, Lat, Lon) ->
    Q = "UPDATE device_" ++ binary_to_list(DevId) ++
        " SET coords=" ++ io_lib:format("'(~p, ~p)'", [Lat, Lon]) ++
        " WHERE id=" ++ integer_to_list(Id),
    query(Q).

-spec update_state(devid(), loc()) -> ok.
update_state(DevId, Loc) ->
    Q = "INSERT INTO current " ++
        "(device, dtm, coords, data) values (" ++
        quote(DevId) ++
        "," ++
        quote(nts_utils:time2string((Loc#loc.dtm))) ++
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

get_last_loc(DevId, Dtm, Fields) ->
    % in nearly all cases we have a loc at this point
    Qdirect = "SELECT " ++ Fields ++ " FROM device_" ++ binary_to_list(DevId) ++
              " WHERE dtm=" ++ quote(nts_utils:time2string(Dtm)),
    case query(Qdirect) of
        {error, E} -> {error, E};
        {_, [R]} -> parse_loc(R);
        {_, []} ->
            Qindirect = "SELECT " ++ Fields ++ " FROM device_" ++ binary_to_list(DevId) ++
                        " WHERE dtm<" ++ quote(nts_utils:time2string(Dtm)) ++
                        " ORDER BY dtm DESC, id DESC LIMIT 1",
            case query(Qindirect) of
                {error, E} -> {error, E};
                {_, [R1]} -> parse_loc(R1);
                {_, []} -> undefined
            end
    end.

get_last_loc(DevId, Fields) ->
    % get the most recent loc
    Qdirect = "SELECT " ++ Fields ++ " FROM device_" ++ binary_to_list(DevId) ++
              " ORDER BY dtm DESC, id DESC LIMIT 1",
    case query(Qdirect) of
        {error, E} -> {error, E};
        {_, [R]} -> parse_loc(R);
        {_, []} -> undefined
    end.

-spec last_loc(devid(), datetime()) -> loc() | {error, atom()}.
last_loc(DevId, Dtm) ->
    get_last_loc(DevId, Dtm, "id, dtm, coords, data").

-spec last_loc(devid()) -> loc() | {error, atom()}.
last_loc(DevId) ->
    get_last_loc(DevId, "id, dtm, coords, data").

-spec last_state(devid(), datetime()) -> {loc(), internal()}
                                         | undefined
                                         | {error, atom()}.
last_state(DevId, Dtm) ->
    get_last_loc(DevId, Dtm, "id, dtm, coords, data, internal").

-spec last_state(devid()) -> {loc(), internal()} | {error, atom()} | undefined.
last_state(DevId) ->
    get_last_loc(DevId, "id, dtm, coords, data, internal").

-spec save_event(event()) -> ok | {error, atom()}.
save_event(#event{} = E) ->
    Q = "INSERT INTO events" ++
        "(device, dtm, coords, type, data) values (" ++
        quote(E#event.device) ++
        "," ++
        quote(nts_utils:time2string((E#event.dtm))) ++
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
        quote(nts_utils:time2string(Start)) ++
        " AND dtm < " ++
        quote(nts_utils:time2string(Stop)) ++
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
        quote(nts_utils:time2string(Start)) ++
        " AND dtm < " ++
        quote(nts_utils:time2string(Stop)) ++
        " AND device = " ++ quote(DevId),
    query(Q).

%% @doc Remove all events except for 'device up' and 'device down'
%% for use when reprocessing data - we will recreate all the other events
-spec clear_events(devid(), datetime()) -> ok | {error, atom()}.
clear_events(DevId, Start) ->
    Q = "DELETE FROM events" ++
        " WHERE dtm > " ++
        quote(nts_utils:time2string(Start)) ++
        " AND type NOT LIKE 'device:activity:%' " ++
        " AND device = " ++ quote(DevId),
    query(Q).

-spec create_device(devid(), atom(), binary()) -> ok | {error, atom()}.
create_device(DevId, Type, Label) ->
    Q = "INSERT INTO device (devid, devtype, label, config) values (" ++
        quote(DevId) ++
        "," ++
        quote(Type) ++
        "," ++
        quote(Label) ++
        ", '{}')",
    case query(Q) of
        ok ->
            initialise_device(DevId);
        E ->
            E
    end.

-spec initialise_device(devid()) -> ok | {error, atom()}.
initialise_device(DevId) ->
    Nid = <<"device_", DevId/binary>>,
    case table_exists(Nid) of
        true -> ok;
        false ->
            case do_initialise_device(Nid) of
                {rollback, Why} ->
                    {error, {rollback, Why}};
                _ -> ok
            end
    end.

do_initialise_device(Nid) ->
    {ok, S} = file:read_file("priv/pg_device.sql"),
    NS = binary:replace(S, <<"device_01">>, Nid, [global]),
    NSL1 = binary:split(NS, <<"\n">>, [global]),
    NSL2 = lists:filter(fun(Q) -> Q /= <<"">> andalso binary:part(Q, {0, 2}) /= <<"--">> end, NSL1),
    NS2 = lists:foldl(fun(E, A) -> <<A/binary, E/binary>> end, <<>>, NSL2),
    NSList = binary:split(NS2, <<";">>, [global]),
    transaction(fun(Conn) ->
        lists:map(fun(Q) -> case query(Conn, Q) of
                                ok -> ok;
                                {[], []} -> ok
                            end
                  end, NSList)
        end).

-spec purge_device(devid()) -> ok | {error, atom()}.
purge_device(DevId) ->
    Q = "DROP TABLE device_" ++ binary_to_list(DevId),
    query(Q).

-spec read_device(devid()) -> {devid(), binary(), binary(), map()} | undefined.
read_device(DevId) ->
    Q = "SELECT * FROM device WHERE devid=" ++ quote(DevId),
    {_, Vals} = query(Q),
    case Vals of
        [] -> undefined;
        [R] ->
            parse_device(R)
    end.

-spec delete_device(devid()) -> ok | {error, atom()}.
delete_device(DevId) ->
    Q = "DELETE FROM device WHERE devid=" ++ quote(DevId),
    query(Q).

-spec update_device(devid(), map()) -> ok | {error, atom()}.
update_device(DevId, Config) ->
    Q = "UPDATE device SET" ++
        " config=" ++ quote(to_json(Config)) ++
        "WHERE devid=" ++ quote(DevId),
    query(Q).

%% helpers

quote(null) -> null;
quote(S) when is_atom(S) ->
    quote(atom_to_list(S));
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

parse_device({_, _, DevId, Type, Label, BConfig}) ->
    {DevId, binary_to_existing_atom(Type, utf8),
     Label, nts_utils:json_decode_map(BConfig)}.

parse_loc({BId, D, Coords, BData}) ->
    Id = binary_to_integer(BId),
    Dt = parse_binary_datetime(D),
    Data = nts_utils:json_decode_map(BData),
    {Lat, Lon} = parse_binary_coords(Coords),
    #loc{id = Id, dtm = Dt, lat = Lat, lon = Lon, data = Data};
parse_loc({BId, D, Coords, BData, BInternal}) ->
    Loc = parse_loc({BId, D, Coords, BData}),
    Int = nts_utils:json_decode_map(BInternal),
    {Loc, Int}.

parse_state({DevId, D, Coords, BData}) ->
    {DevId, parse_loc({<<"0">>, D, Coords, BData})}.

-spec parse_binary_coords(binary() | null) -> {float(), float()} | undefined.
parse_binary_coords(null) ->
    {0, 0};
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

-spec parse_binary_datetime(binary() | null) -> datetime() | undefined.
parse_binary_datetime(null) ->
    undefined;
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

list_to_arith(L) ->
    case string:chr(L, $.) of
        0 -> list_to_integer(L);
        _ -> list_to_float(L)
    end.

parse_frame(DevId, R) ->
    {BId, BHex, Frame, BRec} = R,
    Id = binary_to_integer(BId),
    Hex = bin2bool(BHex),
    Rec = parse_binary_datetime(BRec),
    Data = case Hex of
               true -> nts_utils:hex2bin(Frame);
               false -> Frame
           end,
    #frame{id = Id, device = DevId, hex = Hex, data = Data, received = Rec}.

convert_both(DevId, R) ->
    {BId, BHex, BFrame, BRec, BDtm, BCoords, BData} = R,
    Id = binary_to_integer(BId),
    Hex = bin2bool(BHex),
    Rec = parse_binary_datetime(BRec),
    Data = case Hex of
               true -> nts_utils:hex2bin(BFrame);
               false -> BFrame
           end,
    Frame = #frame{id = Id, device = DevId, hex = Hex, data = Data, received = Rec},
    Loc = parse_loc({BId, BDtm, BCoords, BData}),
    {Frame, Loc}.

bin2bool(<<"t">>) -> true;
bin2bool(<<"f">>) -> false.

log_query(Q) ->
    case application:get_env(nts, log_queries) of
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
              binary:split(BType, <<":">>, [global])).

parse_event({BId, DevId, BDtm, BCoords, BType, BData}) ->
    Id = binary_to_integer(BId),
    Dtm = parse_binary_datetime(BDtm),
    {Lat, Lon} = parse_binary_coords(BCoords),
    Type = decode_event_type(BType),
    Data = nts_utils:json_decode_map(BData),
    #event{id = Id, device = DevId, dtm = Dtm, lat = Lat, lon = Lon, type = Type, data = Data}.


table_exists(Tname) when is_binary(Tname) ->
    table_exists(binary_to_list(Tname));
table_exists(Tname) ->
    Q = "SELECT * FROM pg_tables WHERE schemaname='public' AND tablename='" ++
         Tname ++
         "'",
    case query(Q) of
        {_, []} -> false;
        _ -> true
    end.

