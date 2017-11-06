%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2017 19:51
%%%-------------------------------------------------------------------
-author("bartekgorny").

-type devid() :: binary().
-type datetime() :: calendar:datetime().

-record(loc, {id = 0, dtm, lat = 0.0, lon = 0.0, data = #{}}).
-type loc() :: #loc{id :: integer(), dtm :: datetime() | undefined, lat :: float(),
                    lon :: float(), data :: map()}.

%% data in location is a map which has three possible keys
-type datapart() :: logistic | status | sensor.

-type eventtype() :: [atom()].
-record(event, {id = 0, device, dtm, lat, lon, type, data = #{}}).
-type event() :: #event{id :: integer(), device :: devid(), dtm :: datetime(), lat :: float(),
                        lon :: float(), type :: eventtype(), data :: map()}.


%% 'hex' tells us if it is a string to be stored as-is, or a binary data which need to be hexlified
%% for storage.
%% 'values' is where we keep parsed data
-record(frame, {id = 0, device, received, type, data = <<>>, hex = false, values = #{}}).
%% frame type may be undefined if we retrieve them from database for reprocessing
%% frame parser returns it with frametype set
-type frametype() :: location | event | hearbeat | undefined.
-type frame() :: #frame{id :: integer(), device :: devid(), received :: datetime(),
                        type :: frametype(), data :: binary(), hex :: boolean(), values :: map()}.

%% internal state of device (e.g. trails for stabilisation, moving avgs etc)
-type internal() :: map().

-define(DEBUG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).

-define(METRICS_SPIRAL, [[db, ops],
                         [db, failed_ops]]
).

-define(assertClose(DtmA, DtmB),
    begin
        case nts_helpers:compare_near_dates(DtmA, DtmB) of
            true -> ok;
            false ->
                erlang:error({assertClose,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {date_a, DtmA},
                               {date_b, DtmB}]})
        end
    end).

-define(assertNotClose(DtmA, DtmB),
    begin
        case nts_helpers:compare_near_dates(neg, DtmA, DtmB) of
            true -> ok;
            false ->
                erlang:error({assertNotClose,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {date_a, DtmA},
                               {date_b, DtmB}]})
        end
    end).

