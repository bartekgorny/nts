%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2017 19:51
%%%-------------------------------------------------------------------
-author("bartekgorny").

-record(loc, {id, dtm, lat, lon, data = #{}}).
-type loc() :: #loc{}.

-type devid() :: binary().
-type datetime() :: calendar:datetime().

%% 'hex' tells us if it is a string to be stored as-is, or a binary data which need to be hexlified
%% for storage.
-record(frame, {id = 0, received, data = <<>>, hex = false}).
-type frame() :: #frame{}.


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