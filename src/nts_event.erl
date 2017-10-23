%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 21:42
%%%-------------------------------------------------------------------
-module(nts_event).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").

%% API
-export([create_event/4, create_event/5]).

-spec create_event(eventtype(), devid(), loc() | undefined, datetime()) ->
    ok.
create_event(EType, DevId, undefined, Dtm) ->
    create_event(EType, DevId, #loc{}, Dtm);
create_event(EType, DevId, Loc, Dtm) ->
    create_event(EType, DevId, Loc, Dtm, #{}).

create_event(EType, DevId, Loc, Dtm, Data) ->
    Evt = #event{device = DevId,
                 dtm = Dtm,
                 type = EType,
                 lat = Loc#loc.lat,
                 lon = Loc#loc.lon,
                 data = Data},
    Evt.
%%    ok = nts_db:save_event(Evt), % crash here
%%    nts_hooks:run(publish_event, [], [EType, Evt]),
%%    ok.
