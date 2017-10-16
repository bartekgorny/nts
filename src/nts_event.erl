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
-export([emit_event/4, emit_event/5]).

emit_event(EType, DevId, undefined, Dtm) ->
    emit_event(EType, DevId, #loc{}, Dtm);
emit_event(EType, DevId, Loc, Dtm) ->
    emit_event(EType, DevId, Loc, Dtm, #{}).

emit_event(EType, DevId, Loc, Dtm, Data) ->
    Evt = #event{device = DevId,
                 dtm = Dtm,
                 type = EType,
                 lat = Loc#loc.lat,
                 lon = Loc#loc.lon,
                 data = Data},
    ok = nts_db:save_event(Evt), % crash here
    nts_hooks:run(publish_event, [], [EType, Evt]).
