%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2017 21:16
%%%-------------------------------------------------------------------
-module(mod_floatfilter).
-author("bartek").
-include_lib("nts/src/nts.hrl").
%% API
-export([handle_input/6]).

-define(MOVE_DISTANCE, 0.1).
-define(TRAIL_LEN, 5).


%% test API
-export([check_moving/2, newstate/0]).

%% mode is a tuple of:
%% moving | stopped
%% true | false, which means are we sure the mode is accurate
%% e.g. {moving, false} means we were moving but now we are not sure and have to
%% collect more data to find out
%% This implementation does not use {stopped, false} state
-type floatmode() :: {moving | stopped, boolean()}.

-type floatstate() :: map().

-type locdata() :: map().

-spec handle_input(frametype(), frame(), loc(), loc(), internal(),
                   nts_device:state()) ->
    {ok, loc(), internal()}.
handle_input(location, _Frame, _OldLoc, NewLoc, Internal, State) ->
    % if state changed to a definite state (different from the previous
    % definite state) create event data and return so that device can
    % generate an event.
    FState0 = maps:get(floatstate, Internal, newstate()),
    FState = decode_state(FState0),
    NewLocData = mapify(NewLoc),
    {FixedLocData, NewFState} = check_moving(NewLocData, FState),
    FixedLoc = nts_location:coords(maps:get(lat, FixedLocData), maps:get(lon, FixedLocData), NewLoc),
    ModePrev = maps:get(mode, FState),
    ModeNext = maps:get(mode, NewFState),
    DevId = nts_device:devid(State),
    maybe_fix_trail(ModePrev,
                    ModeNext,
                    maps:get(trail, FState),
                    DevId),
    NewFStateEnc = encode_state(NewFState),
    Internal1 = maybe_add_event(ModePrev, ModeNext, DevId, FixedLoc, Internal),
    {ok, FixedLoc, Internal1#{floatstate => NewFStateEnc}}.

newstate() -> #{refloc => undefined, mode => {moving, true}, trail => []}.

%% @doc Receives new location and floatfilter state; returns fixed location (meaning it may have
%% its coordinates reset to those of the reference location) and modified state.
-spec check_moving(locdata(), floatstate()) -> {locdata(), floatstate()}.
check_moving(NewLoc, #{refloc := undefined} = FState) ->
    {NewLoc, FState#{refloc => NewLoc}};
check_moving(NewLoc, FState) ->
    #{refloc := RefLoc, mode := Mode, trail := Trail} = FState,
    IsMove = is_move(NewLoc, RefLoc),
    {NewMode, FixedLoc, NewState} = check_moving(Mode, IsMove, Trail,
                                                 NewLoc, FState), 
    % ^ duplication for greater readability
    {FixedLoc, NewState#{mode => NewMode}}.

-spec check_moving(Mode :: floatmode(),
                   IsMove :: boolean(),
                   Trail :: [map()],
                   NewLoc :: locdata(),
                   FState :: floatstate()) ->
    {floatmode(), locdata(), floatstate()}.
check_moving({moving, true}, true, [], NewLoc, FState) ->
    {{moving, true}, NewLoc, FState#{refloc => NewLoc}};
check_moving({moving, true}, false, [], NewLoc, FState) ->
    {{moving, false}, set_to_ref(NewLoc, FState), add_to_trail(NewLoc, FState)};
check_moving({moving, false}, true, _Trail, NewLoc, FState) ->
    % turns out we are still moving
    % re-save locations from Trail (they were saved with coords set to ref)
    {{moving, true}, NewLoc, FState#{trail => []}};
check_moving({moving, false}, false, Trail, NewLoc, FState) ->
    case length(Trail) of
        ?TRAIL_LEN ->
            {{stopped, true}, set_to_ref(NewLoc, FState), FState#{trail => []}};
        _ ->
            {{moving, false}, set_to_ref(NewLoc, FState), add_to_trail(NewLoc, FState)}
    end;
check_moving({stopped, true}, false, [], NewLoc, FState) ->
    {{stopped, true}, set_to_ref(NewLoc, FState), FState};
check_moving({stopped, true}, true, [], NewLoc, FState) ->
    %% if needed we could add some logic to make sure he left for good (meaning: enter another
    %% state and collect some trail before changing to `moving`)
    {{moving, true}, NewLoc, FState};
check_moving(_, _, _, NewLoc, FState) ->
    % catch-all - reset state
    ?WARNING_MSG("no previous match, resetting state, ~p", {NewLoc, FState}),
    {{moving, true}, NewLoc, #{}}.

is_move(A, B) ->
    #{lat := LatA, lon := LonA} = A,
    #{lat := LatB, lon := LonB} = B,
    Dist = nts_utils:distance({LatA, LonA}, {LatB, LonB}),
    Dist > ?MOVE_DISTANCE.

%% set location coords to those of reference location
set_to_ref(Loc, #{refloc := Ref}) ->
    #{lat := Lat, lon := Lon} = Ref,
    Loc#{lat => Lat, lon := Lon}.

add_to_trail(Loc, FState) ->
    Trail = maps:get(trail, FState),
    FState#{trail => [Loc | Trail]}.

mapify(Loc) ->
    {Lat, Lon} = nts_location:coords(Loc),
    #{id => nts_location:id(Loc), lat => Lat, lon => Lon}.

encode_state(#{mode := {M, S}} = FState) ->
    FState#{mode => <<(encode_m(M))/binary, ",", (encode_s(S))/binary>>}.

decode_state(#{mode := {_, _}} = FState) ->
    FState;
decode_state(#{mode := BMode} = FState) ->
    [M, S] = binary:split(BMode, <<",">>),
    FState#{mode => {decode_m(M), decode_s(S)}}.

encode_m(M) ->
    atom_to_binary(M, utf8).

decode_m(M) ->
    binary_to_existing_atom(M, utf8).

encode_s(true) -> <<"true">>;
encode_s(false) -> <<"false">>.

decode_s(<<"true">>) -> true;
decode_s(<<"false">>) -> false.

maybe_fix_trail({moving, false}, {moving, true}, Trail, DevId) ->
    lists:map(fun(#{id := Id, lat := Lat, lon := Lon}) ->
                  nts_db:update_coords(DevId, Id, Lat, Lon)
              end,
              Trail),
    ok;
maybe_fix_trail(_, _, _, _) ->
    ok.

maybe_add_event({stopped, _}, {moving, true}, DevId, Loc, Internal) ->
    nts_device:add_event(nts_event:create_event([device, moving, started],
                                                DevId,
                                                Loc,
                                                nts_location:dtm(Loc)),
                         Internal);
maybe_add_event({moving, _}, {stopped, true}, DevId, Loc, Internal) ->
    nts_device:add_event(nts_event:create_event([device, moving, stopped],
                                                DevId,
                                                Loc,
                                                nts_location:dtm(Loc)),
                         Internal);
maybe_add_event(_, _, _, _, Internal) ->
    Internal.
