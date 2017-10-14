%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2017 22:19
%%%-------------------------------------------------------------------
-module(toolset_SUITE).
-author("bartekgorny").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

-compile(export_all).

all() ->
    [stabiliser].


init_per_suite(C) ->
    C.

end_per_suite(_Config) -> ok.

stabiliser(_) ->
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}],
        {3, 3}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 4, 1}],
        {3, 3}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 4, 1}, {5, 5}],
        {5, 5}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 40}],
        {3, 3}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}],
        {3, 3}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}, {7, 5}],
        {7, 5}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}, {7, 43},
         {8, 44}, {9, 45}],
        {9, 45}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}, {7, 43},
         {8, 44}, {9, 45}, {10, 4}],
        {9, 45}),
    apply_and_check(
        [{1, 1}, {2, 2}, {3, 3}, {1000, 40}],
        {1000, 40}),
    ok.

apply_and_check(Defs, Exp) ->
    {Last, _} = apply_defs(Defs),
    {Dtm, _, Lon} = Last,
    {_, {_, _, Sec}} = Dtm,
    Offset = 10000 * (Lon - 1),
    ?assertEqual(Exp, {Sec, round(Offset)}).

apply_defs(Defs) ->
    Locs = loc_gen(Defs),
    {FirstLoc, FirstSat} = hd(Locs),
    {First, State} = mod_stabiliser:filter_loc(FirstLoc, FirstSat, undefined),
    lists:foldl(fun filter_loc/2, {First, State}, tl(Locs)).

filter_loc({Loc, Sat}, {_, State}) ->
    ct:pal("{Loc, Sat, State}: ~p", [{Loc, Sat, State}]),
    mod_stabiliser:filter_loc(Loc, Sat, State).


loc_gen(Defs) ->
    lists:reverse(loc_gen(Defs, [])).

loc_gen([], Acc) -> Acc;
loc_gen([H|T], Acc) ->
    loc_gen(T, [loc(H)|Acc]).

% offset - 1 is 0.0001 deg, which is about eleven meters; this gives us
% a speed of about 40km/h. An offset of 10 gives us an "insane" speed.
loc({Time, Offset}) ->
    loc({Time, Offset, 8});
loc({Time, Offset, Sat}) ->
    {{{{2017, 9, 12}, {5, 14, Time}},
      1, 1 + Offset / 10000},
     Sat}.
