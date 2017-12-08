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
-import(nts_helpers, [fromnow/1]).

-compile(export_all).

all() ->
    [
     %stabiliser,
     floatfilter
     %dateutil,
     %utilities,
     %mapper
    ].


init_per_suite(C) ->
    C.

end_per_suite(_Config) -> ok.

stabiliser(_) ->
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}],
                    {3, 3}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 4, 1}],
                    {3, 3}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 4, 1}, {5, 5}],
                    {5, 5}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 40}],
                    {3, 3}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}],
                    {3, 3}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}, {7, 5}],
                    {7, 5}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}, {7, 43},
                     {8, 44}, {9, 45}],
                    {9, 45}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {4, 40}, {5, 41}, {6, 42}, {7, 43},
                     {8, 44}, {9, 45}, {10, 4}],
                    {9, 45}),
    apply_and_check(stabiliser, 
                    [{1, 1}, {2, 2}, {3, 3}, {1000, 40}],
                    {1000, 40}),
    ok.

floatfilter(_) ->
    % moving
    apply_and_check(floatfilter,
                    [{1, 1}, {1, 20}],
                    {1, 20}),
    % uncertain, then moved
    apply_and_check(floatfilter,
                    [{1, 1}, {1, 5}, {1, 8}, {1, 3}],
                    {1, 1}),
    % stopped, then moved
    apply_and_check(floatfilter,
                    [{1, 1}, {1, 5}, {1, 8}, {1, 3}, {1, 4}, {1, 7}, {1, 20}],
                    {1, 20}),
    ok.

mapper(_) ->
    SensorDefs = #{sensor_a => 1, sensor_b => 2, sensor_c => 3},
    % straight, no custom mapping
    ?assertEqual({sensor_a, 5}, mod_mapping:map_sensor(sensor_a, 5, 4, SensorDefs, #{})),
    ?assertEqual({sensor_a, 0}, mod_mapping:map_sensor(sensor_a, 0, 4, SensorDefs, #{})),
    ?assertEqual({sensor_a, 0}, mod_mapping:map_sensor(sensor_a, undefined, 4, SensorDefs, #{})),
    ?assertEqual({sensor_b, 5}, mod_mapping:map_sensor(sensor_b, 5, 4, SensorDefs, #{})),
    ?assertEqual({sensor_b, 0}, mod_mapping:map_sensor(sensor_b, 0, 4, SensorDefs, #{})),
    ?assertEqual({sensor_b, 4}, mod_mapping:map_sensor(sensor_b, undefined, 4, SensorDefs, #{})),
    ?assertEqual({sensor_c, 5}, mod_mapping:map_sensor(sensor_c, 5, 4, SensorDefs, #{})),
    ?assertEqual({sensor_c, 4}, mod_mapping:map_sensor(sensor_c, 0, 4, SensorDefs, #{})),
    ?assertEqual({sensor_c, 4}, mod_mapping:map_sensor(sensor_c, undefined, 4, SensorDefs, #{})),
    % remap to itself to change type
    Conf = #{sensor_a => #{name => sensor_a, type => 3}},
    ?assertEqual({sensor_a, 5}, mod_mapping:map_sensor(sensor_a, 5, 4, SensorDefs, Conf)),
    ?assertEqual({sensor_a, 4}, mod_mapping:map_sensor(sensor_a, 0, 4, SensorDefs, Conf)),
    ?assertEqual({sensor_a, 4}, mod_mapping:map_sensor(sensor_a, undefined, 4, SensorDefs, Conf)),
    % remap from something else
    Conf1 = #{input_a => #{name => sensor_a, type => 3}},
    ?assertEqual({sensor_a, 5}, mod_mapping:map_sensor(input_a, 5, 4, SensorDefs, Conf1)),
    ?assertEqual({sensor_a, 4}, mod_mapping:map_sensor(input_a, 0, 4, SensorDefs, Conf1)),
    ?assertEqual({sensor_a, 4}, mod_mapping:map_sensor(input_a, undefined, 4, SensorDefs, Conf1)),
    ok.

dateutil(_) ->
    ?assertClose(fromnow(0), fromnow(0)),
    ?assertClose(fromnow(-9), fromnow(-8)),
    ?assertClose(fromnow(-8), fromnow(-9)),
    ?assertClose(fromnow(1), fromnow(2)),
    ?assertClose(fromnow(2), fromnow(1)),
    ?assertNotClose(fromnow(1), fromnow(-1)),
    ?assertNotClose(fromnow(1), fromnow(3)),
    ?assertNotClose(fromnow(3), fromnow(1)),
    ok.

utilities(_) ->
    % insort
    Lst = [{10, z}, {20, a}],
    Lst1 = nts_utils:insort({5, 234}, Lst),
    ?assertEqual([{5, 234}, {10, z}, {20, a}], Lst1),
    Lst2 = nts_utils:insort({15, 23}, Lst1),
    ?assertEqual([{5, 234}, {10, z}, {15, 23}, {20, a}], Lst2),
    Lst3 = nts_utils:insort({25, 32}, Lst2),
    ?assertEqual([{5, 234}, {10, z}, {15, 23}, {20, a}, {25, 32}], Lst3),
    Lst4 = nts_utils:insort({1, a}, []),
    ?assertEqual([{1, a}], Lst4),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_and_check(ModId, Defs, Exp) ->
    {Last, _} = apply_defs(ModId, Defs),
    #{dtm := Dtm, lon := Lon} = Last,
    {_, {_, _, Sec}} = Dtm,
    Offset = 10000 * (Lon - 1),
    ?assertEqual(Exp, {Sec, round(Offset)}).

apply_defs(ModId, Defs) ->
    Locs = loc_gen(Defs),
    FirstLoc = hd(Locs),
    Fun = procfun(ModId),
    Initial = initial_state(ModId),
    {FirstRes, State} = Fun(FirstLoc, {0, Initial}),
    lists:foldl(Fun, {FirstRes, State}, tl(Locs)).


procfun(stabiliser) ->
    fun(Loc, {_, State}) ->
        mod_stabiliser:filter_loc(Loc, maps:get(sat, Loc), State)
    end;
procfun(floatfilter) ->
    fun(Loc, {_, State}) ->
        mod_floatfilter:check_moving(Loc, State)
    end.

initial_state(stabiliser) -> undefined;
initial_state(floatfilter) -> mod_floatfilter:newstate().

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
    #{dtm => {{2017, 9, 12}, {5, 14, Time}},
      lat =>1, lon => 1 + Offset / 10000,
      sat => Sat}.
