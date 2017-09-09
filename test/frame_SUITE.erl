%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2017 22:41
%%%-------------------------------------------------------------------
-module(frame_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("nts/src/nts.hrl").

all() ->
    [formula].

init_per_suite(C) ->
    application:ensure_all_started(nts),
    C.

formula(_) ->
    Frame1 = <<"a00129,20120307132629,F1,21.290000,52.290000,0,12,191,8,2,1094,0,12.40,12.69,0,1094,,,,,,,,,0">>,
    Res1 = nts_frame:parse(formula, Frame1),
    #{'IN1' := 0,
      'OUT1' := 0,
      altitude := 191,
      device := <<"00129">>,
      direction := 12,
      dtm := {{2012,3,7},{13,26,29}},
      frame_type := <<"F1">>,
      lattitude := 52.29,
      longitude := 21.29,
      mileage := 1094,
      report_id := 2,
      sat := 8,
      speed := 0,
      voltage_1 := 12.4,
      voltage_2 := 12.69} = Res1,
    Frame2 = <<"10285,20120410084652,F1,18.837392,53.006626,0,334,91,11,2,33509,1,13.25,13.57,0,,,,,,,,,,0">>,
    Res2 = nts_frame:parse(formula, Frame2),
    ?assertEqual(<<"10285">>, maps:get(device, Res2)),
    ok.


%%%%%%%%%%%%%%%%


