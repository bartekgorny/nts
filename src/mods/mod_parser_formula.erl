%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Sep 2017 12:45
%%%-------------------------------------------------------------------
-module(mod_parser_formula).
-author("bartek").
-include_lib("nts/src/nts.hrl").

%% API
-export([parse_frame/1]).

parse_frame(Frame) ->
    Frame1 = strip($a, Frame),
    <<Id:8/binary-unit:5, $,, Dt:8/binary-unit:14, $,, Fid:8/binary-unit:2, $,, Reszta/binary>> = Frame1,
    RList = binary:split(Reszta, <<",">>, [global]),
    DMap = #{dtm => parse_dt(Dt), frame_type => Fid},
    Values = process_fields(field_defs(Fid), RList, DMap),
    #frame{type = maps:get(report_type, Values), device = Id, values = Values}.

process_fields([], _, Acc) -> Acc;
process_fields([F|Tail], [Val|VTail], Acc) ->
    {Key, Func} = F,
    NVal = erlang:Func(Val),
    Acc1 = maps:put(Key, NVal, Acc),
    case Key of
        report_id ->
            Acc2 = maps:put(report_type, report_type(NVal), Acc1),
            process_fields(field_defs(maps:get(frame_type, Acc2), NVal), VTail, Acc2);
        _ ->
            process_fields(Tail, VTail, Acc1)
    end.

strip(Ch, <<Ch, Bin/binary>>) -> Bin;
strip(_Ch, Bin) -> Bin.

-spec parse_dt(binary()) -> datetime().
parse_dt(B) ->
    <<BYr:8/binary-unit:4, BMth:8/binary-unit:2, BDay:8/binary-unit:2, BH:8/binary-unit:2,
        BMin:8/binary-unit:2, BSec:8/binary-unit:2>> = B,
    [Yr, Mth, Day, H, Min, Sec] = lists:map(fun binary_to_integer/1,
        [BYr, BMth, BDay, BH, BMin, BSec]),
    {{Yr, Mth, Day}, {H, Min, Sec}}.


-spec field_defs(binary()) -> [{atom(), atom()} | {atom(), atom(), atom()}].
field_defs(<<"F1">>) ->
    [{longitude, binary_to_float},
     {latitude, binary_to_float},
     {speed, binary_to_integer},
     {direction, binary_to_integer},
     {altitude, binary_to_integer},
     {sat, binary_to_integer},
     {report_id, binary_to_integer}].

-spec field_defs(binary(), integer()) -> [{atom(), atom()} | {atom(), atom(), atom()}].
field_defs(<<"F1">>, 2) ->
     [{mileage, binary_to_integer},
      {'IN1', binary_to_integer},
      {voltage_1, binary_to_float},
      {voltage_2, binary_to_float},
      {'OUT1', binary_to_integer}].

report_type(2) -> location.
