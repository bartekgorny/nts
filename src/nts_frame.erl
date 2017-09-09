%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2017 23:31
%%%-------------------------------------------------------------------
-module(nts_frame).
-author("bartekgorny").

-include_lib("nts/src/nts.hrl").

%% API
-export([parse/2, strip/2]).


-spec parse(atom(), binary()) -> map().
parse(formula, Frame) ->
    Frame1 = strip($a, Frame),
    <<Id:8/binary-unit:5, $,, Dt:8/binary-unit:14, $,, Fid:8/binary-unit:2, Reszta/binary>> = Frame1,
    RList = binary:split(Reszta, <<",">>, [global]),
    DMap = #{device => Id, dtm => parse_dt(Dt), frame_type => Fid},
    process_fields(field_defs(Fid), tl(RList), DMap).

process_fields([], _, Acc) -> Acc;
process_fields([F|Tail], [Val|VTail], Acc) ->
    {Key, NVal} = case F of
               {K, Func} ->
                   {K, erlang:Func(Val)};
               {K, Mod, Func} ->
                   {K, Mod:Func(Val)}
           end,
    Acc1 = maps:put(Key, NVal, Acc),
    process_fields(Tail, VTail, Acc1).

strip(Ch, <<Ch, Bin/binary>>) -> Bin;
strip(_Ch, Bin) -> Bin.

-spec parse_dt(binary()) -> datetime().
parse_dt(B) ->
    <<BYr:8/binary-unit:4, BMth:8/binary-unit:2, BDay:8/binary-unit:2, BH:8/binary-unit:2,
        BMin:8/binary-unit:2, BSec:8/binary-unit:2>> = B,
    [Yr, Mth, Day, H, Min, Sec] = lists:map(fun binary_to_integer/1,
                                            [BYr, BMth, BDay, BH, BMin, BSec]),
    {{Yr, Mth, Day}, {H, Min, Sec}}.


field_defs(<<"F1">>) ->
    [{longitude, binary_to_float},
     {lattitude, binary_to_float},
     {speed, binary_to_integer},
     {direction, binary_to_integer},
     {altitude, binary_to_integer},
     {sat, binary_to_integer},
     {report_id, binary_to_integer},
     {mileage, binary_to_integer},
     {'IN1', binary_to_integer},
     {voltage_1, binary_to_float},
     {voltage_2, binary_to_float},
     {'OUT1', binary_to_integer}].

