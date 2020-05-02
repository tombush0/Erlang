%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. kwi 2020 20:24
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("grzeg").
-include_lib("eunit/include/eunit.hrl").

-record(station, {name, location, measurements}).
-record(monitor, {stations}).

%% API
-export([]).

%% Helper functions to get sample monitor and dates
gen_monitor_with_vals() ->
  D0 = {{2020, 3, 31}, {15, 47, 00}},
  D1 = {{2020, 3, 30}, {15, 47, 00}},
  D2 = {{2020, 3, 31}, {14, 47, 00}},
  #monitor{stations = [
    #station{name = "S1", location = {1, 1}, measurements = #{
      {"PM10", D1}=> 25,
      {"PM10", D2}=> 125,
      {"PM10", D0}=> 15,
      {"PM25", D1}=> 255,
      {"PM25", D0}=> 15
    }}]}.

d0() -> {{2020, 3, 31}, {15, 47, 00}}.
d1() -> {{2020, 3, 30}, {15, 47, 00}}.
d2() -> {{2020, 3, 31}, {14, 47, 00}}.

% tests ( eunit:test(pollution). )
add_station_test_() ->

  Exp1 = #monitor{stations = [
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},
  Exp2 = #monitor{stations = [
    #station{name = "S2", location = {2, 2}, measurements = #{}},
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},
  Exp3 = #monitor{stations = [
    #station{name = "S3", location = {3, 3}, measurements = #{}},
    #station{name = "S2", location = {2, 2}, measurements = #{}},
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},

  Res0 = pollution:createMonitor(),
  Res1 = pollution:addStation("S1", {1, 1}, Res0),
  Res2 = pollution:addStation("S2", {2, 2}, Res1),
  Res3 = pollution:addStation("S3", {3, 3}, Res2),

  [
    ?_assertEqual(Exp1, Res1),
    ?_assertEqual(Exp2, Res2),
    ?_assertEqual(Exp3, Res3)
  ].

add_value_test_() ->

  Res1 = pollution:createMonitor(),
  Res2 = pollution:addStation("S1", {1, 1}, Res1),
  Res3 = pollution:addValue("S1", "PM10", d0(), 15, Res2),
  Res4 = pollution:addValue("S1", "PM10", d1(), 25, Res3),
  Res5 = pollution:addValue("S1", "PM10", d2(), 125, Res4),
  Res6 = pollution:addValue("S1", "PM25", d0(), 15, Res5),
  Res7 = pollution:addValue("S1", "PM25", d1(), 255, Res6),

  Exp3 = gen_monitor_with_vals(),
  ?_assertEqual(Exp3, Res7).

remove_val_test_() ->
  Res1 = gen_monitor_with_vals(),
  Res2 = pollution:addValue("S1", "MM", d0(), 15, Res1),
  Res3 = pollution:removeValue("S1", "MM", d0(), Res2),
  ?_assertEqual(Res1, Res3).

get_one_val_test_() ->
  ?_assertEqual(15, pollution:getOneValue("S1", "PM10", d0(), gen_monitor_with_vals())).

get_station_mean_test_() ->
  [
    ?_assertEqual(55.0, pollution:getStationMean("S1", "PM10", gen_monitor_with_vals())),
    ?_assertEqual(135.0, pollution:getStationMean("S1", "PM25", gen_monitor_with_vals()))
  ].

get_daily_mean_test_() ->
  [
    ?_assertEqual(51 + 2 / 3, pollution:getDailyMean("S1", d0(), gen_monitor_with_vals())),
    ?_assertEqual(140.0, pollution:getDailyMean("S1", d1(), gen_monitor_with_vals()))
  ].

get_daily_over_limit_test_() ->
  [
    ?_assertEqual(1, pollution:getDailyOverLimit("S1", d0(), "PM10", 20, gen_monitor_with_vals())),
    ?_assertEqual(1, pollution:getDailyOverLimit("S1", d1(), "PM10", 20, gen_monitor_with_vals())),
    ?_assertEqual(0, pollution:getDailyOverLimit("S1", d0(), "PM25", 20, gen_monitor_with_vals())),
    ?_assertEqual(1, pollution:getDailyOverLimit("S1", d1(), "PM25", 20, gen_monitor_with_vals()))
  ].

get_mean_of_every_type_test_() ->
  [{T1, V1}, {T2, V2}] = pollution:getMeanOfEveryType("S1", gen_monitor_with_vals()),
  [
    ?_assertEqual("PM25", T1),
    ?_assertEqual(135.0, V1),
    ?_assertEqual("PM10", T2),
    ?_assertEqual(55.0, V2)
  ].
