%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. kwi 2020 17:32
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
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


% tests ( eunit:test(pollution_server) )
all_test_() ->
  Exp1 = #monitor{stations = [
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},
  Exp2 = #monitor{stations = [
    #station{name = "S2", location = {2, 2}, measurements = #{}},
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},
  [{T1, V1}, {T2, V2}] = test_get_mean_of_every_type(),
  [
    ?_assertEqual(Exp1, test_adding1()),
    ?_assertEqual(Exp2, test_adding2()),
    ?_assertEqual(gen_monitor_with_vals(), test_add_value()),
    ?_assertEqual(gen_monitor_with_vals(), test_remove_val()),
    ?_assertEqual(15, test_get_one_val()),
    ?_assertEqual(55.0, test_get_station_mean_PM10()),
    ?_assertEqual(135.0, test_get_station_mean_PM25()),
    ?_assertEqual(51 + 2 / 3, test_get_daily_mean_d0()),
    ?_assertEqual(140.0, test_get_daily_mean_d1()),
    ?_assertEqual(1, test_get_daily_over_limit_d1_PM10()),
    ?_assertEqual(1, test_get_daily_over_limit_d1_PM25()),
    ?_assertEqual(1, test_get_daily_over_limit_d0_PM10()),
    ?_assertEqual(0, test_get_daily_over_limit_d0_PM25()),
    ?_assertEqual("PM25", T1),
    ?_assertEqual(135.0, V1),
    ?_assertEqual("PM10", T2),
    ?_assertEqual(55.0, V2)
  ].


test_adding1() ->
  pollution_server:start(),
  pollution_server:addStation("S1", {1, 1}),
  Res = pollution_server:getMonitor(),
  pollution_server:stop(),
  Res.

test_adding2() ->
  pollution_server:start(),
  pollution_server:addStation("S1", {1, 1}),
  pollution_server:addStation("S2", {2, 2}),
  Res = pollution_server:getMonitor(),
  pollution_server:stop(),
  Res.

test_add_value() ->
  pollution_server:start(),
  pollution_server:addStation("S1", {1, 1}),
  pollution_server:addValue("S1", "PM10", d0(), 15),
  pollution_server:addValue("S1", "PM10", d1(), 25),
  pollution_server:addValue("S1", "PM10", d2(), 125),
  pollution_server:addValue("S1", "PM25", d0(), 15),
  pollution_server:addValue("S1", "PM25", d1(), 255),
  Res = pollution_server:getMonitor(),
  pollution_server:stop(),
  Res.

test_remove_val() ->
  pollution_server:start(gen_monitor_with_vals()),
  pollution_server:addValue("S1", "MM", d0(), 15),
  pollution_server:removeValue("S1", "MM", d0()),
  Res = pollution_server:getMonitor(),
  pollution_server:stop(),
  Res.

test_get_one_val() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getOneValue("S1", "PM10", d0()),
  pollution_server:stop(),
  Res.

test_get_station_mean_PM10() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getStationMean("S1", "PM10"),
  pollution_server:stop(),
  Res.

test_get_station_mean_PM25() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getStationMean("S1", "PM25"),
  pollution_server:stop(),
  Res.

test_get_daily_mean_d0() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getDailyMean("S1", d0()),
  pollution_server:stop(),
  Res.

test_get_daily_mean_d1() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getDailyMean("S1", d1()),
  pollution_server:stop(),
  Res.

test_get_daily_over_limit_d1_PM10() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getDailyOverLimit("S1", d1(), "PM10", 20),
  pollution_server:stop(),
  Res.

test_get_daily_over_limit_d1_PM25() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getDailyOverLimit("S1", d1(), "PM25", 20),
  pollution_server:stop(),
  Res.

test_get_daily_over_limit_d0_PM10() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getDailyOverLimit("S1", d0(), "PM10", 20),
  pollution_server:stop(),
  Res.

test_get_daily_over_limit_d0_PM25() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getDailyOverLimit("S1", d0(), "PM25", 20),
  pollution_server:stop(),
  Res.

test_get_mean_of_every_type() ->
  pollution_server:start(gen_monitor_with_vals()),
  Res = pollution_server:getMeanOfEveryType("S1"),
  pollution_server:stop(),
  Res.

