%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. maj 2020 22:02
%%%-------------------------------------------------------------------
-module(pollution_gen_server_tests).
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
    }},
    #station{name = "S2", location = {2, 2}, measurements = #{}}]}.

d0() -> {{2020, 3, 31}, {15, 47, 00}}.
d1() -> {{2020, 3, 30}, {15, 47, 00}}.
d2() -> {{2020, 3, 31}, {14, 47, 00}}.


all_test_() ->
  Exp1 = #monitor{stations = [
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},
  Exp2 = #monitor{stations = [
    #station{name = "S2", location = {2, 2}, measurements = #{}},
    #station{name = "S1", location = {1, 1}, measurements = #{}}
  ]},
  {setup,
    fun() -> pollution_gen_server:start_link() end,
    fun(_) ->
      [
        ?_assertEqual(ok, pollution_gen_server:addStation("S1", {1, 1})),
        ?_assertEqual(Exp1, pollution_gen_server:getMonitor()),
        ?_assertEqual(ok, pollution_gen_server:addStation("S2", {2, 2})),
        ?_assertEqual(Exp2, pollution_gen_server:getMonitor()),
        ?_assertEqual(gen_monitor_with_vals(), test_add_value()),
        ?_assertEqual(15, pollution_gen_server:getOneValue("S1", "PM10", d0())),
        ?_assertEqual(55.0, pollution_gen_server:getStationMean("S1", "PM10")),
        ?_assertEqual(135.0, pollution_gen_server:getStationMean("S1", "PM25")),
        ?_assertEqual(51 + 2 / 3, pollution_gen_server:getDailyMean("S1", d0())),
        ?_assertEqual(140.0, pollution_gen_server:getDailyMean("S1", d1())),
        ?_assertEqual(1, pollution_gen_server:getDailyOverLimit("S1", d1(), "PM10", 20)),
        ?_assertEqual(1, pollution_gen_server:getDailyOverLimit("S1", d1(), "PM25", 20)),
        ?_assertEqual(1, pollution_gen_server:getDailyOverLimit("S1", d0(), "PM10", 20)),
        ?_assertEqual(0, pollution_gen_server:getDailyOverLimit("S1", d0(), "PM25", 20)),
        ?_assertEqual([{"PM25", 135.0}, {"PM10", 55.0}], pollution_gen_server:getMeanOfEveryType("S1"))
      ]
    end
  }.

test_add_value() ->
  pollution_gen_server:addValue("S1", "PM10", d0(), 15),
  pollution_gen_server:addValue("S1", "PM10", d1(), 25),
  pollution_gen_server:addValue("S1", "PM10", d2(), 125),
  pollution_gen_server:addValue("S1", "PM25", d0(), 15),
  pollution_gen_server:addValue("S1", "PM25", d1(), 255),
  pollution_gen_server:getMonitor().
