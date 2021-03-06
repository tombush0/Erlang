%%%-------------------------------------------------------------------
%%% @author Grzegorz Poręba
%%% @copyright (C) 2020, <agh.cs.wokstym>
%%% @doc
%%%
%%% @end
%%% Created : 29. mar 2020 12:29
%%%-------------------------------------------------------------------
-module(pollution).
-author("Grzegorz Poręba").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getStation/2, getDailyOverLimit/5, getMeanOfEveryType/2]).

-record(station, {name, location, measurements}).
-record(monitor, {stations}).

createMonitor() -> #monitor{stations = []}.

getStation(Coords, Monitor) when is_tuple(Coords) ->
  Found = lists:keyfind(Coords, #station.location, Monitor#monitor.stations),
  case Found of
    false -> {error, no_station_found_by_coords};
    _ -> Found
  end;
getStation(Name, Monitor) ->
  Found = lists:keyfind(Name, #station.name, Monitor#monitor.stations),
  case Found of
    false -> {error, no_station_found_by_name};
    _ -> Found
  end.


addStation(Name, Coords, Monitor) ->
  Stations = Monitor#monitor.stations,
  case {lists:keyfind(Coords, #station.location, Stations), lists:keyfind(Name, #station.name, Stations)} of
    {false, false} -> #monitor{stations = [#station{name = Name, location = Coords, measurements = #{}} | Stations]};
    _ -> {error, station_already_added}
  end.

addValue(NameOrCords, Type, Date, Value, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ ->
      case maps:is_key({Type, Date}, Station#station.measurements) of
        true -> {error, measurement_already_added};
        _ ->
          NewStation = Station#station{measurements = maps:put({Type, Date}, Value, Station#station.measurements)},
          #monitor{stations = [NewStation | lists:filter(fun(StationInMonitor) -> Station /= StationInMonitor end, Monitor#monitor.stations)]}
      end
  end.

removeValue(NameOrCords, Type, Date, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ ->
      case maps:is_key({Type, Date}, Station#station.measurements) of
        false -> {error, no_such_measurement};
        _ ->
          NewStation = Station#station{measurements = maps:remove({Type, Date}, Station#station.measurements)},
          #monitor{stations = [NewStation | lists:filter(fun(StationInMonitor) -> Station /= StationInMonitor end, Monitor#monitor.stations)]}
      end
  end.

getOneValue(NameOrCords, Type, Date, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ -> maps:get({Type, Date}, Station#station.measurements, {error, no_such_measurement}) %% defaultowo zwracamy error
  end.


getStationMean(NameOrCords, Type, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ ->
      SumAll = fun({MapType, _}, Val, {Sum, Nr}) ->
        case MapType of
          Type -> {Sum + Val, Nr + 1};
          _ -> {Sum, Nr}
        end end,
      {Sum, Nr} = maps:fold(SumAll, {0, 0}, Station#station.measurements),
      Sum / Nr
  end.

getDailyMean(NameOrCords, {Date, _Time}, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ ->
      SumAllFun = fun({_, {MapDate, _}}, Val, {Sum, Nr}) ->
        case MapDate of
          Date -> {Sum + Val, Nr + 1};
          _ -> {Sum, Nr}
        end end,
      {Sum, Nr} = maps:fold(SumAllFun, {0, 0}, Station#station.measurements),
      Sum / Nr
  end.

getDailyOverLimit(NameOrCords, {Date, _}, Type, Limit, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ ->
      SumAllFun = fun({MapType, {MapDate, _}}, Val, Sum) ->
        case {MapDate, MapType, Val > Limit} of
          {Date, Type, true} -> Sum + 1;
          _ -> Sum
        end end,
      maps:fold(SumAllFun, 0, Station#station.measurements)
  end.

%% Helper method for getMeanOfEveryType
isThisTypeAlreadyInRes(_, []) -> false;
isThisTypeAlreadyInRes(Type, [{HeadType, _} | _]) when Type == HeadType -> true;
isThisTypeAlreadyInRes(Type, [_ | T]) -> isThisTypeAlreadyInRes(Type, T).

getMeanOfEveryType(NameOrCords, Monitor) ->
  Station = getStation(NameOrCords, Monitor),
  case Station of
    {error, ErrorName} -> {error, ErrorName};
    _ ->
      GetTypeAndMeanPairsFun = fun({MapType, _}, _, Res) ->
        case isThisTypeAlreadyInRes(MapType, Res) of
          false -> [{MapType, getStationMean(NameOrCords, MapType, Monitor)} | Res];
          _ -> Res
        end end,
      maps:fold(GetTypeAndMeanPairsFun, [], Station#station.measurements)
  end.




