%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. kwi 2020 16:27
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("grzeg").

%% API
-export([ addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyOverLimit/4, getMeanOfEveryType/1,getMonitor/0, stop/0, start/0, start/1, swapMonitor/1]).
-export([init/0, init/1]).


start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).
start(Monitor) ->
  register(pollutionServer, spawn(pollution_server, init, [Monitor])).

init() ->
  loop(pollution:createMonitor()).
init(Monitor) ->
  loop(Monitor).

addStation(Name, Coords) ->
  call(addStation, [Name, Coords]).
addValue(NameOrCords, Type, Date, Value) ->
  call(addValue, [NameOrCords, Type, Date, Value]).
removeValue(NameOrCords, Type, Date) ->
  call(removeValue, [NameOrCords, Type, Date]).
getOneValue(NameOrCords, Type, Date) ->
  call( getOneValue, [NameOrCords, Type, Date]).
getStationMean(NameOrCords, Type) ->
  call(getStationMean, [NameOrCords, Type]).
getDailyMean(NameOrCords, DateT) ->
  call(getDailyMean, [NameOrCords, DateT]).
getDailyOverLimit(NameOrCords, DateT, Type, Limit) ->
  call(getDailyOverLimit, [NameOrCords, DateT, Type, Limit]).
getMeanOfEveryType(NameOrCords) ->
  call(getMeanOfEveryType, [NameOrCords]).
getMonitor() ->
  call(getMonitor, []).
stop() ->
  call(stop, []).
swapMonitor(NewMonitor) ->
  call(swapMonitor, [NewMonitor]).

call(Message, Args) ->
  pollutionServer ! {request, self(), Message, Args},
  receive
    {reply, Reply} -> Reply;
    {error, ErrorName} -> {error, ErrorName}
  end.



loop(Monitor) ->
  receive
    {request, Pid, addStation, [Name, Coords]} ->
      case pollution:addStation(Name, Coords, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        ResMonitor ->
          Pid ! {reply, ok},
          loop(ResMonitor)
      end;
    {request, Pid, addValue, [NameOrCords, Type, Date, Value]} ->
      case pollution:addValue(NameOrCords, Type, Date, Value, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        ResMonitor ->
          Pid ! {reply, ok},
          loop(ResMonitor)
      end;
    {request, Pid, removeValue, [NameOrCords, Type, Date]} ->
      case pollution:removeValue(NameOrCords, Type, Date, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        ResMonitor ->
          Pid ! {reply, ok},
          loop(ResMonitor)
      end;
    {request, Pid, getOneValue, [NameOrCords, Type, Date]} ->
      case pollution:getOneValue(NameOrCords, Type, Date, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        Value ->
          Pid ! {reply, Value},
          loop(Monitor)
      end;
    {request, Pid, getStationMean, [NameOrCords, Type]} ->
      case pollution:getStationMean(NameOrCords, Type, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        Value ->
          Pid ! {reply, Value},
          loop(Monitor)
      end;
    {request, Pid, getDailyMean, [NameOrCords, DateT]} ->
      case pollution:getDailyMean(NameOrCords, DateT, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        Value ->
          Pid ! {reply, Value},
          loop(Monitor)
      end;
    {request, Pid, getDailyOverLimit, [NameOrCords, DateT, Type, Limit]} ->
      case pollution:getDailyOverLimit(NameOrCords, DateT, Type, Limit, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        Value ->
          Pid ! {reply, Value},
          loop(Monitor)
      end;
    {request, Pid, getMeanOfEveryType, [NameOrCords]} ->
      case pollution:getMeanOfEveryType(NameOrCords, Monitor) of
        {error, ErrorName} ->
          Pid ! {error, ErrorName},
          loop(Monitor);
        Value ->
          Pid ! {reply, Value},
          loop(Monitor)
      end;
    {request, Pid, getMonitor, _} ->
      Pid ! {reply, Monitor},
      loop(Monitor);
    {request, Pid, swapMonitor, NewMonitor} ->
      Pid ! {reply, ok},
      loop(NewMonitor);
    {request, Pid, stop, _} ->
      Pid ! {reply, ok}
  end.


