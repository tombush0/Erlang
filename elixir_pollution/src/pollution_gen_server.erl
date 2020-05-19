%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. maj 2020 18:45
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("grzeg").

%% API
-export([start_link/0, stop/0, getOneValue/3, addStation/2, addValue/4, crash/0, getStationMean/2, getDailyMean/2, getDailyOverLimit/4, getMeanOfEveryType/1, getMonitor/0, removeValue/3, setMonitor/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).


%% START %%
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, pollution:createMonitor(), []).
init(Monitor) ->
  {ok, Monitor}.

%% INTERFACE %%
stop() -> gen_server:call(?MODULE, terminate).
getOneValue(NameOrCords, Type, Date)            -> gen_server:call(?MODULE, {getOneValue, [NameOrCords, Type, Date]}).
getStationMean(NameOrCords, Type)               -> gen_server:call(?MODULE, {getStationMean, [NameOrCords, Type]}).
getDailyMean(NameOrCords, DateT)                -> gen_server:call(?MODULE, {getDailyMean, [NameOrCords, DateT]}).
getDailyOverLimit(NameOrCords, DateT, Type, Limit)
                                    -> gen_server:call(?MODULE, {getDailyOverLimit, [NameOrCords, DateT, Type, Limit]}).
getMeanOfEveryType(NameOrCords)                 -> gen_server:call(?MODULE, {getMeanOfEveryType, [NameOrCords]}).
getMonitor()                                    -> gen_server:call(?MODULE, getMonitor).

addStation(Name, Coords)                        -> gen_server:cast(?MODULE, {addStation, [Name, Coords]}).
addValue(NameOrCords, Type, Date, Value)        -> gen_server:cast(?MODULE, {addValue, [NameOrCords, Type, Date, Value]}).
removeValue(NameOrCords, Type, Date)            -> gen_server:cast(?MODULE, {removeValue, [NameOrCords, Type, Date]}).
setMonitor(NewMonitor)                          -> gen_server:cast(?MODULE, {setMonitor, NewMonitor}).
crash()                                         -> gen_server:cast(?MODULE, crash).

%% HANDLERS                               {stop,Reason,Reply,NewState}
handle_call(terminate, _From, Monitor) -> {stop, normal, ok, Monitor};
handle_call({getOneValue, [NameOrCords, Type, Date]}, _From, Monitor) ->
  {reply, pollution:getOneValue(NameOrCords, Type, Date, Monitor), Monitor};
handle_call({getStationMean, [NameOrCords, Type]}, _From, Monitor) ->
  {reply, pollution:getStationMean(NameOrCords, Type, Monitor), Monitor};
handle_call({getDailyMean, [NameOrCords, DateT]}, _From, Monitor) ->
  {reply, pollution:getDailyMean(NameOrCords, DateT, Monitor), Monitor};
handle_call({getDailyOverLimit, [NameOrCords, DateT, Type, Limit]}, _From, Monitor) ->
  {reply, pollution:getDailyOverLimit(NameOrCords, DateT, Type, Limit, Monitor), Monitor};
handle_call({getMeanOfEveryType, [NameOrCords]}, _From, Monitor) ->
  {reply, pollution:getMeanOfEveryType(NameOrCords, Monitor), Monitor};
handle_call(getMonitor, _From, Monitor) -> {reply, Monitor, Monitor}.


handle_cast({addStation, [Name, Coords]}, Monitor) ->
  handle_cast_result(pollution:addStation(Name, Coords, Monitor), Monitor);
handle_cast({addValue, [NameOrCords, Type, Date, Value]}, Monitor) ->
  handle_cast_result(pollution:addValue(NameOrCords, Type, Date, Value, Monitor), Monitor);
handle_cast({removeValue, [NameOrCords, Type, Date]}, Monitor) ->
  handle_cast_result(pollution:removeValue(NameOrCords, Type, Date, Monitor), Monitor);
handle_cast({setMonitor, NewMonitor}, _Monitor) ->
  {noreply, NewMonitor};
handle_cast(crash, N) ->
  no:exist(),
  {noreply, N}.

handle_cast_result({error, ErrorName}, OldMonitor) ->
  erlang:display({error,ErrorName}),
  {noreply, OldMonitor};
handle_cast_result(ResMonitor, _OldMonitor) ->
  {noreply, ResMonitor}.



handle_info(_Info, State) ->
  {noreply, State}.


terminate(normal, Monitor) -> erlang:display(Monitor), ok.



