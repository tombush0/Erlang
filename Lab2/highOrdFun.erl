%%%-------------------------------------------------------------------
%%% @author Grzegorz Poręba
%%% @copyright (C) 2020, <agh.cs.wokstym>
%%% @doc
%%%
%%% @end
%%% Created : 26. mar 2020 14:12
%%%-------------------------------------------------------------------
-module(highOrdFun).
-author("Grzegorz Poręba").

%% API
-export([map/2, filter/2, digitsSum/1, mapNrToList/1, randomElems/3]).

%% highOrdFun:filter(fun(X) -> X rem 3==0 end, highOrdFun:randomElems(1000000, 0, 1000000)).

randomElems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1, N)].

map(_, []) -> [];
map(Fun1, [H | T]) -> [Fun1(H) | map(Fun1, T)].

filter(Fun1, L) -> [H || H <- L, Fun1(H) == true].

mapNrToList(0) -> [];
mapNrToList(Nr) -> [Nr rem 10 | mapNrToList(Nr div 10)].

digitsSum(Nr) -> lists:foldl(fun(Sum, Dig) -> Sum + Dig end, 0, mapNrToList(Nr)).


