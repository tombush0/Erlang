%%%-------------------------------------------------------------------
%%% @author Grzegorz Poręba
%%% @copyright (C) 2020, <agh.cs.wokstym>
%%% @doc
%%%
%%% @end
%%% Created : 26. mar 2020 14:12
%%%-------------------------------------------------------------------
-module(qsort).
-author("Grzegorz Poręba").

%% API
-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Fun1T, _v} = timer:tc(Fun1, [List]),
  {Fun2T, _v} = timer:tc(Fun2, [List]),
  io:format("Czas pierwszej funkcji: ~b\nCzas drugiej funkcji: ~b~n ", [Fun1T, Fun2T]).

