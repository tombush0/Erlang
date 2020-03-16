%%%-------------------------------------------------------------------
%%% @author wokstym
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. mar 2020 14:20
%%%-------------------------------------------------------------------
-module(main).
-author("wokstym").

%% API
-export([factorial/1, main/1, power/2, contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/2]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

main(N) -> io:format("~B~n",[factorial(N)]).

power(A, 1) -> A;
power(A, B) -> A* power(A, B-1).

contains([], _) -> false;
contains([H|_], H) -> true;
contains([_|T], X) -> contains(T, X).


duplicateElements([])-> [];
duplicateElements([H|T])-> [H, H| duplicateElements(T)].


sumFloats (L) -> sumFloatsTail(L, 0).

sumFloatsTail([], SUM) -> SUM;
sumFloatsTail([H|T], SUM) -> sumFloatsTail(T, SUM+H).

