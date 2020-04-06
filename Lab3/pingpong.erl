%%%-------------------------------------------------------------------
%%% @author Grzegorz Poręba
%%% @copyright (C) 2020, <agh.cs.wokstym>
%%% @doc
%%%
%%% @end
%%% Created : 29. mar 2020 12:29
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Grzegorz Poręba").

%% API
-export([start/0, stop/0, play/1, ping/1, pong/0]).


start() ->
  register(pong, spawn(pingpong, pong, [])),
  register(ping, spawn(pingpong, ping, [0])),
  io:fwrite("Started processes~n").



stop() ->
  pong ! stop,
  ping ! stop.

play(N) ->
  ping ! N.

ping(Sum) ->
  receive
    0 ->
      io:fwrite("Sum of ping: ~w~n", [Sum]),
      ping(Sum);
    N -> timer:sleep(1000),
      io:fwrite("Ping: ~w, sum of ping: ~w~n", [N, Sum+N]),
      pong ! (N - 1),
      ping(Sum+N);
    stop ->
      terminate()
  after
    20000 ->
      io:fwrite("Ping has timeouted ~n"),
      terminate()
  end.


pong() ->
  receive
    0 ->
      pong();
    N -> timer:sleep(1000),
      io:fwrite("Pong: ~w~n", [N]),
      ping ! (N - 1),
      pong();
    stop ->
      terminate()
  after
    20000 ->
      io:fwrite("Pong has timeouted ~n"),
      terminate()
  end.

terminate() ->
  io:fwrite("Terminated process~n"),
  ok.
