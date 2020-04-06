%%%-------------------------------------------------------------------
%%% @author Grzegorz Poręba
%%% @copyright (C) 2020, <agh.cs.wokstym>
%%% @doc
%%%
%%% @end
%%% Created : 16. mar 2020 17:59
%%%-------------------------------------------------------------------
-module(onp).
-author("grzeg").

%% API
-export([onp/1]).

onp([]) -> 0;
onp(Expression) ->
  Tokens = string:tokens(Expression, " "),
  try
    io:format("~w~n", parse([], Tokens))
  catch
    error:badarith -> io:format("Blad, bledne dzialanie (dzielenie przez 0 lub sqrt z liczby ujemnej lub tan ~n")
  end.

as_number(S) ->
  case string:to_float(S) of
    {error, no_float} -> list_to_integer(S);
    {N, _} -> N
  end.

%% assuming always after operations is left one number
parse(StackH, []) ->
  StackH;
%%assuming data is correct first element should be number
parse([], [ArgsH | ArgsT]) ->
  parse([as_number(ArgsH)], ArgsT);
parse([First, Second | StackT], ["*" | ArgsT]) ->
  parse([Second * First | StackT], ArgsT);
parse([First, Second | StackT], ["/" | ArgsT]) ->
  parse([Second / First | StackT], ArgsT);
parse([First, Second | StackT], ["+" | ArgsT]) ->
  parse([Second + First | StackT], ArgsT);
parse([First, Second | StackT], ["-" | ArgsT]) ->
  parse([Second - First | StackT], ArgsT);
parse([StackH | StackT], ["sqrt" | ArgsT]) ->
  parse([math:sqrt(StackH) | StackT], ArgsT);
parse([First, Second | StackT], ["pow" | ArgsT]) ->
  parse([math:pow(Second, First) | StackT], ArgsT);
parse([StackH | StackT], ["sin" | ArgsT]) ->
  parse([math:sin(StackH) | StackT], ArgsT);
parse([StackH | StackT], ["cos" | ArgsT]) ->
  parse([math:cos(StackH) | StackT], ArgsT);
parse([StackH | StackT], ["tan" | ArgsT]) ->
  parse([math:tan(StackH) | StackT], ArgsT);
parse([StackH | StackT], ["opp" | ArgsT]) ->
  parse([-StackH | StackT], ArgsT);
parse([First, Second | StackT], ["avg" | ArgsT]) ->
  parse([(Second + First) / 2 | StackT], ArgsT);
parse(Stack, [ArgsH | ArgsT]) ->
  parse([as_number(ArgsH) | Stack], ArgsT).
