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

onp([])-> 0;
onp(Expression) ->
  Tokens = string:tokens(Expression, " "),
  try
    io:format("~w~n",parse([], Tokens))
  catch
    error:badarith -> io:format("Blad, bledne dzialanie (dzielenie przez 0 lub sqrt z liczby ujemnej lub tan ~n")
  end.


get_head([H| _])-> H.
get_second_el([H| T]) -> get_head(T).

%% assuming always after operations is left one number
parse(StackH , []) ->
  StackH;
%%assuming data is correct first element should be number
parse([], [ArgsH|ArgsT]) ->
  try
    parse([list_to_float(ArgsH)], ArgsT)
  catch
    error:badarg ->
      parse([list_to_integer(ArgsH)], ArgsT)
  end;
parse( Stack , [ArgsH|ArgsT]) when ArgsH ==  "*"->
  parse([get_second_el(Stack) *get_head(Stack)]++ lists:nthtail(2, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "/"->
  parse([get_second_el(Stack) /get_head(Stack)]++ lists:nthtail(2, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "+"->
  parse([get_second_el(Stack) +get_head(Stack)]++ lists:nthtail(2, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "-"->
  parse([get_second_el(Stack) -get_head(Stack)]++ lists:nthtail(2, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "sqrt"->
  parse([math:sqrt(get_head(Stack)) ]++ lists:nthtail(1, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "pow"->
  parse([math:pow(get_second_el(Stack) ,get_head(Stack))]++ lists:nthtail(2, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "sin"->
  parse([math:sin(get_head(Stack)) ]++ lists:nthtail(1, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "cos"->
  parse([math:cos(get_head(Stack)) ]++ lists:nthtail(1, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "tan"->
  parse([math:tan(get_head(Stack)) ]++ lists:nthtail(1, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "opp"->
  parse([-get_head(Stack) ]++ lists:nthtail(1, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) when ArgsH ==  "avg"->
  parse([(get_second_el(Stack) +get_head(Stack))/2]++ lists:nthtail(2, Stack), ArgsT);
parse(Stack, [ArgsH|ArgsT]) ->
  try
    parse([list_to_float(ArgsH)]++ Stack, ArgsT)
  catch
    error:badarg ->
      parse([list_to_integer(ArgsH)]++ Stack, ArgsT)
  end.