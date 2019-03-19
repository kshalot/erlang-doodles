%%%-------------------------------------------------------------------
%%% @author karol
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2019 12:32
%%%-------------------------------------------------------------------
-module(onp).
-author("karol").

%% API
-export([onp/1]).

onp(X) when is_list(X) ->
  Y = string:tokens(X, " "),
  [Out] = lists:foldl(fun onp/2, [], Y),
  Out.

onp("+", [A, B | R]) -> [B + A | R];

onp("-", [A, B | R]) -> [B - A | R];

onp("*", [A, B |R]) -> [B * A | R];

onp("/", [A, B | R]) -> [B / A | R];

onp("^", [A, B | R]) -> [math:pow(B, A) | R];

onp("sqrt", [A | R]) -> [math:sqrt(A) | R];

onp("sin", [A | R]) -> [math:sin(A) | R];

onp("cos", [A | R]) -> [math:cos(A) | R];

onp("tan", [A | R]) -> [math:tan(A) | R];

onp("ctg", [A | R]) -> [1 / math:tan(A) | R];

onp(O, Stack) -> [list_to_number(O) | Stack].

list_to_number(X) when is_list(X) ->
  case string:to_float(X) of
    {error, _} -> list_to_integer(X);
    {F, _} -> F
  end.