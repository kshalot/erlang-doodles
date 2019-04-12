%%%-------------------------------------------------------------------
%%% @author karol
%%% @copyright (C) 2019, <AGH University of Science and Technology>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2019 12:31
%%%-------------------------------------------------------------------
-module(pollution).
-author("karol").
-include("../include/pollution.hrl").

-type key() :: {name, string()} | {coords, coords()}.

%%====================================================================
%% API
%%====================================================================

-export([create_monitor/0, add_station/3, add_measurement/5, remove_measurement/4,
         get_measurement/4, get_station_mean/3, get_daily_mean/3]).

create_monitor() ->
  #monitor{}.

-spec add_station(string(), coords(), monitor()) -> monitor() | {error, station_exists}.
add_station(N, C, M) ->
  case station_exists(N, C, M) of
    true -> {error, station_exists};
    false ->
      S = #station{name = N, coords = C},
      #monitor{by_name = BN, by_coords = BC} = M,
      M#monitor{
        by_name = BN#{N => S},
        by_coords = BC#{C => S}}
  end.

-spec add_measurement(key(), calendar:datetime(), measurement_type(), float(), monitor()) ->
  monitor() | {error, no_station} | {error, measurement_exists}.
add_measurement(K, Tm, Tp, V, M) ->
  case find_station(K, M) of
    #station{data = D} = S ->
      case find_measurement(Tm, Tp, S) of
        {error, not_found} ->
          ND = [{Tm, Tp, V} | D],
          update_station(S#station{data = ND}, M);
        _ -> {error, measurement_exists}
      end;
    {error, not_found} -> {error, no_station}
  end.

-spec remove_measurement(key(), calendar:datetime(), measurement_type(), monitor()) ->
  monitor() | {error, no_measurement} | {error, no_station}.
remove_measurement(K, Tm, Tp, M) ->
  case find_station(K, M) of
    #station{data = D} = S ->
      case find_measurement(Tm, Tp, S) of
        {error, not_found} -> {error, no_measurement};
        Msr ->
          ND = lists:filter(fun(X) ->
            not measurement_equals(Msr, X) end, D),
          update_station(S#station{data = ND}, M)
      end;
    _ -> {error, no_station}
  end.

-spec get_measurement(key(), calendar:datetime(), measurement_type(), monitor()) ->
  measurement() | {error, not_found} | {error, no_station}.
get_measurement(K, Tm, Tp, M) ->
  case find_station(K, M) of
    #station{} = S ->
      find_measurement(Tm, Tp, S);
    _ -> {error, no_station}
  end.

-spec get_station_mean(key(), measurement_type(), monitor()) ->
  float() | {error, no_station} | undefined.
get_station_mean(K, Tp, M) ->
  case find_station(K, M) of
    #station{data = D} ->
      Vals = lists:filtermap(fun({_, T, V}) ->
        case T =:= Tp of
          false -> false;
          _ -> {true, V}
        end end, D),
      mean(Vals);
    {error, not_found} -> {error, no_station}
  end.

-spec get_daily_mean(calendar:date(), measurement_type(), monitor()) ->
  float() | undefined.
get_daily_mean(Tm, Tp, #monitor{by_name = BN}) ->
  Vals = lists:flatmap(
    fun({_, #station{data = D}}) ->
      lists:filtermap(fun({{X, _}, T, V}) ->
        case X =:= Tm andalso T =:= Tp of
          true -> {true,V};
          _ -> false
        end
      end, D)
  end, maps:to_list(BN)),
  mean(Vals).

%%====================================================================
%% Internal functions
%%====================================================================

-spec station_exists(string(), coords(), monitor()) -> boolean().
station_exists(N, C, M) ->
  case {find_station({name, N}, M), find_station({coords, C}, M)} of
    {{error, not_found}, {error, not_found}} -> false;
    _ -> true
  end.

-spec find_station(key(), monitor()) -> station() | {error, not_found}.
find_station({name, N}, #monitor{by_name = Sts}) ->
  case maps:get(N, Sts, not_found) of
    not_found -> {error, not_found};
    #station{} = S -> S
  end;

find_station({coords, C}, #monitor{by_coords = Sts}) ->
  case maps:get(C, Sts, not_found) of
    not_found -> {error, not_found};
    #station{} = S -> S
  end.

-spec find_measurement(calendar:datetime(), measurement_type(), station()) ->
  measurement() | {error, not_found}.
find_measurement(Tm, Tp, S) ->
  D = S#station.data,
  T = {Tm, Tp, 0.0},
  case lists:filter(fun(X) -> measurement_equals(T, X) end, D) of
    [Msr] -> Msr;
    [] -> {error, not_found}
  end.

-spec measurement_equals(measurement(), measurement()) -> boolean().
measurement_equals({Dt, T, _}, {Dt, T, _}) -> true;
measurement_equals(_, _) -> false.

-spec update_station(station(), monitor()) -> monitor().
update_station(S, #monitor{by_name = BN, by_coords = BC} = M) ->
  M#monitor{
    by_name = BN#{S#station.name := S},
    by_coords = BC#{S#station.coords := S}
  }.

-spec mean([float()]) -> float() | undefined.
mean([]) -> undefined;
mean(Vals) -> lists:sum(Vals) / length(Vals).
