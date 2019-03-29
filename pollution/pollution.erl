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
-include("pollution.hrl").

-type key() :: {name, string()} | {coords, coords()}.

%% API
-export([create_monitor/0, add_station/3, add_measurement/5, remove_measurement/4,
         get_measurement/4, get_station_mean/3, get_daily_mean/3]).

create_monitor() ->
  #monitor{}.

-spec add_station(string(), coords(), monitor()) -> monitor() | {error, station_exists}.
add_station(Name, Coords, Monitor) ->
  case station_exists(Name, Coords, Monitor) of
    true -> {error, station_exists};
    false ->
      Station = #station{name = Name, coords = Coords},
      #monitor{by_name = BN, by_coords = BC} = Monitor,
      Monitor#monitor{
        by_name = BN#{Name => Station},
        by_coords = BC#{Coords => Station}}
  end.

-spec add_measurement(key(), calendar:datetime(), measurement_type(), float(), monitor()) ->
  monitor() | {error, no_station} | {error, measurement_exists}.
add_measurement(Key, Time, Type, Value, Monitor) ->
  case find_station(Key, Monitor) of
    #station{data = Data} = S ->
      case find_measurement(Time, Type, S) of
        {error, not_found} ->
          NewData = [{Time, Type, Value} | Data],
          update_station(S#station{data = NewData}, Monitor);
        _ -> {error, measurement_exists}
      end;
    {error, not_found} -> {error, no_station}
  end.

-spec remove_measurement(key(), calendar:datetime(), measurement_type(), monitor()) ->
  monitor() | {error, no_measurement} | {error, no_station}.
remove_measurement(Key, Time, Type, Monitor) ->
  case find_station(Key, Monitor) of
    #station{data = Data} = S ->
      case find_measurement(Time, Type, S) of
        {error, not_found} -> {error, no_measurement};
        Measurement ->
          NewData = lists:filter(fun(D) ->
            not measurement_equals(Measurement, D) end, Data),
          update_station(S#station{data = NewData}, Monitor)
      end;
    _ -> {error, no_station}
  end.

-spec get_measurement(key(), calendar:datetime(), measurement_type(), monitor()) ->
  measurement() | {error, not_found} | {error, no_station}.
get_measurement(Key, Time, Type, Monitor) ->
  case find_station(Key, Monitor) of
    #station{} = S ->
      find_measurement(Time, Type, S);
    _ -> {error, no_station}
  end.

-spec get_station_mean(key(), measurement_type(), monitor()) ->
  float() | {error, no_station} | undefined.
get_station_mean(Key, Type, Monitor) ->
  case find_station(Key, Monitor) of
    #station{data = Data} ->
      Vals = lists:filtermap(fun({_, T, V}) ->
        case T =:= Type of
          false -> false;
          _ -> {true, V}
        end end, Data),
      mean(Vals);
    {error, not_found} -> {error, no_station}
  end.

-spec get_daily_mean(calendar:date(), measurement_type(), monitor()) ->
  float() | undefined.
get_daily_mean(Date, Type, Monitor) ->
  %%todo
  undefined.

%% Helper functions

-spec station_exists(string(), coords(), monitor()) -> boolean().
station_exists(Name, Coords, Monitor) ->
  case {find_station({name, Name}, Monitor), find_station({coords, Coords}, Monitor)} of
    {{error, not_found}, {error, not_found}} -> false;
    _ -> true
  end.

-spec find_station(key(), monitor()) -> station() | {error, not_found}.
find_station({name, Name}, #monitor{by_name = Stations}) ->
  case maps:get(Name, Stations, not_found) of
    not_found -> {error, not_found};
    #station{} = S -> S
  end;

find_station({coords, Coords}, #monitor{by_name = Stations}) ->
  case maps:get(Coords, Stations, not_found) of
    not_found -> {error, not_found};
    #station{} = S -> S
  end.

-spec find_measurement(calendar:datetime(), measurement_type(), station()) ->
  measurement() | {error, not_found}.
find_measurement(Time, Type, Station) ->
  Data = Station#station.data,
  Target = {Time, Type, 0.0},
  case lists:filter(fun(D) -> measurement_equals(Target, D) end, Data) of
    [Measurement] -> Measurement;
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