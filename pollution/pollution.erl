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

%% API
-export([create_monitor/0, add_station/3]).

create_monitor() ->
  #monitor{}.

add_station(Name, Coords, Monitor) ->
  case station_exists(Name, Coords, Monitor) of
    true -> {error, stationexists};
    false ->
      Station = #station{name = Name, coords = Coords},
      #monitor{by_name = BN, by_coords = BC} = Monitor,
      Monitor#monitor{
        by_name = BN#{Name => Station},
        by_coords = BC#{Coords => Station}}
  end.

%%add_value(NameCoords, Timestamp, Kind, Value, Monitor)




%% Helper functions

station_exists(Name, Coords, Monitor) ->
  false.

%% This shit ain't working. New data tye for first argument? Food for thought.
-spec find_station(string(), monitor()) -> {ok, station()} | {error, not_found}.
find_station(Name, #monitor{by_name = Stations}) ->
  case maps:get(Name, Stations, not_found) of
    not_found -> {error, not_found};
    #station{} = S -> {ok, S}
  end;

find_station(Name, #monitor{by_coords = Stations}) ->
  case maps:get(Name, Stations, not_found) of
    not_found -> {error, not_found};
    #station{} = S -> {ok, S}
  end.