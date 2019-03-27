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
      % Updating a map returns a new map, so pattern matching comes
      % to the rescue, but there surely is a more elegant solution
      % List probably?
      #monitor{monitored_stations = Stations} = Monitor,
      Monitor#monitor{monitored_stations = Stations#{Name => Station}},
      Monitor
  end.

station_exists(Name, Coords, Monitor) ->
  false.

