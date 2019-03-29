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

%% Helper functions

-type key() :: {name, string()} | {coords, coords()}.

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

