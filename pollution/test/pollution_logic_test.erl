%%%-------------------------------------------------------------------
%% @doc pollution logic tests
%% @end
%%%-------------------------------------------------------------------

-module(pollution_logic_test).
-author("karol").

-include("../include/pollution.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NAME1, "station1").
-define(NAME2, "station2").
-define(COORD1, {0.0, 0.0}).
-define(COORD2, {1.0, 1.0}).
-define(DATETIME1, {{2019, 04, 13}, {21, 37, 00}}).
-define(DATETIME2, {{2019, 04, 13}, {00, 00, 00}}).

create_monitor_test() ->
    ?assertEqual(#monitor{}, pollution_logic:create_monitor()).

add_station_test() ->
    M = pollution_logic:create_monitor(),
    ?assertMatch(#monitor{}, pollution_logic:add_station(?NAME1, ?COORD1, M)).

add_station_duplicate_test() ->
    M = pollution_logic:create_monitor(),
    M1 = pollution_logic:add_station(?NAME1, ?COORD1, M),
    ?assertMatch({error, _}, pollution_logic:add_station(?NAME1, ?COORD1, M1)). 

add_measurement_test() ->
    M = pollution_logic:create_monitor(),
    M1 = pollution_logic:add_station(?NAME1, ?COORD1, M),
    
    %% Adding measurement by name and coords
    ?assertEqual(
      pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME1, pm10, 1.0, M1),
      pollution_logic:add_measurement({name, ?NAME1}, ?DATETIME1, pm10, 1.0, M1)),
    
    %% Getting measurement
    M2 = pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME1, pm10, 1.0, M1),
    ?assertEqual({?DATETIME1, pm10, 1.0}, pollution_logic:get_measurement({name, ?NAME1}, ?DATETIME1, pm10, M2)),
    
    %% Duplicate measurement
    ?assertEqual({error, measurement_exists},
      pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME1, pm10, 1.0, M2)),
    
    %% Stating doesn't exists
    ?assertEqual({error, no_station},
      pollution_logic:add_measurement({coords, ?COORD2}, ?DATETIME1, pm10, 1.0, M1)).

remove_measurement_test() ->
    M = pollution_logic:create_monitor(),
    M1 = pollution_logic:add_station(?NAME1, ?COORD1, M),
    M2 = pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME1, pm10, 1.0, M1),
    
    ?assertEqual(M1, pollution_logic:remove_measurement({coords, ?COORD1}, ?DATETIME1, pm10, M2)).
    
get_station_mean_test() ->
    {V1, V2} = {1.0, 5.0},
    ExpectedMean = (V1 + V2)/2,
    M = pollution_logic:create_monitor(),
    M1 = pollution_logic:add_station(?NAME1, ?COORD1, M),
    M2 = pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME1, pm10, V1, M1),
    M3 = pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME2, pm10, V2, M2),
    ?assertEqual(ExpectedMean, pollution_logic:get_station_mean({coords, ?COORD1}, pm10, M3)).

get_daily_mean_test() ->
    {V1, V2} = {1.0, 5.0},
    ExpectedMean = (V1 + V2)/2,
    M = pollution_logic:create_monitor(),
    M1 = pollution_logic:add_station(?NAME1, ?COORD1, M),
    M2 = pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME1, pm10, V1, M1),
    M3 = pollution_logic:add_measurement({coords, ?COORD1}, ?DATETIME2, pm10, V2, M2),
    {Day, _} = ?DATETIME1,
    ?assertEqual(ExpectedMean, pollution_logic:get_daily_mean(Day, pm10, M3)).
