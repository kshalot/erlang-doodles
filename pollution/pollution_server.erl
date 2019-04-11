-module(pollution_server).
-author("karol").

-include("pollution.hrl").

-type key() :: {name, string()} | {coords, coords()}.
% Internal state of the server is defined as an instance of monitor()
-type state() :: monitor().

%% API
-export([start/0, add_station/2, add_measurement/4, remove_measurement/3,
        get_measurement/3, get_station_mean/2, get_daily_mean/2]).


%% Public functions

-spec start() -> ok.
start() ->
    register(?MODULE, spawn(init/1)),
    ok.

-spec add_station(string(), coords()) ->
    ok | {error, station_exists}.
add_station(N, C) ->
    pass.

-spec add_measurement(key(), calendar:datetime(), measurement_type(), float()) ->
    ok | {error, no_station | measurement_exists}.
add_measurement(K, Tm, Tp, V) ->
    pass.

-spec remove_measurement(key(), calendar:datetime(), measurement_type()) ->
    ok | {error, no_measurement | no_station}.
remove_measurement(K, Tm, Tp) ->
    pass.

-spec get_measurement(key(), calendar:datetime(), measurement_type()) ->
    float() | {error, not_found | no_station}.
get_measurement(K, Tm, Tp) ->
    pass.

-spec get_station_mean(key(), measurement_type()) ->
    float() | {error, no_station} | undefined.
get_station_mean(K, Tp) ->
    pass.

-spec get_daily_mean(calendar:date(), measurement_type()) ->
    float() | undefined.
get_daily_mean(Tm, Tp) ->
    pass.


%% Server functions

init() ->
    loop(#monitor{}).

-spec loop(state()) -> state().
loop(State) ->
    receive
        {stop, Sender} ->
            Sender ! {stopped, State},
            State;
        {Request, Sender} ->
            pass;
        _ ->
            loop(State)
    end.
