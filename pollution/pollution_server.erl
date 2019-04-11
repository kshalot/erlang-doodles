-module(pollution_server).
-author("karol").

-include("pollution.hrl").

-type key() :: {name, string()} | {coords, coords()}.
% Internal state of the server is defined as an instance of monitor()
-type state() :: monitor().

%% API
-export([start/0]).


%% Public functions

-spec start() -> ok.
start() ->
    register(?MODULE, spawn(init/1)),
    ok.

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
            {Response, NewState} = respond(Request, State),
            Sender ! Response,
            loop(NewState);
        _ ->
            loop(State)
    end.

respond({add_station, N, C}, State) ->
    case pollution:add_station(N, C, S) of
        #monitor{} = M ->
            {ok, M};
        Failed ->
            {Failed, State}
    end;

respond({add_measurement, K, Tm, Tp, V}, State) ->
    case pollution:add_measurement(K, Tm, Tp, V, State) of
        #monitor{} = M ->
            {ok, M};
        Failed ->
            {Failed, State}
    end;

respond({remove_measurement, K, Tm, Tp}, State) ->
    case pollution:remove_measurement(K, Tm, Tp, State) of
        #monitor{} = M ->
            {ok, M};
        Failed ->
            {Failed, State}
    end;

respond({get_measurement, K, Tm, Tp}, State) ->
    case pollution:get_measurement(K, Tm, Tp, State) of
        measurement() = 
