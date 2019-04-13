%%%-------------------------------------------------------------------
%% @doc pollution gen server
%% @end
%%%-------------------------------------------------------------------

-module(pollution_server).
-author("karol").

-behavior(gen_server).

-include("../include/pollution.hrl").
%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([start_link/0, stop/0, add_station/2, add_measurement/4, remove_measurement/3,
         get_measurement/3, get_station_mean/2, get_daily_mean/2]).

%% Internal state of the server is defined as an instance of monitor()
-type state() :: monitor().

-define(SERVER, ?MODULE).

%%====================================================================
%% Client Call
%%====================================================================

-spec start_link() -> ok.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> state().
stop()->
    gen_server:stop(?SERVER).

-spec add_station(string(), coords()) ->
    ok | {error, station_exists | timeout}.
add_station(N, C) ->
    gen_server:call(?MODULE, {add_station, N, C}).

-spec add_measurement(key(), calendar:datetime(), measurement_type(), float()) ->
    ok | {error, no_station | measurement_exists | timeout}.
add_measurement(K, Tm, Tp, V) ->
    gen_server:call(?MODULE, {add_measurement, K, Tm, Tp, V}).

-spec remove_measurement(key(), calendar:datetime(), measurement_type()) ->
    ok | {error, no_measurement | no_station | timeout}.
remove_measurement(K, Tm, Tp) ->
    gen_server:call(?MODULE, {remove_measurement, K, Tm, Tp}).

-spec get_measurement(key(), calendar:datetime(), measurement_type()) ->
    ok | {error, not_found | no_station | timeout}.
get_measurement(K, Tm, Tp) ->
    gen_server:call(?MODULE, {get_measurement, K, Tm, Tp}).

-spec get_station_mean(key(), measurement_type()) ->
    ok | {error, no_station | timeout} | undefined.
get_station_mean(K, Tp) ->
    gen_server:call(?MODULE, {get_station_mean, K, Tp}).

-spec get_daily_mean(calendar:date(), measurement_type()) ->
    ok | {error, timeout} | undefined.
get_daily_mean(Dt, Tp) ->
    gen_server:call(?MODULE, {get_daily_mean, Dt, Tp}).

%%====================================================================
%% Server Callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting... ~n", [{local, ?SERVER}, self()]),
    State = #monitor{},
    {ok, State}.

terminate(Reason, State) ->
    io:format("Server terminated [reason:~p, state~p~n]", [Reason, State]).

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({add_station, N, C}, _From, State) ->
    case pollution:add_station(N, C, State) of
        #monitor{} = M ->
            {reply, ok, M};
        Failed ->
            {reply, Failed, State}
    end;

handle_call({add_measurement, K, Tm, Tp, V}, _From, State) ->
    case pollution:add_measurement(K, Tm, Tp, V, State) of
        #monitor{} = M ->
            {reply, ok, M};
        Failed ->
            {reply, Failed, State}
    end;

handle_call({remove_measurement, K, Tm, Tp}, _From, State) ->
    case pollution:remove_measurement(K, Tm, Tp, State) of
        #monitor{} = M ->
            {reply, ok, M};
        Failed ->
            {reply, Failed, State}
    end;

handle_call({get_measurement, K, Tm, Tp}, _From, State) ->
    case pollution:get_measurement(K, Tm, Tp, State) of
        Value when is_float(Value) ->
            {reply, Value, State};
        Failed ->
            {reply, Failed, State}
    end;

handle_call({get_station_mean, K, Tp}, _From, State) ->
    case pollution:get_station_mean(K, Tp, State) of
        Value when is_float(Value) ->
            {reply, Value, State};
        Failed ->
            {reply, Failed, State}
    end;

handle_call({get_daily_mean, Tm, Tp}, _From, State) ->
    case pollution:get_daily_mean(Tm, Tp, State) of
        Value when is_float(Value) ->
            {reply, Value, State};
        Failed ->
            {reply, Failed, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.
