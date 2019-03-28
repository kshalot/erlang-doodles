%%%-------------------------------------------------------------------
%%% @author karol
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2019 20:24
%%%-------------------------------------------------------------------
-author("karol").

-type coords() :: {float(), float()}.

-type measurement() :: {calendar:datetime(), string(), float()}.

-record(station, {
  name :: string(),
  coords :: coords(),
  data = [] :: [measurement()]
}).
-type station() :: #station{}.

-record(monitor, {
  by_name = #{} :: #{string() => station()},
  by_coords = #{} :: #{coords() => station()}
}).
-type monitor() :: #monitor{}.