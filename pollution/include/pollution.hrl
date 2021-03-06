%%%-------------------------------------------------------------------
%%% @author karol
%%% @copyright (C) 2019, <AGH University of Science and Technology>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2019 20:24
%%%-------------------------------------------------------------------
-author("karol").

-type coords() :: {float(), float()}.
-type measurement_type() :: pm10 | pm25 | pm1 | temp | humid | press.
-type measurement() :: {calendar:datetime(), measurement_type(), float()}.
-type key() :: {name, string()} | {coords, coords()}.

-record(station, {
  name :: string(),
  coords :: coords(),
  data = [] :: [measurement()]
}).
-type station() :: #station{}.

%% This seems like a redundancy, but it makes
%% life easier when it comes to later indexing
%% stored stations. Still looking for a more
%% elegant solution.
-record(monitor, {
  by_name = #{} :: #{string() => station()},
  by_coords = #{} :: #{coords() => station()}
}).
-type monitor() :: #monitor{}.
