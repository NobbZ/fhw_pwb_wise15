%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 17:02
%%%-------------------------------------------------------------------
-module(crafter_app).
-author("Norbert Melzer").

-behaviour(application).

%% API
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
  %observer:start(),
  Values = read_values(),
  IntermediateStorage = read_storage(),
  Recipes = read_recipes(),
  InitialStorage = cr_storage:add_fluid_to_storage(IntermediateStorage, read_fluid()),
  %supervisor:start_link({local, supervisor}, cr_supervisor, {Values, Recipes, InitialStorage}),
  crftr_global_data:start(Values, Recipes),
  cr_left_dfs:start(InitialStorage, Recipes, Values),
  %loop_forever(),
  {ok, self()}.

stop(_) ->
  ok.

%% number_of_cores() ->
%%   Fallback = 4,
%%   case erlang:system_info(logical_processors_available) of
%%     unknown -> Fallback;
%%     C when C =< 0 -> Fallback;
%%     C -> C
%%   end.

read_values() ->
  Line = io:get_line(""),
  cr_values:parse(Line).

read_storage() ->
  Line = io:get_line(""),
  cr_storage:parse(Line).

read_recipes() ->
  Line = io:get_line(""),
  cr_recipes:parse(Line).

read_fluid() ->
  Line = io:get_line(""),
  {Fluid, _} = string:to_integer(Line),
  Fluid.
