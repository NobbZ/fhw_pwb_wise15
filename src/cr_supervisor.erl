%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2015 08:23
%%%-------------------------------------------------------------------
-module(cr_supervisor).
-author("nmelzer").

-behaviour(supervisor).

%% Supervisor
-export([init/1]).

%% API
-export([]).

-define(CHILDSPEC(ChildName, Starter), {}).

init({Values, Recipes, Storage}) ->
  Flags = {one_for_one, 1, 3600},
  ChildSpecs = [
    {glbl_data, {crftr_global_data, start, [Values, Recipes], permanent, brutal_kill, worker, [crftr_global_data]}},
    {left_dfs, {cr_left_dfs, start, [Storage, Recipes, Values]}, permanent, brutal_kill, worker, [cr_left_dfs]}
  ],
  {ok, {Flags, ChildSpecs}}.
