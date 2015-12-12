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

init(_) -> {ok, {{one_for_one, 1, 5}, []}}.
