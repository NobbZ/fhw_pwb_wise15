%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
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

-spec(init(_) -> {ok, {{one_for_one, 1, 5}, []}}).
init(_) -> {ok, {{one_for_one, 1, 5}, []}}.
