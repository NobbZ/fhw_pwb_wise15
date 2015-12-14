%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 22:16
%%%-------------------------------------------------------------------
-module(crftr_global_data).
-author("nmelzer").

-behaviour(gen_server).

%% Gen Server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% API
-export([start/2, get_recipes/0, get_values/0]).

-record(state, {values, recipes}).

%% API
start(Values, Recipes) ->
  gen_server:start_link({local, state_info}, ?MODULE, {Values, Recipes}, []).

get_values() ->
  gen_server:call(state_info, get_values).

get_recipes() ->
  gen_server:call(state_info, get_recipes).

%% Gen Server Callbacks
init({Values, Recipes}) ->
  {ok, #state{values = Values, recipes = Recipes}}.

handle_call(get_values, _, #state{values = V} = S) ->
  {reply, V, S};
handle_call(_Request, _From, State) ->
  {stop, "Unknown call", State}.

handle_cast(_Request, State) ->
  {stop, "Unknown call", State}.

handle_info(_Info, State) ->
  {stop, "Unknown info", State}.

terminate(_Reason, _State) ->
  none.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
