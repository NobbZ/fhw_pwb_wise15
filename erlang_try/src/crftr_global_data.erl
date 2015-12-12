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

-export_type([state/0]).

-record(state, {values :: cr_values:values()
  ,             recipes :: cr_recipes:recipes()}).
-opaque state() :: #state{}.
-type calls() :: get_recipes | get_values.
-type reply(A) :: {reply, A, state()}.
-type reply() :: reply(cr_values:values()) | reply(cr_recipes:recipes()).
-type casts() :: term(). % Specify later
-type noreply() :: {noreply, _}. % -----""----
-type stop() :: {stop, string(), state()}.
-type reason() :: normal | shutdown | {shutdown, term()} | term().
-type vsn() :: term() | {down, term()}.

%% API
-spec start(cr_values:values(), cr_recipes:recipes()) -> {ok, pid()}.
start(Values, Recipes) ->
  gen_server:start_link({local, state_info}, ?MODULE, {Values, Recipes}, []).

-spec(get_values() -> cr_values:values()).
get_values() ->
  gen_server:call(state_info, get_values).

-spec(get_recipes() -> cr_recipes:recipes()).
get_recipes() ->
  gen_server:call(state_info, get_recipes).

%% Gen Server Callbacks
-spec init({cr_values:values(), cr_recipes:recipes()}) -> {ok, state()}.
init({Values, Recipes}) ->
  {ok, #state{values = Values, recipes = Recipes}}.

-spec handle_call(calls(), {pid(), _}, state()) -> reply() | stop().
handle_call(get_values, _, #state{values = V} = S) ->
  {reply, V, S};
handle_call(_Request, _From, State) ->
  {stop, "Unknown call", State}.

-spec handle_cast(casts(), state()) -> noreply() | stop().
handle_cast(_Request, State) ->
  {stop, "Unknown call", State}.

-spec handle_info(term, state()) -> stop().
handle_info(_Info, State) ->
  {stop, "Unknown info", State}.

-spec terminate(reason(), state()) -> none.
terminate(_Reason, _State) ->
  none.

-spec code_change(vsn(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
