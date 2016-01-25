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
-export([start/2, get_recipes/0, get_values/0, print_solution/2]).

-record(state, {values, recipes, max}).

%% API
start(Values, Recipes) ->
  gen_server:start_link({local, state_info}, ?MODULE, {Values, Recipes}, []).

get_values() ->
  gen_server:call(state_info, get_values).

get_recipes() ->
  gen_server:call(state_info, get_recipes).

print_solution(Score, Path) ->
  gen_server:cast(state_info, {print, Score, Path}).

%% Gen Server Callbacks
init({Values, Recipes}) ->
  {ok, #state{values = Values, recipes = Recipes, max = -1000000000}}.

handle_call(get_values, _, #state{values = V} = S) ->
  {reply, V, S};
handle_call(_Request, _From, State) ->
  {stop, "Unknown call", State}.

handle_cast({print, Score, Path}, #state{max = MaxScore} = S) ->
  NewMax = case Score > MaxScore of
    true -> print_path(Path), io:format(standard_error, "~p~n", [Score]), Score;
    false -> MaxScore
  end,
  {noreply, S#state{max = NewMax}};
handle_cast(_Request, State) ->
  {stop, "Unknown call", State}.

handle_info(_Info, State) ->
  {stop, "Unknown info", State}.

terminate(_Reason, _State) ->
  io:puts("terminated global data"),
  none.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

print_path(Path) ->
  really_print_path(lists:reverse(Path)).

really_print_path(Path) ->
  PathString = format_path(Path),
  io:format("[~s]~n", [PathString]).

format_path([]) -> "";
format_path([I]) -> io_lib:format("~p", [I]);
format_path([I|Is]) -> lists:concat([io_lib:format("~p,", [I]), format_path(Is)]).
