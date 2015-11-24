%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 22:16
%%%-------------------------------------------------------------------
-module(crftr_global_data).
-author("nmelzer").

-behaviour(gen_server).

%% Gen Server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([]).

-export_type([state/0]).

-type state() :: {cr_values:values(), cr_recipes:recipes()}.
-type calls() :: get_recipes | get_values.
-type reply(A) :: {reply, A, state()}.
-type reply() :: reply(cr_values:values()) | reply(cr_recipes:recipes()).
-type casts() :: term(). % Specify later
-type noreply() :: {noreply, _}. % -----""----
-type stop() :: {stop, string(), state()}.
-type reason() :: normal | shutdown | {shutdown, term()} | term().
-type vsn() :: term() | {down, term()}.

%% Gen Server Callbacks
-spec init(state()) -> {ok, state()}.
init({Values, Recipes}) ->
  {ok, {Values, Recipes}}.

-spec handle_call(calls(), {pid(), _}, state()) -> reply() | stop().
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
