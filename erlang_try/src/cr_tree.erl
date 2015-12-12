%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2015 09:43
%%%-------------------------------------------------------------------
-module(cr_tree).
-author("Norbert Melzer").

-behaviour(gen_server).

-record(node, {storage, value = 0, children = [], parent_id}).
-record(path_info, {path = [], value = 0}).

%% gen_server
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
  terminate/2]).

%% API
-export([start_link/2, get_child/2, get_value/1, get_node/1]).

start_link(Storage, RecipeCount) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Storage, RecipeCount}, []).

get_node(NodeId) when is_integer(NodeId) andalso NodeId >= 0 ->
  gen_server:call(?MODULE, {get_node, NodeId}).

get_child(_NodeId, _ChildIdx) ->
  void.

get_value(#node{value = V})      -> V;
get_value(#path_info{value = V}) -> V;
get_value(NodeId) when is_integer(NodeId) andalso NodeId >= 0 ->
  case get_node(NodeId) of
    unknown -> unknown;
    Node -> get_value(Node)
  end.

%% gen_server callbacks
code_change(_, State, _) -> {ok, State}.

handle_call({get_node, NodeId}, _, Table) ->
  Reply = case ets:lookup(Table, NodeId) of
    [{_Id, NodeContent}] -> NodeContent;
    _ -> unknown
  end,
  {reply, Reply, Table}.

handle_cast(_, S) -> {noreply, S}.

handle_info(_, S) -> {noreply, S}.

%% @TODO Initialise table and stuff…
init(_) -> ignore.

terminate(_, _) -> ignore.

