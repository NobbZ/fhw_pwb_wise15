%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2015 09:43
%%%-------------------------------------------------------------------
-module(cr_tree).
-author("Norbert Melzer").

-behaviour(gen_server).

-type key_type() :: non_neg_integer().

-record(node, {storage :: cr_storage:storage()
  ,            value = 0 :: integer()
  ,            children = array:new(unevaluated) :: array()
  ,            parent_id :: key_type()}).
-opaque(node() :: #node{}).
-record(path_info, {path = [] :: list(cr_recipes:recipe_id())
  ,                 value = 0 :: cr_values:value()}).
-opaque(path_info() :: #path_info{}).
-type(node_entry() :: node() | path_info() | unevaluated).

-type(entry() :: {key_type(), node() | path_info()}).
-type(table_row() :: {key_type(), node_entry()}).

-type(get_node() :: key_type()).

%% gen_server
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
         terminate/2]).

%% API
-export([start_link/2, get_child/2, get_value/1, get_node/1]).

-export_type([node/0, path_info/0, entry/0]).

-spec(start_link(cr_storage:storage(), non_neg_integer()) -> {ok, pid()}).
start_link(Storage, RecipeCount) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {Storage, RecipeCount}, []).

-spec get_node(NodeId :: key_type()) -> node() | path_info().
get_node(NodeId) when is_integer(NodeId) andalso NodeId >= 0 ->
  gen_server:call(?MODULE, {get_node, NodeId}).

-spec get_child(NodeId :: key_type(), ChildIdx :: cr_recipes:recipe_id()) -> entry().
get_child(NodeId, ChildIdx) ->
  void.

-spec get_value(Node :: node() | path_info() | key_type()) -> cr_values:value().
get_value(#node{value = V}) -> V;
get_value(#path_info{value = V}) -> V;
get_value(NodeId) when is_integer(NodeId) andalso NodeId >= 0 ->
  get_value(get_node(NodeId)).

%% gen_server callbacks
-spec(code_change(_, _, _) -> {ok, _}).
code_change(_, State, _) -> {ok, State}.

-spec(handle_call(get_node(), _, ets:tid()) -> {reply, Reply, ets:tid()}
  when Reply :: node() | path_info()).
handle_call({get_node, NodeId}, _, Table) ->
  [{_Id, NodeContent}] = ets:lookup(Table, NodeId),
  {reply, NodeContent, Table}.

-spec(handle_cast(_, S) -> {noreply, S}).
handle_cast(_, S) -> {noreply, S}.

-spec(handle_info(_, S) -> {noreply, S}).
handle_info(_, S) -> {noreply, S}.

%% @TODO Initialise table and stuff…
-spec(init(_) -> ignore).
init(_) -> ignore.

-spec(terminate(_, _) -> ignore).
terminate(_, _) -> ignore.

