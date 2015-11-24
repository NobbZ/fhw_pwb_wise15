%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 00:10
%%%-------------------------------------------------------------------
-module(cr_recipes).
-author("nmelzer").

%% API
-export([parse/1]).

-export_type([recipes/0, recipe/0]).

-record(recipe, {consumes = [] :: list(non_neg_integer())
  ,              produces = [] :: list(non_neg_integer())
  ,              fluid_cost = 0 :: cr_storage:fluid()}).
-opaque recipe() :: #recipe{}.

-record(recipes, {rcps = array:new() :: array:array(recipe())}).
-opaque recipes() :: #recipes{}.

-spec parse(string()) -> recipes().
parse(Line) ->
  Expr = correctify(string:concat(string:strip(Line, both), ".")),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  RelaxedArray = array:from_list(ListOfValues),
  {recipes, array:fix(RelaxedArray)}.

-spec correctify(InputString :: string()) -> string().
correctify([$(|T]) -> [${|correctify(T)];
correctify([$)|T]) -> [$}|correctify(T)];
correctify([H|T])  -> [H|correctify(T)];
correctify([])     -> [].
