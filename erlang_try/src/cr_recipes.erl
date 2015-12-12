%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 00:10
%%%-------------------------------------------------------------------
-module(cr_recipes).
-author("Norbert Melzer").

%% API
-export([parse/1, is_recipe/1, recipe_equals/2]).

-export_type([recipes/0, recipe/0, recipe_id/0]).

%% @type recipe(). A recipe holds the instruction to produce some output from
%%   some input.
-record(recipe, {consumes = [] :: list(non_neg_integer())
  ,              produces = [] :: list(non_neg_integer())
  ,              fluid_cost = 0 :: cr_storage:fluid()}).
-opaque recipe() :: #recipe{}.

%% @type recipes(). Does contain all known recipes.
-record(recipes, {rcps = array:new() :: array()}). % Dialyzer 2.6 doesn't know about array/1
-opaque recipes() :: #recipes{}.

-type recipe_id() :: non_neg_integer().

%% @doc Parses some given string into a bunch of {@link recipe().
%%   <code>recipe()</code>s}.
-spec parse(string()) -> recipes().
parse(Line) ->
  Expr = correctify(string:concat(string:strip(Line, both), ".")),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  ListOfRecipes = lists:map(fun convert_tuple_to_recipe/1, ListOfValues),
  RelaxedArray = array:from_list(ListOfRecipes),
  #recipes{rcps = array:fix(RelaxedArray)}.

-spec is_recipe(term()) -> boolean().
is_recipe(#recipe{}) -> true;
is_recipe(_)         -> false.

-spec is_recipes(term()) -> boolean().
is_recipes(#recipes{}) -> true;
is_recipes(_)          -> false.

-spec recipe_equals(recipe(), recipe()) -> boolean().
recipe_equals(L, R) ->
  lists:sort(L#recipe.consumes) == lists:sort(R#recipe.consumes)
  andalso lists:sort(L#recipe.produces) == lists:sort(R#recipe.produces)
  andalso L#recipe.fluid_cost == R#recipe.fluid_cost.

%%% PRIVATE STUFF!

%% @doc Converts a tuple to a proper {@link recipe()}.
-spec convert_tuple_to_recipe(ListOfTuples :: {[cr_storage:item_id()], [cr_storage:item_id()], cr_storage:fluid()}) -> recipe().% {list(non_neg_integer()), list(non_neg_integer()), non_neg_integer()}) -> list(recipe()).
convert_tuple_to_recipe({ListIn, ListOut, Fluid}) ->
  #recipe{consumes = ListIn, produces = ListOut, fluid_cost = Fluid}.

%% @doc Converts a string that uses haskell-like tuple syntax into one using
%%   erlang-tuple-syntax.
-spec correctify(InputString :: string()) -> string().
correctify(L) -> correctify(L, []).

correctify([], L) when is_list(L)     -> lists:reverse(L);
correctify([$(|T], L) when is_list(L) -> correctify(T, [${|L]);
correctify([$)|T], L) when is_list(L) -> correctify(T, [$}|L]);
correctify([H|T], L) when is_list(L)  -> correctify(T, [H|L]).
