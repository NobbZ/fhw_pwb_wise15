%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Dez 2015 20:09
%%%-------------------------------------------------------------------
-module(cr_recipes_tests).
-author("nmelzer").

%% API
-export([]).

-include("recipes.hrl").

-include_lib("eunit/include/eunit.hrl").

parse_empty_test() ->
  Exp = #recipes{rcps = []},
  Act = cr_recipes:parse("[]"),
  ?assertMatch(Exp, Act).

parse_single_recipe_test() ->
  Exp =
    #recipes{rcps = [#recipe{produces = [1], consumes = [2], fluid_cost = 0}]},
  Act = cr_recipes:parse("[([2],[1],0)]"),
  ?assertMatch(Exp, Act).

parse_complex_test() ->
  Exp = #recipes{rcps = [
    #recipe{consumes = [1, 2], produces = [0], fluid_cost = 5},
    #recipe{consumes = [], produces = [1, 2], fluid_cost = 100},
    #recipe{consumes = [0], produces = [1, 2], fluid_cost = 1}]},
  Act = cr_recipes:parse("[([1,2],[0],5),([],[1,2],100),([0],[1,2],1)]"),
  ?assertMatch(Exp, Act).

is_recipe_test() ->
  ?assert(cr_recipes:is_recipe(#recipe{consumes = [], produces = [], fluid_cost = 0})),
  lists:map(fun(R) -> ?assert(cr_recipes:is_recipe(R)) end, [
    #recipe{consumes = [1, 2], produces = [0], fluid_cost = 5},
    #recipe{consumes = [], produces = [1, 2], fluid_cost = 100},
    #recipe{consumes = [0], produces = [1, 2], fluid_cost = 1}]).

is_not_recipe_test() ->
  lists:map(fun(NR) -> ?assertNot(cr_recipes:is_recipe(NR)) end, [
    #recipe{consumes = a},
    #recipe{produces = a},
    #recipe{fluid_cost = a},
    "FooBar",
    atom,
    42]).
