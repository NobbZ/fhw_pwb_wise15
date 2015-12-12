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
-export([parse/1, is_recipe/1, recipe_equals/2, is_recipes/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(recipe, {consumes = [] , produces = [], fluid_cost = 0}).
-record(recipes, {rcps = []}).

%% @doc Parses some given string into a bunch of `recipe{}'s}.
parse(Line) ->
  Expr = correctify(string:concat(string:strip(Line, both), ".")),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  ListOfRecipes = lists:map(fun convert_tuple_to_recipe/1, ListOfValues),
  #recipes{rcps = ListOfRecipes}.

%% @doc Checks wether its argument is a valid {@link recipe()} or not.
is_recipe(#recipe{consumes = C, produces = P, fluid_cost = F}) ->
  is_list(C) andalso is_list(P) andalso is_integer(F) andalso F >= 0;
is_recipe(_) -> false.

%% @doc Checks wether its argument is a valid {@link recipes()} or not.
is_recipes(#recipes{rcps = Rs}) -> array:is_array(Rs);
is_recipes(_)                   -> false.

%% @doc checks wether two recipes are equal or not.
%%   Will fail with `badarg' when one of its arguments is not a valid {@link recipe()}.
%%   Two recipes will considered equal when they are consuming the same things and
%%   produce the same things and they do consume the same amount of fluid.
recipe_equals(L, R) ->
  lists:sort(L#recipe.consumes) == lists:sort(R#recipe.consumes)
    andalso lists:sort(L#recipe.produces) == lists:sort(R#recipe.produces)
    andalso L#recipe.fluid_cost == R#recipe.fluid_cost.

%%% PRIVATE STUFF!

%% @doc Converts a tuple to a proper {@link recipe()}.
convert_tuple_to_recipe({ListIn, ListOut, Fluid}) ->
  #recipe{consumes = ListIn, produces = ListOut, fluid_cost = Fluid}.

%% @doc Converts a string that uses haskell-like tuple syntax into one using
%%   erlang-tuple-syntax.
correctify(L) -> correctify(L, []).

%% @doc Recursive helper for {@link correctify/1}.
correctify([], L) when is_list(L)     -> lists:reverse(L);
correctify([$(|T], L) when is_list(L) -> correctify(T, [${|L]);
correctify([$)|T], L) when is_list(L) -> correctify(T, [$}|L]);
correctify([H|T], L) when is_list(L)  -> correctify(T, [H|L]).

-ifdef(TEST).

parse_empty_test() ->
  Exp = #recipes{rcps = []},
  Act = parse("[]"),
  ?assertMatch(Exp, Act).

parse_single_recipe_test() ->
  Exp =
    #recipes{rcps = [#recipe{produces = [1], consumes = [2], fluid_cost = 0}]},
  Act = parse("[([2],[1],0)]"),
  ?assertMatch(Exp, Act).

parse_complex_test() ->
  Exp = #recipes{rcps = [
    #recipe{consumes = [1, 2], produces = [0], fluid_cost = 5},
    #recipe{consumes = [], produces = [1, 2], fluid_cost = 100},
    #recipe{consumes = [0], produces = [1, 2], fluid_cost = 1}]},
  Act = parse("[([1,2],[0],5),([],[1,2],100),([0],[1,2],1)]"),
  ?assertMatch(Exp, Act).

is_recipe_test() ->
  ?assert(is_recipe(#recipe{consumes = [], produces = [], fluid_cost = 0})),
  lists:map(fun(R) -> ?assert(is_recipe(R)) end, [
    #recipe{consumes = [1, 2], produces = [0], fluid_cost = 5},
    #recipe{consumes = [], produces = [1, 2], fluid_cost = 100},
    #recipe{consumes = [0], produces = [1, 2], fluid_cost = 1}]).

is_not_recipe_test() ->
  lists:map(fun(NR) -> ?assertNot(is_recipe(NR)) end, [
    #recipe{consumes = a},
    #recipe{produces = a},
    #recipe{fluid_cost = a},
    "FooBar",
    atom,
    42]).


-endif.
