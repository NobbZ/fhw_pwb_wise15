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

-include("recipes.hrl").

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

