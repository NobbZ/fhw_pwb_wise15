%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Dez 2015 22:29
%%%-------------------------------------------------------------------
-module(cr_random).
-author("nmelzer").

%% API
%-export([start/3, run/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start_link(Values, Recipes, Storage) ->
%%   Value = cr_storage:calc_value(Values, Storage),
%%   Pid = spawn_link(fun() -> walk(Values, Recipes, Storage, Value, []) end),
%%   {ok, Pid}.
%%
%% walk(Values, Recipes, Storage, Value, Path) ->
%%   Step = random:uniform(cr_recipes:recipes_count(Recipes)) - 1,
%%   Recipe = cr_recipes:get_recipe(Recipes, Step),
%%   Path2 = [Step|Path],
%%   case cr_recipes:apply_to_storage(Recipe, Storage) of
%%     impossible -> io:format("~p~n", [lists:reverse(Path)]);
%%     Storage2 -> {} % TODO
%%   end.

