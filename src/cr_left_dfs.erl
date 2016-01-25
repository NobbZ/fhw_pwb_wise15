%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Dez 2015 11:19
%%%-------------------------------------------------------------------
-module(cr_left_dfs).
-author("nmelzer").

%% API
-export([start/3, run/4]).

start(Storage, Recipes, Values) ->
  spawn_link(?MODULE, run, [Storage, Recipes, Values, []]).

run(Storage, Recipes, Values, Path) ->
  put(root_storage, Storage), % \
  put(recipes, Recipes),      %  > Save for later use
  put(values, Values),        % /
  InitialValue = cr_storage:calc_value(Storage, Values),
  put(max_score, InitialValue),
  crftr_global_data:print_solution(InitialValue, []),
  cr_recipes:foldl(fun reducer/3, {Storage, Path}, Recipes).

reducer(Idx, R, {Storage, Path}) ->
  CurPath = [Idx|Path],
  CurStorage = cr_recipes:apply_to_storage(R, Storage),
  case CurStorage of
    impossible -> {};
    _ -> CurValue = case CurStorage of
      error -> get(max_score) - 100;
      _     -> cr_storage:calc_value(CurStorage, get(values))
    end,
      case CurValue > get(max_score) of
        true  ->
          put(max_score, CurValue),
          crftr_global_data:print_solution(CurValue, CurPath);
        false -> void
      end,
      cr_recipes:foldl(fun reducer/3, {CurStorage, CurPath}, get(recipes))
  end,
  {Storage, Path}.
