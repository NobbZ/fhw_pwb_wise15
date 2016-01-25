%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2016 15:23
%%%-------------------------------------------------------------------
-module(cr_bfs).
-author("nmelzer").

%% API
-export([start/3, run/4]).

start(Storage, Recipes, Values) ->
  spawn_link(?MODULE, run, [Storage, Recipes, Values, []]).

run(Storage, Recipes, Values, Path) ->
  put(root_storage, Storage),
  put(recipes, Recipes),
  put(values, Values),
  InitialValue = cr_storage:calc_value(Storage, Values),
  put(max_score, InitialValue),
  crftr_global_data:print_solution(InitialValue, []),
  Count = cr_recipes:recipes_count(Recipes),
  RsAsList = cr_recipes:recipes_to_list(Recipes),
  Idxs = lists:seq(0, Count - 1),
  IndexedButUnsorted = lists:zip(Idxs, RsAsList),
  SortedList = lists:sort(fun recipe_sorter/2, IndexedButUnsorted),
  put(indexed_list, SortedList),
  %io:format("~p~n", [SortedList]),
  lists:map(fun({Idx, R}) -> mapper(Idx, R, Storage, Path) end, lists:reverse(SortedList)).

recipe_sorter({_, RcpL}, {_, RcpR}) ->
  Values = get(values),
%%   io:format(standard_error, "~p: ~p~n", [RcpL, cr_recipes:guess_score(RcpL, Values)]),
%%   io:format(standard_error, "~p: ~p~n", [RcpR, cr_recipes:guess_score(RcpR, Values)]),
%%   io:format(standard_error, "====================", []),
  cr_recipes:guess_score(RcpL, Values) =< cr_recipes:guess_score(RcpR, Values).

mapper(Idx, R, Storage, Path) ->
  CurPath = [Idx|Path],
  %io:format("~p, ~p, ~p~n", [Idx, R, Storage]),
  CurStorage = cr_recipes:apply_to_storage(R, Storage),
  %io:format(standard_error, "~p~n", [CurPath]),
  case CurStorage of
    impossible -> {};
    _ ->
      CurValue = case CurStorage of
        error -> get(max_score) - 100;
        _ -> cr_storage:calc_value(CurStorage, get(values))
      end,
      %io:format(standard_error, "~p~n", [CurValue]),
      case CurValue > get(max_score) of
        true ->
          put(max_score, CurValue),
          crftr_global_data:print_solution(CurValue, CurPath);
        false -> void
      end,
      lists:map(fun({Idx, R}) -> mapper(Idx, R, CurStorage, CurPath) end, get(indexed_list))
  end.
