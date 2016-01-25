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
  io:format("Initialising ~s~n", [?MODULE]),
  spawn_link(?MODULE, run, [Storage, Recipes, Values, []]).

run(Storage, Recipes, Values, Path) ->
  io:format("And I'm in run~n"),
  put(root_storage, Storage), % \
  io:format("Stored storage~n"),
  put(recipes, Recipes),      %  > Save for later use
  io:format("Stored recipes~n"),
  put(values, Values),        % /
  io:format("Stored values~n"),
  InitialValue = cr_storage:calc_value(Storage, Values),
  put(max_score, InitialValue),
  io:format("Stored initialvalue~n"),
  io:format("Before printing initial~n"),
  crftr_global_data:print_solution(InitialValue, []),
  io:format("After printing initial~n"),
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
      % io:format("~p~n", [CurValue]),
      case CurValue > get(max_score) of
        true  ->
          %io:format("Max Value: ~p~n", CurValue),
          put(max_score, CurValue),
          crftr_global_data:print_solution(CurValue, CurPath);
        false -> void
      end,
      cr_recipes:foldl(fun reducer/3, {CurStorage, CurPath}, get(recipes))
  end,
  {Storage, Path}.


% run(Storage, Recipes, Values, Path) ->
%  Process = fun(Idx, R, {Max, Path, MyStore, Fun}) ->
%    CurrentStorage = cr_recipes:apply_to_storage(R, MyStore),
%    CurrentValue = cr_storage:calc_value(CurrentStorage, Values),
%    case CurrentValue of
%      void ->
%        {Max, Path, MyStore};
%      Val ->
%        case cr_recipes:foldl(Fun, {impossible, [Idx|Path], CurrentStorage, Fun}) of
%          {impossible, _, _} ->
%            crftr_global_data:print_solution(Val, [Idx|Path]);
%          _ -> {Max, Path, MyStore}
%        end
%    end
%  end,
%  _CurrentValue = cr_storage:calc_value(Storage, Values),
%  cr_recipes:foldl(Process, {impossible, Path, Storage, Process}, Recipes).

