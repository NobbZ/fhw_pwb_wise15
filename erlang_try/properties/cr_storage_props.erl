%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Dez 2015 21:49
%%%-------------------------------------------------------------------
-module(cr_storage_props).
-author("nmelzer").

%% API
-export([runner/0]).

-include("storage.hrl").

-include_lib("proper/include/proper.hrl").

-define(SAMPLES, 1000).

runner() ->
  io:format("can_consume_from_filled"),
  proper:quickcheck(can_consume_from_filled(), ?SAMPLES),
  io:format("cant_consume_from_empty"),
  proper:quickcheck(cant_consume_from_empty(), ?SAMPLES),
  io:format("consuming_an_item_leaves_one_item_less_in_sum"),
  proper:quickcheck(consuming_an_item_leaves_one_item_less_in_sum(), ?SAMPLES),
  io:format("producing_an_item_leaves_one_item_more_in_sum"),
  proper:quickcheck(producing_an_item_leaves_one_item_more_in_sum(), ?SAMPLES).

storage() ->
  ?LET({Xs, F}, {non_empty(list(non_neg_integer())), non_neg_integer()},
    #storage{storage = Xs, fluid = F}).

dropzero(#storage{storage = Ss} = S, I) ->
  NewI = I rem length(Ss),
  {S#storage{storage = dropzero(Ss, NewI, [])}, NewI}.

dropzero([_|T], 0, Acc) -> lists:reverse(Acc, [0|T]);
dropzero([H|T], I, Acc) -> dropzero(T, I - 1, [H|Acc]).

storage_and_index() ->
  ?LET({S, I}, {storage(), non_neg_integer()},
    dropzero(S, I)).

filled_storage() ->
  ?LET({Xs, F}, {non_empty(list(integer(1, inf))), integer(1, inf)},
    #storage{storage = Xs, fluid = F}).

filled_storage_and_index() ->
  ?LET({S, I}, {filled_storage(), non_neg_integer()},
    {S, I rem length(S#storage.storage)}).

can_consume_from_filled() ->
  ?FORALL({S, Item}, filled_storage_and_index(),
    cr_storage:consume_item(S, Item) /= impossible).

cant_consume_from_empty() ->
  ?FORALL({S, Item}, storage_and_index(),
    cr_storage:consume_item(S, Item) == impossible).

check_consuming_an_item_leaves_one_item_less_in_sum(S, Item) ->
  NewItem = Item rem length(S#storage.storage),
  case cr_storage:consume_item(S, NewItem) of
    #storage{storage = SL} ->
      lists:sum(SL) == lists:sum(S#storage.storage) - 1;
    impossible -> lists:nth(NewItem + 1, S#storage.storage) == 0
  end.

consuming_an_item_leaves_one_item_less_in_sum() ->
  ?FORALL({S, Item}, {storage(), non_neg_integer()},
    check_consuming_an_item_leaves_one_item_less_in_sum(S, Item)).

check_producing_an_item_leaves_one_item_more_in_sum(S, I) ->
  lists:sum(S#storage.storage) + 1 ==
    lists:sum((cr_storage:produce_item(S, I))#storage.storage).

producing_an_item_leaves_one_item_more_in_sum() ->
  ?FORALL({S, Item}, filled_storage_and_index(),
    check_producing_an_item_leaves_one_item_more_in_sum(S, Item)).
