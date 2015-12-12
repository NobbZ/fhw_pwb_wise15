%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 00:08
%%%-------------------------------------------------------------------
-module(cr_storage).
-author("nmelzer").

%% API
-export([parse/1, add_fluid_to_storage/2, consume_item/2, produce_item/2]).

-include("storage.hrl").

%% @doc Parses the given `String' into a storage.
parse(Line) ->
  Expr = string:concat(string:strip(Line, both), "."),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  #storage{storage = ListOfValues}.

%% @doc Puts some amount of `Fluid' into the given `Store' and returns the
%%   new one.
add_fluid_to_storage(#storage{fluid = OldFluid} = S, AddFluid) ->
  S#storage{fluid = OldFluid + AddFluid}.

%% @doc Consumes exactly one piece of an item from the storage.
consume_item(#storage{storage = C} = S, ItemID) ->
  case consume_item_from_list(C, ItemID) of
    impossible -> impossible;
    List       -> S#storage{storage = List}
  end.

%% @doc Produces exactly one piece of an item into the storage.
produce_item(#storage{storage = C} = S, ItemID) ->
  S#storage{storage = produce_item_from_list(C, ItemID)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private helpers

consume_item_from_list([0|_], 0) -> impossible;
consume_item_from_list([H|T], 0) -> [H - 1|T];
consume_item_from_list(List, ID) -> consume_item_from_list(List, ID, []).

consume_item_from_list([], _, Acc)    -> lists:reverse(Acc);
consume_item_from_list([0|_], 0, _)   -> impossible;
consume_item_from_list([H|T], 0, Acc) -> lists:reverse(Acc, [H - 1|T]);
consume_item_from_list([H|T], I, Acc) ->
  consume_item_from_list(T, I - 1, [H|Acc]).

produce_item_from_list([H|T], 0) -> [H + 1|T];
produce_item_from_list(List, ID) -> produce_item_from_list(List, ID, []).

produce_item_from_list([], _, Acc)    -> lists:reverse(Acc);
produce_item_from_list([H|T], 0, Acc) -> lists:reverse(Acc, [H + 1|T]);
produce_item_from_list([H|T], I, Acc) ->
  produce_item_from_list(T, I - 1, [H|Acc]).
