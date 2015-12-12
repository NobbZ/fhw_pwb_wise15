%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Dez 2015 20:55
%%%-------------------------------------------------------------------
-module(cr_storage_tests).
-author("nmelzer").

%% API
-export([]).

-include("storage.hrl").

-include_lib("eunit/include/eunit.hrl").

parse_empty_test() ->
  Exp = #storage{},
  Act = cr_storage:parse("[]"),
  ?assertMatch(Exp, Act).

parse_filled_test() ->
  Exp = #storage{storage = [1,2,3,4,5]},
  Act = cr_storage:parse("[1,2,3,4,5]"),
  ?assertMatch(Exp, Act).

consume_first_test() ->
  Exp = #storage{storage = [0,2,3,4,5]},
  Act = cr_storage:consume_item(#storage{storage = [1,2,3,4,5]}, 0),
  ?assertMatch(Exp, Act).

consume_middle_test() ->
  Exp = #storage{storage = [1,2,2,4,5]},
  Act = cr_storage:consume_item(#storage{storage = [1,2,3,4,5]}, 2),
  ?assertMatch(Exp, Act).

consume_last_test() ->
  Exp = #storage{storage = [1,2,3,4,4]},
  Act = cr_storage:consume_item(#storage{storage = [1,2,3,4,5]}, 4),
  ?assertMatch(Exp, Act).

consume_outside_test() ->
  Exp = #storage{storage = [1,2,3,4,5]},
  Act = cr_storage:consume_item(#storage{storage = [1,2,3,4,5]}, 5),
  ?assertMatch(Exp, Act).

consume_item_which_is_not_in_stock_test() ->
  ?assertMatch(impossible, cr_storage:consume_item(#storage{storage = [0,0]}, 0)),
  ?assertMatch(impossible, cr_storage:consume_item(#storage{storage = [0,0]}, 1)).

produce_first_test() ->
  Exp = #storage{storage = [2,2,3,4,5]},
  Act = cr_storage:produce_item(#storage{storage = [1,2,3,4,5]}, 0),
  ?assertMatch(Exp, Act).

produce_middle_test() ->
  Exp = #storage{storage = [1,2,4,4,5]},
  Act = cr_storage:produce_item(#storage{storage = [1,2,3,4,5]}, 2),
  ?assertMatch(Exp, Act).

produce_last_test() ->
  Exp = #storage{storage = [1,2,3,4,6]},
  Act = cr_storage:produce_item(#storage{storage = [1,2,3,4,5]}, 4),
  ?assertMatch(Exp, Act).

produce_outside_test() ->
  Exp = #storage{storage = [1,2,3,4,5]},
  Act = cr_storage:produce_item(#storage{storage = [1,2,3,4,5]}, 5),
  ?assertMatch(Exp, Act).
