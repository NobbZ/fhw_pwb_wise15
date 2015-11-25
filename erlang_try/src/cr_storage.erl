%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2015 00:08
%%%-------------------------------------------------------------------
-module(cr_storage).
-author("nmelzer").

%% API
-export([parse/1, add_fluid_to_storage/2]).

-export_type([storage/0, fluid/0, item_id/0]).

-type(fluid() :: non_neg_integer()).
-type(item_id() :: non_neg_integer()).

-record(storage, {storage = array:new() :: array() % Dialyzer 2.6 doesn't know about array:array/1
  ,               fluid = 0 :: fluid()}).

-opaque(storage() :: #storage{}).

-spec parse(string()) -> storage().
parse(Line) ->
  Expr = string:concat(string:strip(Line, both), "."),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  RelaxedArray = array:from_list(ListOfValues),
  #storage{storage = array:fix(RelaxedArray)}.

-spec add_fluid_to_storage(storage(), fluid()) -> storage().
add_fluid_to_storage(#storage{fluid = OldFluid} = S, AddFluid) ->
  S#storage{fluid = OldFluid + AddFluid}.
