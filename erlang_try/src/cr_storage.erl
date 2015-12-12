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
-export([parse/1, add_fluid_to_storage/2]).

-export_type([storage/0, fluid/0, item_id/0]).

%% @type fluid(). Describes amounts of fluids.
-type(fluid() :: non_neg_integer()).
%% @type item_id(). Describes how an items ID looks like.
-type(item_id() :: non_neg_integer()).

-record(storage, {storage = [] :: list()
  ,               fluid   = 0  :: fluid()}).

%% @type storage(). The datatype of an actual storrage.
-opaque(storage() :: #storage{}).


%% @doc Parses the given `String' into a storage.
-spec parse(String :: string()) -> storage().
parse(Line) ->
  Expr = string:concat(string:strip(Line, both), "."),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  RelaxedArray = array:from_list(ListOfValues),
  #storage{storage = array:fix(RelaxedArray)}.

%% @doc Puts some amount of `Fluid' into the given `Store' and returns the
%%   new one.
-spec add_fluid_to_storage(Store :: storage(), Fluid :: fluid()) -> storage().
add_fluid_to_storage(#storage{fluid = OldFluid} = S, AddFluid) ->
  S#storage{fluid = OldFluid + AddFluid}.
