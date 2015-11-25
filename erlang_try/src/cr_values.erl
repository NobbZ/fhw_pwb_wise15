%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 23:53
%%%-------------------------------------------------------------------
-module(cr_values).
-author("nmelzer").

%% API
-export([parse/1]).

-export_type([values/0]).

-record(values, {vals = array:new() :: array()}).
-opaque values() :: #values{}.

-spec parse(string()) -> values().
parse(Line) ->
  Expr = string:concat(string:strip(Line, both), "."),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  RelaxedArray = array:from_list(ListOfValues),
  {values, array:fix(RelaxedArray)}.
