%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%%   <inf100760@fh-wedel.de> [http://stud.fh-wedel.de/~inf100760/]
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2015 23:53
%%%-------------------------------------------------------------------
-module(cr_values).
-author("nmelzer").

%% API
-export([parse/1, to_list/1, size/1, at/2]).

-record(values, {vals = [], size = 0}).

to_list(#values{vals = Vs}) -> Vs.

size(#values{size = S}) -> S.

at(#values{vals = Vs, size = S}, Idx) when Idx < S ->
  get_idx(Vs, Idx).

parse(Line) ->
  Expr = string:concat(string:strip(Line, both), "."),
  {ok, Tokens, _} = erl_scan:string(Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, ListOfValues, _} = erl_eval:expr(Form, []),
  #values{vals = ListOfValues, size = length(ListOfValues)}.

%% Private stuff

get_idx([H|_], 0) -> H;
get_idx([_|T], I) when I > 0 -> get_idx(T, I - 1).
