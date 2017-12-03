%%%-------------------------------------------------------------------
%%% @author Nicholas Gunder
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 01. Dec 2017 17.22
%%%-------------------------------------------------------------------
-module(aoc2017).

%% API exports
-export([go/1]).

%%====================================================================
%% API functions
%%====================================================================
go(Day) ->
  day(Day).

%%====================================================================
%% Internal functions
%%====================================================================
day(all, 0)->
  done;
day(all, Num) ->
  day(Num),
  day(all, Num - 1).

day(all) ->
  day(all, 25);
day(1) ->
  io:format("Day 1 part A: ~w~n", [day1:part_a()]),
  io:format("Day 1 part B: ~w~n", [day1:part_b()]);
day(2) ->
  io:format("Day 2 part A: ~w~n", [day2:part_a()]),
  io:format("Day 2 part B: ~w~n", [day2:part_b()]);
day(3) ->
  io:format("Day 3 part A: ~w~n", [day3:part_a()]),
  io:format("Day 3 part B: ~w~n", [day3:part_b()]);
day(Invalid) ->
  io:format("Day ~w is not valid or has not be completed...~n", [Invalid]).