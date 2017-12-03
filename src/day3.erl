%%%-------------------------------------------------------------------
%%% @author ngunder
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2017 09.42
%%%-------------------------------------------------------------------
-module(day3).
-author("ngunder").
%% --- Day 3: Spiral Memory ---
%%
%% You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
%%
%% Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while
%% spiraling outward. For example, the first few squares are allocated like this:
%%
%% 17  16  15  14  13
%% 18   5   4   3  12
%% 19   6   1   2  11
%% 20   7   8   9  10
%% 21  22  23---> ...
%% While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the
%% location of the only access port for this memory system) by programs that can only move up, down, left, or right.
%% They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
%%
%% For example:
%%
%% Data from square 1 is carried 0 steps, since it's at the access port.
%% Data from square 12 is carried 3 steps, such as: down, left, left.
%% Data from square 23 is carried only 2 steps: up twice.
%% Data from square 1024 must be carried 31 steps.
%% How many steps are required to carry the data from the square identified in your puzzle input all the way to the
%% access port?
%%
%% Your puzzle answer was 371.
%%
%% --- Part Two ---
%%
%% As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in
%% the same allocation order as shown above, they store the sum of the values in all adjacent squares, including
%% diagonals.
%%
%% So, the first few squares' values are chosen as follows:
%%
%% Square 1 starts with the value 1.
%% Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
%% Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
%% Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
%% Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
%% Once a square is written, its value does not change. Therefore, the first few squares would receive the following
%% values:
%%
%% 147  142  133  122   59
%% 304    5    4    2   57
%% 330   10    1    1   54
%% 351   11   23   25   26
%% 362  747  806--->   ...
%% What is the first value written that is larger than your puzzle input?
%%
%% Your puzzle answer was 369601.
%%
%% Both parts of this puzzle are complete! They provide two gold stars: **
%%
%% At this point, you should return to your advent calendar and try another puzzle.
%%
%% Your puzzle input was 368078.

-define(INPUT, 368078).
-record(square, {x, y, val=0, pos}).
%% API
-export([part_a/0, part_b/0, part_b/1, test/0]).

part_a() ->
  part_a(?INPUT).
part_b() ->
  part_b(?INPUT).

part_b(Num) ->
  part_b(Num, 1).

part_b(_, 100) ->
  io:format("Stop...Number is too large");
part_b(Num, Find) ->
  #square{val=V} = hd(gen_map(Find-1, fun part_b_val/2)),
  case V > Num of
    true ->
      V;
    false ->
      part_b(Num, Find+1)
  end.


part_a(Num) ->
  #square{x=X, y=Y} = hd(gen_map(Num-1, fun part_a_val/2)),
  abs(X) + abs(Y).

gen_map(Num, Fun) ->
  gen_map(Num, 2, 1, {1,0}, [#square{x=0, y=0, val=1, pos=1}], Fun).


gen_map(0, _, _, _, Acc, _) ->
  Acc;
% Push Out X and Square Size
gen_map(Num, Cur, Max, {CPosX, CPosY}, Acc, F) when CPosX =:= Max andalso CPosY =:= -Max ->
  S = #square{x=CPosX, y=CPosY, pos=Cur},
  gen_map(Num-1, Cur+1, Max+1, {CPosX+1, CPosY}, [S#square{val=F(S, Acc)}|Acc], F);
% Reach Top-Right corner
gen_map(Num, Cur, Max, {CPosX, CPosY}, Acc, F) when CPosX =:= Max andalso CPosY < Max ->
  S = #square{x=CPosX, y=CPosY, pos=Cur},
  gen_map(Num-1, Cur+1, Max, {CPosX, CPosY+1}, [S#square{val=F(S, Acc)}|Acc], F);
% Reach Top-Left corner
gen_map(Num, Cur, Max, {CPosX, CPosY}, Acc, F) when CPosX > -Max andalso CPosY =:= Max ->
  S = #square{x=CPosX, y=CPosY, pos=Cur},
  gen_map(Num-1, Cur+1, Max, {CPosX-1, CPosY}, [S#square{val=F(S, Acc)}|Acc], F);
% Reach Bottom-Left corner
gen_map(Num, Cur, Max, {CPosX, CPosY}, Acc, F) when CPosX =:= -Max andalso CPosY > -Max ->
  S = #square{x=CPosX, y=CPosY, pos=Cur},
  gen_map(Num-1, Cur+1, Max, {CPosX, CPosY-1}, [S#square{val=F(S, Acc)}|Acc], F);
% Reach Bottom-Right corner
gen_map(Num, Cur, Max, {CPosX, CPosY}, Acc, F) ->
  S = #square{x=CPosX, y=CPosY, pos=Cur},
  gen_map(Num-1, Cur+1, Max, {CPosX+1, CPosY}, [S#square{val=F(S, Acc)}|Acc], F).

part_a_val(_, _) ->
  0.
part_b_val(S = #square{}, Acc) ->
  lists:sum([V||#square{val=V}<-get_nabo(S,Acc,[])]).

get_nabo(_, [], Nabos) ->
  Nabos;
get_nabo(S=#square{pos = P}, [#square{pos = P}|Rest], Nabos) ->
  get_nabo(S, Rest, Nabos);
get_nabo(S=#square{x=SX, y=SY}, [Nabo=#square{x=X, y=Y}|Rest], Nabos)
  when abs(SX - X) =< 1 andalso abs(SY - Y) =< 1 ->
  get_nabo(S, Rest, [Nabo|Nabos]);
get_nabo(S, [_|Rest], Nabos) ->
  get_nabo(S, Rest, Nabos).


test() ->
  0=part_a(1),
  3=part_a(12),
  2=part_a(23),
  31=part_a(1024),
  1=part_b(1),
  1=part_b(2),
  2=part_b(3),
  4=part_b(4),
  5=part_b(5),
  806=part_b(23),
  pass.