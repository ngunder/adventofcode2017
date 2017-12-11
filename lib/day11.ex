defmodule Day11 do
  @moduledoc """
  --- Day 11: Hex Ed ---

  Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in
  distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"

  Fortunately for her, you have plenty of experience with infinite grids.

  Unfortunately for you, it's a hex grid.

  The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast,
  southeast, south, southwest, and northwest:

    \ n  /
  nw +--+ ne
    /    \
    -+      +-
    \    /
  sw +--+ se
    / s  \

  You have the path the child process took. Starting where he started, you need to determine the fewest number of
  steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

  For example:

  ne,ne,ne is 3 steps away.
  ne,ne,sw,sw is 0 steps away (back where you started).
  ne,ne,s,s is 2 steps away (se,se).
  se,sw,se,sw,sw is 3 steps away (s,s,sw).

  --- Part Two ---
  How many steps away is the furthest he ever got from his starting position?
  """

  defp common_part do
    File.read!("res/day11.input") |>
      String.split(",") |>
      get_loc({0,0},0)
  end
  def part_a do
    {pos,_}=common_part()
      return(pos,0)
  end

  def part_b do
    {_pos,max}=common_part()
    max
  end

  def test do
    {pos1, _}="ne,ne,ne" |> String.split(",") |> get_loc({0,0},0)
    3=return(pos1, 0)
    {pos2, _}="ne,ne,sw,sw" |> String.split(",") |> get_loc({0,0},0)
    0=return(pos2, 0)
    {pos3, _}="se,sw,se,sw,sw" |> String.split(",") |> get_loc({0,0},0)
    3=return(pos3, 0)
    :pass
  end

  defp get_loc([], pos, furthest) do
    {pos, furthest}
  end
  defp get_loc(["n"|t], {x,y}, furthest) do
    get_loc_w_furthest(t, {x,y+1}, furthest)
  end
  defp get_loc(["s"|t], {x,y}, furthest) do
    get_loc_w_furthest(t, {x,y-1}, furthest)
  end
  defp get_loc(["ne"|t], {x,y}, furthest) do
    get_loc_w_furthest(t, {x+1,y+0.5}, furthest)
  end
  defp get_loc(["nw"|t], {x,y}, furthest) do
    get_loc_w_furthest(t, {x-1,y+0.5}, furthest)
  end
  defp get_loc(["sw"|t], {x,y}, furthest) do
    get_loc_w_furthest(t, {x-1,y-0.5}, furthest)
  end
  defp get_loc(["se"|t], {x,y}, furthest) do
    get_loc_w_furthest(t, {x+1,y-0.5}, furthest)
  end

  defp return({x,y}, steps) when x==0 and y==0 do
    steps
  end
  defp return({x,y}, steps) when x>0 and y>0 do
    return({x-1, y-0.5}, steps+1)
  end
  defp return({x,y}, steps) when x>0 do
    return({x-1, y+0.5}, steps+1)
  end
  defp return({x,y}, steps) when x<0 and y>0 do
    return({x+1, y-0.5}, steps+1)
  end
  defp return({x,y}, steps) when x<0 do
    return({x+1, y+0.5}, steps+1)
  end
  defp return({x,y}, steps) when y>0 and x==0 do
    return({x, y-1}, steps+1)
  end
  defp return({x,y}, steps) when y<0 and x==0 do
    return({x, y+1}, steps+1)
  end

  defp get_loc_w_furthest(t, {x,y}, furthest) do
    case (new_furthest = return({x,y}, 0)) > furthest do
      true ->
        get_loc(t, {x,y}, new_furthest)
      false ->
        get_loc(t, {x,y}, furthest)
    end
  end

end
