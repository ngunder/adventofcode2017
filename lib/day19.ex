defmodule Day19 do
  @moduledoc """
  Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input),
  but it's confused about where to go.

  Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take,
  starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it
  reaches the end (located somewhere within the diagram) and stop there.

  Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only
  turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't
  change its direction, but it can use them to keep track of where it's been. For example:

       |
       |  +--+
       A  |  C
   F---|----E|--+
       |  |  |  D
       +B-+  +--+

  Given this diagram, the packet needs to take the following path:

  Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to
  the first +.

  Travel right, up, and right, passing through B in the process.
  Continue down (collecting C), right, and up (collecting D).
  Finally, go all the way left through E and stopping at F.
  Following the path to the end, the letters it sees on its path are ABCDEF.

  The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it
  would see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line
  wrapping.)

  --- Part Two ---

  The packet is curious how many steps it needs to go.

  For example, using the same routing diagram from the example above...

       |
       |  +--+
       A  |  C
   F---|--|-E---+
       |  |  |  D
       +B-+  +--+

  ...the packet would go:

  6 steps down (including the first line at the top of the diagram).
  3 steps right.
  4 steps up.
  3 steps right.
  4 steps down.
  3 steps right.
  2 steps up.
  13 steps left (including the F it stops on).
  This would result in a total of 38 steps.

  How many steps does the packet need to go?
"""
  def test do
    File.read!("res/day19_test.input") |>
      String.split("\n") |>
      start()
  end


  def part_a do
    {r,_} = File.read!("res/day19.input") |>
      String.split("\n") |>
      start()
    r
  end
  def part_b do
    {_,s} = File.read!("res/day19.input") |>
      String.split("\n") |>
      start()
    s
  end

  defp start(ll) do
    {x,y}=find_start(hd(ll), 0)
    follow_rails(loc(ll, {x,y}), :south, ll, {x,y}, "", 0)
  end

  defp follow_rails(" ", _, _ll, _loc, beento, steps) do
    {String.reverse(beento), steps}
  end
  defp follow_rails("+", dir, ll, {x,y}, beento, steps) do
    {res, dir, new_xy} = turn(ll, dir, {x,y})
    follow_rails(res, dir, ll, new_xy, beento, steps+1)
  end
  defp follow_rails(current, dir, ll, old_xy, beento, steps) when current >= "A" and current <= "Z" do
    new_xy=straight(dir, old_xy)
    follow_rails(loc(ll, new_xy), dir, ll, new_xy, current <> beento, steps+1)
  end
  defp follow_rails(_, dir, ll, old_xy, beento, steps) do
    new_xy=straight(dir, old_xy)
    follow_rails(loc(ll, new_xy), dir, ll, new_xy, beento, steps+1)
  end


  defp find_start(<<?|, _r::binary>>, count) do
    {count, 0}
  end
  defp find_start(<<_, r::binary>>, count) do
    find_start(r, count+1)
  end

  defp loc(ll, {x,y}) do
    case String.at(Enum.at(ll, y, " "), x) do
      nil ->
        " "
      other ->
        other
    end
  end

  defp turn(ll, dir, {x,y}) when dir == :north or dir == :south do
    case {loc(ll, {x+1, y}), loc(ll, {x-1, y})} do
      {" ", res} ->
        {res, :west, {x-1, y}}
      {res, " "} ->
        {res, :east, {x+1, y}}
    end
  end

  defp turn(ll, dir, {x,y}) when dir == :east or dir == :west do
    case {loc(ll, {x, y+1}), loc(ll, {x, y-1})} do
      {" ", res} ->
        {res, :north, {x, y-1}}
      {res, " "} ->
        {res, :south, {x, y+1}}
    end
  end

  defp straight(:north, {x,y}) do
    {x, y-1}
  end
  defp straight(:south, {x,y}) do
    {x, y+1}
  end
  defp straight(:east, {x,y}) do
    {x+1, y}
  end
  defp straight(:west, {x,y}) do
    {x-1, y}
  end
end
