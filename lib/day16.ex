defmodule Day16 do
  @moduledoc """
  --- Day 16: Permutation Promenade ---

  You come upon a very unusual sight; a group of programs here appear to be dancing.

  There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b
  stands in position 1, and so on until p, which stands in position 15.

  The programs' dance consists of a sequence of dance moves:

  Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example,
  s3 on abcde produces cdeab).
  Exchange, written xA/B, makes the programs at positions A and B swap places.
  Partner, written pA/B, makes the programs named A and B swap places.
  For example, with only five programs standing in a line (abcde), they could do the following dance:

  s1, a spin of size 1: eabcd.
  x3/4, swapping the last two programs: eabdc.
  pe/b, swapping programs e and b: baedc.
  After finishing their dance, the programs end up in order baedc.

  You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs
  standing after their dance?

  --- Part Two ---

  Now that you're starting to get a feel for the dance moves, you turn your attention to the dance as a whole.

  Keeping the positions they ended up in from their previous dance, the programs perform it again and again: including
  the first dance, a total of one billion (1000000000) times.

  In the example above, their second dance would begin with the order baedc, and use the same dance moves:

  s1, a spin of size 1: cbaed.
  x3/4, swapping the last two programs: cbade.
  pe/b, swapping programs e and b: ceadb.
  In what order are the programs standing after their billion dances?

    NOT pdibcoejnfmagkhl
"""
  def test_a do
    'baedc'=test_a('abcde')
    :pass
  end
  def test_a(start) do
    File.read!("res/day16_test.input") |>
      String.split(",") |>
      Enum.reduce(start, fn(x, list) ->  run_instruction(x, list) end)
  end
  ################################################################################
  def test_b do
    'ceadb'=test_b(2, 'abcde')
    :pass
  end
  def test_b(0, acc) do
    acc
  end
  def test_b(n, acc) do
    test_b(n-1, test_a(acc))
  end
  ################################################################################
  def part_b do
    instructions=File.read!("res/day16.input") |>
      String.split(",")
    part_b(instructions, 'abcdefghijklmnop', [])
  end
  def part_b(instructions, acc, history) do
    case acc in history do
      true ->
         loop_count = length(history)
         Enum.at(Enum.reverse(history), rem(1000000000, loop_count))
      false ->
        part_b(instructions, part_a(instructions, acc), [acc|history])

    end
  end
  ################################################################################
  def part_a do
    instructions=File.read!("res/day16.input") |>
      String.split(",")
    part_a(instructions, 'abcdefghijklmnop')
  end
  def part_a(instructions, start) do
    instructions |> Enum.reduce(start, fn(x, list) -> run_instruction(x, list) end)
  end
  ################################################################################
  defp run_instruction(<<?s, rest::binary>>, list) do
    spin(list, String.to_integer(rest))
  end
  defp run_instruction(<<?x, rest::binary>>, list) do
    [num1, num2]=String.split(rest, "/")
    exchange(list, String.to_integer(num1), String.to_integer(num2))
  end
  defp run_instruction(<<?p, rest::binary>>, list) do
    [num1, num2]=String.split(rest, "/")
    {[char1], [char2]} = {String.to_charlist(num1),String.to_charlist(num2)}
    partner(list, char1, char2)
  end
  ################################################################################
  def spin(list, num) when length(list) < num do
    spin(list, num-length(list))
  end
  def spin(list, num) do
    spin(list, length(list)-num, [])
  end
  def spin(tail, 0, acc) do
    tail++Enum.reverse(acc)
  end
  def spin([h|t], num, acc) do
    spin(t, num-1, [h|acc])
  end
  ################################################################################
  defp exchange(list, posa, posb) do
    {itema, _} = List.pop_at(list, posa)
    {itemb, _} = List.pop_at(list, posb)
    partner(list, itema, itemb)
  end
  ################################################################################
  def partner(list, namea, nameb) do
    partner(list, namea, nameb, [])
  end
  def partner([], _namea, _nameb, acc) do
    Enum.reverse(acc)
  end
  def partner([namea|t], namea, nameb, acc) do
    partner(t, namea, nameb, [nameb|acc])
  end
  def partner([nameb|t], namea, nameb, acc) do
    partner(t, namea, nameb, [namea|acc])
  end
  def partner([other|t], namea, nameb, acc) do
    partner(t, namea, nameb, [other|acc])
  end
end
