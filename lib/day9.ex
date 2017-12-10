defmodule Day9 do
  @moduledoc """
  --- Day 9: Stream Processing ---

  A large stream blocks your path. According to the locals, it's not safe to cross the stream at the moment because it's
  full of garbage. You look down at the stream; rather than water, you discover that it's a stream of characters.

  You sit for a while and record part of the stream (your puzzle input). The characters represent groups - sequences
  that begin with { and end with }. Within a group, there are zero or more other things, separated by commas: either
  another group or garbage. Since groups can contain other groups, a } only closes the most-recently-opened unclosed
  group - that is, they are nestable. Your puzzle input represents a single, large group which itself contains many
  smaller ones.

  Sometimes, instead of a group, you will find garbage. Garbage begins with < and ends with >. Between those angle
  brackets, almost any character can appear, including { and }. Within garbage, < has no special meaning.

  In a futile attempt to clean up the garbage, some program has canceled some of the characters within it using !:
  inside garbage, any character that comes after ! should be ignored, including <, >, and even another !.

  You don't see any characters that deviate from these rules. Outside garbage, you only find well-formed groups,
  and garbage always terminates according to the rules above.

  Here are some self-contained pieces of garbage:

  <>, empty garbage.
  <random characters>, garbage containing random characters.
  <<<<>, because the extra < are ignored.
  <{!>}>, because the first > is canceled.
  <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
  <!!!>>, because the second ! and the first > are canceled.
  <{o"i!a,<{i<a>, which ends at the first >.
  Here are some examples of whole streams and the number of groups they contain:

  {}, 1 group.
  {{{}}}, 3 groups.
  {{},{}}, also 3 groups.
  {{{},{},{{}}}}, 6 groups.
  {<{},{},{{}}>}, 1 group (which itself contains garbage).
  {<a>,<a>,<a>,<a>}, 1 group.
  {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
  {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
  Your goal is to find the total score for all groups in your input. Each group is assigned a score which is one more
  than the score of the group that immediately contains it. (The outermost group gets a score of 1.)

  {}, score of 1.
  {{{}}}, score of 1 + 2 + 3 = 6.
  {{},{}}, score of 1 + 2 + 2 = 5.
  {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
  {<a>,<a>,<a>,<a>}, score of 1.
  {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
  {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
  {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
  What is the total score for all groups in your input?
"""
  def common_part(charlist) do
    {count, _, removed, _, _} = charlist |>
      List.foldl({0, 0, 0, false, false}, fn(char, acc) -> count_groups(char, acc) end)
    {count,removed}
  end

  def part_a do
    {a,_b}=File.read!("res/day9.input") |>
      String.to_charlist |>
      common_part
    a
  end
  def test do
    {0,0}=common_part('<>')
    {0,17}=common_part('<random characters>')
    {0,3}=common_part('<<<<>')
    {0,2}=common_part('<{!>}>')
    {0,0}=common_part('<!!>')
    {0,0}=common_part('<!!!>>')
    {0,10}=common_part('<{o"i!a,<{i<a>')
    {6,0}=common_part('{{{}}}')
    {5,0}=common_part('{{}{}}}')
    {16,0}=common_part('{{{},{},{{}}}}')
    {1,4}=common_part('{<a>,<a>,<a>,<a>}')
    {9,8}=common_part('{{<ab>},{<ab>},{<ab>},{<ab>}}')
    common_part('{{<!!>},{<!!>},{<!!>},{<!!>}}')
    #common_part('{{<a!>},{<a!>},{<a!>},{<ab>}}')
    :pass
  end
  def part_b do
    {_a,b}=File.read!("res/day9.input") |>
      String.to_charlist |>
      common_part
    b
  end
  def test_b do
    0=common_part('<>')
    17=common_part('<random characters>')
    0=common_part('<<<<>')
    0=common_part('<{!>}>')
    0=common_part('<!!>')
    0=common_part('<!!!>>')
    0=common_part('<{o"i!a,<{i<a>')
    6=common_part('{{{}}}')
    5=common_part('{{}{}}}')
    16=common_part('{{{},{},{{}}}}')
    1=common_part('{<a>,<a>,<a>,<a>}')
    9=common_part('{{<ab>},{<ab>},{<ab>},{<ab>}}')
    9=common_part('{{<!!>},{<!!>},{<!!>},{<!!>}}')
    3=common_part('{{<a!>},{<a!>},{<a!>},{<ab>}}')
    :pass
  end

  defp count_groups(?{, {c, n, r, false, false}) do
    {c + 1 + n, n + 1, r, false, false}
  end
  defp count_groups(?}, {c, n, r, false, false}) do
    {c, n - 1, r, false, false}
  end
  defp count_groups(?<, {c, n, r, false, false}) do
    {c, n, r, true, false}
  end
  defp count_groups(?>, {c, n, r, true, false}) do
    {c, n, r, false, false}
  end
  defp count_groups(?!, {c, n, r, garbage_bool, not_bool}) do
    {c, n, r, garbage_bool, not not_bool}
  end
  defp count_groups(_, {c, n, r, true, false}) do
    {c, n, r+1, true, false}
  end
  defp count_groups(_, {c, n, r, garbage_bool, true}) do
    {c, n, r, garbage_bool, false}
  end
  defp count_groups(_, acc) do
    acc
  end
end
