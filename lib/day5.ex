defmodule Day5 do
  @moduledoc """
  --- Day 5: A Maze of Twisty Trampolines, All Alike ---

  An urgent interrupt arrives from the CPU: it's trapped in a maze of jump instructions, and it would like assistance
  from any programs with spare cycles to help find the exit.

  The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to the previous instruction,
  and 2 skips the next one. Start at the first instruction in the list. The goal is to follow the jumps until one leads
  outside the list.

  In addition, these instructions are a little strange; after each jump, the offset of that instruction increases by 1.
  So, if you come across an offset of 3, you would move three instructions forward, but change it to a 4 for the next
  time it is encountered.

  For example, consider the following list of jump offsets:

  0
  3
  0
  1
  -3
  Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset
  values will be written all on one line, with the current instruction marked in parentheses. The following steps would
  be taken before an exit is found:

  (0) 3  0  1  -3  - before we have taken any steps.
  (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented
  to 1.
  2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented
  again, now to 2.
  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.

  In this example, the exit is reached in 5 steps.

  How many steps does it take to reach the exit?
  """

  @doc """
  Part A for Day 4
  """
  def common_part(file) do
    instruction_list = File.read!(file) |>
      String.split("\n") |>
      Enum.map(&String.to_integer/1)
    instruction_length = length(instruction_list)
    {instruction_length, 0..(instruction_length-1) |> Enum.zip(instruction_list) |> Map.new}
  end

  def part_a do
    {instruction_length, instruction_map} = common_part("res/day5.input")
    process_instructions(instruction_map, {0,0}, 0, instruction_length, false)
  end

  @doc """
  Part B for Day 4
  """
  def part_b do
    {instruction_length, instruction_map} = common_part("res/day5.input")
    process_instructions(instruction_map, {0,0}, 0, instruction_length, true)
  end

  def test_a do
    {instruction_length, instruction_map} = common_part("res/day5_test.input")
    process_instructions(instruction_map, {0,0}, 0, instruction_length, false)
  end

  def test_b do
    {instruction_length, instruction_map} = common_part("res/day5_test.input")
    process_instructions(instruction_map, {0,0}, 0, instruction_length, true)
  end

  def process_instructions(_, {cur_loc,val}, steps, max_size, _)
      when cur_loc + val >= max_size or cur_loc + val < 0 do
    steps + 1
  end

  def process_instructions(instruction_map, {cur_loc, val}, steps, max_size, true)
      when val >= 3 do
    newmap = Map.replace(instruction_map, cur_loc, val - 1)
    process_instructions(newmap, {cur_loc+val, Map.fetch!(newmap, cur_loc+val)}, steps+1, max_size, true)
  end

  def process_instructions(instruction_map, {cur_loc, val}, steps, max_size, bool_part) do
    newmap = Map.replace(instruction_map, cur_loc, val + 1)
    process_instructions(newmap, {cur_loc+val, Map.fetch!(newmap, cur_loc+val)}, steps+1, max_size, bool_part)

  end
end
