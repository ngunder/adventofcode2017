defmodule Day6 do
  @moduledoc """
  A debugger program here is having an issue: it is trying to repair a memory reallocation routine, but it keeps getting
  stuck in an infinite loop.

  In this area, there are sixteen memory banks; each memory bank can hold any number of blocks. The goal of the
  reallocation routine is to balance the blocks between the memory banks.

  The reallocation routine operates in cycles. In each cycle, it finds the memory bank with the most blocks (ties won by
  the lowest-numbered memory bank) and redistributes those blocks among the banks. To do this, it removes all of the
  blocks from the selected bank, then moves to the next (by index) memory bank and inserts one of the blocks. It
  continues doing this until it runs out of blocks; if it reaches the last memory bank, it wraps around to the first
  one.

  The debugger would like to know how many redistributions can be done before a blocks-in-banks configuration is
  produced that has been seen before.

  For example, imagine a scenario with only four memory banks:

  The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
  Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the
  7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third
  bank gets one back. The final result looks like this: 2 4 1 2.
  Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each
  gets one block. The result is: 3 1 2 3.
  Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins
  the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
  The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
  The third bank is chosen, and the same thing happens: 2 4 1 2.
  At this point, we've reached a state we've seen before: 2 4 1 2 was already seen. The infinite loop is detected after
  the fifth block redistribution cycle, and so the answer in this example is 5.

  Given the initial block counts in your puzzle input, how many redistribution cycles must be completed before a
  configuration is produced that has been seen before?
  """

  @doc """
  Part A for Day 4
  """

  def part_a do
    common_part("res/day6.input") |>
      process_memory(MapSet.new(), :a)
  end

  @doc """
  Part B for Day 4
  """
  def part_b do
    common_part("res/day6.input") |>
      process_memory(MapSet.new(), :b)
  end

  def common_part(file) do
    instruction_list = File.read!(file) |>
      String.split("\t") |>
      Enum.map(&String.to_integer/1)
    instruction_length = length(instruction_list)
    0..(instruction_length-1) |>
      Enum.zip(instruction_list) |>
      Map.new()
  end

  def test_a do
    common_part("res/day6_test.input") |>
      process_memory(MapSet.new(), :a)
  end

  def test_b do
    common_part("res/day6_test.input") |>
      process_memory(MapSet.new(), :b)
  end

  def process_memory(mem_map, mem_history_set, part) do
    case {MapSet.member?(mem_history_set,  mem_map), part} do
      {true,:a} ->
        MapSet.size(mem_history_set)
      {true, :b} ->
        process_memory(mem_map, MapSet.new(), :a)
      {false, part} ->
        mem_list = Map.to_list(mem_map)
        {max_pos,max_val} = mem_list |> Enum.max_by(fn({_pos, val}) -> val end)
        mem_list |>
          List.keyreplace(max_pos, 0, {max_pos,0}) |>
          distribute(max_val, max_pos+1) |>
          Map.new() |>
          process_memory(MapSet.put(mem_history_set, mem_map), part)
    end
  end


  def distribute(list, 0, _) do
    list
  end
  def distribute([{k,v}|t], max_val, 0) do
    distribute(t++[{k,v+1}], max_val-1, 0)
  end
  def distribute([{k,v}|t], max_val, pos) do
    distribute(t++[{k,v}], max_val, pos-1)
  end


end

