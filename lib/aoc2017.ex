defmodule Aoc2017 do
  @moduledoc false

  def go(day) do
    do_day(day)
  end

  def do_day(:all, 0) do
    :done
  end
  def do_day(:all, num) do
    do_day(num)
    do_day(:all, num-1)
  end

  def do_day(:all) do
    do_day(:all, 25)
  end
  def do_day(1) do
    IO.inspect :day1.part_a, label: "Day 1 part A"
    IO.inspect :day1.part_b, label: "Day 1 part B"
  end
  def do_day(2) do
    IO.inspect :day2.part_a, label: "Day 2 part A"
    IO.inspect :day2.part_b, label: "Day 2 part B"
  end
  def do_day(3) do
    IO.inspect :day3.part_a, label: "Day 3 part A"
    IO.inspect :day3.part_b, label: "Day 3 part B"
  end
  def do_day(4) do
    IO.inspect Day4.part_a, label: "Day 4 part A"
    IO.inspect Day4.part_b, label: "Day 4 part B"
  end
  def do_day(5) do
    IO.inspect Day5.part_a, label: "Day 5 part A"
    IO.inspect Day5.part_b, label: "Day 5 part B"
  end
  def do_day(6) do
    IO.inspect Day6.part_a, label: "Day 6 part A"
    IO.inspect Day6.part_b, label: "Day 6 part B"
  end
  def do_day(7) do
    IO.inspect Day7.part_a, label: "Day 7 part A"
    try do
      IO.puts "Day 7 part B: "
      Day7.part_b
      catch
      x -> x
    end
  end
  def do_day(8) do
    IO.inspect Day8.part_a, label: "Day 8 part A"
    IO.inspect Day8.part_b, label: "Day 8 part B"
  end
  def do_day(9) do
    IO.inspect Day9.part_a, label: "Day 9 part A"
    IO.inspect Day9.part_b, label: "Day 9 part B"
  end
  def do_day(10) do
    IO.inspect Day10.part_a, label: "Day 10 part A"
    IO.inspect Day10.part_b, label: "Day 10 part B"
  end
  def do_day(11) do
    IO.inspect Day11.part_a, label: "Day 11 part A"
    IO.inspect Day11.part_b, label: "Day 11 part B"
  end
  def do_day(12) do
    IO.inspect Day12.part_a, label: "Day 12 part A"
    IO.inspect Day12.part_b, label: "Day 12 part B"
  end
  def do_day(13) do
    IO.inspect Day13.part_a, label: "Day 13 part A"
    IO.inspect Day13.part_b, label: "Day 13 part B"
  end
  def do_day(14) do
    IO.inspect Day14.part_a, label: "Day 14 part A"
    IO.inspect Day14.part_b, label: "Day 14 part B"
  end
  def do_day(15) do
    IO.inspect Day15.part_a, label: "Day 15 part A"
    IO.inspect Day15.part_b, label: "Day 15 part B"
  end
  def do_day(16) do
    IO.inspect Day16.part_a, label: "Day 16 part A"
    IO.inspect Day16.part_b, label: "Day 16 part B"
  end
  def do_day(17) do
    IO.inspect Day17.part_a, label: "Day 17 part A"
    IO.inspect Day17.part_b, label: "Day 17 part B"
  end
  def do_day(18) do
    IO.inspect Day18.part_a, label: "Day 18 part A"
    IO.inspect Day18.part_b, label: "Day 18 part B"
  end
  def do_day(19) do
    IO.inspect Day19.part_a, label: "Day 19 part A"
    IO.inspect Day19.part_b, label: "Day 19 part B"
  end
  def do_day(20) do
    IO.inspect Day20.part_a, label: "Day 20 part A"
    IO.inspect Day20.part_b, label: "Day 20 part B"
  end
  def do_day(invalid) do
    IO.inspect invalid, label: "This day is invalid"
  end
end
