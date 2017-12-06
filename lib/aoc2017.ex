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
    IO.inspect :day1.part_b, label: "Day 2 part B"
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
  def do_day(invalid) do
    IO.inspect invalid, label: "This day is invalid"
  end
end
