defmodule Day4 do
  @moduledoc """
  Some doc stuff
  """

  @doc """
  Part A for Day 4 - My first Elixir program!
  """
  def part_a do
    common fn(enumerable,_fun) -> enumerable end
  end

  @doc """
  Part B for Day 4 w/sorting for each phrase to make compare on
  """
  def part_b do
    common (&Enum.map/2)
  end

  defp common (special_sort) do
    File.stream!("res/day4.input") |>
    Stream.map( &(String.replace(&1, "\n", "")) ) |>
    Stream.with_index |>
    Enum.count(
      fn( {contents, _line_num} ) ->
        String.split(contents, " ") |>
        special_sort.(fn(x)->Enum.sort(String.to_charlist(x)) end) |>
        Enum.group_by(fn(word) -> word end) |>
        Map.to_list |>
        Enum.reduce(true, fn({_k,v}, acc2) -> length(v) === 1 and acc2 end)
      end) |>
    IO.inspect(label: "Result")
  end
end
