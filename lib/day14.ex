defmodule Day14 do
  @moduledoc """
  --- Day 14: Disk Defragmentation ---

  Suddenly, a scheduled job activates the system's disk defragmenter. Were the situation different, you might sit and
  watch it for a while, but today, you just don't have that kind of time. It's soaking up valuable system resources
  that are needed elsewhere, and so the only option is to help it finish its task as soon as possible.

  The disk in question consists of a 128x128 grid; each square of the grid is either free or used. On this disk, the
  state of the grid is tracked by the bits in a sequence of knot hashes.

  A total of 128 knot hashes are calculated, each corresponding to a single row in the grid; each hash contains 128
  bits which correspond to individual grid squares. Each bit of a hash indicates whether that square is free (0) or
  used (1).

  The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding to the row.
  For example, if your key string were flqrgnkx, then the first row would be given by the bits of the knot hash of
  flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.

  The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits correspond to
  4 bits, for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent binary
  value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on; a hash that begins
  with a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.

  Continuing this process, the first 8 rows and columns for key flqrgnkx appear as follows, using # to denote used
  squares, and . to denote free ones:

  ##.#.#..-->
  .#.#.#.#
  ....#.#.
  #.#.##.#
  .##.#...
  ##..#..#
  .#...#..
  ##.#.##.-->
  |      |
  V      V
  In this example, 8108 squares are used across the entire 128x128 grid.

  Given your actual key string, how many squares are used?

  --- Part Two ---

  Now, all the defragmenter needs to know is the number of regions. A region is a group of used squares that are all
  adjacent, not including diagonals. Every used square is in exactly one region: lone used squares form their own
  isolated regions, while several adjacent squares all count as a single region.

  In the example above, the following nine regions are visible, each marked with a distinct digit:

  11.2.3..-->
  .1.2.3.4
  ....5.6.
  7.8.55.9
  .88.5...
  88..5..8
  .8...8..
  88.8.88.-->
  |      |
  V      V
  Of particular interest is the region marked 8; while it does not appear contiguous in this small view,
  all of the squares marked 8 are connected when considering the whole 128x128 grid. In total, in this example,
  1242 regions are present.

  How many regions are present given your key string?

  Your puzzle input is hxtvlmkl.

  NOT 3417
  """
  require Record
  Record.defrecord :get_next_val, [:caller]
  Record.defrecord :get_val, [:caller]
  Record.defrecord :get_nums, [:caller]
  Record.defrecord :change_val, [:new_val, :caller]
  Record.defrecord :unreg_val, [:old_val, :caller]

  Record.defrecord :square_state, [:x, :y, val: :default]
  Record.defrecord :done, []

  def part_a do
    test_a('hxtvlmkl')
  end

  def part_b do
    test_b('hxtvlmkl')
  end

  def test_a do
    test_a('flqrgnkx')
  end
  def test_a(string) do
    gen_list_of_hashes(string)
    |> Enum.reduce(0, fn (x, acc) -> count_bin(x, 0) + acc end)
  end

  def test_b do
    test_b('flqrgnkx')
  end

  def test_b(string) do
    hash_list = gen_list_of_hashes(string)
    for a <- 0..127, do: spawn_map(Enum.at(hash_list, a), {0, a})
    pid = spawn_link fn -> val_holder(1, []) end
    :global.register_name(:val_holder, pid)
    for name <- :global.registered_names(), do: reach_out_process(name)
    result = length(sync_call(:global.whereis_name(:val_holder), get_nums(caller: self())))
    for name <- :global.registered_names(), do: async_call(name, done())
    result
  end

  defp reach_out_process(:val_holder) do
  end
  defp reach_out_process({x, y}) do
    :timer.sleep(1)
    pid = :global.whereis_name({x, y})
    case sync_call(pid, get_val(caller: self())) do
      {_, :default} ->
        {
          {x, y},
          async_call(pid, change_val(new_val: sync_call(:val_holder, get_next_val(caller: self())), caller: self()))
        }
      _something ->
        {{x, y}, :do_nothing}
    end
  end

  defp count_bin([], acc) do
    acc
  end
  defp count_bin([?1 | t], acc) do
    count_bin(t, acc + 1)
  end
  defp count_bin([_ | t], acc) do
    count_bin(t, acc)
  end

  defp gen_list_of_hashes(string) do
    for i <- 0..127, do: String.to_charlist(Day10.help_day14(string ++ '-' ++ Integer.to_charlist(i)))
  end

  def spawn_map([], _) do
    :done
  end
  def spawn_map([?1 | t], {x, y}) do
    pid = spawn_link fn -> square(square_state(x: x, y: y)) end
    :global.register_name({x, y}, pid)
    spawn_map(t, {x + 1, y})
  end
  def spawn_map([?0 | t], {x, y}) do
    spawn_map(t, {x + 1, y})
  end

  defp val_holder(val, nums) do
    receive do
      get_next_val(caller: caller) ->
        async_call(caller, val)
        val_holder(val + 1, [val | nums])
      unreg_val(old_val: someval) ->
        IO.inspect someval, label: "remove"
        val_holder(val, nums -- [someval])
      get_nums(caller: caller) ->
        async_call(caller, nums)
        val_holder(val, nums)
      done() ->
        :done
    end
  end

  ## Decided to use processes
  defp square(ss = square_state(x: x, y: y, val: val)) do
    receive do
      get_val(caller: caller) ->
        async_call(caller, {{x, y}, val})
        square(ss)
      change_val(new_val: change_val) ->
        case change_val < val do
          true ->
            case val == :default do
              false ->
                async_call(:val_holder, {:unreg_val, val, self()})
              true ->
                :ok
            end
            async_call({x + 1, y}, change_val(new_val: change_val, caller: self()))
            async_call({x, y + 1}, change_val(new_val: change_val, caller: self()))
            async_call({x - 1, y}, change_val(new_val: change_val, caller: self()))
            async_call({x, y - 1}, change_val(new_val: change_val, caller: self()))
            square(square_state(ss, val: change_val))
          _ ->
            square(ss)
        end
      done() ->
        :done
    end
  end


  def async_call(target, msg) when is_pid(target) do
    send target, msg
  end
  def async_call(target, msg) do
    case :global.whereis_name(target) do
      :undefined ->
        :fail
      pid ->
        send pid, msg
    end
  end

  def sync_call(target, msg) do
    async_call(target, msg)
    receive do
      msg ->
        msg
    end
  end
end
