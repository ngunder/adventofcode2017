defmodule Day7 do
  @moduledoc """
  --- Day 7: Recursive Circus ---

  Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves
  into a bit of trouble. A recursive algorithm has gotten out of hand, and now they're balanced precariously in a large
  tower.

  One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several
  more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding
  their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the
  disc below them balanced but with no disc of their own.

  You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out
  their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing
  on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this
  in an orderly fashion; by the time you're done, you're not sure which program gave which information.

  For example, if your list is the following:

  pbga (66)
  xhth (57)
  ebii (61)
  havc (66)
  ktlj (57)
  fwft (72) -> ktlj, cntj, xhth
  qoyq (66)
  padx (45) -> pbga, havc, qoyq
  tknk (41) -> ugml, padx, fwft
  jptl (61)
  ugml (68) -> gyxo, ebii, jptl
  gyxo (61)
  cntj (57)
  ...then you would be able to recreate the structure of the towers that looks like this:

                  gyxo
                /
           ugml - ebii
         /      \
        |         jptl
        |
        |         pbga
       /        /
  tknk --- padx - havc
       \        \
             |    qoyq
        |
        |         ktlj
         \      /
           fwft - cntj
                \
                  xhth
  In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft.
  Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any
  other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)

  Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom
  program?
  --- Part Two ---

  The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending
  all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's
  fixed, they're stuck here.

  For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are
  supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of
  the programs in that tower.

  In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same
  weight, and they do: 61.

  However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match.
  This means that the following sums must all be the same:

  ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
  padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
  fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

  As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above
  ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the
  towers balanced. If this change were made, its weight would be 60.

  Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?
  """

  def part_a do
    build_tree("res/day7.input") |>
      :digraph_utils.arborescence_root()
  end
  def test_a do
    build_tree("res/day7_test.input") |>
      :digraph_utils.arborescence_root()
  end
  def part_b do
    digraph=build_tree("res/day7.input")
    {:yes, root} = :digraph_utils.arborescence_root(digraph)
    {root, weight} = :digraph.vertex(digraph, root)
    find_imbalance(digraph, {root, String.to_integer(weight)})
  end
  def test_b do
    digraph=build_tree("res/day7_test.input")
    {:yes, root} = :digraph_utils.arborescence_root(digraph)
    {root, weight} = :digraph.vertex(digraph, root)
    find_imbalance(digraph, {root, String.to_integer(weight)})
  end

  def build_tree(filename) do
    digraph = :digraph.new([:acyclic])
    File.read!(filename) |>
      String.split("\n") |>
      Enum.map(
        fn(line) ->
          String.split(line, [" ", "(", ")", ","], trim: true) |>
            process_line(digraph)
        end)
      digraph
  end

  def process_line([vertex, weight], digraph) do
    :digraph.add_vertex(digraph, vertex, weight)
  end
  def process_line([vertex, weight|more], digraph) do
    :digraph.add_vertex(digraph, vertex, weight)
    process_edges(more, {vertex, weight}, digraph)
  end

  def process_edges([], {_, _}, _) do
    :done
  end
  def process_edges(["->"|edges], {vertex, weight}, digraph) do
    process_edges(edges, {vertex, weight}, digraph)
  end
  def process_edges([edge_vertex|edges], {vertex, weight}, digraph) do
    case :digraph.add_edge(digraph, vertex, edge_vertex) do
      {:error, {:bad_vertex, new_v}} ->
         :digraph.add_vertex(digraph, new_v)
         :digraph.add_edge(digraph, vertex, edge_vertex)
      _ ->
         :ok
    end
    process_edges(edges, {vertex, weight}, digraph)
  end

  def find_imbalance(digraph, {r, w}) do
    children=:digraph.out_neighbours(digraph, r)
    children_w=check_weights(digraph, children, [])
    case match_weights(children_w) do
      true ->
        Enum.sum(children_w)+w
      false ->
        IO.inspect Enum.zip(get_children_weight(digraph, children, []),children), label: "branches"
        IO.inspect children_w, label: "total_branch_weights"
        throw :break
    end

  end

  def check_weights(_digraph, [], acc) do
    acc
  end

  def check_weights(digraph, [child|t], acc) do
    {child, weight} = :digraph.vertex(digraph, child)
    sub_branch_weight=find_imbalance(digraph, {child, String.to_integer(weight)})
    check_weights(digraph, t, [sub_branch_weight|acc])
  end

  def match_weights([]) do
    true
  end
  def match_weights([_]) do
    true
  end
  def match_weights([h,h|t]) do
    match_weights([h|t])
  end
  def match_weights(_) do
    false
  end
  def get_children_weight(_, [], acc) do
    acc
  end
  def get_children_weight(digraph, [child|tail], acc) do
    {_, weight} = :digraph.vertex(digraph, child)
    get_children_weight(digraph, tail, [weight|acc])
  end
end