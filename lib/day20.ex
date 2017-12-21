defmodule PState do
  defstruct p: {nil, nil, nil}, v: {nil, nil, nil}, a: {nil, nil, nil}, bosspid: nil, id: nil
end

defmodule Day20 do
  @moduledoc """
  --- Day 20: Particle Swarm ---

  Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate too many particles, and it won't be
  able to finish them all in time to render the next frame at this rate.

  It transmits to you a buffer (your puzzle input) listing each particle in order (starting with particle 0,
  then particle 1, particle 2, and so on). For each particle, it provides the X, Y, and Z coordinates for the particle's
  position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

  Each tick, all particles are updated simultaneously. A particle's properties are updated in the following order:

  Increase the X velocity by the X acceleration.
  Increase the Y velocity by the Y acceleration.
  Increase the Z velocity by the Z acceleration.
  Increase the X position by the X velocity.
  Increase the Y position by the Y velocity.
  Increase the Z position by the Z velocity.
  Because of seemingly tenuous rationale involving z-buffering, the GPU would like to know which particle will stay
  closest to position <0,0,0> in the long term. Measure this using the Manhattan distance, which in this situation
  is simply the sum of the absolute values of a particle's X, Y, and Z position.

  For example, suppose you are only given two particles, both of which stay entirely on the X-axis (for simplicity).
  Drawing the current states of particles 0 and 1 (in that order) with an adjacent a number line and diagram of
  current X positions (marked in parenthesis), the following would take place:

  p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

  p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

  p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

  p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)
  At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will
  stay closest.

  Which particle will stay closest to position <0,0,0> in the long term?

  --- Part Two ---
  To simplify the problem further, the GPU would like to remove any particles that collide. Particles collide if their
  positions ever exactly match. Because particles are updated simultaneously, more than two particles can collide at
  the same time and place. Once particles collide, they are removed and cannot collide with anything else after
  that tick.

  For example:

  p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
  p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
  p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

  p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
  p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
  p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

  p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
  p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
  p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

  ------destroyed by collision------
  ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
  ------destroyed by collision------                      (3)
  p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

  In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. On the next tick,
  particle 3 passes through unharmed.

  How many particles are left after all collisions are resolved?
  """

  defp common(file, type) do
    {proclist, _i} = File.read!(file)
                    |> String.split("\n")
                    |> List.foldl(
                         {[], 0},
                         fn (p, {t, acc}) -> {[parse(p, acc, self()) | t], acc + 1}
                         end
                       )
    indexed = Enum.with_index(Enum.reverse(proclist))
    send(self(), {:proclist, indexed})
    boss([], type)
  end
  def test_a do
    common("res/day20_test_a.input", :normal)
  end
  def test_b do
    common("res/day20_test_b.input", :collide)
  end
  def part_a do
    common("res/day20.input", :normal)
  end
  def part_b do
    common("res/day20.input", :collide)
  end

  defp parse(p, i, bosspid) do
    String.split(p, ["p=<", "v=<", "a=<", " ", ">", ">, "], trim: true)
    |> process_points()
    |> spawn_points(i, bosspid)
  end

  defp spawn_points({p, v, a}, i, bosspid) do
    spawn(Day20, :particle, [%PState{p: p, v: v, a: a, id: i, bosspid: bosspid}])
  end

  defp process_points([a, b, c]) do
    p = String.split(a, ",")
        |> convert_point()
    v = String.split(b, ",")
        |> convert_point()
    a = String.split(c, ",")
        |> convert_point()
    {p, v, a}
  end

  defp convert_point([a, b, c]) do
    {String.to_integer(a), String.to_integer(b), String.to_integer(c)}
  end
  ############################### Particle process
  def particle(pstate) do
    send(pstate.bosspid, {:started, pstate.id})
    receive do
      :tick ->
        particle__started(pstate)
      :stop ->
        :ok
    end
  end

  def particle__started(pstate) do
    {px, py, pz} = pstate.p
    {vx, vy, vz} = pstate.v
    {ax, ay, az} = pstate.a

    #Increase the X velocity by the X acceleration.
    vx = vx + ax
    #Increase the Y velocity by the Y acceleration.
    vy = vy + ay
    #Increase the Z velocity by the Z acceleration.
    vz = vz + az
    #Increase the X position by the X velocity.
    px = px + vx
    #Increase the Y position by the Y velocity.
    py = py + vy
    #Increase the Z position by the Z velocity.
    pz = pz + vz
    send(pstate.bosspid, {:loc, pstate.id, self(), {px, py, pz}})
    receive do
      :tick ->
        particle__started(%{pstate | p: {px, py, pz}, v: {vx, vy, vz}})
      :stop ->
        :ok
    end
  end
  ############################### Boss process

  def boss([], type) do
    ## Wait for all of the procs to start
    receive do
      {:proclist, proclist} ->
        boss__wait_for_proc_starts(proclist, proclist, type)
    end
  end

  defp boss__wait_for_proc_starts([], proclist, type) do
    boss__tick(proclist, 0, type)
  end
  defp boss__wait_for_proc_starts(procs, proclist, type) do
    receive do
      {:started, id} ->
        boss__wait_for_proc_starts(List.keydelete(procs, id, 1), proclist, type)
    end
  end
  defp boss__tick(proclist, count, type) do
    for {proc, _id} <- proclist, do: send(proc, :tick)
    boss__grab(proclist, proclist, [], count, type)
  end
  defp boss__grab([], proclist, results, 500, :collide) do
    for {proc, _id} <- proclist, do: send(proc, :stop)
    length(List.keysort(results, 1))
  end
  defp boss__grab([], proclist, results, 500, :normal) do
    for {proc, _id} <- proclist, do: send(proc, :stop)
    {{_pid, id}, _} = hd(List.keysort(results, 1))
    id
  end
  defp boss__grab([], proclist, results, count, :collide) do
    ids = remove_collisions(results, [])
    newproclist=remove_ids(proclist, ids)
    boss__tick(newproclist, count + 1, :collide)
  end
  defp boss__grab([], proclist, _results, count, type) do
    boss__tick(proclist, count + 1, type)
  end
  defp boss__grab(procs, proclist, results, count, :collide) do
    receive do
      {:loc, id, pid, {px, py, pz}} ->
        boss__grab(List.keydelete(procs, pid, 0), proclist, [{{pid, id}, {px, py, pz}} | results], count, :collide)
    end
  end
  defp boss__grab(procs, proclist, results, count, :normal) do
    receive do
      {:loc, id, pid, {px, py, pz}} ->
        boss__grab(List.keydelete(procs, pid, 0), proclist, [{{pid, id}, abs(px) + abs(py) + abs(pz)} | results], count,
          :normal)
    end
  end

  defp remove_collisions([], acc) do
    acc
  end
  defp remove_collisions([x={{_pid, _id}, xyz} | rest], acc) do
    if List.keymember?(rest, xyz, 1) do
      {removed, newlist} = delete_all(xyz, [x | rest], [], [])
      remove_collisions(newlist, removed++acc)
    else
      remove_collisions(rest, acc)
    end
  end

  defp delete_all(_item, [], newlist, removed) do
    {removed, newlist}
  end
  defp delete_all(xyz, [{{pid, id}, xyz}|t], newlist, removed) do
    send(pid, :stop)
    delete_all(xyz, t, newlist, [id|removed])
  end
  defp delete_all(xyz, [other|t], newlist, removed) do
    delete_all(xyz, t, [other|newlist], removed)
  end

  defp remove_ids(proclist, []) do
    proclist
  end
  defp remove_ids(proclist, [id1|rest]) do
    remove_ids(List.keydelete(proclist, id1, 1), rest)
  end

end
