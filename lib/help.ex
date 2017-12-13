defmodule Help do
  @moduledoc false
  

  def print_dot_every(step, steps) do
    case rem(step, steps) === 0 do
      true -> IO.write "."
      false -> :blah
    end
  end

end
