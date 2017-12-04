defmodule Aoc2017.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aoc2017,
      version: "0.0.1",
      elixir: "~> 1.5.2",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end
end