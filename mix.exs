defmodule TestRyuFloat.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_ryu_float,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:propcheck, "~> 1.2"},
      {:benchee, "~> 1.0"},
      {:stream_data, "~> 0.5.0"}
    ]
  end
end
