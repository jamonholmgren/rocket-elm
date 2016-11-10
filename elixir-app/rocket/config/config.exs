# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :rocket,
  ecto_repos: [Rocket.Repo]

# Configures the endpoint
config :rocket, Rocket.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "8DaQXWZMQyHBJxYbPy0xUTOoiV14jCyMt9ROgH9J3LbuAG+0qMGUPYA5FfAdBJcu",
  render_errors: [view: Rocket.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Rocket.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
