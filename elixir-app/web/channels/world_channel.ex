defmodule Rocket.WorldChannel do
  use Rocket.Web, :channel

  def join("world:game", payload, socket) do
    IO.puts "Connected to world:game."
    IO.inspect payload
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  def join(room, _params, _socket) do
    IO.puts "Error joining #{room}."
    {:error, %{reason: "unauthorized"}}
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    IO.puts "Connected to socket."
    IO.inspect payload
    IO.inspect socket
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (world:game).
  def handle_in("update", payload, socket) do
    IO.puts "Shouted"
    IO.inspect payload
    IO.inspect socket
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
