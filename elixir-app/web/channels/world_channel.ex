defmodule Rocket.WorldChannel do
  use Rocket.Web, :channel

  def join("world:game", payload, socket) do
    IO.puts "Connected to world:game."
    # IO.inspect payload
    # IO.inspect socket
    # IO.inspect socket.channel_pid
    if authorized?(payload) do
      send(self, {:after_join, payload})
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  def handle_info({:after_join, payload}, socket) do
    # IO.puts "After-join world:game."
    client_id = :rand.uniform(100_000_000)
    push socket, "user:joined", %{id: client_id}
    broadcast! socket, "user:entered", %{user: client_id, payload: payload}
    # IO.inspect msg
    # IO.inspect socket
    # IO.inspect socket.channel_pid
    {:noreply, socket}
  end

  def join(room, _params, _socket) do
    # IO.puts "Error joining #{room}."
    {:error, %{reason: "unauthorized"}}
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    # IO.puts "Connected to socket."
    # IO.inspect payload
    # IO.inspect socket
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (world:game).
  def handle_in("update", payload, socket) do
    # IO.puts "Shouted"
    # IO.inspect payload
    # IO.inspect socket
    broadcast socket, "update", payload
    {:noreply, socket}
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
