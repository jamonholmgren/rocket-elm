defmodule Rocket.WorldChannel do
  use Rocket.Web, :channel

  # def join("world:game", payload, socket) do
  #   IO.puts "Connected with #{payload}."
  #   if authorized?(payload) do
  #     {:ok, socket}
  #   else
  #     {:error, %{reason: "unauthorized"}}
  #   end
  # end

  def join(chan, payload, socket) do
    IO.puts "Connected to #{chan} with #{payload}."
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    IO.puts "Connected to #{socket} with #{payload}."
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (world:game).
  def handle_in("shout", payload, socket) do
    IO.puts "Shouted to #{socket} with #{payload}."
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
