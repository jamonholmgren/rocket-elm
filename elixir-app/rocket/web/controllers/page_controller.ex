defmodule Rocket.PageController do
  use Rocket.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
