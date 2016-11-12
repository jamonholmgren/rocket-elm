module Server exposing (Socket, Msg, pushToSocket, socketInit, update, listen)

import Phoenix.Socket as PS exposing (Socket)
-- import Phoenix.Channel
import Phoenix.Push as Push
-- import Json.Decode as JD exposing ((:=))

import Json.Encode as JE

type alias Socket a = PS.Socket a
type alias Msg a = PS.Msg a

socketInit : Socket a
socketInit =
  PS.init "ws://localhost:4000/socket/websocket"

update : PS.Msg a -> PS.Socket a -> (PS.Socket a, Cmd (PS.Msg a))
update msg socket =
  PS.update msg socket

listen : PS.Socket a -> (PS.Msg a -> a) -> Sub a
listen socket msg =
  PS.listen socket msg

pushToSocket : PS.Socket PS.Msg -> String -> (PS.Socket PS.Msg, Cmd (PS.Msg PS.Msg))
pushToSocket socket payload =
  let
    payloadJSON = (JE.object [("payload", JE.string payload)])
    push = Push.init "new:msg" "rooms:world"
           |> Push.withPayload payloadJSON
  in
    PS.push push socket
