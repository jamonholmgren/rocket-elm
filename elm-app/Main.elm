module Main exposing (..)

-- Main program for Rocket Elm, a project by
-- Jamon Holmgren (https://github.com/jamonholmgren)
-- Learning Elm by trial and fire and lots of mistakes.
-- License is MIT.

import Mover exposing (Mover, MoverUpdate)
import Ship exposing (Ship, tickShip, shipView, initShip)
import Bullet exposing (Bullet, tickBullets, bulletViews, initBullet)
import Smoke exposing (Smoke, tickSmokes, smokeViews, initSmoke)
import Enemy exposing (Enemy, tickEnemies, enemyAI, enemyViews, initEnemy)
import Collision exposing (collisionDetection)

-- Multiplayer support
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel
import Phoenix.Push as Push
import Json.Encode as JE
import Regex exposing (HowMany, regex)
import Array exposing (Array)

-- Other modules
import Html exposing (Html, div, p, text, a, button)
import Html.Attributes exposing (style, href, target)
import Html.Events exposing (onClick)
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (x, y, viewBox, fill, width, height, xlinkHref)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Keyboard
import Char exposing (fromCode)
import String exposing (fromChar)
import Set exposing (Set)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
-- Represents the whole "world" we're working with.
type alias Model =
  { game : Maybe String
  , id : Maybe Int
  , ship : Ship
  , bullets : List Bullet
  , smokes : List Smoke
  , enemies : List Enemy
  , score : Int
  , keys : Set String
  , socket : Socket.Socket Msg
  , connected : Bool
  }

type Msg
  = Frame Time
  | AITick Time
  | KeyDownMsg Keyboard.KeyCode
  | KeyUpMsg Keyboard.KeyCode
  | JoinGame
  | JoinedGame JE.Value
  | LeftGame JE.Value
  | PhoenixMsg (Socket.Msg Msg)
  | ReceiveWorldUpdate JE.Value
  | ReceiveUserEntered JE.Value
  | SocketResponse JE.Value
  | SocketError JE.Value

-- Creates the initial world with default values.
init : ( Model, Cmd Msg )
init =
  let
    fastEnemy = { initEnemy | x = 900, y = 500, ts = 2 }
    mediumEnemy = { initEnemy | x = 100, y = 100, ts = 2 }
    slowEnemy = { initEnemy | x = 100, y = 500, ts = 2 }
    insaneEnemy = { initEnemy | x = 500, y = 900, ts = 2 }
    enemies = [] -- [ fastEnemy, mediumEnemy, slowEnemy, insaneEnemy ]
  in
    ( { game = Nothing
      , id = Nothing
      , ship = initShip
      , bullets = []
      , smokes = []
      , enemies = enemies
      , score = 0
      , keys = Set.empty
      , socket = socketInit
      , connected = False
      }, Cmd.none)

-- UPDATE

-- Our all-powerful update function.
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ ship, bullets, smokes, keys, enemies } as model) =
  case msg of
    Frame timeDiff ->
      let
        -- Normalize the time diff based on normal FPS vs current FPS
        diff = timeDiff / 30

        -- Update the ship to properly represent current user commands.
        -- For example, if we're holding "W", then we want ship.acc = 1.0
        -- Or if we're holding "A", we want ship.turn = -1.0
        newShip = { ship
                  | acc = (accKey keys)
                  , turn = (turnKey keys)
                  , firing = (firingKey keys)
                  }

        newBullets = bullets
                     ++ (fireBullets ship)
                     ++ ((List.concatMap fireBulletsEnemy) enemies)
      in
        -- New model with updated ship and list of bullets.
        -- All of the "ticks" happen as we construct this final
        -- model record.
        ( { model
          | ship =    (tickShip diff newShip)
          , bullets = (tickBullets diff newBullets)
          , smokes =  (tickSmokes diff smokes)
          , enemies = (tickEnemies diff enemies)
          } |> collisionDetection
        , Cmd.none)

    AITick _ ->
      let
        -- Retarget toward our ship
        newEnemies =  List.map (enemyAI ship) enemies

        -- Add smoke if low HP, either our ship or the enemies
        newSmokes = smokes ++ (List.filterMap addSmoke newEnemies) ++ (List.filterMap addSmoke [ship])

        -- Push update to server
        (socket, phxMsg) = updateServer model
        -- a = Debug.log "socket" model.socket
      in
        -- New model
        ( { model
          | smokes = newSmokes
          , enemies = newEnemies
          , socket = socket
          }, Cmd.map PhoenixMsg phxMsg)

    KeyDownMsg k ->
      -- Only thing we do here is add a key to our set of current keys.
      ({ model | keys = (addKey k keys) }, Cmd.none)

    KeyUpMsg k ->
      -- Only thing we do here is remove a key from our set of current keys.
      ({ model | keys = (removeKey k keys) }, Cmd.none)

    JoinedGame game ->
      ({model | connected = True}, Cmd.none)

    LeftGame game ->
      ({model | connected = False}, Cmd.none)

    SocketResponse wat ->
      -- let
      --   a = Debug.log "SocketResponse" wat
      -- in
        (model, Cmd.none)

    SocketError wat ->
      -- let
        -- a = Debug.log "SocketError" wat
      -- in
        (model, Cmd.none)

    PhoenixMsg msg ->
      let
        -- a = Debug.log "PhoenixMsg" msg
        ( socket, phxCmd ) = Socket.update msg model.socket
      in
        ( { model | socket = socket }
        , Cmd.map PhoenixMsg phxCmd
        )


    JoinGame ->
      let
        game = Debug.log "topic" "world:game"
        channel = Channel.init game
                  |> Channel.onJoin JoinedGame
                  |> Channel.onClose LeftGame
        ( socket, phxCmd ) = Socket.join channel model.socket
      in
        ( { model | socket = socket }
        , Cmd.map PhoenixMsg phxCmd
        )

    ReceiveWorldUpdate json ->
      let
        a = Debug.log "Received world update" json
      in
        ((handleServerUpdate model (extractPayload (toString json))), Cmd.none)

    ReceiveUserEntered json ->
      let
        userId = Just (extractUserId (toString json))
      in
        if model.id == Nothing then
          -- This is me! Yay!
          ({model | id = userId}, Cmd.none)
        else
          (model, Cmd.none)


-- Check if a key is being pressed
-- Usage: if keys ?? "D" then
(??) : Set String -> String -> Bool
(??) keys k =
  Set.member k keys
infixr 9 ??


-- Adds a pressed key to our keys set
addKey : Int -> Set String -> Set String
addKey k keys =
  Set.insert (fromChar <| fromCode <| k) keys


-- Removes a released key from our keys set
removeKey : Int -> Set String -> Set String
removeKey k keys =
  Set.remove (fromChar <| fromCode <| k) keys


-- Checks if W/S are held and sets the ship acceleration to -1, 1, or 0.
accKey : Set String -> Float
accKey keys =
  if keys ?? "W" then 1.0
  else if keys ?? "S" then -1.0
  else 0.0


-- Checks if A/D are held and sets the ship turn to -1, 1, or 0.
turnKey : Set String -> Float
turnKey keys =
  if keys ?? "A" then -1.0
  else if keys ?? "D" then 1.0
  else 0.0


-- Checks if J is held and sets the ship `firing` to True.
firingKey : Set String -> Bool
firingKey keys =
  keys ?? "J"


-- Checks if ship is firing and weapon cooldown has
-- happened and creates one or more bullets if so.
fireBullets : Ship -> List Bullet
fireBullets ship =
  if (ship.firing && ship.cooldown == 0) then
    [ { initBullet
      | x = ship.x
      , y = ship.y
      , d = ship.d
      , friendly = True
    } ]
  else
    []

fireBulletsEnemy : Enemy -> List Bullet
fireBulletsEnemy enemy =
  if (enemy.firing && enemy.cooldown == 0) then
    [ { initBullet
      | x = enemy.x
      , y = enemy.y
      , d = enemy.d
      , friendly = False
    } ]
  else
    []

addSmoke : Mover a -> Maybe Smoke
addSmoke {hp, x, y} =
  if hp <= 5 then
    Just  { initSmoke | x = x, y = y }
  else
    Nothing


-- SUBSCRIPTIONS

-- We subscribe to a few different types of events.
-- AnimationFrame pushes a tick every frame for drawing
-- The AITick is for updating things that don't need to happen as often, like AI
-- intentions.
-- We listen to a socket connection so we can communicate to the server.
-- The other two are keyboard -- keys pushed and keys released.
-- Before we launch the game, our only subscription is to the socket connection.
subscriptions : Model -> Sub Msg
subscriptions model =
  if model.connected then
    Sub.batch
      [ AnimationFrame.diffs Frame
      , Time.every (100 * millisecond) AITick
      , Keyboard.downs KeyDownMsg
      , Keyboard.ups KeyUpMsg
      , Socket.listen model.socket PhoenixMsg
      ]
  else
    Socket.listen model.socket PhoenixMsg


-- Multiplayer

socketInit : Socket Msg
socketInit =
  "ws://localhost:4000/socket/websocket"
  |> Socket.init
  |> Socket.on "update" "world:game" ReceiveWorldUpdate
  |> Socket.on "user:entered" "world:game" ReceiveUserEntered


updateServer : Model -> (Socket.Socket Msg, Cmd (Socket.Msg Msg))
updateServer ({ship} as model) =
  let
    -- payload will include:
    -- time: time payload was generated, for fast-forwarding reasons
    -- ship: updates on my ship position
    -- nb: any new bullets that have been created recently
    -- ib: any of my bullets that have impacted something recently (and what they hit)
    -- Everything else will be handled client-side!
    shipUpdate = [ ship.x
                 , ship.y
                 , ship.d
                 , toFloat ship.hp
                 , ship.acc
                 , ship.turn
                 ]
                 |> List.map round
                 |> List.map toString
                 |> String.join ","

    clientId = (toString (Maybe.withDefault 0 model.id))

    payload = clientId ++ "," ++ shipUpdate
            -- ++ "nb:ID,x,y,d,s;nb:ID,x,y,d,s;nb:ID:x,y,d,s;ib:ID,x,y,shipID"
  in
    pushToSocket model.socket payload

handleServerUpdate : Model -> MoverUpdate -> Model
handleServerUpdate model payload =
  let
    -- update single ships by id, including HP
    -- add new bullets
    -- impact bullets, reduce HP of other ships accordingly
    -- tick everything forward by time delta since payload was generated

    -- We are now getting

    a = Debug.log "Server pushed update" payload
    newEnemies =
      if model.id == Just payload.id then
        -- My own update, no change
        model.enemies
      else
        -- Check if there is no enemy with that ID first
        if List.any (\en -> en.id == Just payload.id) model.enemies then
          -- Enemy update, let's update it
          model.enemies
          |> List.map (\en ->
            if en.id == Just payload.id then
              { en
              | x = payload.x
              , y = payload.y
              , d = payload.d
              , hp = payload.hp
              , acc = payload.acc
              , turn = payload.turn
              }
            else
              en
          )
        else
          -- New enemy sighting!
          { initEnemy
          | id = Just payload.id
          , x = payload.x
          , y = payload.y
          , d = payload.d
          , hp = payload.hp
          , acc = payload.acc
          , turn = payload.turn
          } :: model.enemies
  in
    { model
    | enemies = newEnemies
    }

-- Okay, this is very hacky. I really just need the ID from this JSON response
-- and apparently the way it's supposed to happen is I have to write 25 lines of
-- decoder logic. Until I actually find the time to figure that out, I'm just
-- going to coerce it into a string and then pull the int out manually.
-- ¯\_(ツ)_/¯
-- "{ user = 83531997, payload = {} }"
extractUserId : String -> Int
extractUserId str =
  str
  |> Regex.replace Regex.All (regex "{ user = ") (\_ -> "")
  |> String.split ","
  |> List.head    |> Maybe.withDefault "0" -- Have to do this to resolve potential errors
  |> String.toInt |> Result.withDefault 0

extractPayload : String -> MoverUpdate
extractPayload str =
  let
    data = str
           |> Regex.replace Regex.All (regex "{ payload = \"") (\_ -> "")
           |> Regex.replace Regex.All (regex "\" }") (\_ -> "")
           |> String.split ","
           |> List.map String.toFloat
           |> List.map (Result.withDefault 0.0)
           |> Array.fromList
  in
    { id = round (item 0 data)
    , x = item 1 data
    , y = item 2 data
    , d = item 3 data
    , hp = round (item 4 data)
    , acc = item 5 data
    , turn = item 6 data
    }

item : Int -> Array Float -> Float
item index data =
  data
  |> Array.get index
  |> Maybe.withDefault -1.0


pushToSocket : Socket.Socket Msg -> String -> (Socket.Socket Msg, Cmd (Socket.Msg Msg))
pushToSocket socket payload =
  let
    payloadJSON = (JE.object [("payload", JE.string payload)])
    cargo = Push.init "update" "world:game"
            |> Push.withPayload payloadJSON
            |> Push.onOk SocketResponse
            |> Push.onError SocketError
  in
    Socket.push cargo socket


-- VIEW


-- Render a div with the game box and debug info.
view : Model -> Html Msg
view model =
  let
    mainView = if model.connected then
                 gameView model
               else
                 button [onClick JoinGame] [ text "Join" ]

    views = [ mainView, debugView model ]
  in
  div [ width "1000px", height "1000px", style [("margin", "50px 50px"), ("text-align", "center")] ]
    views


-- Game box, built out of SVG nodes.
gameView : Model -> Html msg
gameView model =
  svg [ viewBox "0 0 1000 1000", width "600px" ]
    [ backgroundView
    , bulletViews model.bullets
    , smokeViews model.smokes
    , shipView model.ship
    , enemyViews model.enemies
    ]


-- Show the current model for debugging and some help text.
debugView : Model -> Html msg
debugView model =
  div []
    [ p [] [ text "WASD to fly, J to fire bullets" ]
    , a [ href "https://github.com/jamonholmgren/rocket-elm", target "_blank" ] [ text "Github Source" ]
    -- , p [] [ text (toString model) ]
    ]


-- Background is just a big blue box.
backgroundView : Html msg
backgroundView =
  rect [ x "0", y "0", width "1000", height "1000", fill "#0B79CE" ] []
