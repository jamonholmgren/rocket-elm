module Main exposing (..)

-- Main program for Rocket Elm, a project by
-- Jamon Holmgren (https://github.com/jamonholmgren)
-- Learning Elm by trial and fire and lots of mistakes.
-- License is MIT.

import Mover exposing (Mover)
import Ship exposing (Ship, tickShip, shipView)
import Bullet exposing (Bullet, tickBullets, bulletViews)
import Smoke exposing (Smoke, tickSmokes, smokeViews)
import Enemy exposing (Enemy, tickEnemies, enemyViews)

-- import Trig exposing (targetDistance, targetDirection, turnDirection)

import Html exposing (Html, div, p, text)
import Html.App as App
import Html.Attributes exposing (style)
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (x, y, viewBox, fill, width, height, xlinkHref)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Keyboard
import Char exposing (fromCode)
import String exposing (fromChar)
import Set exposing (Set)


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
-- Represents the whole "world" we're working with.
type alias Model =
  { ship : Ship
  , bullets : List Bullet
  , smokes : List Smoke
  , enemies : List Enemy
  , score : Int
  , keys : Set String
  }


-- Creates the initial world with default values.
init : ( Model, Cmd Msg )
init =
  let
    fastEnemy = { initEnemy | x = 900, y = 500, ts = 9 }
    mediumEnemy = { initEnemy | x = 100, y = 100, ts = 7 }
    slowEnemy = { initEnemy | x = 100, y = 500, ts = 4 }
    enemies = [ fastEnemy, mediumEnemy, slowEnemy ]
  in
  ( { ship = initShip
    , bullets = []
    , smokes = []
    , enemies = enemies
    , score = 0
    , keys = Set.empty
    }, Cmd.none )


initShip : Ship
initShip =
  { x = 500
  , y = 500
  , d = 0
  , s = 2.0
  , ts = 10.0 -- Top speed
  , acc = 0.0
  , turn = 0.0
  , hp = 100
  , reload = 0
  }

initEnemy : Enemy
initEnemy =
  { x = 100
  , y = 100
  , d = 0
  , s = 5.0
  , ts = 10.0 -- Top speed
  , acc = 0.0
  , turn = 0.0
  , hp = 10
  , reload = 0
  , targetX = 500
  , targetY = 500
  }

-- UPDATE


-- We respond to keyboard events and tick every n milliseconds.
type Msg
  = Frame Time
  | AITick Time
  | KeyDownMsg Keyboard.KeyCode
  | KeyUpMsg Keyboard.KeyCode


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
        -- Then "tick" the ship forward, turn it, etc.
        s = { ship |
                 acc = (accKey keys)
               , turn = (turnKey keys)
               } |> tickShip diff

        -- Same with bullets
        b = tickBullets diff bullets

        newSmokes = smokes |> tickSmokes diff

        -- Tick all the enemy ships forward too
        -- and retarget them to this ship
        newEnemies = tickEnemies diff enemies

        -- And lastly, we want to add another bullet to the list if the ship
        -- is ready to fire and we're holding down "J".
        -- This also results in the ship's reloader engaging which means we
        -- have to update the ship again.
        -- This makes me feel slightly uneasy, updating the ship multiple times.
        -- Wonder if there's a more logical way?
        (newBullets, newShip) = (fireBullet keys b s)
      in
        -- New model with updated ship and list of bullets.
        ({ model
            | ship = newShip
            , bullets = newBullets
            , smokes = newSmokes
            , enemies = newEnemies
        }, Cmd.none)

    AITick _ ->
      let
        -- Retarget toward our ship
        newEnemies =  List.map (\e ->
                        { e
                        | targetX = ship.x
                        , targetY = ship.y
                        }
                      ) enemies

        -- Add smoke if accelerating, either our ship or the enemies
        newSmokes = smokes ++ (List.filterMap addSmoke newEnemies) ++ (List.filterMap addSmoke [ship])

      in
        -- New model
        ({ model
            | smokes = newSmokes
            , enemies = newEnemies
        }, Cmd.none)


    KeyDownMsg k ->
      -- Only thing we do here is add a key to our set of current keys.
      ({ model | keys = (addKey k keys) }, Cmd.none)

    KeyUpMsg k ->
      -- Only thing we do here is remove a key from our set of current keys.
      ({ model | keys = (removeKey k keys) }, Cmd.none)


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


-- Checks if J is held and fires a bullet. Also updates the ship to
-- include a reloading weapon if the bullet is fired.
fireBullet : Set String -> List Bullet -> Ship -> (List Bullet, Ship)
fireBullet keys bullets ship =
  if (ship.reload == 0 && keys ?? "J") then
    let
      newBullets =
        { x = ship.x
        , y = ship.y
        , d = ship.d
        , s = 15
        , ts = 15
        , acc = 0
        , turn = 0
        , friendly = True
        } :: bullets
    in
      ( newBullets, {ship | reload = 10} )
  else
    (bullets, ship)

addSmoke : Mover a -> Maybe Smoke
addSmoke {acc, x, y} =
  if acc > 0 then
    Just  { x = x
          , y = y
          , size = 10.0
          , alpha = 0.75
          }
  else
    Nothing


-- SUBSCRIPTIONS

-- We subscribe to three types of events. One is a time tick of every 30 ms.
-- The other two are keyboard -- keys pushed and keys released.
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ AnimationFrame.diffs Frame
    , Time.every (100 * millisecond) AITick
    , Keyboard.downs KeyDownMsg
    , Keyboard.ups KeyUpMsg
    ]


-- VIEW


-- Render a div with the game box and debug info.
view : Model -> Html Msg
view model =
  div [ width "1000px", height "1000px", style [("margin", "50px 50px"), ("text-align", "center")] ]
    [ gameView model
    , debugView model
    ]


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
    -- , p [] [ text (toString model) ]
    ]


-- Background is just a big blue box.
backgroundView : Html msg
backgroundView =
  rect [ x "0", y "0", width "1000", height "1000", fill "#0B79CE" ] []
