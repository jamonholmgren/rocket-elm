module Ship exposing (Ship, tickShip, shipView)
import Mover exposing (tickMover, moverView)

import Html exposing (Html)

type alias Mover a =
    { a |
      x : Float
    , y : Float
    , d : Float         -- direction
    , s : Float         -- speed
    , acc : Float       -- accelerating -1 0 1
    , turn : Float      -- turning -1 0 1
    }


type alias Ship =
    Mover { hp : Int }


tickShip : Ship -> Ship
tickShip ship =
    tickMover ship 0 0 1000 1000


shipView : Ship -> Html msg
shipView ship =
    let
        rocketImg = if ship.acc > 0 then
                        "./assets/rocket-burn.svg"
                    else
                        "./assets/rocket.svg"
    in
        moverView ship rocketImg (40, 40)
