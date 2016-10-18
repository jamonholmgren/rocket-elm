module Ship exposing (Ship, tickShip, shipView)
import Mover exposing (Mover, tickMover, moverView)

import Html exposing (Html)


type alias Ship =
    Mover
        { hp : Int
        , reload : Int
        }


tickShip : Ship -> Ship
tickShip ship =
    tickMover {ship | reload = clamp 0 100 (ship.reload - 1) } 0 0 1000 1000


shipView : Ship -> Html msg
shipView ship =
    let
        rocketImg = if ship.acc > 0 then
                        "./assets/rocket-burn.svg"
                    else
                        "./assets/rocket.svg"
    in
        moverView ship rocketImg (40, 40)
