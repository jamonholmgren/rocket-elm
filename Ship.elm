module Ship exposing (Ship, tickShip, shipView)
import Mover exposing (Mover, tickMover, moverView)

-- The Ship is the item that the user controls.
-- It's a little rocket ship that blasts around
-- the constrained game box and fires bullets wildly.

import Html exposing (Html)


-- Ship is a Mover that also has hp and a weapon cooldown reload.
type alias Ship =
    Mover
        { hp : Int
        , reload : Int
        }


-- Moves/turns the ship and cools down the weapon every tick.
tickShip : Ship -> Ship
tickShip ship =
    tickMover {ship | reload = weaponCooldown(ship.reload) } 0 0 1000 1000


-- Cooldown a weapon from max 100 down to cooled 0.
weaponCooldown : Int -> Int
weaponCooldown r =
    clamp 0 100 (r - 1)


-- Renders the ship (aka rocket) with the provided SVGs.
-- If the ship is accelerating, shows the ship rocket engines
-- burning. 40x40 px within the 1000x1000 game box.
shipView : Ship -> Html msg
shipView ship =
    let
        rocketImg = if ship.acc > 0 then
                        "./assets/rocket-burn.svg"
                    else
                        "./assets/rocket.svg"
    in
        moverView ship rocketImg (40, 40)
