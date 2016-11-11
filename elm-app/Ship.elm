module Ship exposing (Ship, tickShip, shipView, initShip)
import Mover exposing (Mover, tickMover, moverView)

-- The Ship is the item that the user controls.
-- It's a little rocket ship that blasts around
-- the constrained game box and fires bullets wildly.

import Time exposing (Time)
import Html exposing (Html)


-- Ship is a Mover that also has hp and a weapon cooldown cooldown.
type alias Ship =
  Mover
    { hp : Int
    , cooldown : Int
    , cooldownMax : Int
    , firing : Bool
    }


initShip : Ship
initShip =
  { x = 500
  , y = 500
  , d = 0
  , s = 2.0
  , size = 40
  , ts = 10.0 -- Top speed
  , acc = 0.0
  , turn = 0.0
  , hp = 100
  , cooldown = 0
  , cooldownMax = 6
  , firing = False
  }

-- Moves/turns the ship and cools down the weapon every tick.
tickShip : Time -> Ship -> Ship
tickShip diff ship =
  tickMover diff { ship | cooldown = (weaponCooldown ship) } 20 20 980 980


-- Cooldown a weapon from cooldownMax down to cooled 0.
weaponCooldown : Ship -> Int
weaponCooldown {cooldown, cooldownMax, firing} =
  if cooldown == 0 && firing then
    cooldownMax
  else
    clamp 0 cooldownMax (cooldown - 1)


-- Renders the ship (aka rocket) with the provided SVGs.
-- If the ship is accelerating, shows the ship rocket engines
-- burning. 40x40 px within the 1000x1000 game box.
shipView : Ship -> Html msg
shipView ship =
  let
    rocketImg = if ship.acc > 0 then
                  "./assets/rocket-4-burn.png"
                else
                  "./assets/rocket-4.png"
  in
    moverView ship rocketImg (40, 40)
