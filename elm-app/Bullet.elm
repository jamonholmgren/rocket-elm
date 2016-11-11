module Bullet exposing (Bullet, tickBullets, bulletViews, initBullet)
import Mover exposing (Mover, tickMover, moverView)

-- Bullet is a module that represents the many bullets flying
-- around the game box. These can be fired by the player or at
-- the player. Bullets are tracked in a giant list and updated
-- via a List.filterMap.

import Time exposing (Time)
import Html exposing (Html)
import Svg exposing (g)


-- Bullet is a Mover that also has a "friendly" flag.
-- If a bullet is "friendly", it won't hit you.
type alias Bullet =
  Mover { friendly : Bool, impacted : Bool }


initBullet : Bullet
initBullet =
  { x = 0
  , y = 0
  , d = 0
  , s = 20
  , ts = 20
  , size = 1
  , acc = 0
  , turn = 0
  , friendly = True
  , impacted = False
  }

-- Take a list of bullets and "tick" them (make them move
-- forward, etc). If a bullet is out of bounds, we remove it
-- from the list of bullets.
tickBullets : Time -> List Bullet -> List Bullet
tickBullets diff bullets =
  List.filterMap (tickBullet diff) bullets


-- Ticks a single bullet (moves it forward, etc). Returns
-- Nothing if we're out of bounds.
tickBullet : Time -> Bullet -> Maybe Bullet
tickBullet diff bullet =
  let
    b = tickMover diff bullet -100 -100 1100 1100
  in
    if b |> isInBounds then Just b else Nothing


-- Just checks if a bullet is in bounds. I should probably
-- not hardcode these values.
isInBounds : Bullet -> Bool
isInBounds b =
  b.x >= -100 && b.y >= -100 && b.x < 1100 && b.y < 1100


-- Renders all bullets from a list of bullets.
-- `g` is an svg "group". We're grouping all bullets in the SVG output.
-- Really just a convenience so I can return one SVG node.
bulletViews : List Bullet -> Html msg
bulletViews bullets =
  g [] (List.map bulletView bullets)


-- Renders one bullet.
bulletView : Bullet -> Html msg
bulletView bullet =
  moverView bullet "./assets/bullet.svg" (10, 10)
