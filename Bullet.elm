module Bullet exposing (Bullet, tickBullets, bulletViews)
import Mover exposing (Mover, tickMover, moverView)


import Html exposing (Html)
import Svg exposing (g)


-- Bullet is a Mover that also has a "friendly" flag.
-- If a bullet is "friendly", it won't hit you.
type alias Bullet =
    Mover { friendly : Bool }


-- Take a list of bullets and "tick" them (make them move
-- forward, etc). If a bullet is out of bounds, we remove it
-- from the list of bullets.
tickBullets : List Bullet -> List Bullet
tickBullets bullets =
    List.filterMap tickBullet bullets


-- Ticks a single bullet (moves it forward, etc). Returns
-- Nothing if we're out of bounds.
tickBullet : Bullet -> Maybe Bullet
tickBullet bullet =
    let
        b = tickMover bullet -100 -100 1100 1100
    in
        if b |> isInBounds then Just b else Nothing


-- Just checks if a bullet is in bounds. I should probably
-- not hardcode these values.
isInBounds : Bullet -> Bool
isInBounds b =
    b.x >= 0 && b.y >= 0 && b.x < 1000 && b.y < 1000


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
