module Bullet exposing (Bullet, tickBullets, bulletViews)
import Mover exposing (Mover, tickMover, moverView)

import Html exposing (Html)
import Svg exposing (g)


type alias Bullet =
    Mover { friendly : Bool }


tickBullets : List Bullet -> List Bullet
tickBullets bullets =
    List.filterMap tickBullet bullets


tickBullet : Bullet -> Maybe Bullet
tickBullet bullet =
    let
        b = tickMover bullet -100 -100 1100 1100
        newBullet = if b |> isInBounds then Just b else Nothing
    in
        newBullet


isInBounds : Bullet -> Bool
isInBounds b =
    b.x >= 0 && b.y >= 0 && b.x < 1000 && b.y < 1000


bulletViews : List Bullet -> Html msg
bulletViews bullets =
    g [] (List.map bulletView bullets)


bulletView : Bullet -> Html msg
bulletView bullet =
    moverView bullet "./assets/bullet.svg" (10, 10)
