module Bullet exposing (Bullet, tickBullet, bulletView)
import Mover exposing (Mover, tickMover, moverView)

import Html exposing (Html)


type alias Bullet =
    Mover {}

Bounds = ((-100, 1100), (-100, 1100))


tickBullets : List Bullet -> List Bullet
tickBullets bullets =
    List.filterMap isInBounds tickBullet bullets


tickBullet : Bullet -> Bullet
tickBullet bullet =
    n = tickMover bullet Bounds


isInBounds : Bullet -> Bool
isInBounds b =
    b.x >= 0 && b.y >= 0 && b.x < 1000 && b.y < 1000


bulletView : Bullet -> Html msg
bulletView bullet =
    let
        rocketImg = if bullet.acc > 0 then
                        "./assets/rocket-burn.svg"
                    else
                        "./assets/rocket.svg"
    in
        moverView bullet rocketImg (40, 40)
