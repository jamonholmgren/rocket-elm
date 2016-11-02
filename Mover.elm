module Mover exposing (Mover, tickMover, moverView)
import Trig exposing (xDelta, yDelta, normalize)

-- Mover is a generic item that has a position, direction, speed,
-- and can accelerate or turn.
--
-- We share this code among items like Ship and Bullet so we don't
-- have to reimplement those functions many times.
--
-- I hope I'm not thinking too "OOP" here. But little games like this
-- are one of the few places where some level of OOP actually works.

import Html exposing (Html)
import Svg exposing (image)
import Svg.Attributes exposing (x, y, width, height, xlinkHref, transform)


type alias Mover a =
  { a
    | x : Float
    , y : Float
    , d : Float         -- direction
    , s : Float         -- speed
    , ts : Float        -- top speed
    , acc : Float       -- accelerating -1 0 1
    , turn : Float      -- turning -1 0 1
  }


tickMover : Mover a -> Float -> Float -> Float -> Float -> Mover a
tickMover mover x1 y1 x2 y2 =
  { mover
    | s = (accelerateMover mover)
    , d = (turnMover mover)
    , x = (moveX mover x1 x2)
    , y = (moveY mover y1 y2)
  }


moveX : Mover a -> Float -> Float -> Float
moveX {x, s, d} x1 x2 =
  clamp x1 x2 x + (xDelta s d)


moveY : Mover a -> Float -> Float -> Float
moveY {y, s, d} y1 y2 =
  clamp y1 y2 y + (yDelta s d)


accelerateMover : Mover a -> Float
accelerateMover mover =
  let
    accFactor = 0.1
    dragFactor = 0.01
  in
    clamp 1 mover.ts mover.s + (mover.acc * accFactor) - dragFactor


turnMover : Mover a -> Float
turnMover mover =
  normalize 0 360 mover.d + mover.turn * 5


moverView : Mover a -> String -> (Float, Float) -> Html msg
moverView mover img (w, h) =
  let
    angle = toString mover.d
    rx = toString (mover.x - (w / 2))
    ry = toString (mover.y - (h / 2))

    transforms =
      "rotate("
      ++ angle
      ++ " " ++ (toString mover.x)
      ++ " " ++ (toString mover.y)
      ++ ")"
  in
    image
      [ x rx
      , y ry
      , width (toString w)
      , height (toString h)
      , xlinkHref img
      , transform transforms
      ] []
