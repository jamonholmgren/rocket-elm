module Mover exposing (Mover, tickMover, moverView)
import Trig exposing (xDelta, yDelta, normalize)

-- Mover is a generic item that has a position, direction, speed,
-- and can accelerate or turn.
--
-- We share this code among items like Ship and Bullet so we don't
-- have to reimplement those functions many times.

import Html exposing (Html)
import Svg exposing (image)
import Svg.Attributes exposing (x, y, width, height, xlinkHref, transform)


type alias Mover a =
  { a
  | x : Float
  , y : Float
  , d : Float         -- direction
  , s : Float         -- speed
  , size : Float      -- size for hitbox
  , hp : Int          -- hit points
  , ts : Float        -- top speed
  , acc : Float       -- accelerating -1 0 1
  , turn : Float      -- turning -1 0 1
  }


tickMover : Float -> Mover a -> Float -> Float -> Float -> Float -> Mover a
tickMover timeDiff mover x1 y1 x2 y2 =
  { mover
  | s = (accelerateMover timeDiff mover)
  , d = (turnMover timeDiff mover)
  , x = (moveX timeDiff mover x1 x2)
  , y = (moveY timeDiff mover y1 y2)
  }


moveX : Float -> Mover a -> Float -> Float -> Float
moveX timeDiff {x, s, d} x1 x2 =
  clamp x1 x2 x + (xDelta timeDiff s d)


moveY : Float -> Mover a -> Float -> Float -> Float
moveY timeDiff {y, s, d} y1 y2 =
  clamp y1 y2 y + (yDelta timeDiff s d)


accelerateMover : Float -> Mover a -> Float
accelerateMover timeDiff mover =
  let
    accFactor = 0.1
    dragFactor = 0.01
  in
    clamp 2 mover.ts mover.s + (mover.acc * accFactor) - dragFactor


turnMover : Float -> Mover a -> Float
turnMover timeDiff mover =
  normalize 0 360 mover.d + mover.turn * (5 * timeDiff)


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
