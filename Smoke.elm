module Smoke exposing (Smoke, tickSmokes, smokeViews, initSmoke)

-- Smoke is a module that represents smoke clouds from rockets
-- and jets and smoke firing.

import Time exposing (Time)
import Html exposing (Html)
import Svg exposing (g, image)
import Svg.Attributes exposing (x, y, width, height, xlinkHref, opacity)


type alias Smoke =
  { x : Float
  , y : Float
  , size : Float
  , alpha : Float
  }


initSmoke : Smoke
initSmoke =
  { x = 0
  , y = 0
  , size = 10.0
  , alpha = 1.0
  }


-- Take a list of smoke items and "tick" them (make them grow and fade).
-- If a smoke item is at zero alpha, we remove it
tickSmokes : Time -> List Smoke -> List Smoke
tickSmokes diff smokes =
  List.filterMap (tickSmoke diff) smokes


-- Ticks a single smoke (grows and fades). Returns
-- Nothing if we're out of bounds.
tickSmoke : Time -> Smoke -> Maybe Smoke
tickSmoke diff smoke =
  let
    s = {smoke | alpha = smoke.alpha - 0.01, size = smoke.size + 1}
  in
    if s |> isVisible then Just s else Nothing


-- Just checks if a smoke is visible.
isVisible : Smoke -> Bool
isVisible b =
  b.alpha > 0.05


-- Renders all smokes from a list of smokes.
-- `g` is an svg "group". We're grouping all smokes in the SVG output.
-- Really just a convenience so I can return one SVG node.
smokeViews : List Smoke -> Html msg
smokeViews smokes =
  g [] (List.map smokeView smokes)


-- Renders one smoke.
smokeView : Smoke -> Html msg
smokeView smoke =
  let
    sx = toString (smoke.x - (smoke.size / 2))
    sy = toString (smoke.y - (smoke.size / 2))
    ss = toString smoke.size
    sa = toString smoke.alpha
  in
    image
      [ x sx
      , y sy
      , width ss
      , height ss
      , opacity sa
      , xlinkHref "./assets/smoke.png"
      ] []
