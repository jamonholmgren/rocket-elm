module Trig exposing
  ( xDelta
  , yDelta
  , targetDirection
  , targetDistance
  , turnDirection
  , facing
  , normalize
  )

-- Module for calculating various useful values for the game.


-- Given a speed and direction, calculates the x delta
-- which is basically how much the x increases or decreases
-- Note that the direction is rotated 90 degrees.
xDelta : Float -> Float -> Float
xDelta s d =
  s * (cos <| degrees <| d - 90)

-- Given a speed and direction, calculates the y delta
-- which is basically how much the y increases or decreases
-- Note that the direction is rotated 90 degrees.
yDelta : Float -> Float -> Float
yDelta s d =
  s * (sin <| degrees <| d - 90)

-- Given a target x and target y, calculates the direction
-- toward that target from the origin. Returns "turns" (0-1.0)
targetDirection : Float -> Float -> Float
targetDirection x y =
  (atan2 x y) / pi

-- Given a target direction and my direction, return the best
-- direction to turn to point toward the target eventually.
-- Will return 0 if we're within 0.01 turns (3.6 degrees).
turnDirection : Float -> Float -> Float
turnDirection td d =
  if facing td d 0.01 then 0
  else if (td - d + 1.0) > 0.5 then 1
  else -1

-- Given a target direction and my direction and a tolerance,
-- are we pointed the right direction yet?
facing : Float -> Float -> Float -> Bool
facing td d tolerance =
  (abs ((td + 1) - (d + 1))) <= tolerance

-- Given a target x and y, calculates the distance toward
-- that target from the origin.
targetDistance : Float -> Float -> Float
targetDistance x y =
  sqrt ((x * x) + (y * y))

-- Given a low and high bounds and a current value, will
-- "wrap" to stay within the bounds.
normalize : Float -> Float -> Float -> Float
normalize low high curr =
  if curr < low then curr + (high - low)
  else if curr >= high then curr - (high - low)
  else curr
