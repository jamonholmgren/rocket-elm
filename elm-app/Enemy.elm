module Enemy exposing (Enemy, tickEnemies, enemyAI, enemyViews, initEnemy)
import Mover exposing (Mover, tickMover, moverView)
import Trig exposing (targetDirection, turnDirection)

-- Enemy ships fly through the game box and try to
-- kill the rocket ship.
-- They start spawning partway into the game.

import Time exposing (Time)
import Html exposing (Html)
import Svg exposing (g)


-- Enemy is a Mover that also has hp and a weapon cooldown reload.
type alias Enemy =
  Mover
    { hp : Int
    , firing : Bool
    , cooldown : Int
    , cooldownMax : Int
    }


initEnemy : Enemy
initEnemy =
  { x = 100
  , y = 100
  , d = 0
  , s = 5.0
  , ts = 10.0 -- Top speed
  , size = 20
  , acc = 0.0
  , turn = 0.0
  , hp = 10
  , firing = False
  , cooldown = 0
  , cooldownMax = 12
  }

-- Take a list of enemies and "tick" them (make them move
-- forward, etc). If a enemy is out of bounds, we remove it
-- from the list of enemies.
tickEnemies : Time -> List Enemy -> List Enemy
tickEnemies diff enemies =
  -- TODO: Change to filterMap when we have death detection
  List.map (tickEnemy diff) enemies


-- Moves/turns the enemy and cools down the weapon every tick.
tickEnemy : Time -> Enemy -> Enemy
tickEnemy diff enemy =
  tickMover diff { enemy
                 | cooldown = (weaponCooldown enemy)
                 } -100 -100 1100 1100


enemyAI : { b | x : Float, y : Float } -> Enemy -> Enemy
enemyAI ({x, y}) e =
  let
    td = targetDirection (e.x - x) (e.y - y)
    dir = turnDirection td e.d
    a = if dir == 0 then 1 else -0.25
    f = if dir == 0 then True else False
  in
    { e
    | turn = dir
    , acc = a
    , firing = f
    }

-- Cooldown a weapon from cooldownMax down to cooled 0.
weaponCooldown : Enemy -> Int
weaponCooldown {cooldown, cooldownMax, firing} =
  if cooldown == 0 && firing then
    cooldownMax
  else
    clamp 0 cooldownMax (cooldown - 1)


-- Renders all enemies from a list of enemies.
-- `g` is an svg "group". We're grouping all enemies in the SVG output.
-- Really just a convenience so I can return one SVG node.
enemyViews : List Enemy -> Html msg
enemyViews enemies =
  g [] (List.map enemyView enemies)


-- Renders the enemy with the provided SVGs.
-- If the enemy is accelerating, shows the enemy rocket engines
-- burning. 40x40 px within the 1000x1000 game box.
enemyView : Enemy -> Html msg
enemyView enemy =
  let
    enemyImg = if enemy.acc > 0 then
                  "./assets/rocket-3-burn.svg"
                else
                  "./assets/rocket-3.svg"
  in
    moverView enemy enemyImg (40, 40)
