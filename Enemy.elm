module Enemy exposing (Enemy, tickEnemies, enemyViews)
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
    , reload : Int
    , targetX : Float
    , targetY : Float
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
                 | reload = weaponCooldown(enemy.reload)
                 } -100 -100 1100 1100
                 |> targetStrategy


targetStrategy : Enemy -> Enemy
targetStrategy ({x, y, d, targetX, targetY} as enemy) =
  let
    td = targetDirection (x - targetX) (y - targetY)
    dir = turnDirection td d
    a = if dir == 0 then 1 else -1
  in
    { enemy
    | turn = dir
    , acc = a
    }

-- Cooldown a weapon from max 100 down to cooled 0.
weaponCooldown : Int -> Int
weaponCooldown r =
  clamp 0 100 (r - 1)


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
