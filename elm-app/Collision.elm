module Collision exposing (collisionDetection)

import Ship exposing (Ship)
import Bullet exposing (Bullet)
import Enemy exposing (Enemy)

import Collision2D exposing (axisAlignedBoundingBox, rectangle)

type alias Model a =
  { a
  | ship : Ship
  , bullets : List Bullet
  , enemies : List Enemy
  }

collisionDetection : Model a -> Model a
collisionDetection ({ship, bullets, enemies} as model) =
  let
    newShip = ship
    (newEnemies, newBullets) = checkCollision enemies bullets
  in
    { model
    | ship = newShip
    , bullets = newBullets
    , enemies = newEnemies
    }

collided : Bullet -> Ship -> Bool
collided bullet ship = --({x, y, size}) ({x as bx, y as by, size as bsize}) =
  let
    s = ship.size / 2
    bs = bullet.size / 2
    shipRect = rectangle (ship.x - s) (ship.y - s) ship.size ship.size
    bulletRect = rectangle (bullet.x - bs) (bullet.y - bs) bullet.size bullet.size
  in
    axisAlignedBoundingBox shipRect bulletRect

notCollided : Bullet -> Ship -> Bool
notCollided bullet ship =
  not (collided bullet ship)

checkCollision : List Enemy -> List Bullet -> (List Enemy, List Bullet)
checkCollision enemies bullets =
  let
    newBullets = List.filter (\b -> List.all (notCollided b) enemies) bullets
  in
    (enemies, newBullets)
