module Main exposing (..)

import Time exposing (Time, inSeconds)
import Signal exposing (Signal)


type alias Input =
    { space : Bool
    , paddle1 : Int
    , paddle2 : Int
    , delta : Time
    }


delta : Signal Time
delta =
    Signal.map inSeconds (fps 35)


input : Signal Input
input =
    Signal.sampleOn delta <|
        Signal.map4 Input
            Keyboard.space
            (Signal.map .y Keyboard.wasd)
            (Signal.map .y Keyboard.arrows)
            delta


( gameWidth, gameHeight ) =
    ( 600, 400 )
( halfWidth, halfHeight ) =
    ( gameWidth / 2, gameHeight / 2 )
( paddleSizeX, paddleSizeY ) =
    ( 8, 20 )



-- Custom object


type alias Object custom =
    { custom
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
    }


type alias Ball =
    Object {}


type alias Player =
    Object { score : Int }


type State
    = Play
    | Pause


type alias Game =
    { state : State
    , ball : Ball
    , player1 : Player
    , player2 : Player
    }


player : Float -> Player
player x =
    { x = x, y = 0, vx = 0, vy = 0, score = 0 }


defaultGame : Game
defaultGame =
    { state = Pause
    , ball = { x = 0, y = 0, vx = 200, vy = 200 }
    , player1 = player (20 - halfWidth)
    , player2 = player (halfwidth - 20)
    }



-- are n and m within c of each other?


near : Float -> Float -> Float -> Bool
near n c m =
    m >= n - c && m <= n + c


within : Ball -> Player -> Bool
within b p =
    near b.x paddleSizeX p.x
        && near b.y paddleSizeY p.y


stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v
    else if upperCollision then
        -(abs v)
    else
        v


stepObj : Time -> Object a -> Object b
stepObj t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + (vx * t)
        , y = y + (vy * t)
    }


stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({ x, y, vx, vy } as ball) player1 player2 =
    if not (ball.x |> near 0 halfWidth) then
        { ball | x = 0, y = 0 }
    else
        stepObj t
            { ball
                | vx =
                    stepV vx (ball `within` player1) (ball `within` player2)
                , vy =
                    stepV vy (y < 7 - halfHeight) (y > halfHeight - 7)
            }



-- step a player forward, making sure it does not fly off the court


stepPlyr : Time -> Int -> Int -> Player -> Player
stepPlyr t dir points player =
    let
        player' =
            stepObj t { player | vy = toFloat dir * 200 }

        y' =
            clamp (22 - halfHeight) (halfHeight - 22) player'.y

        score' =
            player.score + points
    in
        { player' | y = y', score = score' }


stepGame : Input -> Game -> Game
stepGame input game =
    let
        { space, paddle1, paddle2, delta } =
            input

        { state, ball, player1, player2 } =
            game

        score1 =
            if ball.x > halfWidth then
                1
            else
                0

        score2 =
            if ball.x < -halfWidth then
                1
            else
                0

        state' =
            if space then
                Play
            else if score1 /= score2 then
                Pause
            else
                state

        ball' =
            if state == Pause then
                ball
            else
                stepBall delta ball player1 player2

        player1' =
            stepPlyr delta paddle1 score1 player1

        player2' =
            stepPlyr delta paddle2 score2 player2
    in
        { game
            | state = state'
            , ball = ball'
            , player1 = player1'
            , player2 = player2'
        }


gameState : Signal Game
gameState =
    Signal.foldp stepGame defaultGame input



-- helper values


pongGreen =
    rgb 60 100 60


textGreen =
    rgb 160 200 160


txt f =
    leftAligned << f << monospace << Text.color textGreen << fromString


msg =
    "SPACE to start, WS and &uarr;&darr; to move"



-- shared function for rendering objects


displayObj : Object a -> Shape -> Form
displayObj obj shape =
    move ( obj.x, obj.y ) (filled white shape)



-- display a game state


display : ( Int, Int ) -> Game -> Element
display ( w, h ) { state, ball, player1, player2 } =
    let
        scores : Element
        scores =
            toString player1.score
                ++ "  "
                ++ toString player2.score
                |> txt (Text.height 50)
    in
        container w h middle <|
            collage gameWidth
                gameHeight
                [ filled pongGreen (rect gameWidth gameHeight)
                , displayObj ball (oval 15 15)
                , displayObj player1 (rect 10 40)
                , displayObj player2 (rect 10 40)
                , toForm scores
                    |> move ( 0, gameHeight / 2 - 40 )
                , toForm
                    (if state == Play then
                        spacer 1 1
                     else
                        txt identity msg
                    )
                    |> move ( 0, 40 - gameHeight / 2 )
                ]


main =
    Signal.map2 display Window.dimensions gameState
