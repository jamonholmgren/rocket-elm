module Main exposing (..)

import Ship exposing (Ship, tickShip, shipView)
import Bullet exposing (Bullet, tickBullets, bulletViews)

import Html exposing (Html, div, p, text)
import Html.App as App
import Svg exposing (svg, rect)
import Svg.Attributes exposing (x, y, viewBox, fill, width, height)
import Time exposing (Time, millisecond)
import Keyboard
import Char exposing (fromCode)
import String exposing (fromChar)
import Set exposing (Set)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { ship : Ship
    , bullets : List Bullet
    , score : Int
    , keys : Set String
    }


init : ( Model, Cmd Msg )
init =
    ({
        ship =
            { x = 500
            , y = 500
            , d = 0
            , s = 2.0
            , acc = 0.0
            , turn = 0.0
            , hp = 100
            }
        , bullets = []
        , score = 0
        , keys = Set.empty
    }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | KeyDownMsg Keyboard.KeyCode
    | KeyUpMsg Keyboard.KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ ship, bullets, keys } as model) =
    case msg of
        Tick _ ->
            let
                s = { ship |
                      acc = (accKey keys)
                    , turn = (turnKey keys)
                    }
                newShip = tickShip s
                newBullets = tickBullets bullets
            in
                ({ model | ship = newShip, bullets = newBullets }, Cmd.none)

        KeyDownMsg k ->
            ({ model |
                keys = (addKey k keys)
                , bullets = (addBullet k model.bullets model.ship)
            }, Cmd.none)

        KeyUpMsg k ->
            ({ model | keys = (removeKey k keys) }, Cmd.none)


-- Check if a key is being pressed
-- Usage: if keys ?? "D" then
(??) : Set String -> String -> Bool
(??) keys k =
    Set.member k keys
infixr 9 ??


addKey : Int -> Set String -> Set String
addKey k keys =
    Set.insert (fromChar <| fromCode <| k) keys


removeKey : Int -> Set String -> Set String
removeKey k keys =
    Set.remove (fromChar <| fromCode <| k) keys


accKey : Set String -> Float
accKey keys =
    if keys ?? "W" then 1.0
    else if keys ?? "S" then -1.0
    else 0.0


turnKey : Set String -> Float
turnKey keys =
    if keys ?? "A" then -1.0
    else if keys ?? "D" then 1.0
    else 0.0


addBullet : Int -> List Bullet -> Ship -> List Bullet
addBullet k bullets ship =
    if (fromChar <| fromCode <| k) == "E" then
        { x = ship.x
        , y = ship.y
        , d = ship.d
        , s = 15
        , acc = 0
        , turn = 0
        , friendly = True
        } :: bullets
    else
        bullets

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (30 * millisecond) Tick
        , Keyboard.downs KeyDownMsg
        , Keyboard.ups KeyUpMsg
        ]


-- VIEW


view : Model -> Html Msg
view model =
    div [ width "1000", height "1000" ]
        [ gameView model
        , debugView model
        ]

gameView : Model -> Html msg
gameView model =
    svg [ viewBox "0 0 1000 1000", width "600px" ]
        [ backgroundView
        , shipView model.ship
        , bulletViews model.bullets
        ]

debugView : Model -> Html msg
debugView model =
    p [] [ text (toString model) ]

backgroundView : Html msg
backgroundView =
    rect [ x "0", y "0", width "1000", height "1000", fill "#0B79CE" ] []
