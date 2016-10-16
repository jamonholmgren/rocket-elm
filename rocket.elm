module Main exposing (..)

import Html exposing (Html, div, p, text)
import Html.App as App
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (..)
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


type alias Rocket =
    { x : Float
    , y : Float
    , d : Float   -- direction
    , s : Float -- speed
    }


type alias Model =
    { rocket : Rocket
    , score : Int
    , keys : Set String
    }


init : ( Model, Cmd Msg )
init =
    ( { rocket = { x = 500, y = 500, d = 0, s = 2 }, score = 0, keys = Set.empty }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | KeyDownMsg Keyboard.KeyCode
    | KeyUpMsg Keyboard.KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rocket } as model) =
    case msg of
        Tick _ ->
            ( { model | rocket =
                { rocket |
                  s = (accelerateBy rocket.s model.keys)
                , d = (turnBy rocket.d model.keys)
                , x = (moveX rocket)
                , y = (moveY rocket)
                }
              } , Cmd.none
            )

        KeyDownMsg k ->
            ( { model | keys = (keyDown k model.keys) }, Cmd.none )

        KeyUpMsg k ->
            ( { model | keys = (keyUp k model.keys) }, Cmd.none )

keyDown : Int -> Set String -> Set String
keyDown k keys =
    Set.insert (fromChar <| fromCode <| k) keys

keyUp : Int -> Set String -> Set String
keyUp k keys =
    Set.remove (fromChar <| fromCode <| k) keys

(??) : Set String -> String -> Bool
(??) keys k =
  Set.member k keys

infixr 9 ??

accelerateBy : Float -> Set String -> Float
accelerateBy s keys =
    if keys ?? "W" then
        clamp 1 10 s + 0.1
    else if keys ?? "S" then
        clamp 1 10 s - 0.1
    else
        s

turnBy : Float -> Set String -> Float
turnBy d keys =
    if keys ?? "A" then
        d - 0.01
    else if keys ?? "D" then
        d + 0.01
    else
        d

moveX : Rocket -> Float
moveX {x, s, d} =
    clamp 0 1000 x + s * (cos <| turns <| d - 0.25)

moveY : Rocket -> Float
moveY {y, s, d} =
    clamp 0 1000 y + s * (sin <| turns <| d - 0.25)


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
    div [ width "1000", height "1000" ] [
      svg [ viewBox "0 0 1000 1000", width "600px" ]
          [ rect [ x "0", y "0", width "1000", height "1000", fill "#0B79CE" ] []
          , rocketView model
          ]
      , p [] [ text (toString model) ]
      , p [] [ text (toString (sin (turns model.rocket.d))) ]
      , p [] [ text (toString (cos (turns model.rocket.d))) ]
    ]


rocketView : Model -> Html msg
rocketView ({rocket, keys} as model) =
    let
        r = rocket
        angle = toString (r.d * 360)
        rx = toString (r.x - 20)
        ry = toString (r.y - 20)

        rocketImg = if keys ?? "W" then
                        "rocket-burn.svg"
                    else
                        "rocket.svg"

        transforms =
            "rotate(" ++ angle
            ++ " " ++ toString(r.x)
            ++ " " ++ toString(r.y)
            ++ ")"
    in
        image [ x rx, y ry, width "40", height "40", xlinkHref rocketImg, transform transforms ] []
