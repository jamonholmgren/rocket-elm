module Main exposing (..)

import Html exposing (Html, div, p, text)
import Html.App as App
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)
import Keyboard


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
    , d : Int   -- direction
    , s : Float -- speed
    }


type alias Model =
    { rocket : Rocket
    , score : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { rocket = { x = 50, y = 50, d = 0, s = 0 }, score = 0 }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rocket } as model) =
    case msg of
        Tick _ ->
            ( { model
                | rocket = { rocket | d = ((rocket.d + 10) % 360) }
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (100 * millisecond) Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [ width "1000", height "1000" ] [
      svg [ viewBox "0 0 1000 1000", width "600px" ]
          [ rect [ x "0", y "0", width "1000", height "1000", fill "#0B79CE" ] []
          , rocketView model.rocket
          ]
      , p [] [ text (toString model) ]
      , p [] [ text (toString Keyboard.arrows)]
    ]


rocketView : Rocket -> Html msg
rocketView r =
    let
        angle = toString r.d
        rx = toString (r.x - 20)
        ry = toString (r.y - 20)

        transforms =
            "rotate(" ++ angle
            ++ " " ++ toString(r.x)
            ++ " " ++ toString(r.y)
            ++ ")"
    in
        image [ x rx, y ry, width "40", height "40", xlinkHref "method-draw-image.svg", transform transforms ] []
