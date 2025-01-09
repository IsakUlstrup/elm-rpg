module Encounters.AutoCounter exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Meter exposing (Meter)


type alias AutoCounter =
    Meter


type Msg
    = ClickedReset


new : Int -> AutoCounter
new maxTime =
    Meter.newEmpty maxTime


tick : Float -> AutoCounter -> AutoCounter
tick dt counter =
    Meter.add (round dt) counter


reset : AutoCounter -> AutoCounter
reset counter =
    Meter.reset counter


view : AutoCounter -> Html Msg
view counter =
    Html.div []
        [ Html.h3 [] [ Html.text (String.fromInt counter.current) ]
        , Html.progress
            [ Html.Attributes.value (String.fromInt counter.current)
            , Html.Attributes.max (String.fromInt counter.max)
            ]
            []
        , Html.button [ onClick ClickedReset ] [ Html.text "Reset" ]
        ]
