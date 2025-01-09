module Encounters.Counter exposing (Counter, Msg(..), decrement, increment, view, zero)

import Html exposing (Html)
import Html.Events exposing (onClick)


type alias Counter =
    Int


type Msg
    = ClickedIncrement
    | ClickedDecrement
    | ClickedReset


zero : Counter
zero =
    0


increment : Counter -> Counter
increment counter =
    counter + 1


decrement : Counter -> Counter
decrement counter =
    counter - 1


view : Counter -> Html Msg
view counter =
    Html.div []
        [ Html.h3 [] [ Html.text (String.fromInt counter) ]
        , Html.button [ onClick ClickedIncrement ] [ Html.text "Increment" ]
        , Html.button [ onClick ClickedDecrement ] [ Html.text "Decrement" ]
        , Html.button [ onClick ClickedReset ] [ Html.text "Reset" ]
        ]
