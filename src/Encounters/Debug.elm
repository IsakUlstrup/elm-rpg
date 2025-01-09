module Encounters.Debug exposing (Msg(..), view)

import Html exposing (Html)
import Html.Events


type Msg
    = ClickedRestoreHealth


view : Html Msg
view =
    Html.div []
        [ Html.div []
            [ Html.button
                [ Html.Events.onClick ClickedRestoreHealth
                ]
                [ Html.text "Heal player" ]

            -- , viewCharacter player [] []
            ]
        ]
