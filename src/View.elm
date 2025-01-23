module View exposing (viewTrailMeter)

import Html exposing (Html)
import Html.Attributes
import Meter exposing (Meter, filledPercentage)


viewTrailMeter : Meter -> Html msg
viewTrailMeter meter =
    let
        ( trailDelay, valueDelay ) =
            if meter.headingDown then
                ( 200, 0 )

            else
                ( 0, 200 )
    in
    Html.div [ Html.Attributes.class "meter" ]
        [ Html.div
            [ Html.Attributes.class "trail"
            , Html.Attributes.style "width" (String.fromInt (filledPercentage meter) ++ "%")
            , Html.Attributes.style "transition-delay" (String.fromInt trailDelay ++ "ms")
            ]
            []
        , Html.div
            [ Html.Attributes.class "value"
            , Html.Attributes.style "width" (String.fromInt (filledPercentage meter) ++ "%")
            , Html.Attributes.style "transition-delay" (String.fromInt valueDelay ++ "ms")
            ]
            []
        ]
