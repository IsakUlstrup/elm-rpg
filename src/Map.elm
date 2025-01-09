module Map exposing
    ( Direction(..)
    , Map
    , MapPosition
    , Position
    , addEntity
    , currenPosition
    , empty
    , getCurrentEntity
    , move
    , positionToString
    , setCurrentEntity
    , tick
    , updateCurrent
    , updateEntities
    , view
    )

import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes


type alias Map a =
    { entities : Dict Position a
    , position : MapPosition
    , transitionDuration : Int
    }


type MapPosition
    = Moving Position Direction Int
    | Still Position


type alias Position =
    ( Int, Int, Int )


type Direction
    = North
    | South
    | West
    | East
    | Up
    | Down
    | Warp Position


empty : Map a
empty =
    Map Dict.empty (Still zero) 900


addEntity : Position -> a -> Map a -> Map a
addEntity position entity map =
    { map
        | entities = Dict.insert position entity map.entities
    }


updateEntities : (Position -> a -> a) -> Map a -> Map a
updateEntities f map =
    { map | entities = Dict.map f map.entities }


updateCurrent : (a -> a) -> Map a -> Map a
updateCurrent f map =
    { map | entities = Dict.update (currenPosition map.position) (Maybe.map f) map.entities }


setCurrentEntity : a -> Map a -> Map a
setCurrentEntity entity map =
    { map | entities = Dict.insert (currenPosition map.position) entity map.entities }


getCurrentEntity : Map a -> Maybe a
getCurrentEntity map =
    Dict.get (currenPosition map.position) map.entities


directionToSoString : Direction -> String
directionToSoString direction =
    case direction of
        North ->
            "north"

        South ->
            "south"

        West ->
            "west"

        East ->
            "east"

        Up ->
            "up"

        Down ->
            "down"

        Warp _ ->
            "warp"


positionToString : Position -> String
positionToString ( x, y, z ) =
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ", " ++ String.fromInt z ++ ")"


zero : Position
zero =
    ( 0, 0, 0 )


north : Position
north =
    ( 0, -1, 0 )


south : Position
south =
    ( 0, 1, 0 )


east : Position
east =
    ( 1, 0, 0 )


west : Position
west =
    ( -1, 0, 0 )


up : Position
up =
    ( 0, 0, 1 )


down : Position
down =
    ( 0, 0, -1 )


add : Position -> Position -> Position
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


addDirection : Direction -> Position -> Position
addDirection direction position =
    case direction of
        North ->
            add north position

        South ->
            add south position

        West ->
            add west position

        East ->
            add east position

        Up ->
            add up position

        Down ->
            add down position

        Warp warpTo ->
            warpTo


currenPosition : MapPosition -> Position
currenPosition state =
    case state of
        Still position ->
            position

        Moving from direction _ ->
            addDirection direction from


tick : Float -> Map a -> Map a
tick dt state =
    { state
        | position =
            case state.position of
                Still _ ->
                    state.position

                Moving from direction cd ->
                    if cd == 0 then
                        Still (addDirection direction from)

                    else
                        Moving from direction (cd - round dt |> max 0)
    }


move : Direction -> Map a -> Map a
move direction map =
    { map
        | position =
            case map.position of
                Still position ->
                    case Dict.get (addDirection direction position) map.entities of
                        Just _ ->
                            Moving position direction map.transitionDuration

                        Nothing ->
                            map.position

                _ ->
                    map.position
    }


view : (Position -> a -> Html msg) -> Map a -> Html msg
view viewTile model =
    let
        currentDirection : String
        currentDirection =
            case model.position of
                Still _ ->
                    "none"

                Moving _ direction _ ->
                    directionToSoString direction

        animationDuration : Attribute msg
        animationDuration =
            Html.Attributes.attribute "style" ("--slide-duration: " ++ String.fromInt model.transitionDuration ++ "ms")

        viewPosition : Position -> List (Html msg)
        viewPosition position =
            model.entities
                |> Dict.get position
                |> Maybe.map (\entity -> [ viewTile position entity ])
                |> Maybe.withDefault []
    in
    Html.section
        [ Html.Attributes.class currentDirection
        , Html.Attributes.class "wipe-container"
        ]
        (case model.position of
            Still position ->
                [ Html.div
                    [ Html.Attributes.class "wipe-panel"
                    , animationDuration
                    ]
                    (viewPosition position)
                ]

            Moving from direction _ ->
                [ Html.div
                    [ Html.Attributes.class "wipe-panel"
                    , animationDuration
                    ]
                    (viewPosition (addDirection direction from))
                , Html.div
                    [ Html.Attributes.class "wipe-panel"
                    , animationDuration
                    ]
                    (viewPosition from)
                ]
        )
