module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)
import Engine.Render as Render
import Html exposing (Html, main_)
import Html.Attributes
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Keyed
import Svg.Lazy



-- TILE
-- TODO: Add height
-- TODO: Add animation state
-- TODO: Add state wrapper


type alias Tile =
    { hue : Int
    }



-- ENTITY


type alias Entity =
    { position : Point
    , height : Int
    }



-- RENDER STUFF


type RenderElement
    = TileElement Int Tile
    | EntityElement Int Entity



-- MODEL


type alias Model =
    { seed : Random.Seed
    , map : Dict Point ( Int, Tile )
    , entities : Dict Int Entity
    , cameraPosition : Point
    }


initTiles : Dict Point ( Int, Tile )
initTiles =
    Point.circle 10 ( 0, 0 )
        |> List.indexedMap
            (\index pos ->
                ( pos
                , ( index |> modBy 3, index * 20 |> modBy 360 |> Tile )
                )
            )
        |> Dict.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Random.initialSeed 32)
        initTiles
        (Dict.singleton 0 (Entity ( 0, 0 ) 0))
        ( 0, 0 )
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedTile Int Point
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile height position ->
            ( { model
                | cameraPosition = position
                , entities = Dict.update 0 (Maybe.map (\e -> { e | position = position, height = height })) model.entities
              }
            , Cmd.none
            )

        Tick _ ->
            ( model
            , Cmd.none
            )



-- VIEW
-- viewTrailMeter : Meter -> Html msg
-- viewTrailMeter meter =
--     let
--         ( trailDelay, valueDelay ) =
--             if meter.headingDown then
--                 ( 200, 0 )
--             else
--                 ( 0, 200 )
--     in
--     Html.div [ Html.Attributes.class "meter" ]
--         [ Html.div
--             [ Html.Attributes.class "trail"
--             , Html.Attributes.style "width" (String.fromInt (filledPercentage meter) ++ "%")
--             , Html.Attributes.style "transition-delay" (String.fromInt trailDelay ++ "ms")
--             ]
--             []
--         , Html.div
--             [ Html.Attributes.class "value"
--             , Html.Attributes.style "width" (String.fromInt (filledPercentage meter) ++ "%")
--             , Html.Attributes.style "transition-delay" (String.fromInt valueDelay ++ "ms")
--             ]
--             []
--         ]


viewTile : List (Svg.Attribute Msg) -> Int -> ( Point, Tile ) -> Svg Msg
viewTile attrs height ( position, tile ) =
    let
        fillColor saturation =
            Svg.Attributes.fill ("hsl(" ++ String.fromInt tile.hue ++ ", " ++ String.fromInt saturation ++ "%, 75%)")
    in
    Svg.g
        ([ Render.hexHeightTransform height position
         , Svg.Attributes.class "tile"
         ]
            ++ attrs
        )
        [ Svg.g [ Svg.Attributes.class "tile-inner" ]
            [ Svg.g [ fillColor 50 ]
                [ Svg.rect
                    [ Svg.Attributes.x "-100"
                    , Svg.Attributes.y "0"
                    , Svg.Attributes.width "200"
                    , Svg.Attributes.height "5000"
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.x "-50"
                    , Svg.Attributes.y "0"
                    , Svg.Attributes.width "100"
                    , Svg.Attributes.height "5000"
                    ]
                    []
                ]
            , Render.viewHex
                [ fillColor 75
                , Svg.Events.onClick (ClickedTile height position)
                ]
            , Svg.text_
                [ Svg.Attributes.stroke "none"
                , Svg.Attributes.fill "black"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.pointerEvents "none"
                ]
                [ Svg.text (Point.toString position) ]
            ]
        ]


viewEntity : List (Svg.Attribute msg) -> ( Int, Entity ) -> Svg msg
viewEntity attrs ( id, entity ) =
    Svg.g
        ([ Render.hexHeightTransform entity.height entity.position
         , Svg.Attributes.opacity "0.5"
         , Svg.Attributes.class "entity"
         ]
            ++ attrs
        )
        [ Svg.circle [ Svg.Attributes.r "80" ] [] ]


viewGrid : Dict Point ( Int, Tile ) -> Dict Int Entity -> Svg Msg
viewGrid tiles entities =
    let
        _ =
            Debug.log "Render" ()

        playerPos =
            Dict.get 0 entities
                |> Maybe.map .position
                |> Maybe.withDefault ( 0, 0 )

        tileList =
            tiles |> Dict.toList |> List.map (\( position, ( height, tile ) ) -> ( position, TileElement height tile ))

        entityList =
            entities |> Dict.toList |> List.map (\( id, entity ) -> ( entity.position, EntityElement id entity ))

        allElements : List ( Point, RenderElement )
        allElements =
            tileList
                ++ entityList
                |> List.filter (\( point, _ ) -> Point.distance point playerPos < 4)
                |> List.sortBy
                    (\( pos, tile ) ->
                        case tile of
                            EntityElement _ _ ->
                                Render.pointToPixel pos |> Tuple.second |> (+) 0.1

                            TileElement _ _ ->
                                Render.pointToPixel pos |> Tuple.second
                    )

        distanceClass pos =
            if Point.distance pos playerPos < 3 then
                Svg.Attributes.class "close"

            else
                Svg.Attributes.class "far"

        viewElement : ( Point, RenderElement ) -> ( String, Svg Msg )
        viewElement ( pos, el ) =
            case el of
                EntityElement id entity ->
                    ( String.fromInt id
                    , viewEntity [ distanceClass pos ] ( id, entity )
                    )

                TileElement height tile ->
                    ( Point.toString pos
                    , viewTile [ distanceClass pos ] height ( pos, tile )
                    )
    in
    Svg.Keyed.node "g" [] (allElements |> List.map viewElement)


view : Model -> Html Msg
view model =
    main_
        [ Html.Attributes.id "game"
        ]
        [ Render.svg []
            [ Render.pointCamera [ Svg.Attributes.class "camera" ]
                [ Svg.Lazy.lazy2 viewGrid model.map model.entities

                --     (model.map
                --         |> Dict.toList
                --         |> List.sortBy (\( pos, _ ) -> Render.pointToPixel pos |> Tuple.second)
                --         |> List.map viewTile
                --     )
                -- , Svg.g [] (model.entities |> Dict.toList |> List.map viewEntity)
                ]
                model.cameraPosition
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
