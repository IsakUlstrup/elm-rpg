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



-- CONSTANTS


renderDistance : Int
renderDistance =
    4



-- TILE
-- TODO: Add height
-- TODO: Add animation state
-- TODO: Add state wrapper


type alias Tile =
    { level : Float
    , hue : Int
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
    , cameraHeight : Int
    }


initTiles : Dict Point ( Int, Tile )
initTiles =
    Point.circle 10 ( 0, 0 )
        |> List.indexedMap
            (\index pos ->
                ( pos
                , ( index |> modBy 3, index * 20 |> modBy 360 |> Tile 1 )
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
        0
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
                , cameraHeight = height
                , entities = Dict.update 0 (Maybe.map (\e -> { e | position = position, height = height })) model.entities
              }
            , Cmd.none
            )

        Tick dt ->
            let
                playerPos =
                    Dict.get 0 model.entities
                        |> Maybe.map .position
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( { model | map = Dict.map (tickTile playerPos dt) model.map }
            , Cmd.none
            )


tickTile : Point -> Float -> Point -> ( Int, Tile ) -> ( Int, Tile )
tickTile playerPos dt position ( height, tile ) =
    let
        rate =
            0.002
    in
    if Point.distance position playerPos < renderDistance then
        ( height, { tile | level = tile.level - (dt * rate) |> max 0 } )

    else
        ( height, { tile | level = tile.level + (dt * rate) |> min 1 } )



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
        fillColor saturation level =
            Svg.Attributes.fill ("hsl(" ++ String.fromInt tile.hue ++ ", " ++ String.fromInt saturation ++ "%, " ++ String.fromInt level ++ "%)")

        transform =
            Svg.Attributes.transform ("translate(0, " ++ String.fromFloat (1500 * tile.level) ++ ")")
    in
    Svg.g
        ([ Render.hexHeightTransform height position
         , Svg.Attributes.class "tile"
         ]
            ++ attrs
        )
        [ Svg.g
            [ Svg.Attributes.class "tile-inner"
            , transform
            ]
            [ Svg.g [ fillColor 75 80 ]
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
            , Render.viewHardcodedHex
                [ fillColor 75 75
                , Svg.Events.onClick (ClickedTile height position)
                ]

            -- , Svg.text_
            --     [ Svg.Attributes.stroke "none"
            --     , Svg.Attributes.fill "black"
            --     , Svg.Attributes.textAnchor "middle"
            --     , Svg.Attributes.pointerEvents "none"
            --     ]
            --     [ Svg.text (Point.toString position) ]
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
        [ Render.viewHardcodedHex []
        ]


viewGrid : Dict Point ( Int, Tile ) -> Dict Int Entity -> Svg Msg
viewGrid tiles entities =
    let
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
                |> List.filter (\( point, _ ) -> Point.distance point playerPos < (renderDistance + 1))
                |> List.sortBy
                    (\( pos, tile ) ->
                        case tile of
                            EntityElement _ _ ->
                                Render.pointToPixel pos |> Tuple.second |> (+) 0.1

                            TileElement _ _ ->
                                Render.pointToPixel pos |> Tuple.second
                    )

        viewElement : ( Point, RenderElement ) -> ( String, Svg Msg )
        viewElement ( pos, el ) =
            case el of
                EntityElement id entity ->
                    ( String.fromInt id
                    , viewEntity [] ( id, entity )
                    )

                TileElement height tile ->
                    ( Point.toString pos
                    , viewTile [] height ( pos, tile )
                    )
    in
    Svg.Keyed.node "g" [] (allElements |> List.map viewElement)


view : Model -> Html Msg
view model =
    main_
        [ Html.Attributes.id "game"
        ]
        [ Render.svg [ Svg.Attributes.class "game-svg" ]
            [ Render.pointHeightCamera [ Svg.Attributes.class "camera" ]
                [ Svg.Lazy.lazy2 viewGrid model.map model.entities

                --     (model.map
                --         |> Dict.toList
                --         |> List.sortBy (\( pos, _ ) -> Render.pointToPixel pos |> Tuple.second)
                --         |> List.map viewTile
                --     )
                -- , Svg.g [] (model.entities |> Dict.toList |> List.map viewEntity)
                ]
                model.cameraPosition
                model.cameraHeight
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
