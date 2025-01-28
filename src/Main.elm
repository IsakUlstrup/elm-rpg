module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Grid as Grid exposing (Grid)
import Engine.Point as Point exposing (Point)
import Engine.Render as Render
import File.Download as Download
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Encode as Encode exposing (Value)
import Ports
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



-- CONSTANTS


zoom : Float
zoom =
    0.7



-- TILE


type alias Tile =
    ()



-- MODEL


type alias Model =
    { seed : Random.Seed
    , map : Grid Tile
    , lastChunk : Point
    , cameraPosition : Point
    , cameraHeight : Int
    , editMode : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Random.initialSeed 32)
        Grid.empty
        ( 0, 0 )
        ( 0, 0 )
        0
        False
    , requestNeighbourChunks ( 0, 0 )
    )


requestNeighbourChunks : Point -> Cmd Msg
requestNeighbourChunks position =
    let
        chunkPosition =
            Grid.pointToChunk position
    in
    (chunkPosition :: Point.neighbours chunkPosition)
        |> List.map Ports.requestChunk
        |> Cmd.batch



-- UPDATE


type Msg
    = ClickedTile Int Point
    | Tick Float
    | GotChunk Ports.Chunk
    | ClickedDownloadChunks
    | ClickedGhostTile Point
    | ClickedToggleEditMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile height position ->
            ( { model
                | cameraPosition = position
                , cameraHeight = height

                -- , entities = Dict.update 0 (Maybe.map (\e -> { e | position = position, height = height })) model.entities
              }
            , Cmd.none
            )

        Tick _ ->
            let
                chunkPosition =
                    Grid.pointToChunk model.cameraPosition
            in
            if chunkPosition /= model.lastChunk then
                ( { model
                    | lastChunk = chunkPosition

                    -- , map = Grid.updateNeighbours (tickTile playerPos dt) model.cameraPosition model.map
                  }
                , requestNeighbourChunks chunkPosition
                )

            else
                ( model
                , Cmd.none
                )

        GotChunk chunk ->
            case chunk.tiles of
                Just tiles ->
                    let
                        -- _ =
                        --     Debug.log "got chunk" data
                        formatedTiles : List ( Point, Tile )
                        formatedTiles =
                            tiles
                                |> List.map
                                    (\remoteTile ->
                                        ( ( remoteTile.q, remoteTile.r )
                                        , ()
                                        )
                                    )
                    in
                    ( { model | map = Grid.insertList formatedTiles model.map }, Cmd.none )

                Nothing ->
                    -- let
                    --     _ =
                    --         Debug.log "Elm: chunk not found" chunk
                    -- in
                    ( model, Cmd.none )

        ClickedDownloadChunks ->
            let
                tileEncoder : ( Point, Tile ) -> Value
                tileEncoder ( ( q, r ), _ ) =
                    Encode.object
                        [ ( "q", Encode.int q )
                        , ( "r", Encode.int r )
                        ]

                z =
                    model.map
                        |> Grid.chunksToList
                        |> List.map
                            (\( pos, chunk ) ->
                                chunk
                                    |> Encode.list tileEncoder
                                    |> Encode.encode 0
                                    |> Download.string (Point.toString pos ++ ".json") "text/json"
                            )
                        |> Cmd.batch

                -- |> List.map (Download.string "")
            in
            ( model, z )

        ClickedGhostTile position ->
            ( { model | map = model.map |> Grid.insert position () }
            , Cmd.none
            )

        ClickedToggleEditMode ->
            ( { model | editMode = not model.editMode }
            , Cmd.none
            )



-- VIEW


viewTile : List (Svg.Attribute Msg) -> ( Point, Tile ) -> Svg Msg
viewTile attrs ( position, tile ) =
    let
        height =
            0

        chunkPos =
            Grid.pointToChunk position

        tileHue =
            (Tuple.first chunkPos |> toFloat) + (Tuple.second chunkPos |> toFloat) * 100

        fillColor saturation level =
            Svg.Attributes.fill ("hsl(" ++ String.fromFloat tileHue ++ ", " ++ String.fromInt saturation ++ "%, " ++ String.fromInt level ++ "%)")
    in
    Svg.g
        ([ Render.hexHeightTransform height position
         , Svg.Attributes.class "tile"
         ]
            ++ attrs
        )
        [ Render.viewHardcodedHex
            [ fillColor 75 75
            , Svg.Events.onClick (ClickedTile height position)
            ]
        , Svg.text_
            [ Svg.Attributes.stroke "none"
            , Svg.Attributes.fill "black"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.pointerEvents "none"
            ]
            [ Svg.text (Point.toString position) ]
        , Svg.text_
            [ Svg.Attributes.stroke "none"
            , Svg.Attributes.fill "black"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.pointerEvents "none"
            , Svg.Attributes.y "30"
            ]
            [ Svg.text (Point.toString chunkPos) ]
        ]


viewGhostTile : List (Svg.Attribute Msg) -> ( Point, Tile ) -> Svg Msg
viewGhostTile attrs ( position, tile ) =
    Svg.g
        ([ Render.hexTransform position
         , Svg.Attributes.class "tile"
         , Svg.Events.onClick (ClickedGhostTile position)
         ]
            ++ attrs
        )
        [ Render.viewHardcodedHex
            [ Svg.Attributes.fill "rgba(0, 0, 0, 0.2)"
            ]
        ]


view : Model -> Html Msg
view model =
    main_
        [ Html.Attributes.id "game"
        ]
        [ Html.div [ Html.Attributes.class "dev-bar" ]
            [ Html.button [ Html.Events.onClick ClickedDownloadChunks ] [ Html.text "download map" ]
            , Html.button [ Html.Events.onClick ClickedToggleEditMode ] [ Html.text "toggle edit mode" ]
            ]
        , Render.svg [ Svg.Attributes.class "game-svg" ]
            [ Render.pointHeightCamera [ Svg.Attributes.class "camera" ]
                [ Svg.g [] (model.map |> Grid.getTiles |> List.map (viewTile []))
                , Svg.g []
                    (if model.editMode then
                        Point.circle 5 model.cameraPosition |> List.map (\pos -> ( pos, () )) |> List.map (viewGhostTile [])

                     else
                        []
                    )
                ]
                model.cameraPosition
                model.cameraHeight
                zoom
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Ports.gotChunk GotChunk
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
