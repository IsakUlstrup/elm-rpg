module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Codec as Codec
import Engine.Grid as Grid exposing (Grid)
import Engine.Point as Point exposing (Point)
import Engine.Render as Render exposing (Camera)
import File.Download as Download
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Ports
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



-- TILE


type alias Tile =
    ()



-- MODEL


type alias Model =
    { seed : Random.Seed
    , map : Grid Tile
    , lastChunk : Point
    , camera : Camera
    , editMode : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Random.initialSeed 32)
        Grid.empty
        ( 0, 0 )
        Render.newCamera
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
    = Tick Float
    | GotChunk Ports.Chunk
    | ClickedDownloadChunks
    | ClickedGhostTile Point
    | ClickedToggleEditMode
    | PressedKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                chunkPosition =
                    Grid.pointToChunk (Render.cameraToPoint model.camera)
            in
            if chunkPosition /= model.lastChunk then
                ( { model
                    | lastChunk = chunkPosition
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
            ( model
            , model.map
                |> Codec.encodeChunks
                |> List.map (\( name, data ) -> Download.string name "text/json" data)
                |> Cmd.batch
            )

        ClickedGhostTile position ->
            ( { model | map = model.map |> Grid.insert position () }
            , Cmd.none
            )

        ClickedToggleEditMode ->
            ( { model | editMode = not model.editMode }
            , Cmd.none
            )

        PressedKey key ->
            ( case key of
                "ArrowLeft" ->
                    { model | camera = Render.moveCameraX -100 model.camera }

                "ArrowRight" ->
                    { model | camera = Render.moveCameraX 100 model.camera }

                "ArrowUp" ->
                    { model | camera = Render.moveCameraY -100 model.camera }

                "ArrowDown" ->
                    { model | camera = Render.moveCameraY 100 model.camera }

                "-" ->
                    { model | camera = Render.zoomCamera -0.1 model.camera }

                "+" ->
                    { model | camera = Render.zoomCamera 0.1 model.camera }

                _ ->
                    model
            , Cmd.none
            )



-- VIEW


viewTile : List (Svg.Attribute Msg) -> ( Point, Tile ) -> Svg Msg
viewTile attrs ( position, tile ) =
    let
        chunkPos =
            Grid.pointToChunk position

        tileHue =
            Point.uniqueId chunkPos * 100

        fillColor saturation level =
            Svg.Attributes.fill ("hsl(" ++ String.fromInt tileHue ++ ", " ++ String.fromInt saturation ++ "%, " ++ String.fromInt level ++ "%)")
    in
    Svg.g
        ([ Render.hexTransform position
         , Svg.Attributes.class "tile"
         ]
            ++ attrs
        )
        [ Render.viewHardcodedHex
            [ fillColor 75 75
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
    let
        cameraPoint =
            Render.cameraToPoint model.camera
    in
    main_
        [ Html.Attributes.id "game"
        ]
        [ Html.div [ Html.Attributes.class "dev-bar" ]
            [ Html.button [ Html.Events.onClick ClickedDownloadChunks ] [ Html.text "download map" ]
            , Html.button [ Html.Events.onClick ClickedToggleEditMode ] [ Html.text "toggle edit mode" ]
            ]
        , Render.svg [ Svg.Attributes.class "game-svg" ]
            [ Render.camera model.camera
                [ Svg.Attributes.class "camera" ]
                [ Svg.g [] (model.map |> Grid.getTiles |> List.map (viewTile []))
                , Svg.g []
                    (if model.editMode then
                        Point.circle 9 cameraPoint |> List.map (\pos -> ( pos, () )) |> List.map (viewGhostTile [])

                     else
                        []
                    )
                , Render.viewHardcodedHex [ Render.hexTransform cameraPoint, Svg.Attributes.opacity "0.2" ]
                ]
            , Svg.circle [ Svg.Attributes.r "5", Svg.Attributes.opacity "0.2" ] []
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Ports.gotChunk GotChunk
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : Decoder Msg
keyDecoder =
    Decode.map PressedKey (Decode.field "key" Decode.string)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
