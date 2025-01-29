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
    , pointerDown : Bool
    , brushSize : Int
    , hoverPoint : Point
    , eraser : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Random.initialSeed 32)
        Grid.empty
        ( 0, 0 )
        Render.newCamera
        False
        False
        1
        ( 0, 0 )
        False
    , requestNeighbourChunks ( 0, 0 )
    )


requestNeighbourChunks : Point -> Cmd Msg
requestNeighbourChunks position =
    position
        |> Grid.pointToChunk
        |> Grid.chunkNeighbours
        |> List.map Ports.requestChunk
        |> Cmd.batch


applyBrush : Bool -> Point -> Model -> Model
applyBrush erase position model =
    let
        brush =
            Point.circle model.brushSize position

        tiles =
            List.map (\pos -> ( pos, () )) brush
    in
    if erase then
        { model | map = model.map |> Grid.removeList brush }

    else
        { model | map = model.map |> Grid.insertList tiles }



-- UPDATE


type Msg
    = Tick Float
    | GotChunk Ports.Chunk
    | PressedKey String
    | MouseDown Point
    | MouseOver Point
    | MouseUp
    | BrushRadiusInput Int
    | EraserInput Bool


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
                    , map = Grid.removeOutsideNeighbours chunkPosition model.map
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

        PressedKey key ->
            case key of
                "ArrowLeft" ->
                    ( { model | camera = Render.moveCameraX -100 model.camera }, Cmd.none )

                "ArrowRight" ->
                    ( { model | camera = Render.moveCameraX 100 model.camera }, Cmd.none )

                "ArrowUp" ->
                    ( { model | camera = Render.moveCameraY -100 model.camera }, Cmd.none )

                "ArrowDown" ->
                    ( { model | camera = Render.moveCameraY 100 model.camera }, Cmd.none )

                "-" ->
                    ( { model | camera = Render.zoomCamera -0.1 model.camera }, Cmd.none )

                "+" ->
                    ( { model | camera = Render.zoomCamera 0.1 model.camera }, Cmd.none )

                " " ->
                    ( { model | editMode = not model.editMode }, Cmd.none )

                "e" ->
                    ( model
                    , model.map
                        |> Codec.encodeChunks
                        |> List.map (\( name, data ) -> Download.string name "text/json" data)
                        |> Cmd.batch
                    )

                _ ->
                    ( model, Cmd.none )

        MouseDown position ->
            ( if model.editMode then
                { model
                    | pointerDown = True
                }
                    |> applyBrush model.eraser position

              else
                model
            , Cmd.none
            )

        MouseUp ->
            ( { model | pointerDown = False }
            , Cmd.none
            )

        MouseOver position ->
            ( if model.editMode && model.pointerDown then
                { model | hoverPoint = position } |> applyBrush model.eraser position

              else
                { model | hoverPoint = position }
            , Cmd.none
            )

        BrushRadiusInput radius ->
            ( { model | brushSize = radius }, Cmd.none )

        EraserInput flag ->
            ( { model | eraser = flag }, Cmd.none )



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
         , Svg.Events.onMouseDown (MouseDown position)
         , Svg.Events.onMouseOver (MouseOver position)
         ]
            ++ attrs
        )
        [ Render.viewHardcodedHex
            [ Svg.Attributes.fill "rgba(0, 0, 0, 0.2)"
            ]
        ]


viewBrushPreview : Int -> Point -> Svg Msg
viewBrushPreview brushRadius position =
    let
        brush =
            Point.circle brushRadius position

        viewPreviewTile tilePosition =
            Render.viewHardcodedHex
                [ Svg.Attributes.fill "rgba(255, 255, 255, 0.2)"
                , Render.hexTransform tilePosition
                ]
    in
    Svg.g [ Svg.Attributes.pointerEvents "none" ] (brush |> List.map viewPreviewTile)


viewEditorToolbar : Bool -> Int -> Html Msg
viewEditorToolbar enabled brushRadius =
    Html.div []
        (if enabled then
            [ Html.text ("Brush radius " ++ String.fromInt brushRadius)
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "10"
                , Html.Attributes.value (String.fromInt brushRadius)
                , Html.Events.onInput (String.toInt >> Maybe.withDefault brushRadius >> BrushRadiusInput)
                ]
                []
            , Html.button [ Html.Events.onClick (EraserInput False) ] [ Html.text "Brush" ]
            , Html.button [ Html.Events.onClick (EraserInput True) ] [ Html.text "Eraser" ]
            ]

         else
            []
        )


view : Model -> Html Msg
view model =
    let
        cameraPoint =
            Render.cameraToPoint model.camera
    in
    main_
        [ Html.Attributes.id "game"
        ]
        [ Render.svg [ Svg.Attributes.class "game-svg" ]
            [ Render.camera model.camera
                [ Svg.Attributes.class "camera" ]
                [ Svg.g [] (model.map |> Grid.getTiles |> List.map (viewTile []))
                , Svg.g []
                    (if model.editMode then
                        Point.circle 9 cameraPoint |> List.map (\pos -> ( pos, () )) |> List.map (viewGhostTile [])

                     else
                        []
                    )
                , Render.viewHardcodedHex
                    [ Render.hexTransform cameraPoint
                    , Svg.Attributes.opacity "0.2"
                    , Svg.Attributes.pointerEvents "none"
                    ]
                , viewBrushPreview model.brushSize model.hoverPoint
                ]
            , Svg.circle
                [ Svg.Attributes.r "5"
                , Svg.Attributes.opacity "0.2"
                , Svg.Attributes.pointerEvents "none"
                ]
                []
            ]
        , viewEditorToolbar model.editMode model.brushSize
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.gotChunk GotChunk
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Browser.Events.onAnimationFrameDelta Tick
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
