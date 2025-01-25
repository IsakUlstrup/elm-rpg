module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Grid as Grid exposing (Grid)
import Engine.Point as Point exposing (Point)
import Engine.Render as Render
import Html exposing (Html, main_)
import Html.Attributes
import Ports
import Random
import RemoteData
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Keyed
import Svg.Lazy



-- CONSTANTS


renderDistance : Int
renderDistance =
    4


zoom : Float
zoom =
    0.7



-- TILE


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
    , map : Grid ( Int, Tile )
    , lastChunk : Point
    , entities : Dict Int Entity
    , cameraPosition : Point
    , cameraHeight : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Random.initialSeed 32)
        Grid.empty
        ( 0, 0 )
        (Dict.singleton 0 (Entity ( 0, 0 ) 0))
        ( 0, 0 )
        0
    , requestNeighbourChunks ( 0, 0 )
    )


requestNeighbourChunks : Point -> Cmd Msg
requestNeighbourChunks position =
    let
        chunkPosition =
            Grid.pointToChunk position
    in
    (chunkPosition :: Point.neighbours chunkPosition)
        |> List.map (Point.toString >> Ports.requestChunk)
        |> Cmd.batch



-- UPDATE


type Msg
    = ClickedTile Int Point
    | Tick Float
    | GotChunk RemoteData.Chunk


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

                chunkPosition =
                    Grid.pointToChunk playerPos
            in
            if chunkPosition /= model.lastChunk then
                ( { model
                    | lastChunk = chunkPosition
                    , map = Grid.updateNeighbours (tickTile playerPos dt) model.cameraPosition model.map
                  }
                , requestNeighbourChunks chunkPosition
                )

            else
                ( { model | map = Grid.updateNeighbours (tickTile playerPos dt) model.cameraPosition model.map }
                , Cmd.none
                )

        GotChunk chunk ->
            let
                -- _ =
                --     Debug.log "got chunk" data
                formatedTiles : List ( Point, ( Int, Tile ) )
                formatedTiles =
                    chunk.tiles
                        |> List.map
                            (\remoteTile ->
                                ( ( remoteTile.q, remoteTile.r ) |> Point.add (Point.scale Grid.chunkSize ( chunk.q, chunk.r ))
                                , ( remoteTile.height, Tile 1 remoteTile.hue )
                                )
                            )
            in
            ( { model | map = Grid.insertList formatedTiles model.map }, Cmd.none )


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


viewTile : List (Svg.Attribute Msg) -> Int -> ( Point, Tile ) -> Svg Msg
viewTile attrs height ( position, tile ) =
    let
        chunkPos =
            Grid.pointToChunk position

        fillColor saturation level =
            Svg.Attributes.fill ("hsl(" ++ String.fromInt tile.hue ++ ", " ++ String.fromInt saturation ++ "%, " ++ String.fromInt level ++ "%)")

        transform =
            Svg.Attributes.transform ("translate(0, " ++ String.fromFloat (2000 * tile.level) ++ ")")
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
        ]


viewEntity : List (Svg.Attribute msg) -> ( Int, Entity ) -> Svg msg
viewEntity attrs ( id, entity ) =
    Svg.g
        ([ Render.hexHeightTransform entity.height entity.position
         , Svg.Attributes.class "entity"
         ]
            ++ attrs
        )
        [ Render.viewHardcodedHex []

        -- , Svg.image
        --     [ Svg.Attributes.xlinkHref "character.png"
        --     , Svg.Attributes.class "sprite"
        --     , Svg.Attributes.width "200"
        --     , Svg.Attributes.height "200"
        --     , Svg.Attributes.x "-100"
        --     , Svg.Attributes.y "-180"
        --     ]
        --     []
        -- <image href="mdn_logo_only_color.png" height="200" width="200" />
        ]


viewGrid : Grid ( Int, Tile ) -> Dict Int Entity -> Svg Msg
viewGrid tiles entities =
    let
        tileDistClass p =
            if Point.distance playerPos p < 2 then
                "close"

            else
                "far"

        playerPos =
            Dict.get 0 entities
                |> Maybe.map .position
                |> Maybe.withDefault ( 0, 0 )

        tileList =
            tiles |> Grid.getTilesRadius playerPos |> List.map (\( position, ( height, tile ) ) -> ( position, TileElement height tile ))

        entityList =
            entities |> Dict.toList |> List.map (\( id, entity ) -> ( entity.position, EntityElement id entity ))

        allElements : List ( Point, RenderElement )
        allElements =
            tileList
                ++ entityList
                |> List.sortBy
                    (\( _, tile ) ->
                        case tile of
                            EntityElement _ _ ->
                                0.1

                            TileElement _ _ ->
                                0
                    )

        viewElement : ( Point, RenderElement ) -> ( String, Svg Msg )
        viewElement ( pos, el ) =
            case el of
                EntityElement id entity ->
                    ( String.fromInt id
                    , viewEntity [ Svg.Attributes.pointerEvents "none" ] ( id, entity )
                    )

                TileElement height tile ->
                    ( Point.toString pos
                    , viewTile [ Svg.Attributes.class (tileDistClass pos) ] height ( pos, tile )
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
