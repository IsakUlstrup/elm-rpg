module Engine.Grid exposing
    ( Grid
    , empty
    , fromList
    , getTiles
    , insert
    , map
    )

import Dict exposing (Dict)
import Engine.Point exposing (Point)


type Grid a
    = Grid (Dict Point (Dict Point a))



-- CONSTANTS


chunkSize : Int
chunkSize =
    3


empty : Grid a
empty =
    Grid Dict.empty


fromList : List ( Point, a ) -> Grid a
fromList tiles =
    List.foldl (\( pos, tile ) grid -> insert pos tile grid) empty tiles


insert : Point -> a -> Grid a -> Grid a
insert position tile (Grid grid) =
    let
        chunkPos =
            pointToChunk position
    in
    (case Dict.get chunkPos grid of
        Just chunk ->
            Dict.insert chunkPos (Dict.insert position tile chunk) grid

        Nothing ->
            Dict.insert chunkPos (Dict.singleton position tile) grid
    )
        |> Grid


getTiles : Point -> Grid a -> List ( Point, a )
getTiles position (Grid grid) =
    let
        chunkPos =
            pointToChunk position
    in
    grid
        |> Dict.get chunkPos
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []


map : (Point -> a -> v) -> Grid a -> Grid v
map f (Grid grid) =
    grid |> Dict.map (\_ chunk -> Dict.map f chunk) |> Grid


pointToChunk : Point -> Point
pointToChunk ( q, r ) =
    ( toFloat q / toFloat chunkSize |> floor
    , toFloat r / toFloat chunkSize |> floor
    )
