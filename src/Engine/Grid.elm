module Engine.Grid exposing
    ( Grid
    , chunkNeighbours
    , chunkSize
    , chunksToList
    , empty
    , filterChunks
    , fromList
    , getTiles
    , insert
    , insertList
    , map
    , missingChunks
    , pointToChunk
    , remove
    , removeList
    , removeOutsideNeighbours
    , updateNeighbours
    )

import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)


type Grid a
    = Grid (Dict Point (Dict Point a))



-- CONSTANTS


chunkSize : Int
chunkSize =
    5


drawDistance : Int
drawDistance =
    2



-- CONSTRUCTORS


empty : Grid a
empty =
    Grid Dict.empty


fromList : List ( Point, a ) -> Grid a
fromList tiles =
    List.foldl (\( pos, tile ) grid -> insert pos tile grid) empty tiles



-- LOGIC


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


remove : Point -> Grid a -> Grid a
remove position (Grid grid) =
    let
        chunkPos =
            pointToChunk position
    in
    grid
        |> Dict.update chunkPos (Maybe.map (Dict.remove position))
        |> Grid


insertNoReplace : Point -> a -> Grid a -> Grid a
insertNoReplace position tile (Grid grid) =
    let
        chunkPos =
            pointToChunk position
    in
    (case Dict.get chunkPos grid of
        Just chunk ->
            let
                insertHelper pos t c =
                    case Dict.get pos c of
                        Just _ ->
                            c

                        Nothing ->
                            Dict.insert pos t c
            in
            Dict.insert chunkPos (insertHelper position tile chunk) grid

        Nothing ->
            Dict.insert chunkPos (Dict.singleton position tile) grid
    )
        |> Grid


insertList : List ( Point, a ) -> Grid a -> Grid a
insertList tiles grid =
    List.foldl (\( pos, tile ) g -> insertNoReplace pos tile g) grid tiles


removeList : List Point -> Grid a -> Grid a
removeList tiles grid =
    List.foldl (\pos g -> remove pos g) grid tiles


getTiles : Grid a -> List ( Point, a )
getTiles (Grid grid) =
    grid
        |> Dict.toList
        |> List.concatMap (\( _, chunk ) -> Dict.toList chunk)


chunkNeighbours : Int -> Point -> List Point
chunkNeighbours radius position =
    List.range -radius radius
        |> List.concatMap
            (\q ->
                List.range -radius radius
                    |> List.map (\r -> ( q, r ))
            )
        |> List.map (Point.add position)


missingChunks : Point -> Grid a -> List Point
missingChunks position (Grid grid) =
    position
        |> chunkNeighbours drawDistance
        |> List.foldl
            (\pos accum ->
                case Dict.get pos grid of
                    Just _ ->
                        accum

                    Nothing ->
                        pos :: accum
            )
            []


map : (Point -> a -> v) -> Grid a -> Grid v
map f (Grid grid) =
    grid |> Dict.map (\_ chunk -> Dict.map f chunk) |> Grid


filterChunks : (Point -> Dict Point a -> Bool) -> Grid a -> Grid a
filterChunks predicate (Grid grid) =
    Grid (Dict.filter predicate grid)


{-| remove all chunks not neighbouring provided chunk position
-}
removeOutsideNeighbours : Point -> Grid a -> Grid a
removeOutsideNeighbours position (Grid grid) =
    Dict.filter (\pos _ -> List.member pos (chunkNeighbours drawDistance position)) grid |> Grid


updateNeighbours : (Point -> a -> a) -> Point -> Grid a -> Grid a
updateNeighbours f position (Grid grid) =
    let
        chunkPos =
            pointToChunk position

        chunks =
            chunkPos :: Point.neighbours chunkPos
    in
    chunks
        |> List.foldl
            (\neighbour g ->
                Dict.update neighbour (Maybe.map (Dict.map f)) g
            )
            grid
        |> Grid


pointToChunk : Point -> Point
pointToChunk ( q, r ) =
    ( toFloat q / toFloat chunkSize |> floor
    , toFloat r / toFloat chunkSize |> floor
    )


chunksToList : Grid a -> List ( Point, List ( Point, a ) )
chunksToList (Grid grid) =
    grid
        |> Dict.toList
        |> List.map (\( pos, chunk ) -> ( pos, Dict.toList chunk ))
