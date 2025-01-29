port module Ports exposing (Chunk, Tile, gotChunk, requestChunk)

import Engine.Point exposing (Point)


port requestChunk : Point -> Cmd msg


type alias Chunk =
    { q : Int
    , r : Int
    , tiles : Maybe (List Tile)
    }


type alias Tile =
    { q : Int
    , r : Int
    }


port gotChunk : (Chunk -> msg) -> Sub msg
