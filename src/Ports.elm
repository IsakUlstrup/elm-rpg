port module Ports exposing (Chunk, gotChunk, requestChunk)


port requestChunk : String -> Cmd msg


type alias Chunk =
    { q : Int
    , r : Int
    , tiles : List Tile
    }


type alias Tile =
    { q : Int
    , r : Int
    , height : Int
    , hue : Int
    }


port gotChunk : (Chunk -> msg) -> Sub msg
