port module Ports exposing (..)

import ChunkType exposing (ChunkType)


port requestChunk : String -> Cmd msg


port gotChunk : (List ChunkType -> msg) -> Sub msg
