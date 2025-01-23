port module Ports exposing (gotChunk, requestChunk)

import RemoteData exposing (Chunk)


port requestChunk : String -> Cmd msg


port gotChunk : (Chunk -> msg) -> Sub msg
