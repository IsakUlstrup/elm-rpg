module Engine.Codec exposing (encodeChunks)

import Engine.Grid as Grid exposing (Grid)
import Engine.Point as Point exposing (Point)
import Json.Decode exposing (Value)
import Json.Encode as Encode


tileEncoder : ( Point, a ) -> Value
tileEncoder ( ( q, r ), _ ) =
    Encode.object
        [ ( "q", Encode.int q )
        , ( "r", Encode.int r )
        ]


encodeChunks : Grid a -> List ( String, String )
encodeChunks grid =
    grid
        |> Grid.chunksToList
        |> List.map
            (\( pos, chunk ) ->
                chunk
                    |> Encode.list tileEncoder
                    |> Encode.encode 0
                    |> (\chunkData -> ( Point.toString pos ++ ".json", chunkData ))
             -- |> Download.string (Point.toString pos ++ ".json") "text/json"
            )
