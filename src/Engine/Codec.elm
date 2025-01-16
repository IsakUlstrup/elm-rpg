module Engine.Codec exposing (decodeMap, encodeMap)

import Dict exposing (Dict)
import Engine.Point exposing (Point)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


pointDecoder : Decoder Point
pointDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "q" Decode.int)
        (Decode.field "r" Decode.int)


mapDecoder : Decoder a -> Decoder (Dict Point a)
mapDecoder tileDecoder =
    Decode.list
        (Decode.map2
            Tuple.pair
            (Decode.field "position" pointDecoder)
            (Decode.field "tile" tileDecoder)
        )
        |> Decode.map Dict.fromList


decodeMap : Decoder a -> String -> Result Decode.Error (Dict Point a)
decodeMap tileDecoder mapJson =
    Decode.decodeString (mapDecoder tileDecoder) mapJson


pointEncoder : Point -> Value
pointEncoder ( q, r ) =
    Encode.object
        [ ( "q", Encode.int q )
        , ( "r", Encode.int r )
        ]


encodeMap : (a -> Value) -> Dict Point a -> Value
encodeMap tileEncoder map =
    Encode.list
        (\( pos, tile ) ->
            Encode.object
                [ ( "position", pointEncoder pos )
                , ( "tile", tileEncoder tile )
                ]
        )
        (Dict.toList map)
