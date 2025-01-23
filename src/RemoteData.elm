module RemoteData exposing (Chunk)


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
