module Models.VertexData exposing (VertexData)

type alias VertexData =
    { uid : String
    , name : String
    , is_committee : Bool
    , cities : List String
    , streets : List String
    , states : List String
    }
