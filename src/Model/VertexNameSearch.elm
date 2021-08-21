module Model.VertexNameSearch exposing (VertexNameSearch)

import Model.VertexData exposing (VertexData)


type alias VertexNameSearch =
    { input : String
    , vertices : List VertexData
    }
