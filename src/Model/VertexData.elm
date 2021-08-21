module Model.VertexData exposing (VertexData, distinct, notUID, Address)

import Set exposing (Set)


type alias VertexData =
    { uid : Int
    , name : String
    , alternate_names : List String
    , is_committee : Bool
    , address: Address
    , alternate_addresses : List Address
    }

type alias Address =
    { alternate_street : Maybe String
    , city : Maybe String
    , state : Maybe String
    , street : Maybe String
    , zip_code: Maybe String
    }


type alias VertexPresence =
    { set : Set Int
    , vertices : List VertexData
    }


notUID : Int -> VertexData -> Bool
notUID uid vertex =
    vertex.uid /= uid


distinct : List VertexData -> VertexPresence
distinct vertices =
    List.foldl bumpVertexPresence (VertexPresence Set.empty []) vertices


bumpVertexPresence : VertexData -> VertexPresence -> VertexPresence
bumpVertexPresence vertexData vertexPresence =
    case Set.member vertexData.uid vertexPresence.set of
        True ->
            vertexPresence

        False ->
            { vertexPresence
                | set = Set.insert vertexData.uid vertexPresence.set
                , vertices = List.singleton vertexData ++ vertexPresence.vertices
            }
