module Models.VertexData exposing (VertexData, distinctVertices, filterVerticesByDirection, notUID)

import Models.Direction exposing (Direction, directionToIsCommittee)
import Set exposing (Set)


type alias VertexData =
    { uid : String
    , name : String
    , is_committee : Bool
    , cities : List String
    , streets : List String
    , states : List String
    }


type alias VertexPresence =
    { set : Set String
    , vertices : List VertexData
    }


notUID : String -> VertexData -> Bool
notUID uid vertex =
    vertex.uid /= uid


distinctVertices : List VertexData -> VertexPresence
distinctVertices vertices =
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


filterVerticesByDirection : Direction -> List VertexData -> List VertexData
filterVerticesByDirection direction vertices =
    case List.filter (sameDirection direction) vertices of
        [] ->
            []

        head :: [] ->
            case sameDirection direction head of
                True ->
                    [ head ]

                False ->
                    []

        list ->
            list


sameDirection : Direction -> VertexData -> Bool
sameDirection direction vertexData =
    vertexData.is_committee == directionToIsCommittee direction
