module Model.Zipped exposing (Zipped, zip)

import Model.Direction exposing (Direction(..))
import Model.EdgeData exposing (EdgeData)
import Model.VertexData exposing (VertexData)


type alias Zipped =
    ( EdgeData, VertexData )


type alias VertexDataWithEdgeIds =
    { src_id : Int
    , dst_id : Int
    , vertex : VertexData
    }

zip : Direction -> List VertexData -> List EdgeData -> List Zipped
zip direction vertices edges =
    let
        vertexSortClause : VertexData -> VertexData -> Order
        vertexSortClause left right =
            Basics.compare left.uid right.uid

        edgeSortClause : EdgeData -> EdgeData -> Order
        edgeSortClause left right =
            case direction of
                In ->
                    Basics.compare left.src_id right.src_id

                Out ->
                    Basics.compare right.dst_id right.dst_id

        sortedVertices : List VertexData
        sortedVertices =
            List.sortWith vertexSortClause vertices

        sortedEdges : List EdgeData
        sortedEdges =
            List.sortWith edgeSortClause edges
    in
    List.map2 Tuple.pair sortedEdges sortedVertices
