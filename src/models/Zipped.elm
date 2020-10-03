module Models.Zipped exposing (Zipped, aggregateZipped, groupBySrcId, zipVerticesAndEdges)

import Dict exposing (Dict)
import Models.Aggregation exposing (Aggregation(..))
import Models.Direction exposing (Direction(..))
import Models.EdgeData exposing (EdgeData)
import Models.Traversal exposing (Traversal)
import Models.VertexData exposing (VertexData)
import Set exposing (Set)


type alias Zipped =
    ( EdgeData, VertexData )


type alias VertexDataWithEdgeIds =
    { src_id : String
    , dst_id : String
    , vertex : VertexData
    }


zipVerticesAndEdges : Direction -> List Traversal -> List VertexData -> List EdgeData -> List Zipped
zipVerticesAndEdges direction traversals vertices edges =
    let
        zipClause : Traversal -> VertexData -> Maybe VertexDataWithEdgeIds
        zipClause traversal vertex =
            case List.member ((\v -> v.uid) vertex) traversal.dst_ids of
                True ->
                    case direction of
                        In ->
                            Just (VertexDataWithEdgeIds vertex.uid traversal.src_id vertex)

                        Out ->
                            Just (VertexDataWithEdgeIds traversal.src_id vertex.uid vertex)

                False ->
                    Nothing

        verticesWithEdgeIds : List VertexDataWithEdgeIds
        verticesWithEdgeIds =
            List.concatMap
                (\traversal ->
                    List.filterMap
                        (\vertexData -> zipClause traversal vertexData)
                        vertices
                )
                traversals

        genericSortClause : ( String, String ) -> ( String, String ) -> Order
        genericSortClause left right =
            case Basics.compare (Tuple.first left) (Tuple.first right) of
                Basics.LT ->
                    Basics.LT

                Basics.EQ ->
                    Basics.compare (Tuple.second left) (Tuple.second right)

                Basics.GT ->
                    Basics.GT

        vertexSortClause : VertexDataWithEdgeIds -> VertexDataWithEdgeIds -> Order
        vertexSortClause left right =
            genericSortClause ( left.src_id, left.dst_id ) ( right.src_id, right.dst_id )

        edgeSortClause : EdgeData -> EdgeData -> Order
        edgeSortClause left right =
            genericSortClause ( left.src_id, left.dst_id ) ( right.src_id, right.dst_id )

        sortedVertices : List VertexData
        sortedVertices =
            List.map (\ve -> ve.vertex) (List.sortWith vertexSortClause verticesWithEdgeIds)

        sortedEdges : List EdgeData
        sortedEdges =
            List.sortWith edgeSortClause edges
    in
    List.map2 Tuple.pair sortedEdges sortedVertices


aggregateZipped : Aggregation -> List Traversal -> List Zipped -> List Zipped
aggregateZipped agg traversals zipped =
    case agg of
        And ->
            case groupBySrcId traversals of
                _ :: [] ->
                    zipped

                head :: tail ->
                    let
                        headSet : Set String
                        headSet =
                            Set.fromList head.dst_ids

                        tailSets : List (Set String)
                        tailSets =
                            List.map (\trv -> Set.fromList trv.dst_ids) tail

                        intersection : Set String
                        intersection =
                            List.foldl Set.intersect headSet tailSets
                    in
                    List.filter
                        (\edgeAndVertex -> Set.member ((\v -> v.uid) (Tuple.second edgeAndVertex)) intersection)
                        zipped

                [] ->
                    zipped

        Or ->
            zipped


groupBySrcId : List Traversal -> List Traversal
groupBySrcId traversals =
    case traversals of
        _ :: [] ->
            traversals

        head :: tail ->
            let
                init : Dict String (List Traversal)
                init =
                    Dict.singleton head.src_id [ head ]

                others : List ( String, Traversal )
                others =
                    List.map (\t -> ( t.src_id, t )) tail

                foldl : ( String, Traversal ) -> Dict String (List Traversal) -> Dict String (List Traversal)
                foldl item body =
                    case Dict.member (Tuple.first item) body of
                        True ->
                            let
                                update : ( String, Traversal ) -> Maybe (List Traversal) -> Maybe (List Traversal)
                                update that maybe =
                                    case maybe of
                                        Just history ->
                                            Just (history ++ [ Tuple.second that ])

                                        Nothing ->
                                            Nothing
                            in
                            Dict.update (Tuple.first item) (update item) body

                        False ->
                            Dict.insert (Tuple.first item) [ Tuple.second item ] body
            in
            List.foldl foldl init others
                |> Dict.toList
                |> List.map
                    (\kv ->
                        Traversal
                            (Tuple.first kv)
                            (List.concatMap (\trv -> trv.dst_ids) (Tuple.second kv))
                    )

        [] ->
            traversals
