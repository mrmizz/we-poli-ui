module Model.Zipped exposing (Zipped, aggregate, groupBySrcId, sortBy, zipVerticesAndEdges)

import Dict exposing (Dict)
import Model.Aggregation exposing (Aggregation(..))
import Model.Direction exposing (Direction(..))
import Model.EdgeData exposing (EdgeData)
import Model.SortBy exposing (SortBy(..))
import Model.Traversal exposing (Traversal)
import Model.VertexData exposing (VertexData)
import Set exposing (Set)


type alias Zipped =
    ( EdgeData, VertexData )


type alias VertexDataWithEdgeIds =
    { src_id : String
    , dst_id : String
    , vertex : VertexData
    }


sortBy : SortBy -> List Zipped -> List Zipped
sortBy option zipped =
    let
        genericSortClauseDESC : (EdgeData -> String) -> Zipped -> Zipped -> Order
        genericSortClauseDESC f left right =
            let
                leftVal : String
                leftVal =
                    f (Tuple.first left)

                rightVal : String
                rightVal =
                    f (Tuple.first right)
            in
            case Basics.compare (String.length leftVal) (String.length rightVal) of
                LT ->
                    GT

                EQ ->
                    case Basics.compare leftVal rightVal of
                        LT ->
                            GT

                        EQ ->
                            EQ

                        GT ->
                            LT

                GT ->
                    LT

        genericSortClauseASC : (EdgeData -> String) -> Zipped -> Zipped -> Order
        genericSortClauseASC f left right =
            let
                leftVal : String
                leftVal =
                    f (Tuple.first left)

                rightVal : String
                rightVal =
                    f (Tuple.first right)
            in
            case Basics.compare (String.length leftVal) (String.length rightVal) of
                EQ ->
                    Basics.compare leftVal rightVal

                val ->
                    val

        isNegativeValue : String -> Bool
        isNegativeValue str =
            case String.toList str |> List.take 2 |> List.reverse |> List.head of
                Just char ->
                    Char.isDigit char

                Nothing ->
                    False

        partitioned : (EdgeData -> String) -> ( List Zipped, List Zipped )
        partitioned f =
            List.partition (\z -> isNegativeValue (f (Tuple.first z))) zipped

        genericSort : (EdgeData -> String) -> List Zipped
        genericSort f =
            case partitioned f of
                ( left, right ) ->
                    List.sortWith (genericSortClauseDESC f) left
                        ++ List.sortWith (genericSortClauseASC f) right
    in
    case option of
        Count ->
            genericSort (\edge -> edge.num_transactions)

        TotalSpend ->
            genericSort (\edge -> edge.total_spend)

        AvgSpend ->
            genericSort (\edge -> edge.avg_spend)

        MaxSpend ->
            genericSort (\edge -> edge.max_spend)

        MinSpend ->
            genericSort (\edge -> edge.min_spend)


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


aggregate : Aggregation -> List Traversal -> List Zipped -> List Zipped
aggregate agg traversals zipped =
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
