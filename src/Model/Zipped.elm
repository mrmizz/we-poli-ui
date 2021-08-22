module Model.Zipped exposing (Zipped, sortBy, zip)

import Model.Direction exposing (Direction(..))
import Model.EdgeData exposing (EdgeData)
import Model.SortBy exposing (SortBy(..))
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
                    Basics.compare left.dst_id right.dst_id

        sortedVertices : List VertexData
        sortedVertices =
            List.sortWith vertexSortClause vertices

        sortedEdges : List EdgeData
        sortedEdges =
            List.sortWith edgeSortClause edges
    in
    List.map2 Tuple.pair sortedEdges sortedVertices


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
