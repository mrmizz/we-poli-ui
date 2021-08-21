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


sortBy : SortBy -> List Zipped -> List Zipped
sortBy option zipped =
    let
        genericSortClauseDESC : (EdgeData -> Int) -> Zipped -> Zipped -> Order
        genericSortClauseDESC f left right =
            let
                leftVal : Int
                leftVal =
                    f (Tuple.first left)

                rightVal : Int
                rightVal =
                    f (Tuple.first right)
            in
            case Basics.compare leftVal rightVal of
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

        genericSortClauseASC : (EdgeData -> Int) -> Zipped -> Zipped -> Order
        genericSortClauseASC f left right =
            let
                leftVal : Int
                leftVal =
                    f (Tuple.first left)

                rightVal : Int
                rightVal =
                    f (Tuple.first right)
            in
            case Basics.compare leftVal rightVal of
                EQ ->
                    Basics.compare leftVal rightVal

                val ->
                    val

        isNegativeValue : Int -> Bool
        isNegativeValue int =
            int < 0

        partitioned : (EdgeData -> Int) -> ( List Zipped, List Zipped )
        partitioned f =
            List.partition (\z -> isNegativeValue (f (Tuple.first z))) zipped

        genericSort : (EdgeData -> Int) -> List Zipped
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
