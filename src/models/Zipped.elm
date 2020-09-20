module Models.Zipped exposing (Zipped, aggregateZipped)

import Models.Aggregation exposing (Aggregation(..))
import Models.EdgeData exposing (EdgeData)
import Models.Traversal exposing (Traversal)
import Models.VertexData exposing (VertexData)
import Set exposing (Set)


type alias Zipped =
    ( EdgeData, VertexData )


aggregateZipped : Aggregation -> List Traversal -> List Zipped -> List Zipped
aggregateZipped agg traversals zipped =
    case agg of
        And ->
            case traversals of
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
