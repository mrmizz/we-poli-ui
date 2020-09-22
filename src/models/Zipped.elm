module Models.Zipped exposing (Zipped, aggregateZipped, groupBySrcId)

import Dict exposing (Dict)
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
