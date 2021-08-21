module Update.Generic exposing (unpackDynamoString, unpackDynamoNumber, unpackDynamoArrayNumber, unpackListDynamoVertexData, unpackListDynamoEdgeData)

import Http.Edge exposing (DynamoEdgeData)
import Http.Generic exposing (DynamoArrayNumber, DynamoBool, DynamoNumber, DynamoString, DynamoVertexData)
import Model.EdgeData exposing (EdgeData)
import Model.VertexData exposing (VertexData)


unpackDynamoString : DynamoString -> String
unpackDynamoString dynamoString =
    dynamoString.value

unpackDynamoNumber: DynamoNumber -> Maybe Int
unpackDynamoNumber dynamoNumber =
    String.toInt dynamoNumber.value


unpackDynamoBool : DynamoBool -> Bool
unpackDynamoBool dynamoBool =
    dynamoBool.value

unpackDynamoArrayNumber: DynamoArrayNumber -> Maybe (List Int)
unpackDynamoArrayNumber dynamoArrayNumber =
    let
        unpack: DynamoNumber -> Maybe (List Int)
        unpack dynamoNumber=
            Maybe.map (\int -> [int]) (unpackDynamoNumber dynamoNumber)

    in
    List.foldl reduce (Just []) (List.map unpack dynamoArrayNumber.list)


unpackListDynamoVertexData: List DynamoVertexData -> Maybe (List VertexData)
unpackListDynamoVertexData list =
    let
        unpack: DynamoVertexData -> Maybe (List VertexData)
        unpack dynamoVertexData =
            Maybe.map (\v -> [v]) (unpackDynamoVertexData dynamoVertexData)
    in
    List.foldl reduce (Just []) (List.map unpack list)



unpackDynamoVertexData : DynamoVertexData -> Maybe VertexData
unpackDynamoVertexData dynamoVertexData =
    let
        maybeValid : Maybe VertexData
        maybeValid =
            Maybe.map f (unpackDynamoNumber dynamoVertexData.uid)

        f: Int -> VertexData
        f uid =
            VertexData
                uid
                (unpackDynamoString dynamoVertexData.name)
                (unpackDynamoBool dynamoVertexData.is_committee)
                (List.map unpackDynamoString dynamoVertexData.cities.list)
                (List.map unpackDynamoString dynamoVertexData.streets.list)
                (List.map unpackDynamoString dynamoVertexData.states.list)
    in
    maybeValid

unpackListDynamoEdgeData : List DynamoEdgeData -> Maybe (List EdgeData)
unpackListDynamoEdgeData list =
    let
        unpack: DynamoEdgeData -> Maybe (List EdgeData)
        unpack dynamoEdgeData =
            Maybe.map (\e -> [e]) (unpackDynamoEdgeData dynamoEdgeData)
    in
    List.foldl reduce (Just []) (List.map unpack list)

unpackDynamoEdgeData : DynamoEdgeData -> Maybe EdgeData
unpackDynamoEdgeData dynamoEdgeData =
    let
        maybe_src_id = (unpackDynamoNumber dynamoEdgeData.src_id)
        maybe_dst_id = (unpackDynamoNumber dynamoEdgeData.dst_id)
        maybe_num_transactions = (unpackDynamoNumber dynamoEdgeData.num_transactions)
        maybe_total_spend = (unpackDynamoNumber dynamoEdgeData.total_spend)
        maybe_avg_spend = (unpackDynamoNumber dynamoEdgeData.avg_spend)
        maybe_max_spend = (unpackDynamoNumber dynamoEdgeData.max_spend)
        maybe_min_spend = (unpackDynamoNumber dynamoEdgeData.min_spend)

        maybeValid =
            case Maybe.map (\int -> EdgeData int) maybe_src_id of
                Just ed1 ->
                    case Maybe.map (\int -> ed1 int ) maybe_dst_id of
                        Just ed2 ->
                            case Maybe.map (\int -> ed2 int ) maybe_num_transactions of
                                Just ed3 ->
                                    case Maybe.map (\int -> ed3 int ) maybe_total_spend of
                                        Just ed4 ->
                                            case Maybe.map (\int -> ed4 int ) maybe_avg_spend of
                                                Just ed5 ->
                                                    case Maybe.map (\int -> ed5 int ) maybe_max_spend of
                                                        Just ed6 ->
                                                            Maybe.map (\int -> ed6 int ) maybe_min_spend

                                                        Nothing ->
                                                            Nothing

                                                Nothing ->
                                                    Nothing

                                        Nothing ->
                                            Nothing

                                Nothing ->
                                    Nothing


                        Nothing ->
                            Nothing


                Nothing ->
                    Nothing

    in
    maybeValid

reduce: Maybe (List a) -> Maybe (List a) -> Maybe (List a)
reduce left right=
    case (left, right) of
        (Just l, Just r) ->
            Just (l ++ r)
        _ ->
            Nothing
