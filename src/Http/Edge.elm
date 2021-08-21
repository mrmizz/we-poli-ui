module Http.Edge exposing (DynamoEdgeData, EdgeDataResponse, buildEdgeDataRequest, edgeDataPost)

import Http
import Http.Generic exposing (DynamoNumber, DynamoString, dynamoNumberDecoder, dynamoNumberEncoder, dynamoStringDecoder)
import Http.Url as Url exposing (batchGetItemURL)
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Direction exposing (Direction(..))


type alias EdgeDataRequest =
    { request_items: RequestItems }

type alias RequestItems =
    { poli_edge: Keys }

type alias Keys =
    { keys: List Key }

type alias Key =
    { src_id : DynamoNumber
    , dst_id : DynamoNumber
    }


type alias EdgeDataResponse =
    { responses : DynamoListEdgeData }


type alias DynamoListEdgeData =
    { poli_edge : List DynamoEdgeData }


type alias DynamoEdgeData =
    { src_id : DynamoNumber
    , dst_id : DynamoNumber
    , num_transactions : DynamoString
    , total_spend : DynamoString
    , avg_spend : DynamoString
    , max_spend : DynamoString
    , min_spend : DynamoString
    }


edgeDataPost : EdgeDataRequest -> (Result Http.Error EdgeDataResponse -> msg) -> Cmd msg
edgeDataPost request toMsg =
    Http.post
        { url = batchGetItemURL
        , body = Http.jsonBody (edgeDataRequestEncoder request)
        , expect = Http.expectJson toMsg edgeDataResponseDecoder
        }


buildEdgeDataRequest : Direction -> (Int, List Int) -> EdgeDataRequest
buildEdgeDataRequest direction (srcId, dstIds) =
    let
        keys : List Key
        keys =
            case direction of
                In ->
                    List.map (\dstId -> { src_id = DynamoNumber (String.fromInt dstId), dst_id = DynamoNumber (String.fromInt srcId) }) dstIds

                Out ->
                    List.map (\dstId -> { src_id = DynamoNumber (String.fromInt srcId), dst_id = DynamoNumber (String.fromInt dstId) }) dstIds
    in
    { request_items = { poli_edge = Keys keys } }


edgeDataRequestEncoder : EdgeDataRequest -> Encode.Value
edgeDataRequestEncoder edgeDataRequest =
    Encode.object
        [ ( "RequestItems", requestItemsEncoder edgeDataRequest.request_items ) ]


requestItemsEncoder : RequestItems -> Encode.Value
requestItemsEncoder requestItems =
    Encode.object
        [ ( "PoliEdge" ++ Url.envTitle, keysEncoder requestItems.poli_edge ) ]


keysEncoder : Keys -> Encode.Value
keysEncoder keys =
    Encode.object
        [ ( "Keys", Encode.list keyEncoder keys.keys ) ]


keyEncoder : Key -> Encode.Value
keyEncoder key =
    Encode.object
        [ ( "src_id", dynamoNumberEncoder key.src_id )
        , ( "dst_id", dynamoNumberEncoder key.dst_id )
        ]


edgeDataResponseDecoder : Decode.Decoder EdgeDataResponse
edgeDataResponseDecoder =
    Decode.map EdgeDataResponse (Decode.field "Responses" poliEdgeDataTableDecoder)


poliEdgeDataTableDecoder : Decode.Decoder DynamoListEdgeData
poliEdgeDataTableDecoder =
    Decode.map DynamoListEdgeData (Decode.field "PoliEdge" (Decode.list edgeDataInnerResponseDecoder))


edgeDataInnerResponseDecoder : Decode.Decoder DynamoEdgeData
edgeDataInnerResponseDecoder =
    Decode.map7 DynamoEdgeData
        (Decode.field "src_id" dynamoNumberDecoder)
        (Decode.field "dst_id" dynamoNumberDecoder)
        (Decode.field "num_transactions" dynamoStringDecoder)
        (Decode.field "total_spend" dynamoStringDecoder)
        (Decode.field "avg_spend" dynamoStringDecoder)
        (Decode.field "max_spend" dynamoStringDecoder)
        (Decode.field "min_spend" dynamoStringDecoder)
