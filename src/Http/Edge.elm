module Http.Edge exposing (DynamoEdgeData, EdgeDataResponse, buildEdgeDataRequest, edgeDataPost)

import Http.Generic exposing (DynamoValue, dynamoNumberValueDecoder, dynamoNumberValueEncoder)
import Http.Url exposing (graphDataURL)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Direction exposing (Direction(..))
import Model.Traversal exposing (Traversal)


type alias EdgeDataRequest =
    { request_items : EdgeDataInnerRequest }


type alias EdgeDataInnerRequest =
    { poli_edge : EdgeDataInnerRequestKeys }


type alias EdgeDataInnerRequestKeys =
    { keys : List EdgeDataInnerRequestKey }


type alias EdgeDataInnerRequestKey =
    { src_id : DynamoValue
    , dst_id : DynamoValue
    }


type alias EdgeDataResponse =
    { responses : PoliEdgeDataTable }


type alias PoliEdgeDataTable =
    { items : List DynamoEdgeData }


type alias DynamoEdgeData =
    { src_id : DynamoValue
    , dst_id : DynamoValue
    , num_transactions : DynamoValue
    , total_spend : DynamoValue
    , avg_spend : DynamoValue
    , max_spend : DynamoValue
    , min_spend : DynamoValue
    }


edgeDataPost : EdgeDataRequest -> (Result Http.Error EdgeDataResponse -> msg) -> Cmd msg
edgeDataPost request toMsg =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (edgeDataRequestEncoder request)
        , expect = Http.expectJson toMsg edgeDataResponseDecoder
        }


buildEdgeDataRequest : Direction -> List Traversal -> EdgeDataRequest
buildEdgeDataRequest direction traversals =
    let
        edges : List ( String, String )
        edges =
            case direction of
                In ->
                    List.concatMap (\trv -> List.map (\dst_id -> ( dst_id, trv.src_id )) trv.dst_ids) traversals

                Out ->
                    List.concatMap (\trv -> List.map (\dst_id -> ( trv.src_id, dst_id )) trv.dst_ids) traversals
    in
    EdgeDataRequest
        (EdgeDataInnerRequest
            (EdgeDataInnerRequestKeys (List.map buildEdgeDataRequestInnerValues edges))
        )


buildEdgeDataRequestInnerValues : ( String, String ) -> EdgeDataInnerRequestKey
buildEdgeDataRequestInnerValues tuple =
    EdgeDataInnerRequestKey (DynamoValue (Tuple.first tuple)) (DynamoValue (Tuple.second tuple))


edgeDataRequestEncoder : EdgeDataRequest -> Encode.Value
edgeDataRequestEncoder edgeDataRequest =
    Encode.object
        [ ( "RequestItems", edgeDataInnerRequestEncoder edgeDataRequest.request_items ) ]


edgeDataInnerRequestEncoder : EdgeDataInnerRequest -> Encode.Value
edgeDataInnerRequestEncoder edgeDataInnerRequest =
    Encode.object
        [ ( "PoliEdge", edgeDataInnerRequestKeysEncoder edgeDataInnerRequest.poli_edge ) ]


edgeDataInnerRequestKeysEncoder : EdgeDataInnerRequestKeys -> Encode.Value
edgeDataInnerRequestKeysEncoder edgeDataInnerRequestKeys =
    Encode.object
        [ ( "Keys", Encode.list edgeDataInnerRequestKeyEncoder edgeDataInnerRequestKeys.keys ) ]


edgeDataInnerRequestKeyEncoder : EdgeDataInnerRequestKey -> Encode.Value
edgeDataInnerRequestKeyEncoder edgeDataInnerRequestKey =
    Encode.object
        [ ( "src_id", dynamoNumberValueEncoder edgeDataInnerRequestKey.src_id )
        , ( "dst_id", dynamoNumberValueEncoder edgeDataInnerRequestKey.dst_id )
        ]


edgeDataResponseDecoder : Decode.Decoder EdgeDataResponse
edgeDataResponseDecoder =
    Decode.map EdgeDataResponse (Decode.field "Responses" poliEdgeDataTableDecoder)


poliEdgeDataTableDecoder : Decode.Decoder PoliEdgeDataTable
poliEdgeDataTableDecoder =
    Decode.map PoliEdgeDataTable (Decode.field "PoliEdge" (Decode.list edgeDataInnerResponseDecoder))


edgeDataInnerResponseDecoder : Decode.Decoder DynamoEdgeData
edgeDataInnerResponseDecoder =
    Decode.map7 DynamoEdgeData
        (Decode.field "src_id" dynamoNumberValueDecoder)
        (Decode.field "dst_id" dynamoNumberValueDecoder)
        (Decode.field "num_transactions" dynamoNumberValueDecoder)
        (Decode.field "total_spend" dynamoNumberValueDecoder)
        (Decode.field "avg_spend" dynamoNumberValueDecoder)
        (Decode.field "max_spend" dynamoNumberValueDecoder)
        (Decode.field "min_spend" dynamoNumberValueDecoder)
