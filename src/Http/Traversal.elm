module Http.Traversal exposing (DynamoTraversal, TraversalResponse, buildTraversalRequest, traversalPost)

import Http.Generic exposing (DynamoArrayValue, DynamoValue, dynamoArrayNumberValueDecoder, dynamoNumberValueDecoder, dynamoNumberValueEncoder)
import Http.Url exposing (graphDataURL)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Traversal exposing (TraversalPage)


type alias TraversalRequest =
    { request_items : TraversalInnerRequest }


type alias TraversalInnerRequest =
    { poli_traversals_page : TraversalInnerRequestKeys }


type alias TraversalInnerRequestKeys =
    { keys : List TraversalInnerRequestKey }


type alias TraversalInnerRequestKey =
    { vertex_id : DynamoValue
    , page_num : DynamoValue
    }


type alias TraversalResponse =
    { responses : PoliTraversalsPageTable }


type alias PoliTraversalsPageTable =
    { items : List DynamoTraversal }


type alias DynamoTraversal =
    { vertex_id : DynamoValue
    , page_num : DynamoValue
    , related_vertex_ids : DynamoArrayValue
    }


traversalPost : TraversalRequest -> (Result Http.Error TraversalResponse -> msg) -> Cmd msg
traversalPost request toMsg =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (traversalRequestEncoder request)
        , expect = Http.expectJson toMsg traversalResponseDecoder
        }


buildTraversalRequest : List TraversalPage -> TraversalRequest
buildTraversalRequest traversalPages =
    TraversalRequest
        (TraversalInnerRequest
            (TraversalInnerRequestKeys (List.map buildTraversalRequestInnerValues traversalPages))
        )


buildTraversalRequestInnerValues : TraversalPage -> TraversalInnerRequestKey
buildTraversalRequestInnerValues traversalPage =
    TraversalInnerRequestKey (DynamoValue traversalPage.vertex_id) (DynamoValue traversalPage.page_number)


traversalRequestEncoder : TraversalRequest -> Encode.Value
traversalRequestEncoder traversalRequest =
    Encode.object
        [ ( "RequestItems", traversalInnerRequestEncoder traversalRequest.request_items ) ]


traversalInnerRequestEncoder : TraversalInnerRequest -> Encode.Value
traversalInnerRequestEncoder traversalInnerRequest =
    Encode.object
        [ ( "PoliTraversalsPage", traversalInnerRequestKeysEncoder traversalInnerRequest.poli_traversals_page ) ]


traversalInnerRequestKeysEncoder : TraversalInnerRequestKeys -> Encode.Value
traversalInnerRequestKeysEncoder traversalInnerRequestKeys =
    Encode.object
        [ ( "Keys", Encode.list traversalInnerRequestKeyEncoder traversalInnerRequestKeys.keys ) ]


traversalInnerRequestKeyEncoder : TraversalInnerRequestKey -> Encode.Value
traversalInnerRequestKeyEncoder traversalInnerRequestKey =
    Encode.object
        [ ( "vertex_id", dynamoNumberValueEncoder traversalInnerRequestKey.vertex_id )
        , ( "page_num", dynamoNumberValueEncoder traversalInnerRequestKey.page_num )
        ]


traversalResponseDecoder : Decode.Decoder TraversalResponse
traversalResponseDecoder =
    Decode.map TraversalResponse (Decode.field "Responses" poliTraversalsPageTable)


poliTraversalsPageTable : Decode.Decoder PoliTraversalsPageTable
poliTraversalsPageTable =
    Decode.map PoliTraversalsPageTable (Decode.field "PoliTraversalsPage" (Decode.list traversalInnerResponseDecoder))


traversalInnerResponseDecoder : Decode.Decoder DynamoTraversal
traversalInnerResponseDecoder =
    Decode.map3 DynamoTraversal
        (Decode.field "vertex_id" dynamoNumberValueDecoder)
        (Decode.field "page_num" dynamoNumberValueDecoder)
        (Decode.field "related_vertex_ids" dynamoArrayNumberValueDecoder)
