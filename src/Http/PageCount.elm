module Http.PageCount exposing (DynamoPageCount, PageCountResponse, buildPageCountRequest, pageCountPost)

import Http
import Http.Generic exposing (DynamoValue, dynamoNumberValueDecoder, dynamoNumberValueEncoder)
import Http.Url exposing (graphDataURL)
import Json.Decode as Decode
import Json.Encode as Encode


type alias PageCountRequest =
    { request_items : PageCountInnerRequest }


type alias PageCountInnerRequest =
    { poli_traversals_page_count : PageCountInnerRequestKeys }


type alias PageCountInnerRequestKeys =
    { keys : List PageCountInnerRequestKey }


type alias PageCountInnerRequestKey =
    { vertex_id : DynamoValue }


type alias PageCountResponse =
    { responses : PoliTraversalsPageCountTable }


type alias PoliTraversalsPageCountTable =
    { items : List DynamoPageCount }


type alias DynamoPageCount =
    { vertex_id : DynamoValue
    , page_count : DynamoValue
    }


pageCountPost : (Result Http.Error PageCountResponse -> msg) -> PageCountRequest -> Cmd msg
pageCountPost toMsg request =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (pageCountRequestEncoder request)
        , expect = Http.expectJson toMsg pageCountResponseDecoder
        }


buildPageCountRequest : List String -> PageCountRequest
buildPageCountRequest vertexIds =
    PageCountRequest
        (PageCountInnerRequest
            (PageCountInnerRequestKeys (List.map (\vertexId -> PageCountInnerRequestKey (DynamoValue vertexId)) vertexIds))
        )


pageCountRequestEncoder : PageCountRequest -> Encode.Value
pageCountRequestEncoder pageCountRequest =
    Encode.object
        [ ( "RequestItems", pageCountInnerRequestEncoder pageCountRequest.request_items ) ]


pageCountInnerRequestEncoder : PageCountInnerRequest -> Encode.Value
pageCountInnerRequestEncoder pageCountInnerRequest =
    Encode.object
        [ ( "PoliTraversalsPageCount", pageCountInnerRequestKeysEncoder pageCountInnerRequest.poli_traversals_page_count ) ]


pageCountInnerRequestKeysEncoder : PageCountInnerRequestKeys -> Encode.Value
pageCountInnerRequestKeysEncoder pageCountInnerRequestKeys =
    Encode.object
        [ ( "Keys", Encode.list pageCountInnerRequestKeyEncoder pageCountInnerRequestKeys.keys ) ]


pageCountInnerRequestKeyEncoder : PageCountInnerRequestKey -> Encode.Value
pageCountInnerRequestKeyEncoder pageCountInnerRequestKey =
    Encode.object [ ( "vertex_id", dynamoNumberValueEncoder pageCountInnerRequestKey.vertex_id ) ]


pageCountResponseDecoder : Decode.Decoder PageCountResponse
pageCountResponseDecoder =
    Decode.map PageCountResponse (Decode.field "Responses" poliTraversalsPageCountTableDecoder)


poliTraversalsPageCountTableDecoder : Decode.Decoder PoliTraversalsPageCountTable
poliTraversalsPageCountTableDecoder =
    Decode.map PoliTraversalsPageCountTable
        (Decode.field "PoliTraversalsPageCount" (Decode.list pageCountInnerResponseDecoder))


pageCountInnerResponseDecoder : Decode.Decoder DynamoPageCount
pageCountInnerResponseDecoder =
    Decode.map2 DynamoPageCount
        (Decode.field "vertex_id" dynamoNumberValueDecoder)
        (Decode.field "page_count" dynamoNumberValueDecoder)
