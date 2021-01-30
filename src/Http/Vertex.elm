module Http.Vertex exposing (VertexDataResponse, buildVertexDataRequest, vertexDataPost)

import Http.Generic exposing (DynamoValue, DynamoVertexData, dynamoNumberValueEncoder, vertexDataInnerResponseDecoder)
import Http.Url exposing (graphDataURL)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias VertexDataRequest =
    { request_items : VertexDataInnerRequest }


type alias VertexDataInnerRequest =
    { poli_vertex : VertexDataInnerRequestKeys }


type alias VertexDataInnerRequestKeys =
    { keys : List VertexDataInnerRequestKey }


type alias VertexDataInnerRequestKey =
    { uid : DynamoValue }


type alias VertexDataResponse =
    { responses : PoliVertexTable }


type alias PoliVertexTable =
    { items : List DynamoVertexData }


vertexDataPost : VertexDataRequest -> (Result Http.Error VertexDataResponse -> msg) -> Cmd msg
vertexDataPost request toMsg =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (vertexDataRequestEncoder request)
        , expect = Http.expectJson toMsg vertexDataResponseDecoder
        }


buildVertexDataRequest : List String -> VertexDataRequest
buildVertexDataRequest uids =
    VertexDataRequest
        (VertexDataInnerRequest
            (VertexDataInnerRequestKeys (List.map buildVertexDataRequestInnerValues uids))
        )


buildVertexDataRequestInnerValues : String -> VertexDataInnerRequestKey
buildVertexDataRequestInnerValues uid =
    VertexDataInnerRequestKey (DynamoValue uid)


vertexDataRequestEncoder : VertexDataRequest -> Encode.Value
vertexDataRequestEncoder vertexDataRequest =
    Encode.object
        [ ( "RequestItems", vertexDataInnerRequestEncoder vertexDataRequest.request_items ) ]


vertexDataInnerRequestEncoder : VertexDataInnerRequest -> Encode.Value
vertexDataInnerRequestEncoder vertexDataInnerRequest =
    Encode.object
        [ ( "PoliVertex", vertexDataInnerRequestKeysEncoder vertexDataInnerRequest.poli_vertex ) ]


vertexDataInnerRequestKeysEncoder : VertexDataInnerRequestKeys -> Encode.Value
vertexDataInnerRequestKeysEncoder vertexDataInnerRequestKey =
    Encode.object
        [ ( "Keys", Encode.list vertexDataInnerRequestKeyEncoder vertexDataInnerRequestKey.keys ) ]


vertexDataInnerRequestKeyEncoder : VertexDataInnerRequestKey -> Encode.Value
vertexDataInnerRequestKeyEncoder vertexDataInnerRequestUID =
    Encode.object
        [ ( "uid", dynamoNumberValueEncoder vertexDataInnerRequestUID.uid ) ]


vertexDataResponseDecoder : Decode.Decoder VertexDataResponse
vertexDataResponseDecoder =
    Decode.map VertexDataResponse (Decode.field "Responses" poliVertexTable)


poliVertexTable : Decode.Decoder PoliVertexTable
poliVertexTable =
    Decode.map PoliVertexTable (Decode.field "PoliVertex" (Decode.list vertexDataInnerResponseDecoder))
