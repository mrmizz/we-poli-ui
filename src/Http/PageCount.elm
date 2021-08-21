module Http.PageCount exposing (DynamoPageCount, PageCountResponse, buildPageCountRequest, pageCountPost)

import Http
import Http.Generic exposing (DynamoNumber, dynamoNumberDecoder, dynamoNumberEncoder)
import Http.Url as Url exposing (getItemURL)
import Json.Decode as Decode
import Json.Encode as Encode


type alias PageCountRequest =
    { table_name: String
    , key: DynamoNumber
    }


type alias PageCountResponse =
    { item : DynamoPageCount }


type alias DynamoPageCount =
    { vertex_id : DynamoNumber
    , page_count : DynamoNumber
    }


pageCountPost : (Result Http.Error PageCountResponse -> msg) -> PageCountRequest -> Cmd msg
pageCountPost toMsg request =
    Http.post
        { url = getItemURL
        , body = Http.jsonBody (pageCountRequestEncoder request)
        , expect = Http.expectJson toMsg pageCountResponseDecoder
        }


buildPageCountRequest : Int -> PageCountRequest
buildPageCountRequest vertexId =
    { table_name = "PoliTraversalsPageCount" ++ Url.envTitle
    , key = DynamoNumber (String.fromInt vertexId)
    }



pageCountRequestEncoder : PageCountRequest -> Encode.Value
pageCountRequestEncoder pageCountRequest =
    let
        key : Encode.Value
        key =
            Encode.object
                [ ("vertex_id", dynamoNumberEncoder pageCountRequest.key)
                ]
    in
    Encode.object
        [ ("TableName", Encode.string pageCountRequest.table_name)
        , ("Key", key)
        ]


pageCountResponseDecoder : Decode.Decoder PageCountResponse
pageCountResponseDecoder =
    Decode.map PageCountResponse (Decode.field "Item" pageCountDecoder)

pageCountDecoder : Decode.Decoder DynamoPageCount
pageCountDecoder =
    Decode.map2 DynamoPageCount
        (Decode.field "vertex_id" dynamoNumberDecoder)
        (Decode.field "page_count" dynamoNumberDecoder)
