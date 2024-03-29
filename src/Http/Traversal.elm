module Http.Traversal exposing (DynamoTraversal, TraversalResponse, buildTraversalRequest, traversalPost)

import Http
import Http.Generic exposing (DynamoArrayNumber, DynamoNumber, DynamoNumberAsInt, DynamoString, dynamoArrayNumberDecoder, dynamoNumberAsIntDecoder, dynamoNumberAsIntEncoder, dynamoNumberDecoder, dynamoNumberEncoder)
import Http.Url as Url exposing (getItemURL)
import Json.Decode as Decode
import Json.Encode as Encode
import Model.SortBy as SortBy exposing (SortBy)


type alias TraversalRequest =
    { table_name : String
    , key : Key
    }


type alias Key =
    { vertex_id : DynamoNumber
    , page_num : DynamoNumberAsInt
    }


type alias TraversalResponse =
    { item : DynamoTraversal }


type alias DynamoTraversal =
    { vertex_id : DynamoNumber
    , page_num : DynamoNumberAsInt
    , related_vertex_ids : DynamoArrayNumber
    }


traversalPost : TraversalRequest -> (Result Http.Error TraversalResponse -> msg) -> Cmd msg
traversalPost request toMsg =
    Http.post
        { url = getItemURL
        , body = Http.jsonBody (traversalRequestEncoder request)
        , expect = Http.expectJson toMsg traversalResponseDecoder
        }


buildTraversalRequest : SortBy -> String -> Int -> TraversalRequest
buildTraversalRequest sortBy srcId pageNumber =
    { table_name = "PoliTraversalsPageSB" ++ SortBy.toString sortBy ++ Url.envTitle
    , key = { vertex_id = DynamoNumber srcId, page_num = DynamoNumberAsInt pageNumber }
    }


traversalRequestEncoder : TraversalRequest -> Encode.Value
traversalRequestEncoder traversalRequest =
    let
        key : Encode.Value
        key =
            Encode.object
                [ ( "vertex_id", dynamoNumberEncoder traversalRequest.key.vertex_id )
                , ( "page_num", dynamoNumberAsIntEncoder traversalRequest.key.page_num )
                ]
    in
    Encode.object
        [ ( "TableName", Encode.string traversalRequest.table_name )
        , ( "Key", key )
        ]


traversalResponseDecoder : Decode.Decoder TraversalResponse
traversalResponseDecoder =
    Decode.map TraversalResponse (Decode.field "Item" dynamoTraversalDecoder)


dynamoTraversalDecoder : Decode.Decoder DynamoTraversal
dynamoTraversalDecoder =
    Decode.map3 DynamoTraversal
        (Decode.field "vertex_id" dynamoNumberDecoder)
        (Decode.field "page_num" dynamoNumberAsIntDecoder)
        (Decode.field "related_vertex_ids" dynamoArrayNumberDecoder)
