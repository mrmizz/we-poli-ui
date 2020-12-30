module HTTP.NamePrefix exposing (VertexNamePrefixResponse, vertexNamePrefixGet)

import HTTP.Generic exposing (DynamoValue, DynamoVertexDataItem, DynamoVertexDataItems, dynamoNumberValueDecoder, dynamoStringValueDecoder, vertexDataInnerResponseDecoder)
import HTTP.URL exposing (prefixURL)
import Http
import Json.Decode as Decode


type alias VertexNamePrefixResponse =
    { items : List VertexNamePrefixInnerResponse }


type alias VertexNamePrefixInnerResponse =
    { prefix : DynamoValue
    , prefix_size : DynamoValue
    , vertices : DynamoVertexDataItems
    }


vertexNamePrefixGet : String -> (Result Http.Error VertexNamePrefixResponse -> msg) -> Cmd msg
vertexNamePrefixGet prefix toMsg =
    Http.get
        { url = prefixURL ++ prefix
        , expect = Http.expectJson toMsg vertexNamePrefixResponseDecoder
        }


vertexNamePrefixResponseDecoder : Decode.Decoder VertexNamePrefixResponse
vertexNamePrefixResponseDecoder =
    Decode.map VertexNamePrefixResponse (Decode.field "Items" (Decode.list vertexNamePrefixInnerResponseDecoder))


vertexNamePrefixInnerResponseDecoder : Decode.Decoder VertexNamePrefixInnerResponse
vertexNamePrefixInnerResponseDecoder =
    Decode.map3 VertexNamePrefixInnerResponse
        (Decode.field "prefix" dynamoStringValueDecoder)
        (Decode.field "prefix_size" dynamoNumberValueDecoder)
        (Decode.field "vertices" dynamoVertexDataInnerDecoder)


dynamoVertexDataInnerDecoder : Decode.Decoder DynamoVertexDataItems
dynamoVertexDataInnerDecoder =
    Decode.map DynamoVertexDataItems (Decode.field "L" (Decode.list dynamoVertexDataInnerInnerDecoder))


dynamoVertexDataInnerInnerDecoder : Decode.Decoder DynamoVertexDataItem
dynamoVertexDataInnerInnerDecoder =
    Decode.map DynamoVertexDataItem (Decode.field "M" vertexDataInnerResponseDecoder)
