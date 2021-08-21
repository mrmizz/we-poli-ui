module Http.NamePrefix exposing (VertexNamePrefixResponse, vertexNamePrefixGet)

import Http
import Http.Generic exposing (DynamoArrayNumber, DynamoNumber, DynamoString, dynamoArrayNumberDecoder, dynamoNumberDecoder, dynamoStringDecoder)
import Http.Url exposing (prefixURL)
import Json.Decode as Decode


type alias VertexNamePrefixResponse =
    { items : List VertexNamePrefixInnerResponse }


type alias VertexNamePrefixInnerResponse =
    { prefix : DynamoString
    , prefix_size : DynamoNumber
    , vertexIds : DynamoArrayNumber
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
        (Decode.field "prefix" dynamoStringDecoder)
        (Decode.field "prefix_size" dynamoNumberDecoder)
        (Decode.field "vertexIds" dynamoArrayNumberDecoder)

