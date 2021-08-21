module Http.Generic exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type alias DynamoVertexData =
    { name : DynamoString
    , uid : DynamoNumber
    , is_committee : DynamoBool
    , cities : DynamoArrayString
    , streets : DynamoArrayString
    , states : DynamoArrayString
    }


type alias DynamoVertexDataItems =
    { items : List DynamoVertexDataItem }


type alias DynamoVertexDataItem =
    { item : DynamoVertexData }


type alias DynamoArrayString =
    { list : List DynamoString }

type alias DynamoArrayNumber =
    { list: List DynamoNumber }


type alias DynamoString =
    { value : String }

type alias DynamoNumber =
    { value : Int }


type alias DynamoBool =
    { value : Bool }


dynamoArrayStringDecoder : Decode.Decoder DynamoArrayString
dynamoArrayStringDecoder =
    Decode.map DynamoArrayString (Decode.field "L" (Decode.list dynamoStringDecoder))


dynamoArrayNumberDecoder : Decode.Decoder DynamoArrayNumber
dynamoArrayNumberDecoder =
    Decode.map DynamoArrayNumber (Decode.field "L" (Decode.list dynamoNumberDecoder))


dynamoNumberDecoder : Decode.Decoder DynamoNumber
dynamoNumberDecoder =
    Decode.map DynamoNumber (Decode.field "N" Decode.int)


dynamoNumberEncoder : DynamoNumber -> Encode.Value
dynamoNumberEncoder dynamoValue =
    Encode.object
        [ ( "N", Encode.int dynamoValue.value ) ]


dynamoStringDecoder : Decode.Decoder DynamoString
dynamoStringDecoder =
    Decode.map DynamoString (Decode.field "S" Decode.string)


dynamoBoolDecoder : Decode.Decoder DynamoBool
dynamoBoolDecoder =
    Decode.map DynamoBool (Decode.field "BOOL" Decode.bool)


vertexDataInnerResponseDecoder : Decode.Decoder DynamoVertexData
vertexDataInnerResponseDecoder =
    Decode.map6 DynamoVertexData
        (Decode.field "name" dynamoStringDecoder)
        (Decode.field "uid" dynamoNumberDecoder)
        (Decode.field "is_committee" dynamoBoolDecoder)
        (Decode.field "cities" dynamoArrayStringDecoder)
        (Decode.field "streets" dynamoArrayStringDecoder)
        (Decode.field "states" dynamoArrayStringDecoder)
