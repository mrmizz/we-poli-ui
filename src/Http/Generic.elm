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
    { value: Int }

type alias DynamoNumberLowLevel =
    { value : String }


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
    let
        decode_ : DynamoNumberLowLevel -> Decode.Decoder DynamoNumber
        decode_ lowLevel =
            case String.toInt lowLevel.value of
                Just int ->
                    Decode.succeed (DynamoNumber int)

                Nothing ->
                    Decode.fail ("error parsing string: " ++ lowLevel.value)

    in
    Decode.map DynamoNumberLowLevel (Decode.field "N" Decode.string)
        |> Decode.andThen decode_


dynamoNumberEncoder : DynamoNumber -> Encode.Value
dynamoNumberEncoder dynamoValue =
    Encode.object
        [ ( "N", Encode.string (String.fromInt dynamoValue.value) ) ]


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
