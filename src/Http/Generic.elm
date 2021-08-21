module Http.Generic exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type alias DynamoVertexData =
    { uid : DynamoNumber
    , name : DynamoString
    , alternate_names : DynamoArrayString
    , is_committee : DynamoBool
    , address : DynamoMapAddress
    , alternate_addresses : DynamoArrayAddress
    }


type alias DynamoMapAddress =
    { map : DynamoAddress }


type alias DynamoAddress =
    { alternate_street : DynamoNullableString
    , city : DynamoNullableString
    , state : DynamoNullableString
    , street : DynamoNullableString
    , zip_code : DynamoNullableString
    }


type alias DynamoArrayAddress =
    { list : List DynamoMapAddress }


type alias DynamoVertexDataItems =
    { items : List DynamoVertexDataItem }


type alias DynamoVertexDataItem =
    { item : DynamoVertexData }


type alias DynamoArrayString =
    { list : List DynamoString }


type alias DynamoArrayNumber =
    { list : List DynamoNumber }


type alias DynamoString =
    { value : String }


type alias DynamoNullableString =
    { value : Maybe String }


type alias DynamoNumber =
    { value : Int }


type alias DynamoNumberLowLevel =
    { value : String }


type alias DynamoBool =
    { value : Bool }


type alias DynamoNull =
    { value : Bool }


dynamoArrayStringDecoder : Decode.Decoder DynamoArrayString
dynamoArrayStringDecoder =
    Decode.map DynamoArrayString (Decode.field "L" (Decode.list dynamoStringDecoder))


dynamoArrayNumberDecoder : Decode.Decoder DynamoArrayNumber
dynamoArrayNumberDecoder =
    Decode.map DynamoArrayNumber (Decode.field "L" (Decode.list dynamoNumberDecoder))


dynamoArrayAddressDecoder : Decode.Decoder DynamoArrayAddress
dynamoArrayAddressDecoder =
    Decode.map DynamoArrayAddress (Decode.field "L" (Decode.list dynamoMapAddressDecoder))


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


dynamoNullDecoder : Decode.Decoder DynamoNull
dynamoNullDecoder =
    Decode.map DynamoNull (Decode.field "NULL" Decode.bool)


dynamoNullableStringDecoder : Decode.Decoder DynamoNullableString
dynamoNullableStringDecoder =
    Decode.oneOf
        [ dynamoStringDecoder
            |> Decode.andThen (\ds -> Decode.succeed (DynamoNullableString (Just ds.value)))
        , dynamoNullDecoder
            |> Decode.andThen (\_ -> Decode.succeed (DynamoNullableString Nothing))
        ]


dynamoMapAddressDecoder : Decode.Decoder DynamoMapAddress
dynamoMapAddressDecoder =
    Decode.map DynamoMapAddress (Decode.field "M" dynamoAddressDecoder)


dynamoAddressDecoder : Decode.Decoder DynamoAddress
dynamoAddressDecoder =
    Decode.map5 DynamoAddress
        (Decode.field "alternate_street" dynamoNullableStringDecoder)
        (Decode.field "city" dynamoNullableStringDecoder)
        (Decode.field "state" dynamoNullableStringDecoder)
        (Decode.field "street" dynamoNullableStringDecoder)
        (Decode.field "zip_code" dynamoNullableStringDecoder)


dynamoVertexDataDecoder : Decode.Decoder DynamoVertexData
dynamoVertexDataDecoder =
    Decode.map6 DynamoVertexData
        (Decode.field "uid" dynamoNumberDecoder)
        (Decode.field "name" dynamoStringDecoder)
        (Decode.field "alternate_names" dynamoArrayStringDecoder)
        (Decode.field "is_committee" dynamoBoolDecoder)
        (Decode.field "address" dynamoMapAddressDecoder)
        (Decode.field "alternate_addresses" dynamoArrayAddressDecoder)
