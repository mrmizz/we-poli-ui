module Update.Generic exposing (unpackDynamoArrayNumber, unpackDynamoNumber, unpackDynamoString, unpackDynamoVertexData, unpackDynamoNumberAsInt)

import Http.Generic exposing (DynamoAddress, DynamoArrayAddress, DynamoArrayNumber, DynamoArrayString, DynamoBool, DynamoMapAddress, DynamoNullableString, DynamoNumber, DynamoNumberAsInt, DynamoString, DynamoVertexData)
import Model.VertexData exposing (Address, VertexData)


unpackDynamoString : DynamoString -> String
unpackDynamoString dynamoString =
    dynamoString.value


unpackDynamoNumber : DynamoNumber -> String
unpackDynamoNumber dynamoNumber =
    dynamoNumber.value

unpackDynamoNumberAsInt : DynamoNumberAsInt -> Int
unpackDynamoNumberAsInt dynamoNumberAsInt =
    dynamoNumberAsInt.value


unpackDynamoBool : DynamoBool -> Bool
unpackDynamoBool dynamoBool =
    dynamoBool.value


unpackDynamoNullableString : DynamoNullableString -> Maybe String
unpackDynamoNullableString dynamoNullableString =
    dynamoNullableString.value


unpackDynamoAddress : DynamoMapAddress -> Address
unpackDynamoAddress dynamoAddress =
    { alternate_street = unpackDynamoNullableString dynamoAddress.map.alternate_street
    , city = unpackDynamoNullableString dynamoAddress.map.city
    , state = unpackDynamoNullableString dynamoAddress.map.state
    , street = unpackDynamoNullableString dynamoAddress.map.street
    , zip_code = unpackDynamoNullableString dynamoAddress.map.zip_code
    }


unpackDynamoArrayNumber : DynamoArrayNumber -> List String
unpackDynamoArrayNumber dynamoArrayNumber =
    List.map unpackDynamoNumber dynamoArrayNumber.list


unpackDynamoArrayString : DynamoArrayString -> List String
unpackDynamoArrayString dynamoArrayString =
    List.map unpackDynamoString dynamoArrayString.list


unpackDynamoArrayAddress : DynamoArrayAddress -> List Address
unpackDynamoArrayAddress dynamoArrayAddress =
    List.map unpackDynamoAddress dynamoArrayAddress.list


unpackDynamoVertexData : DynamoVertexData -> VertexData
unpackDynamoVertexData dynamoVertexData =
    { uid = unpackDynamoNumber dynamoVertexData.uid
    , name = unpackDynamoString dynamoVertexData.name
    , alternate_names = unpackDynamoArrayString dynamoVertexData.alternate_names
    , is_committee = unpackDynamoBool dynamoVertexData.is_committee
    , address = unpackDynamoAddress dynamoVertexData.address
    , alternate_addresses = unpackDynamoArrayAddress dynamoVertexData.alternate_addresses
    }
