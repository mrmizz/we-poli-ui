module Update.Generic exposing (unpackDynamoString, unpackDynamoVertexData, unpackDynamoNumber, unpackDynamoArrayNumber)

import Http.Generic exposing (DynamoArrayNumber, DynamoBool, DynamoNumber, DynamoString, DynamoVertexData)
import Model.VertexData exposing (VertexData)


unpackDynamoString : DynamoString -> String
unpackDynamoString dynamoString =
    dynamoString.value

unpackDynamoNumber: DynamoNumber -> Int
unpackDynamoNumber dynamoNumber =
    dynamoNumber.value


unpackDynamoBool : DynamoBool -> Bool
unpackDynamoBool dynamoBool =
    dynamoBool.value

unpackDynamoArrayNumber: DynamoArrayNumber -> List Int
unpackDynamoArrayNumber dynamoArrayNumber =
    List.map unpackDynamoNumber dynamoArrayNumber.list


unpackDynamoVertexData : DynamoVertexData -> VertexData
unpackDynamoVertexData dynamoVertexData =
    VertexData
        (unpackDynamoNumber dynamoVertexData.uid)
        (unpackDynamoString dynamoVertexData.name)
        (unpackDynamoBool dynamoVertexData.is_committee)
        (List.map unpackDynamoString dynamoVertexData.cities.list)
        (List.map unpackDynamoString dynamoVertexData.streets.list)
        (List.map unpackDynamoString dynamoVertexData.states.list)
