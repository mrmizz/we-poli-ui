module Update.Generic exposing (unpackDynamoValue, unpackDynamoVertexData)

import Http.Generic exposing (DynamoBool, DynamoValue, DynamoVertexData)
import Model.VertexData exposing (VertexData)


unpackDynamoValue : DynamoValue -> String
unpackDynamoValue dynamoValue =
    dynamoValue.value


unpackDynamoBool : DynamoBool -> Bool
unpackDynamoBool dynamoBool =
    dynamoBool.value


unpackDynamoVertexData : DynamoVertexData -> VertexData
unpackDynamoVertexData dynamoVertexData =
    VertexData
        (unpackDynamoValue dynamoVertexData.uid)
        (unpackDynamoValue dynamoVertexData.name)
        (unpackDynamoBool dynamoVertexData.is_committee)
        (List.map unpackDynamoValue dynamoVertexData.cities.list)
        (List.map unpackDynamoValue dynamoVertexData.streets.list)
        (List.map unpackDynamoValue dynamoVertexData.states.list)
