module Update.NamePrefix exposing (updateWithVertexNamePrefixRequest, updateWithVertexNamePrefixResponse)

import Http
import Http.Generic exposing (DynamoVertexDataItem, DynamoVertexDataItems)
import Http.NamePrefix exposing (VertexNamePrefixResponse, vertexNamePrefixGet)
import Model.Direction as Direction
import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Model.VertexData as VertexData exposing (VertexData)
import Msg.Msg exposing (Msg(..))
import Update.Generic exposing (unpackDynamoVertexData)
import Util.Util exposing (printBool)


updateWithVertexNamePrefixRequest : Model -> String -> ( Model, Cmd Msg )
updateWithVertexNamePrefixRequest model prefix =
    let
        clean : String -> String
        clean input =
            printBool (Direction.toIsCommittee model.direction_selected)
                |> String.append "_"
                |> String.append (String.replace " " "" input)
                |> String.toLower
    in
    case String.length prefix >= 3 of
        False ->
            ( { model | vertex_name_search = prefix }, Cmd.none )

        True ->
            ( { model | vertex_name_search = prefix }
            , vertexNamePrefixGet (clean prefix) VertexNamePrefixGetReceived
            )


updateWithVertexNamePrefixResponse : Model -> Result Http.Error VertexNamePrefixResponse -> ( Model, Cmd Msg )
updateWithVertexNamePrefixResponse model result =
    case result of
        Ok response ->
            case unpackVertexNamePrefixResponse response of
                [] ->
                    ( { model | vertex_name_search_response = [] }, Cmd.none )

                vertices ->
                    ( { model | vertex_name_search_response = VertexData.filterByDirection model.direction_selected vertices }
                    , Cmd.none
                    )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


unpackVertexNamePrefixResponse : VertexNamePrefixResponse -> List VertexData
unpackVertexNamePrefixResponse response =
    case List.head response.items of
        Just head ->
            unpackDynamoVertexDataInner head.vertices

        Nothing ->
            []


unpackDynamoVertexDataInner : DynamoVertexDataItems -> List VertexData
unpackDynamoVertexDataInner dynamoVertexDataInner =
    List.map unpackDynamoVertexDataInnerInner dynamoVertexDataInner.items


unpackDynamoVertexDataInnerInner : DynamoVertexDataItem -> VertexData
unpackDynamoVertexDataInnerInner dynamoVertexDataInnerInner =
    unpackDynamoVertexData dynamoVertexDataInnerInner.item
