module Update.Vertex exposing (updateWithVertexDataResponse)

import Http exposing (Error(..))
import Http.Vertex exposing (VertexDataResponse)
import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Model.Traversal as Traversal exposing (Traversal(..))
import Model.VertexData exposing (VertexData)
import Model.VertexNameSearch
import Model.Zipped as Zipped
import Msg.Msg exposing (Msg(..), VertexDataClient(..))
import Update.Generic exposing (unpackListDynamoVertexData)


updateWithVertexDataResponse : Model -> VertexDataClient -> Result Http.Error VertexDataResponse -> ( Model, Cmd Msg )
updateWithVertexDataResponse model client result =
    case result of
        Ok response ->
            let
                unpack : VertexDataResponse -> Maybe (List VertexData)
                unpack vertexDataResponse =
                    unpackListDynamoVertexData vertexDataResponse.responses.items

            in
            case unpack response of
                Just vertices ->
                    case client of
                        ForNameSearch ->
                            updateForNameSearch model vertices

                        ForTraversal ->
                            updateForTraversal model vertices

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )


        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )

updateForNameSearch : Model -> List VertexData -> ( Model, Cmd Msg )
updateForNameSearch model vertices =
    let
        old =
            model.vertex_name_search
    in
    ( { model
        | vertex_name_search = { old | vertices = vertices }
        }
    , Cmd.none
     )


updateForTraversal : Model -> List VertexData -> ( Model, Cmd Msg )
updateForTraversal model vertices =
    case model.traversal of
        Waiting pageCount ->
            ( { model | traversal = WaitingForEdges pageCount vertices }
            , Cmd.none
            )


        WaitingForVertices pageCount edges ->
            ( { model
                | state = VertexRequestsSuccess False
                , traversal = Traversal.Done pageCount
                , zipped = Zipped.zip model.direction_selected vertices edges
             }
            , Cmd.none
            )

        _ ->
            ( { model | state = DataIntegrityFailure }, Cmd.none )