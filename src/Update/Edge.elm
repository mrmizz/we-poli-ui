module Update.Edge exposing (updateWithEdgeDataResponse)

import Http
import Http.Edge exposing (DynamoEdgeData, EdgeDataResponse)
import Model.EdgeData exposing (EdgeData)
import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Model.Traversal as Traversal exposing (Traversal(..))
import Model.Zipped as Zipped exposing (Zipped)
import Msg.Msg exposing (Msg(..))
import Update.Generic exposing (unpackListDynamoEdgeData)


updateWithEdgeDataResponse : Model -> Result Http.Error EdgeDataResponse -> ( Model, Cmd Msg )
updateWithEdgeDataResponse model result =
    case result of
        Ok response ->
            let
                maybeEdges : Maybe (List EdgeData)
                maybeEdges =
                    unpackListDynamoEdgeData response.responses.poli_edge
            in
            case maybeEdges of
                Just edges ->
                    case model.traversal of
                        Waiting pageCount ->
                            ( { model | traversal = WaitingForVertices pageCount edges }
                            , Cmd.none
                            )

                        WaitingForEdges pageCount vertices ->
                            ( { model
                            | state = VertexRequestsSuccess False
                            , traversal = Traversal.Done pageCount
                            , zipped = Zipped.zip model.direction_selected vertices edges
                            }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | state = DataIntegrityFailure }, Cmd.none )

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )


        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )
