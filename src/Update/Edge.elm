module Update.Edge exposing (updateWithEdgeDataResponse)

import Http
import Http.Edge exposing (DynamoEdgeData, EdgeDataResponse, buildEdgeDataRequest, edgeDataPost)
import Model.EdgeData as EdgeData exposing (EdgeData)
import Model.Model exposing (Model)
import Model.PageCount
import Model.State exposing (State(..))
import Model.VertexData as VertexData exposing (VertexData)
import Model.Zipped as Zipped exposing (Zipped)
import Msg.Msg exposing (Msg(..))
import Update.Generic exposing (unpackDynamoValue)


updateWithEdgeDataResponse : Model -> Result Http.Error EdgeDataResponse -> ( Model, Cmd Msg )
updateWithEdgeDataResponse model result =
    case result of
        Ok response ->
            let
                unpack : EdgeDataResponse -> List EdgeData
                unpack edgeDataResponse =
                    List.map unpack_ edgeDataResponse.responses.items

                unpack_ : DynamoEdgeData -> EdgeData
                unpack_ dynamoEdgeData =
                    EdgeData
                        (unpackDynamoValue dynamoEdgeData.src_id)
                        (unpackDynamoValue dynamoEdgeData.dst_id)
                        (unpackDynamoValue dynamoEdgeData.num_transactions)
                        (unpackDynamoValue dynamoEdgeData.total_spend)
                        (unpackDynamoValue dynamoEdgeData.avg_spend)
                        (unpackDynamoValue dynamoEdgeData.max_spend)
                        (unpackDynamoValue dynamoEdgeData.min_spend)

                edges : List EdgeData
                edges =
                    unpack response

                incrementedTotalEdges : List EdgeData
                incrementedTotalEdges =
                    model.edge_data_response ++ edges

                semi : Model
                semi =
                    { model | edge_data_response = incrementedTotalEdges }
            in
            case model.page_count of
                Just pageCount ->
                    case pageCount.edge_data.pending of
                        head :: tail ->
                            ( { semi
                                | page_count =
                                    Just
                                        { pageCount
                                            | edge_data = Model.PageCount.EdgeDataPageCount (pageCount.edge_data.made ++ [ head ]) tail
                                        }
                              }
                            , edgeDataPost
                                (buildEdgeDataRequest model.direction_selected [ head ])
                                EdgeDataPostReceived
                            )

                        [] ->
                            let
                                edgeDataResponse : List EdgeData
                                edgeDataResponse =
                                    List.map EdgeData.format semi.edge_data_response

                                traversalDataResponse : List VertexData
                                traversalDataResponse =
                                    (\vp -> vp.vertices) (VertexData.distinct semi.traversal_data_response)

                                zipped : List Zipped
                                zipped =
                                    Zipped.zipVerticesAndEdges
                                        model.direction_selected
                                        model.traversal_response
                                        traversalDataResponse
                                        edgeDataResponse
                            in
                            ( { semi
                                | state = VertexRequestsSuccess False
                                , edge_data_response = edgeDataResponse
                                , traversal_data_response = traversalDataResponse
                                , zipped = Zipped.sortBy model.sort_by_selected zipped
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )
