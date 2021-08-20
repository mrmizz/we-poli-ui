module Update.Vertex exposing (updateWithVertexDataResponse)

import Http
import Http.Edge exposing (buildEdgeDataRequest, edgeDataPost)
import Http.Vertex exposing (VertexDataResponse, buildVertexDataRequest, vertexDataPost)
import Model.Model exposing (Model)
import Model.PageCount
import Model.State exposing (State(..))
import Model.VertexData exposing (VertexData)
import Msg.Msg exposing (Msg(..))
import Update.Generic exposing (unpackDynamoVertexData)


updateWithVertexDataResponse : Model -> Result Http.Error VertexDataResponse -> ( Model, Cmd Msg )
updateWithVertexDataResponse model result =
    case result of
        Ok response ->
            let
                unpack : VertexDataResponse -> List VertexData
                unpack vertexDataResponse =
                    List.map unpackDynamoVertexData vertexDataResponse.responses.items

                vertices : List VertexData
                vertices =
                    unpack response

                incrementedTotalVertices : List VertexData
                incrementedTotalVertices =
                    model.traversal_data_response ++ vertices
            in
            case model.page_count of
                Just pageCount ->
                    case pageCount.vertex_data.pending of
                        head :: tail ->
                            ( { model
                                | traversal_data_response = incrementedTotalVertices
                                , page_count =
                                    Just
                                        { pageCount
                                            | vertex_data = Model.PageCount.VertexDataPageCount (pageCount.vertex_data.made ++ [ head ]) tail
                                        }
                              }
                            , vertexDataPost
                                (buildVertexDataRequest head.dst_ids)
                                VertexDataPostReceived
                            )

                        [] ->
                            case model.traversal_response of
                                head :: tail ->
                                    ( { model
                                        | traversal_data_response = incrementedTotalVertices
                                        , page_count =
                                            Just
                                                { pageCount
                                                    | edge_data = Model.PageCount.EdgeDataPageCount [ head ] tail
                                                }
                                      }
                                    , edgeDataPost
                                        (buildEdgeDataRequest model.direction_selected [ head ])
                                        EdgeDataPostReceived
                                    )

                                [] ->
                                    ( { model | state = DataIntegrityFailure }, Cmd.none )

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )
