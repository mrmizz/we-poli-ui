module Update.Update exposing (update)

import Http
import Http.Edge exposing (DynamoEdgeData, EdgeDataResponse, buildEdgeDataRequest, edgeDataPost)
import Http.Generic exposing (DynamoBool, DynamoValue, DynamoVertexData, DynamoVertexDataItem, DynamoVertexDataItems)
import Http.NamePrefix exposing (VertexNamePrefixResponse, vertexNamePrefixGet)
import Http.PageCount exposing (DynamoPageCount, PageCountResponse, buildPageCountRequest, pageCountPost)
import Http.Traversal exposing (DynamoTraversal, TraversalResponse, buildTraversalRequest, traversalPost)
import Http.Vertex exposing (VertexDataResponse, buildVertexDataRequest, vertexDataPost)
import Model.Aggregation exposing (Aggregation(..))
import Model.Direction as Direction exposing (Direction(..))
import Model.EdgeData as EdgeData exposing (EdgeData)
import Model.Model exposing (Model, initialModel)
import Model.PageCount exposing (EdgeDataPageCount, PageCount, TraversalsPageCount, VertexDataPageCount)
import Model.State exposing (State(..))
import Model.Traversal exposing (Traversal, TraversalPage)
import Model.VertexData as VertexData exposing (VertexData)
import Model.Zipped as Zipped
import Msg.Msg exposing (Msg(..), resetViewport)
import Util.Util exposing (printBool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SearchInput prefix ->
            updateWithVertexNamePrefixRequest model prefix

        VertexNamePrefixGetReceived result ->
            updateWithVertexNamePrefixResponse model result

        TraversalRequestMade ->
            updateWithPageCountRequest model

        ChildTraversalRequestMade vertexData ->
            updateWithChildPageCountRequest model vertexData

        PageCountPostReceived result ->
            updateWithPageCountResponse model result

        TraversalPostReceived result ->
            updateWithTraversalResponse model result

        VertexDataPostReceived result ->
            updateWithVertexDataResponse model result

        EdgeDataPostReceived result ->
            updateWithEdgeDataResponse model result

        ClearSearch ->
            ( initialModel, Cmd.none )

        EditSearch ->
            ( { model | state = BuildingRequest }, Cmd.none )

        ConfirmSearch ->
            ( { model | state = SearchConfirmed }, Cmd.none )

        AggOptionSelected ->
            updateWithAggOption model

        DirectionOptionSelected ->
            updateWithDirectionOption model

        VertexSelected vertex ->
            ( { model | vertices_selected = updateVertexSelected vertex model.vertices_selected }, resetViewport )

        DeleteVertexSelection vertex ->
            ( { model | vertices_selected = updateVertexDeleted vertex model.vertices_selected }, Cmd.none )

        SortByOptionSelected sortBy ->
            ( { model
                | sort_by_selected = sortBy
                , zipped = Zipped.sortBy sortBy model.zipped
              }
            , Cmd.none
            )


updateWithDirectionOption : Model -> ( Model, Cmd Msg )
updateWithDirectionOption model =
    case model.direction_selected of
        In ->
            ( { model
                | direction_selected = Out
                , vertices_selected = []
                , vertex_name_search_response = []
                , vertex_name_search = ""
              }
            , Cmd.none
            )

        Out ->
            ( { model
                | direction_selected = In
                , vertices_selected = []
                , vertex_name_search_response = []
                , vertex_name_search = ""
              }
            , Cmd.none
            )


cleanVertexNameInput : String -> Model -> String
cleanVertexNameInput input model =
    printBool (Direction.toIsCommittee model.direction_selected)
        |> String.append "_"
        |> String.append (String.replace " " "" input)
        |> String.toLower


updateVertexSelected : VertexData -> List VertexData -> List VertexData
updateVertexSelected vertex vertices =
    (\gp -> gp.vertices) (VertexData.distinct (List.singleton vertex ++ vertices))


updateVertexDeleted : VertexData -> List VertexData -> List VertexData
updateVertexDeleted vertex vertices =
    case vertices of
        _ :: [] ->
            []

        _ ->
            List.filter (VertexData.notUID vertex.uid) vertices


updateWithVertexNamePrefixRequest : Model -> String -> ( Model, Cmd Msg )
updateWithVertexNamePrefixRequest model prefix =
    case String.length prefix >= 3 of
        False ->
            ( { model | vertex_name_search = prefix }, Cmd.none )

        True ->
            ( { model | vertex_name_search = prefix }
            , vertexNamePrefixGet (cleanVertexNameInput prefix model) VertexNamePrefixGetReceived
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


unpackDynamoVertexData : DynamoVertexData -> VertexData
unpackDynamoVertexData dynamoVertexData =
    VertexData
        (unpackDynamoValue dynamoVertexData.uid)
        (unpackDynamoValue dynamoVertexData.name)
        (unpackDynamoBool dynamoVertexData.is_committee)
        (List.map unpackDynamoValue dynamoVertexData.cities.list)
        (List.map unpackDynamoValue dynamoVertexData.streets.list)
        (List.map unpackDynamoValue dynamoVertexData.states.list)


unpackDynamoValue : DynamoValue -> String
unpackDynamoValue dynamoValue =
    dynamoValue.value


unpackDynamoBool : DynamoBool -> Bool
unpackDynamoBool dynamoBool =
    dynamoBool.value


updateWithPageCountRequest : Model -> ( Model, Cmd Msg )
updateWithPageCountRequest model =
    ( { model
        | state = Loading
        , traversal_response = []
        , traversal_data_response = []
        , edge_data_response = []
        , zipped = []
        , page_count = Nothing
      }
    , pageCountPost PageCountPostReceived (buildPageCountRequest (List.map (\v -> v.uid) model.vertices_selected))
    )


updateWithChildPageCountRequest : Model -> VertexData -> ( Model, Cmd Msg )
updateWithChildPageCountRequest model vertexData =
    ( { model
        | state = Loading
        , vertices_selected = [ vertexData ]
        , traversal_response = []
        , traversal_data_response = []
        , edge_data_response = []
        , zipped = []
        , page_count = Nothing
        , direction_selected = Direction.switch model.direction_selected
      }
    , pageCountPost PageCountPostReceived (buildPageCountRequest [ (\v -> v.uid) vertexData ])
    )


updateWithPageCountResponse : Model -> Result Http.Error PageCountResponse -> ( Model, Cmd Msg )
updateWithPageCountResponse model result =
    case result of
        Ok response ->
            let
                pageCount : PageCount
                pageCount =
                    unpackPageCountResponse response
            in
            case pageCount.traversals.pending of
                head :: tail ->
                    ( { model | page_count = Just { pageCount | traversals = TraversalsPageCount [ head ] tail } }
                    , traversalPost
                        (buildTraversalRequest [ head ])
                        TraversalPostReceived
                    )

                [] ->
                    ( { model | page_count = Just pageCount, state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


unpackPageCountResponse : PageCountResponse -> PageCount
unpackPageCountResponse pageCountResponse =
    let
        unpackDynamoPageCount : DynamoPageCount -> List TraversalPage
        unpackDynamoPageCount dynamoPageCount =
            case String.toInt (unpackDynamoValue dynamoPageCount.page_count) of
                Just int ->
                    List.range 1 int
                        |> List.map String.fromInt
                        |> List.map (\pageNum -> TraversalPage (unpackDynamoValue dynamoPageCount.vertex_id) pageNum)

                Nothing ->
                    []
    in
    PageCount
        (TraversalsPageCount [] (List.concatMap unpackDynamoPageCount pageCountResponse.responses.items))
        (VertexDataPageCount [] [])
        (EdgeDataPageCount [] [])


unpackVertexDataResponse : VertexDataResponse -> List VertexData
unpackVertexDataResponse vertexDataResponse =
    List.map unpackDynamoVertexData vertexDataResponse.responses.items


updateWithAggOption : Model -> ( Model, Cmd Msg )
updateWithAggOption model =
    case model.aggregation_selected of
        And ->
            ( { model | aggregation_selected = Or }, Cmd.none )

        Or ->
            ( { model | aggregation_selected = And }, Cmd.none )


updateWithTraversalResponse : Model -> Result Http.Error TraversalResponse -> ( Model, Cmd Msg )
updateWithTraversalResponse model result =
    case result of
        Ok response ->
            let
                traversals : List Traversal
                traversals =
                    unpackTraversalResponse response

                incrementedTotalTraversals : List Traversal
                incrementedTotalTraversals =
                    model.traversal_response ++ traversals
            in
            case model.page_count of
                Just pageCount ->
                    case pageCount.traversals.pending of
                        head :: tail ->
                            ( { model
                                | traversal_response = incrementedTotalTraversals
                                , page_count =
                                    Just
                                        { pageCount
                                            | traversals = TraversalsPageCount (pageCount.traversals.made ++ [ head ]) tail
                                        }
                              }
                            , traversalPost
                                (buildTraversalRequest [ head ])
                                TraversalPostReceived
                            )

                        [] ->
                            ( { model
                                | traversal_response = incrementedTotalTraversals
                                , page_count =
                                    Just
                                        { pageCount
                                            | vertex_data = VertexDataPageCount traversals model.traversal_response
                                        }
                              }
                            , vertexDataPost
                                (buildVertexDataRequest (List.concatMap (\trv -> trv.dst_ids) traversals))
                                VertexDataPostReceived
                            )

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


updateWithVertexDataResponse : Model -> Result Http.Error VertexDataResponse -> ( Model, Cmd Msg )
updateWithVertexDataResponse model result =
    case result of
        Ok response ->
            let
                vertices : List VertexData
                vertices =
                    unpackVertexDataResponse response

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
                                            | vertex_data = VertexDataPageCount (pageCount.vertex_data.made ++ [ head ]) tail
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
                                                    | edge_data = EdgeDataPageCount [ head ] tail
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


unpackTraversalResponse : TraversalResponse -> List Traversal
unpackTraversalResponse traversalResponse =
    List.map unpackDynamoTraversal traversalResponse.responses.items


unpackDynamoTraversal : DynamoTraversal -> Traversal
unpackDynamoTraversal dynamoTraversal =
    Traversal
        (unpackDynamoValue dynamoTraversal.vertex_id)
        (List.map unpackDynamoValue dynamoTraversal.related_vertex_ids.list)


updateWithEdgeDataResponse : Model -> Result Http.Error EdgeDataResponse -> ( Model, Cmd Msg )
updateWithEdgeDataResponse model result =
    case result of
        Ok response ->
            let
                edges : List EdgeData
                edges =
                    unpackEdgeDataResponse response

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
                                            | edge_data = EdgeDataPageCount (pageCount.edge_data.made ++ [ head ]) tail
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
                            in
                            ( { semi
                                | state = VertexRequestsSuccess
                                , edge_data_response = edgeDataResponse
                                , traversal_data_response = traversalDataResponse
                                , zipped =
                                    Zipped.zipVerticesAndEdges
                                        model.direction_selected
                                        model.traversal_response
                                        traversalDataResponse
                                        edgeDataResponse
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


unpackEdgeDataResponse : EdgeDataResponse -> List EdgeData
unpackEdgeDataResponse edgeDataResponse =
    List.map unpackDynamoEdgeData edgeDataResponse.responses.items


unpackDynamoEdgeData : DynamoEdgeData -> EdgeData
unpackDynamoEdgeData dynamoEdgeData =
    EdgeData
        (unpackDynamoValue dynamoEdgeData.src_id)
        (unpackDynamoValue dynamoEdgeData.dst_id)
        (unpackDynamoValue dynamoEdgeData.num_transactions)
        (unpackDynamoValue dynamoEdgeData.total_spend)
        (unpackDynamoValue dynamoEdgeData.avg_spend)
        (unpackDynamoValue dynamoEdgeData.max_spend)
        (unpackDynamoValue dynamoEdgeData.min_spend)
