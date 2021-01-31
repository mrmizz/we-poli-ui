module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Http.Edge exposing (DynamoEdgeData, EdgeDataResponse, buildEdgeDataRequest, edgeDataPost)
import Http.Generic exposing (DynamoBool, DynamoValue, DynamoVertexData, DynamoVertexDataItem, DynamoVertexDataItems)
import Http.NamePrefix exposing (VertexNamePrefixResponse, vertexNamePrefixGet)
import Http.PageCount exposing (DynamoPageCount, PageCountResponse, buildPageCountRequest, pageCountPost)
import Http.Traversal exposing (DynamoTraversal, TraversalResponse, buildTraversalRequest, traversalPost)
import Http.Vertex exposing (VertexDataResponse, buildVertexDataRequest, vertexDataPost)
import List
import Model.Aggregation as Aggregation exposing (Aggregation(..))
import Model.Direction as Direction exposing (Direction(..))
import Model.EdgeData as EdgeData exposing (EdgeData)
import Model.Model as Model exposing (Model, initialModel)
import Model.PageCount exposing (..)
import Model.SortBy exposing (SortBy(..))
import Model.State exposing (State(..))
import Model.Traversal exposing (Traversal, TraversalPage)
import Model.VertexData as VertexData exposing (VertexData)
import Model.Zipped as Zipped exposing (Zipped)
import Msg.Msg exposing (Msg(..))
import Set exposing (Set)
import Util.Util exposing (printBool)
import View.Loading



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = Model.init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- TODO: evaluate Edge Triplet request instead Traversal, Vertex, then Edge Requests
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model | vertices_selected = updateVertexSelected vertex model.vertices_selected }, Cmd.none )

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



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] (elementView model)


elementView : Model -> Element Msg
elementView model =
    case model.state of
        BuildingRequest ->
            viewBuildingRequest model

        SearchConfirmed ->
            viewSearchConfirmed model

        Loading ->
            View.Loading.view

        VertexRequestsSuccess ->
            viewRequestSuccess
                model.direction_selected
                model

        RequestFailure error ->
            viewRequestFailure error

        DataIntegrityFailure ->
            Element.text "Data Integrity Failure"


viewSearchConfirmed : Model -> Element Msg
viewSearchConfirmed model =
    almostDropdownHeadAndBody (dropdownHead model)
        [ makeVertexIdsRequestButton
        , editSearchButton
        , clearSearchButton
        , viewAggParam model.aggregation_selected
        , viewVerticesConfirmed model.vertices_selected
        ]


viewVerticesConfirmed : List VertexData -> Element Msg
viewVerticesConfirmed vertices =
    Element.column []
        [ Element.text "Queued for Search: ", buildVerticesConfirmedView vertices ]


buildVerticesConfirmedView : List VertexData -> Element Msg
buildVerticesConfirmedView vertices =
    fromVerticesToTable vertices


viewVertexNamePrefixResponse : Model -> Element Msg
viewVertexNamePrefixResponse model =
    Element.column []
        [ Element.text "Potential Search Matches:", buildPotentialSearchMatchView model.vertex_name_search_response ]


buildPotentialSearchMatchView : List VertexData -> Element Msg
buildPotentialSearchMatchView vertices =
    fromVerticesToTableWithSelectVertexButton vertices


viewVerticesSelected : Model -> Element Msg
viewVerticesSelected model =
    Element.column []
        [ Element.text "We're Searching For:", buildVerticesSelectedView model.vertices_selected ]


buildVerticesSelectedView : List VertexData -> Element Msg
buildVerticesSelectedView vertices =
    fromVerticesToTableWithDeleteVertexButton vertices


viewBuildingRequest : Model -> Element Msg
viewBuildingRequest model =
    case model.vertex_name_search of
        "" ->
            viewBuildingRequestWithNoInputButMaybeSomeConfirmed model

        _ ->
            case model.vertices_selected of
                [] ->
                    buildBuildingRequestView model [ viewVertexNamePrefixResponse model ]

                _ ->
                    buildBuildingRequestView model
                        [ Element.el [] confirmSearchButton
                        , viewVerticesSelected model
                        , viewVertexNamePrefixResponse model
                        ]


viewNoInput : Model -> Element Msg
viewNoInput model =
    buildBuildingRequestView model []


viewBuildingRequestWithNoInputButMaybeSomeConfirmed : Model -> Element Msg
viewBuildingRequestWithNoInputButMaybeSomeConfirmed model =
    case model.vertices_selected of
        [] ->
            viewNoInput model

        _ ->
            buildBuildingRequestView model [ Element.column [] [ confirmSearchButton ], viewVerticesSelected model ]


viewRequestSuccess : Direction -> Model -> Element Msg
viewRequestSuccess direction model =
    almostDropdownHeadAndBody (dropdownHead model)
        [ makeVertexIdsRequestButton
        , editSearchButton
        , clearSearchButton
        , viewAggParam model.aggregation_selected
        , sortByRadio model
        , viewDirectedResponse model direction
        ]


viewRequestFailure : Http.Error -> Element Msg
viewRequestFailure error =
    case error of
        Http.BadUrl string ->
            Element.column []
                [ Element.text ("Bad Url: " ++ string)
                , requestFailureRow
                ]

        Http.Timeout ->
            Element.column []
                [ Element.text "Server Timeout"
                , requestFailureRow
                ]

        Http.NetworkError ->
            Element.column []
                [ Element.text "Network Error"
                , requestFailureRow
                ]

        Http.BadStatus int ->
            Element.column []
                [ Element.text (String.fromInt int ++ " Error: Bad Input")
                , requestFailureRow
                ]

        Http.BadBody body ->
            Element.column []
                [ Element.text ("Bad Body: " ++ body)
                , requestFailureRow
                ]


requestFailureRow : Element Msg
requestFailureRow =
    Element.row [ Element.spacing 12 ] [ clearSearchButton, editSearchButton, returnToSearchButton ]


viewAggParam : Aggregation -> Element Msg
viewAggParam agg =
    Element.row [] [ Element.text "Aggregation: ", viewAggButton agg ]


viewAggButton : Aggregation -> Element Msg
viewAggButton agg =
    Input.button [] { onPress = Just AggOptionSelected, label = buttonStyle (Element.text (Aggregation.print agg)) }


almostDropdownHeadAndBody : Element Msg -> List (Element Msg) -> Element Msg
almostDropdownHeadAndBody head body =
    Element.row backgroundStyle
        [ borderElement
        , Element.column dropdownStyle
            [ Element.el dropdownHeadStyle head
            , Element.column dropdownBodyStyle body
            ]
        , borderElement
        ]


borderElement : Element Msg
borderElement =
    Element.el [ Element.width (Element.fillPortion 1) ] Element.none


backgroundStyle : List (Element.Attribute Msg)
backgroundStyle =
    [ Background.color (Element.rgb255 50 125 200)
    , Font.color (Element.rgb255 250 250 250)
    , Element.width Element.fill
    , Element.height Element.fill
    , Border.innerGlow (Element.rgb255 20 50 100) 5
    ]


dropdownHeadStyle : List (Element.Attribute Msg)
dropdownHeadStyle =
    [ Element.width Element.fill
    , Element.padding 50
    , Font.size 35
    , Border.innerGlow (Element.rgb255 20 50 100) 5
    ]


dropdownBodyStyle : List (Element.Attribute Msg)
dropdownBodyStyle =
    [ Font.extraLight
    , Element.width Element.fill
    , Element.height Element.fill
    , Element.scrollbarX
    , Element.spacing 12
    , Element.paddingXY 100 50
    , Element.centerX
    , Border.glow (Element.rgb255 210 210 210) 1.5
    ]


dropdownStyle : List (Element.Attribute Msg)
dropdownStyle =
    [ Element.width (Element.fillPortion 10)
    , Element.height Element.fill
    , Border.glow (Element.rgb255 210 210 210) 1.5
    , Background.color (Element.rgb255 119 136 153)
    ]


almostDropdownHead : Model -> (Model -> Element Msg) -> Element Msg
almostDropdownHead model anotherElement =
    Element.row [ Element.centerX ]
        [ Element.text "Poli Graph Search: "
        , anotherElement model
        ]


dropdownHead : Model -> Element Msg
dropdownHead model =
    almostDropdownHead model (\_ -> directionOptionText model)


dropdownHeadWithDirectionButton : Model -> Element Msg
dropdownHeadWithDirectionButton model =
    almostDropdownHead model directionOptionButton


directedBuildingRequestDropdownBody : Model -> Element Msg -> List (Element Msg) -> Element Msg
directedBuildingRequestDropdownBody model head body =
    case model.direction_selected of
        In ->
            buildingRequestDropdownBody model "vendor name" head body

        Out ->
            buildingRequestDropdownBody model "committee name" head body


buildingRequestDropdownBody : Model -> String -> Element Msg -> List (Element Msg) -> Element Msg
buildingRequestDropdownBody model entityType head body =
    almostDropdownHeadAndBody head
        ([ Input.search [ Font.color (Element.rgb255 0 0 0) ]
            { onChange = SearchInput
            , text = model.vertex_name_search
            , placeholder = Just (Input.placeholder [] (Element.text entityType))
            , label = Input.labelHidden "hidden label"
            }
         ]
            ++ body
        )


buildBuildingRequestView : Model -> List (Element Msg) -> Element Msg
buildBuildingRequestView model body =
    directedBuildingRequestDropdownBody model (dropdownHeadWithDirectionButton model) body


directionOptionButton : Model -> Element Msg
directionOptionButton model =
    case model.direction_selected of
        In ->
            Input.button []
                { onPress = Just DirectionOptionSelected
                , label = buttonStyle (Element.text "Vendors")
                }

        Out ->
            Input.button []
                { onPress = Just DirectionOptionSelected
                , label = buttonStyle (Element.text "Committees")
                }


directionOptionText : Model -> Element Msg
directionOptionText model =
    case model.direction_selected of
        In ->
            Element.text "Vendors"

        Out ->
            Element.text "Committees"


makeVertexIdsRequestButton : Element Msg
makeVertexIdsRequestButton =
    Input.button [] { onPress = Just TraversalRequestMade, label = buttonStyle (Element.text "Search") }


clearSearchButton : Element Msg
clearSearchButton =
    Input.button [] { onPress = Just ClearSearch, label = buttonStyle (Element.text "Clear Search") }


editSearchButton : Element Msg
editSearchButton =
    Input.button [] { onPress = Just EditSearch, label = buttonStyle (Element.text "Edit Search") }


confirmSearchButton : Element Msg
confirmSearchButton =
    Input.button [] { onPress = Just ConfirmSearch, label = buttonStyle (Element.text "Confirm Search") }


returnToSearchButton : Element Msg
returnToSearchButton =
    Input.button [] { onPress = Just ConfirmSearch, label = buttonStyle (Element.text "Return To Existing Search") }


viewDirectedResponse : Model -> Direction -> Element Msg
viewDirectedResponse model direction =
    case direction of
        In ->
            viewDirectedResponseWithText model "Related Committees"

        Out ->
            viewDirectedResponseWithText model "Related Vendors"


viewDirectedResponseWithText : Model -> String -> Element Msg
viewDirectedResponseWithText model textToDisplay =
    Element.column [ Element.spacing 12 ]
        [ Element.column []
            [ Element.text "Searched: "
            , fromVerticesToTable model.vertices_selected
            ]
        , Element.column []
            [ Element.text textToDisplay
            , fromVerticesAndEdgesToTableWithSearchButton
                (Zipped.aggregate
                    model.aggregation_selected
                    model.traversal_response
                    model.zipped
                    |> Zipped.sortBy model.sort_by_selected
                )
            ]
        ]


fromVerticesToTable : List VertexData -> Element Msg
fromVerticesToTable vertices =
    Element.column tableStyle
        (List.map fromVertexDataToRow vertices)


tableStyle : List (Element.Attribute Msg)
tableStyle =
    [ Element.spacing 10 ]


fromVertexDataToRow : VertexData -> Element Msg
fromVertexDataToRow vertex =
    almostFromVertexDataToRow vertex []


almostFromVertexDataToRow : VertexData -> List (Element Msg) -> Element Msg
almostFromVertexDataToRow vertex moreElements =
    Element.row [ Element.spacing 10 ]
        (moreElements
            ++ [ Element.column []
                    [ uidColumn vertex
                    , Element.row []
                        [ isCommitteeAndStateColumn vertex
                        , nameAndCitiesColumn vertex
                        ]
                    ]
               ]
        )


columnStyle : List (Element.Attribute Msg)
columnStyle =
    [ Border.dotted
    , Border.rounded 10
    , Border.innerGlow (Element.rgb255 20 50 100) 5
    , Element.padding 5
    ]


uidColumn : VertexData -> Element Msg
uidColumn vertex =
    Element.column columnStyle
        [ Element.text "uid:"
        , Element.text vertex.uid
        ]


isCommitteeColumn : VertexData -> Element Msg
isCommitteeColumn vertex =
    Element.column columnStyle
        [ Element.text "is_committee:"
        , Element.text (printBool vertex.is_committee)
        ]


nameColumn : VertexData -> Element Msg
nameColumn vertex =
    Element.column columnStyle
        [ Element.text "name:"
        , Element.text vertex.name
        ]


citiesColumn : VertexData -> Element Msg
citiesColumn vertex =
    -- TODO: toLower & drop duplicates in the backend
    Element.column columnStyle
        [ Element.text "cities:"
        , Element.text
            (List.map String.toLower vertex.cities
                |> Set.fromList
                |> Set.toList
                |> String.join ", "
                |> String.toUpper
            )
        ]


statesColumn : VertexData -> Element Msg
statesColumn vertex =
    Element.column columnStyle
        [ Element.text "states:"
        , Element.text (String.join ", " vertex.states)
        ]


isCommitteeAndStateColumn : VertexData -> Element Msg
isCommitteeAndStateColumn vertex =
    Element.column []
        [ isCommitteeColumn vertex
        , statesColumn vertex
        ]


nameAndCitiesColumn : VertexData -> Element Msg
nameAndCitiesColumn vertex =
    Element.column []
        [ nameColumn vertex
        , citiesColumn vertex
        ]


edgeColumn : EdgeData -> Element Msg
edgeColumn edge =
    Element.column columnStyle
        [ Element.text ("src_id: " ++ edge.src_id)
        , Element.text ("dst_id: " ++ edge.dst_id)
        , Element.text ("transactions count: " ++ edge.num_transactions)
        , Element.text ("total spend: " ++ edge.total_spend)
        , Element.text ("avg spend: " ++ edge.avg_spend)
        , Element.text ("max spend: " ++ edge.max_spend)
        , Element.text ("min spend: " ++ edge.min_spend)
        ]


almostFromVerticesToTable : List VertexData -> (VertexData -> Msg) -> String -> Element Msg
almostFromVerticesToTable vertices buttonMsg buttonName =
    Element.column tableStyle
        (List.map (fromVertexDataToRowWithButton buttonMsg buttonName) vertices)


fromVertexDataToRowWithButton : (VertexData -> Msg) -> String -> VertexData -> Element Msg
fromVertexDataToRowWithButton buttonMsg buttonName vertex =
    almostFromVertexDataToRow vertex
        [ Input.button [] { onPress = Just (buttonMsg vertex), label = buttonStyle (Element.text buttonName) } ]


fromVerticesToTableWithSelectVertexButton : List VertexData -> Element Msg
fromVerticesToTableWithSelectVertexButton vertices =
    almostFromVerticesToTable vertices VertexSelected "select"


fromVerticesToTableWithDeleteVertexButton : List VertexData -> Element Msg
fromVerticesToTableWithDeleteVertexButton vertices =
    almostFromVerticesToTable vertices DeleteVertexSelection "delete"


fromVerticesAndEdgesToTableWithSearchButton : List Zipped -> Element Msg
fromVerticesAndEdgesToTableWithSearchButton verticesAndEdges =
    let
        row : Zipped -> Element Msg
        row tuple =
            almostFromVertexDataToRow
                (Tuple.second tuple)
                [ Input.button []
                    { onPress = Just (ChildTraversalRequestMade (Tuple.second tuple))
                    , label = buttonStyle (Element.text "Search")
                    }
                , edgeColumn (Tuple.first tuple)
                ]
    in
    Element.column tableStyle (List.map row verticesAndEdges)


buttonStyle : Element Msg -> Element Msg
buttonStyle button =
    Element.el
        [ Background.color (Element.rgb255 220 220 220)
        , Font.size 17
        , Element.paddingXY 7 3
        , Border.rounded 10
        , Border.glow (Element.rgb255 210 210 210) 1.5
        ]
        button


sortByOptions : List (Input.Option SortBy Msg)
sortByOptions =
    [ Input.option Count (Element.text "Transactions Count")
    , Input.option TotalSpend (Element.text "Total Spend")
    , Input.option AvgSpend (Element.text "Average Spend")
    , Input.option MaxSpend (Element.text "Max Spend")
    , Input.option MinSpend (Element.text "Min Spend")
    ]


sortByRadio : Model -> Element Msg
sortByRadio model =
    Input.radio
        []
        { onChange = \sb -> SortByOptionSelected sb
        , options = sortByOptions
        , selected = Just model.sort_by_selected
        , label = Input.labelLeft [] (Element.text "Sort By: ")
        }
