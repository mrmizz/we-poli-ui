module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Set exposing (Set)



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { state : State
    , vertex_name_search : String
    , vertex_name_search_response : List VertexData
    , vertices_selected : List VertexData
    , aggregation_selected : String
    , direction_selected : Direction
    , vertex_data_response : List VertexData
    }


type alias VertexData =
    { uid : String
    , name : String
    , is_committee : Bool
    , cities : List String
    , streets : List String
    , states : List String
    }


type alias VertexPresence =
    { set : Set String
    , vertices : List VertexData
    }


getVertexId : VertexData -> String
getVertexId vertexData =
    vertexData.uid


hasUID : String -> VertexData -> Bool
hasUID uid vertex =
    vertex.uid == uid


distinctVertices : List VertexData -> VertexPresence
distinctVertices vertices =
    List.foldl updateVertexPresence (VertexPresence Set.empty []) vertices


filterVerticesByDirection : Direction -> List VertexData -> List VertexData
filterVerticesByDirection direction vertices =
    case List.filter (sameDirection direction) vertices of
        [] ->
            []

        head :: [] ->
            case sameDirection direction head of
                True ->
                    [ head ]

                False ->
                    []

        list ->
            list


sameDirection : Direction -> VertexData -> Bool
sameDirection direction vertexData =
    vertexData.is_committee == directionToIsCommittee direction


directionToIsCommittee : Direction -> Bool
directionToIsCommittee direction =
    case direction of
        In ->
            False

        Out ->
            True


getVertices : VertexPresence -> List VertexData
getVertices vertexPresence =
    vertexPresence.vertices


updateVertexPresence : VertexData -> VertexPresence -> VertexPresence
updateVertexPresence vertexData vertexPresence =
    case Set.member vertexData.uid vertexPresence.set of
        True ->
            vertexPresence

        False ->
            { vertexPresence
                | set = Set.insert vertexData.uid vertexPresence.set
                , vertices = List.singleton vertexData ++ vertexPresence.vertices
            }


printBool : Bool -> String
printBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


type State
    = BuildingRequest
    | SearchConfirmed
    | Loading
    | VertexRequestsSuccess
    | RequestFailure Http.Error


initialModel : Model
initialModel =
    { state = BuildingRequest
    , vertex_name_search = ""
    , vertex_name_search_response = []
    , aggregation_selected = defaultAggregationInput
    , vertices_selected = []
    , direction_selected = Out
    , vertex_data_response = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


defaultAggregationInput : String
defaultAggregationInput =
    "Or"



-- UPDATE


type Msg
    = ClearSearch
    | EditSearch
    | ConfirmSearch
    | SearchInput String
    | AggOptionSelected
    | DirectionOptionSelected
    | VertexSelected VertexData
    | DeleteVertexSelection VertexData
    | VertexIdsRequestMade
    | ChildVertexIdsRequestMade VertexData
    | VertexIdsPostReceived (Result Http.Error VertexIdsResponse)
    | VertexDataPostReceived (Result Http.Error VertexDataResponse)
    | VertexNamePrefixGetReceived (Result Http.Error VertexNamePrefixResponse)


type Direction
    = In
    | Out


switchDirection : Direction -> Direction
switchDirection direction =
    case direction of
        In ->
            Out

        Out ->
            In


updateDirectionOption : Model -> ( Model, Cmd Msg )
updateDirectionOption model =
    case model.direction_selected of
        In ->
            ( { model | direction_selected = Out, vertices_selected = [], vertex_name_search_response = [], vertex_name_search = "" }, Cmd.none )

        Out ->
            ( { model | direction_selected = In, vertices_selected = [], vertex_name_search_response = [], vertex_name_search = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput prefix ->
            -- TODO: dynamo sort key (is_committee)
            -- vendor names are dominating committee names
            updateWithVertexNamePrefixRequest model prefix VertexNamePrefixGetReceived

        VertexNamePrefixGetReceived result ->
            updateWithVertexNamePrefixResponse model result

        VertexIdsRequestMade ->
            case model.direction_selected of
                In ->
                    updateWithVertexIdRequest model "in"

                Out ->
                    updateWithVertexIdRequest model "out"

        ChildVertexIdsRequestMade vertexData ->
            updateWithChildVertexIdRequest model vertexData

        VertexIdsPostReceived result ->
            updateWithVertexIdResponse model result

        VertexDataPostReceived result ->
            updateWithVertexDataResponse model result

        ClearSearch ->
            ( initialModel, Cmd.none )

        EditSearch ->
            ( { model | state = BuildingRequest }, Cmd.none )

        ConfirmSearch ->
            ( { model | state = SearchConfirmed }, Cmd.none )

        AggOptionSelected ->
            updateAggInputAndOptions model

        DirectionOptionSelected ->
            updateDirectionOption model

        VertexSelected vertex ->
            ( { model | vertices_selected = updateVertexSelected vertex model.vertices_selected }, Cmd.none )

        DeleteVertexSelection vertex ->
            ( { model | vertices_selected = updateVertexDeleted vertex model.vertices_selected }, Cmd.none )


cleanVertexNameInput : String -> String
cleanVertexNameInput input =
    String.replace " " "" input
        |> String.toLower


updateVertexSelected : VertexData -> List VertexData -> List VertexData
updateVertexSelected vertex vertices =
    getVertices (distinctVertices (List.singleton vertex ++ vertices))


updateVertexDeleted : VertexData -> List VertexData -> List VertexData
updateVertexDeleted vertex vertices =
    case vertices of
        _ :: [] ->
            []

        _ ->
            List.filter (hasUID vertex.uid) vertices


updateWithVertexNamePrefixRequest : Model -> String -> (Result Http.Error VertexNamePrefixResponse -> Msg) -> ( Model, Cmd Msg )
updateWithVertexNamePrefixRequest model prefix toMsg =
    case String.length (cleanVertexNameInput prefix) >= 3 of
        False ->
            ( { model | vertex_name_search = prefix }, Cmd.none )

        True ->
            ( { model | vertex_name_search = prefix }
            , vertexNamePrefixGet (cleanVertexNameInput prefix) toMsg
            )


updateWithVertexNamePrefixResponse : Model -> Result Http.Error VertexNamePrefixResponse -> ( Model, Cmd Msg )
updateWithVertexNamePrefixResponse model result =
    case result of
        Ok response ->
            case unpackVertexNamePrefixResponse response of
                [] ->
                    ( { model | vertex_name_search_response = [] }, Cmd.none )

                vertices ->
                    ( { model | vertex_name_search_response = filterVerticesByDirection model.direction_selected vertices }
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


unpackDynamoVertexDataInner : DynamoVertexDataInner -> List VertexData
unpackDynamoVertexDataInner dynamoVertexDataInner =
    List.map unpackDynamoVertexDataInnerInner dynamoVertexDataInner.item


unpackDynamoVertexDataInnerInner : DynamoVertexDataInnerInner -> VertexData
unpackDynamoVertexDataInnerInner dynamoVertexDataInnerInner =
    unpackDynamoVertexData dynamoVertexDataInnerInner.items


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


updateWithVertexIdRequest : Model -> String -> ( Model, Cmd Msg )
updateWithVertexIdRequest model directionStr =
    ( { model | state = Loading }
    , vertexIdsPost
        (buildVertexIdsRequest directionStr model.vertices_selected model.aggregation_selected)
        VertexIdsPostReceived
    )


updateWithChildVertexIdRequest : Model -> VertexData -> ( Model, Cmd Msg )
updateWithChildVertexIdRequest model vertexData =
    case vertexData.is_committee of
        True ->
            ( { model | state = Loading, vertices_selected = [ vertexData ], direction_selected = switchDirection model.direction_selected }
            , vertexIdsPost
                (buildVertexIdsRequest "out" [ vertexData ] model.aggregation_selected)
                VertexIdsPostReceived
            )

        False ->
            ( { model | state = Loading, vertices_selected = [ vertexData ], direction_selected = switchDirection model.direction_selected }
            , vertexIdsPost
                (buildVertexIdsRequest "in" [ vertexData ] model.aggregation_selected)
                VertexIdsPostReceived
            )


updateWithVertexIdResponse : Model -> Result Http.Error VertexIdsResponse -> ( Model, Cmd Msg )
updateWithVertexIdResponse model result =
    case result of
        Ok response ->
            ( model
            , vertexDataPost (buildVertexDataRequest (List.take 99 response.response_vertex_ids)) VertexDataPostReceived
            )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


updateWithVertexDataResponse : Model -> Result Http.Error VertexDataResponse -> ( Model, Cmd Msg )
updateWithVertexDataResponse model result =
    case result of
        Ok response ->
            ( { model | state = VertexRequestsSuccess, vertex_data_response = unpackVertexDataResponse response }
            , Cmd.none
            )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


unpackVertexDataResponse : VertexDataResponse -> List VertexData
unpackVertexDataResponse vertexDataResponse =
    List.map unpackDynamoVertexData vertexDataResponse.responses.items


updateAggInputAndOptions : Model -> ( Model, Cmd Msg )
updateAggInputAndOptions model =
    case model.aggregation_selected of
        "Or" ->
            ( { model | aggregation_selected = "And" }, Cmd.none )

        _ ->
            ( { model | aggregation_selected = "Or" }, Cmd.none )



-- HTTP


type alias VertexIdsRequest =
    { vertex_ids : List String
    , direction : String
    , agg : String
    }


type alias VertexIdsResponse =
    { request_vertex_ids : List String
    , response_vertex_ids : List String
    }


vertexIdsPost : VertexIdsRequest -> (Result Http.Error VertexIdsResponse -> Msg) -> Cmd Msg
vertexIdsPost request msg =
    Http.post
        { url = "https://7qfeute799.execute-api.us-west-2.amazonaws.com/default/v1/tap-in"
        , body = Http.jsonBody (vertexIdsRequestEncoder request)
        , expect = Http.expectJson msg vertexIdsResponseDecoder
        }


buildVertexIdsRequest : String -> List VertexData -> String -> VertexIdsRequest
buildVertexIdsRequest directionString vertices agg =
    VertexIdsRequest (List.map getVertexId vertices) directionString agg


vertexIdsRequestEncoder : VertexIdsRequest -> Encode.Value
vertexIdsRequestEncoder request =
    Encode.object
        [ ( "vertex_ids", Encode.list Encode.string request.vertex_ids )
        , ( "direction", Encode.string request.direction )
        , ( "agg", Encode.string request.agg )
        ]


vertexIdsResponseDecoder : Decode.Decoder VertexIdsResponse
vertexIdsResponseDecoder =
    Decode.map2 VertexIdsResponse
        (Decode.field "vertex_ids" (Decode.list Decode.string))
        (Decode.field "response_vertex_ids" (Decode.list Decode.string))


type alias VertexNamePrefixResponse =
    { items : List VertexNamePrefixInnerResponse }


type alias VertexNamePrefixInnerResponse =
    { prefix : DynamoValue
    , prefix_size : DynamoValue
    , vertices : DynamoVertexDataInner
    }


type alias DynamoVertexData =
    { name : DynamoValue
    , uid : DynamoValue
    , is_committee : DynamoBool
    , cities : DynamoArrayValue
    , streets : DynamoArrayValue
    , states : DynamoArrayValue
    }


type alias DynamoVertexDataInnerInner =
    { items : DynamoVertexData }


type alias DynamoVertexDataInner =
    { item : List DynamoVertexDataInnerInner }


vertexNamePrefixGet : String -> (Result Http.Error VertexNamePrefixResponse -> Msg) -> Cmd Msg
vertexNamePrefixGet prefix toMsg =
    Http.get
        { url = "https://yf87qmn85l.execute-api.us-west-2.amazonaws.com/v1/poli/prefix/" ++ prefix
        , expect = Http.expectJson toMsg vertexNamePrefixResponseDecoder
        }


vertexNamePrefixResponseDecoder : Decode.Decoder VertexNamePrefixResponse
vertexNamePrefixResponseDecoder =
    Decode.map VertexNamePrefixResponse (Decode.field "Items" (Decode.list vertexNamePrefixInnerResponseDecoder))


vertexNamePrefixInnerResponseDecoder : Decode.Decoder VertexNamePrefixInnerResponse
vertexNamePrefixInnerResponseDecoder =
    Decode.map3 VertexNamePrefixInnerResponse
        (Decode.field "prefix" dynamoStringValueDecoder)
        (Decode.field "prefix_size" dynamoNumberValueDecoder)
        (Decode.field "vertices" dynamoVertexDataInnerDecoder)


dynamoVertexDataInnerDecoder : Decode.Decoder DynamoVertexDataInner
dynamoVertexDataInnerDecoder =
    Decode.map DynamoVertexDataInner (Decode.field "L" (Decode.list dynamoVertexDataInnerInnerDecoder))


dynamoVertexDataInnerInnerDecoder : Decode.Decoder DynamoVertexDataInnerInner
dynamoVertexDataInnerInnerDecoder =
    Decode.map DynamoVertexDataInnerInner (Decode.field "M" vertexDataInnerResponseDecoder)


type alias VertexDataRequest =
    { request_items : VertexDataInnerRequest }


type alias VertexDataInnerRequest =
    { poli_vertex : VertexDataInnerRequestKey }


type alias VertexDataInnerRequestKey =
    { keys : List VertexDataInnerRequestUID }


type alias VertexDataInnerRequestUID =
    { uid : VertexDataInnerRequestUIDValue }


type alias VertexDataInnerRequestUIDValue =
    { number : String }


vertexDataPost : VertexDataRequest -> (Result Http.Error VertexDataResponse -> Msg) -> Cmd Msg
vertexDataPost request toMsg =
    Http.post
        { url = "https://yf87qmn85l.execute-api.us-west-2.amazonaws.com/v1/poli/vertex"
        , body = Http.jsonBody (vertexDataRequestEncoder request)
        , expect = Http.expectJson toMsg vertexDataResponseDecoder
        }


buildVertexDataRequest : List String -> VertexDataRequest
buildVertexDataRequest uids =
    VertexDataRequest
        (VertexDataInnerRequest
            (VertexDataInnerRequestKey (List.map buildVertexDataRequestInnerValues uids))
        )


buildVertexDataRequestInnerValues : String -> VertexDataInnerRequestUID
buildVertexDataRequestInnerValues uid =
    VertexDataInnerRequestUID (VertexDataInnerRequestUIDValue uid)


vertexDataRequestEncoder : VertexDataRequest -> Encode.Value
vertexDataRequestEncoder vertexDataRequest =
    Encode.object
        [ ( "RequestItems", vertexDataInnerRequestEncoder vertexDataRequest.request_items ) ]


vertexDataInnerRequestEncoder : VertexDataInnerRequest -> Encode.Value
vertexDataInnerRequestEncoder vertexDataInnerRequest =
    Encode.object
        [ ( "PoliVertex", vertexDataInnerRequestKeyEncoder vertexDataInnerRequest.poli_vertex ) ]


vertexDataInnerRequestKeyEncoder : VertexDataInnerRequestKey -> Encode.Value
vertexDataInnerRequestKeyEncoder vertexDataInnerRequestKey =
    Encode.object
        [ ( "Keys", Encode.list vertexDataInnerRequestUIDEncoder vertexDataInnerRequestKey.keys ) ]


vertexDataInnerRequestUIDEncoder : VertexDataInnerRequestUID -> Encode.Value
vertexDataInnerRequestUIDEncoder vertexDataInnerRequestUID =
    Encode.object
        [ ( "uid", vertexDataInnerRequestUIDValueEncoder vertexDataInnerRequestUID.uid ) ]


vertexDataInnerRequestUIDValueEncoder : VertexDataInnerRequestUIDValue -> Encode.Value
vertexDataInnerRequestUIDValueEncoder vertexDataInnerRequestUIDValue =
    Encode.object
        [ ( "N", Encode.string vertexDataInnerRequestUIDValue.number ) ]


type alias VertexDataResponse =
    { responses : PoliVertexTable }


type alias PoliVertexTable =
    { items : List DynamoVertexData }


vertexDataResponseDecoder : Decode.Decoder VertexDataResponse
vertexDataResponseDecoder =
    Decode.map VertexDataResponse (Decode.field "Responses" poliVertexTable)


poliVertexTable : Decode.Decoder PoliVertexTable
poliVertexTable =
    Decode.map PoliVertexTable (Decode.field "PoliVertex" (Decode.list vertexDataInnerResponseDecoder))


type alias DynamoArrayValue =
    { list : List DynamoValue }


type alias DynamoValue =
    { value : String }


type alias DynamoBool =
    { value : Bool }


dynamoArrayStringValueDecoder : Decode.Decoder DynamoArrayValue
dynamoArrayStringValueDecoder =
    Decode.map DynamoArrayValue (Decode.field "L" (Decode.list dynamoStringValueDecoder))


dynamoNumberValueDecoder : Decode.Decoder DynamoValue
dynamoNumberValueDecoder =
    Decode.map DynamoValue (Decode.field "N" Decode.string)


dynamoStringValueDecoder : Decode.Decoder DynamoValue
dynamoStringValueDecoder =
    Decode.map DynamoValue (Decode.field "S" Decode.string)


dynamoBoolDecoder : Decode.Decoder DynamoBool
dynamoBoolDecoder =
    Decode.map DynamoBool (Decode.field "BOOL" Decode.bool)


vertexDataInnerResponseDecoder : Decode.Decoder DynamoVertexData
vertexDataInnerResponseDecoder =
    Decode.map6 DynamoVertexData
        (Decode.field "name" dynamoStringValueDecoder)
        (Decode.field "uid" dynamoNumberValueDecoder)
        (Decode.field "is_committee" dynamoBoolDecoder)
        (Decode.field "cities" dynamoArrayStringValueDecoder)
        (Decode.field "streets" dynamoArrayStringValueDecoder)
        (Decode.field "states" dynamoArrayStringValueDecoder)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] (background (elementView model))


elementView : Model -> Element Msg
elementView model =
    case model.state of
        BuildingRequest ->
            viewBuildingRequest model

        SearchConfirmed ->
            viewSearchConfirmed model

        Loading ->
            viewLoading

        VertexRequestsSuccess ->
            viewRequestSuccess model.direction_selected model

        RequestFailure error ->
            viewRequestFailure error


almostDropdownBody : List (Element Msg) -> Element Msg
almostDropdownBody elements =
    Element.column
        [ Font.extraLight
        , Element.width (Element.fill)
        , Background.color (Element.rgb255 119 136 153)
        , Border.rounded 25
        , Element.spacing 12
        , Element.padding 30
        ]
        elements


viewSearchConfirmed : Model -> Element Msg
viewSearchConfirmed model =
    Element.column []
        [ dropdownHead model
        , almostDropdownBody
            [ makeVertexIdsRequestButton
            , clearSearchButton
            , editSearchButton
            , viewAggParam model.aggregation_selected
            , viewVerticesConfirmed model.vertices_selected
            ]
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


viewLoading : Element Msg
viewLoading =
    Element.el [] (Element.text "Loading . . .")


viewRequestSuccess : Direction -> Model -> Element Msg
viewRequestSuccess direction model =
    Element.column []
        [ dropdownHead model
        , almostDropdownBody
            [ makeVertexIdsRequestButton
            , editSearchButton
            , clearSearchButton
            , viewAggParam model.aggregation_selected
            , viewDirectedResponse model direction
            ]
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


viewAggParam : String -> Element Msg
viewAggParam agg =
    Element.row [] [ Element.text "Aggregation: ", viewAggButton agg ]


viewAggButton : String -> Element Msg
viewAggButton aggName =
    Input.button [] { onPress = Just AggOptionSelected, label = buttonStyle (Element.text aggName) }


background : Element Msg -> Element Msg
background moreElements =
    Element.el
        [ Background.color (Element.rgb255 50 125 200)
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.el [ Font.color (Element.rgb255 250 250 250), Element.centerX ] moreElements)


almostDropdownHead : Model -> (Model -> Element Msg) -> Element Msg
almostDropdownHead model anotherElement =
    Element.row
        [ Element.padding 50
        , Element.centerX
        , Font.size 35
        ]
        [ Element.text "Poli Graph Search: "
        , anotherElement model
        ]


dropdownHead : Model -> Element Msg
dropdownHead model =
    almostDropdownHead model (\_ -> directionOptionText model)


dropdownHeadWithDirectionButton : Model -> Element Msg
dropdownHeadWithDirectionButton model =
    almostDropdownHead model directionOptionButton


directedBuildingRequestDropdownBody : Model -> List (Element Msg) -> Element Msg
directedBuildingRequestDropdownBody model moreElements =
    case model.direction_selected of
        In ->
            buildingRequestDropdownBody model "vendor name" moreElements

        Out ->
            buildingRequestDropdownBody model "committee name" moreElements


buildingRequestDropdownBody : Model -> String -> List (Element Msg) -> Element Msg
buildingRequestDropdownBody model entityType moreElements =
    almostDropdownBody
        ([ Input.search [ Font.color (Element.rgb255 0 0 0) ]
            { onChange = SearchInput
            , text = model.vertex_name_search
            , placeholder = Just (Input.placeholder [] (Element.text entityType))
            , label = Input.labelHidden "hidden label"
            }
         ]
            ++ moreElements
        )


buildBuildingRequestView : Model -> List (Element Msg) -> Element Msg
buildBuildingRequestView model moreElements =
    Element.column []
        [ dropdownHeadWithDirectionButton model
        , directedBuildingRequestDropdownBody model moreElements
        ]


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
    Input.button [] { onPress = Just VertexIdsRequestMade, label = buttonStyle (Element.text "Search") }


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
            , fromVerticesToTableWithSearchButton model.vertex_data_response
            ]
        ]



-- TODO: more fields


fromVerticesToTable : List VertexData -> Element Msg
fromVerticesToTable vertices =
    Element.column []
        (List.map fromVertexDataToRow vertices)


fromVertexDataToRow : VertexData -> Element Msg
fromVertexDataToRow vertex =
    almostFromVertexDataToRow vertex Element.none


almostFromVertexDataToRow : VertexData -> Element Msg -> Element Msg
almostFromVertexDataToRow vertex anotherElement =
    Element.row [ Element.spacing 25 ]
        [ anotherElement
        , uidColumn vertex
        , nameColumn vertex
        ]


uidColumn : VertexData -> Element Msg
uidColumn vertex =
    Element.column []
        [ Element.text "uid:"
        , Element.text vertex.uid
        ]


nameColumn : VertexData -> Element Msg
nameColumn vertex =
    Element.column []
        [ Element.text "name:"
        , Element.text vertex.name
        ]


almostFromVerticesToTable : List VertexData -> (VertexData -> Msg) -> String -> Element Msg
almostFromVerticesToTable vertices buttonMsg buttonName =
    Element.column []
        (List.map (fromVertexDataToRowWithButton buttonMsg buttonName) vertices)


fromVertexDataToRowWithButton : (VertexData -> Msg) -> String -> VertexData -> Element Msg
fromVertexDataToRowWithButton buttonMsg buttonName vertex =
    almostFromVertexDataToRow vertex (Input.button [] { onPress = Just (buttonMsg vertex), label = buttonStyle (Element.text buttonName) })


fromVerticesToTableWithSelectVertexButton : List VertexData -> Element Msg
fromVerticesToTableWithSelectVertexButton vertices =
    almostFromVerticesToTable vertices VertexSelected "select"


fromVerticesToTableWithDeleteVertexButton : List VertexData -> Element Msg
fromVerticesToTableWithDeleteVertexButton vertices =
    almostFromVerticesToTable vertices DeleteVertexSelection "delete"


fromVerticesToTableWithSearchButton : List VertexData -> Element Msg
fromVerticesToTableWithSearchButton vertices =
    almostFromVerticesToTable vertices ChildVertexIdsRequestMade "Search"


buttonStyle : Element Msg -> Element Msg
buttonStyle button =
    Element.el
        [ Background.color (Element.rgb255 220 220 220)
        , Font.size 17
        , Element.paddingXY 7 3
        , Border.rounded 10
        , Border.glow (Element.rgb255 210 210 210) 3
        ]
        button
