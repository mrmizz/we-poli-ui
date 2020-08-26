module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
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
    case model.state of
        BuildingRequest ->
            Element.layout [] (background [ viewBuildingRequest model ])

        SearchConfirmed ->
            viewSearchConfirmed model

        Loading ->
            viewLoading

        VertexRequestsSuccess ->
            viewRequestSuccess model.direction_selected model

        RequestFailure error ->
            viewRequestFailure error


styledView : Html Msg -> Html Msg
styledView html =
    Element.html html
        |> Element.el [ Element.centerX, Element.moveDown 25 ]
        |> Element.layout []


viewSearchConfirmed : Model -> Html Msg
viewSearchConfirmed model =
    div [ class "dropdown" ]
        [ Element.layout [] dropdownHead
        , makeVertexIdsRequestButton
        , defaultClearSearchButton
        , editSearchButton
        , viewAggParam model.aggregation_selected
        , viewVerticesConfirmed model.vertices_selected
        ]


viewVerticesConfirmed : List VertexData -> Html Msg
viewVerticesConfirmed vertices =
    ul [ class "dropdown" ] ([ text "Queued for Search: " ] ++ List.map fromVertexDataToHTMLNoButtons vertices)


viewVertexNamePrefixResponse : Model -> Html Msg
viewVertexNamePrefixResponse model =
    ul [ class "dropdown" ]
        ([ text "Potential Search Matches:" ] ++ buildPotentialSearchMatch model.vertex_name_search_response)


buildPotentialSearchMatch : List VertexData -> List (Html Msg)
buildPotentialSearchMatch vertexData =
    List.map fromVertexDataToHTMLWithSelectVertexButton vertexData


viewVertexSelected : VertexData -> Html Msg
viewVertexSelected vertexData =
    fromVertexDataToHTMLWithDeleteVertexButton vertexData


viewVertexIdsSelected : Model -> Html Msg
viewVertexIdsSelected model =
    ul [ class "dropdown" ]
        ([ text "We're Searching For:" ] ++ List.map viewVertexSelected model.vertices_selected)


viewBuildingRequest : Model -> Element Msg
viewBuildingRequest model =
    case model.vertex_name_search of
        "" ->
            viewBuildingRequestWithNoInputButMaybeSomeConfirmed model

        _ ->
            case model.vertices_selected of
                [] ->
                    Element.el [] (dropdownHeadAndBody model [ directionOptionButton model.direction_selected, viewVertexNamePrefixResponse model ])

                _ ->
                    Element.el []
                        (dropdownHeadAndBody model
                            [ directionOptionButton model.direction_selected
                            , div [] [ confirmSearchButton ]
                            , viewVertexIdsSelected model
                            , viewVertexNamePrefixResponse model
                            ]
                        )


viewNoInput : Model -> Element Msg
viewNoInput model =
    Element.el [] (dropdownHeadAndBody model [ directionOptionButton model.direction_selected ])


viewBuildingRequestWithNoInputButMaybeSomeConfirmed : Model -> Element Msg
viewBuildingRequestWithNoInputButMaybeSomeConfirmed model =
    case model.vertices_selected of
        [] ->
            viewNoInput model

        _ ->
            Element.el [] (dropdownHeadAndBody model [ directionOptionButton model.direction_selected, div [] [ confirmSearchButton ], viewVertexIdsSelected model ])


viewLoading : Html Msg
viewLoading =
    div [ class "dropdown" ] [ text "Loading . . ." ]


viewRequestSuccess : Direction -> Model -> Html Msg
viewRequestSuccess direction model =
    div [ class "dropdown" ]
        [ Element.layout [] dropdownHead
        , defaultClearSearchButton
        , editSearchButton
        , viewAggParam model.aggregation_selected
        , viewDirectedResponse model direction
        ]


viewRequestFailure : Http.Error -> Html Msg
viewRequestFailure error =
    case error of
        Http.BadUrl string ->
            div []
                [ text ("Bad Url: " ++ string)
                , div [] [ defaultClearSearchButton, editSearchButton, returnToSearchButton ]
                ]

        Http.Timeout ->
            div []
                [ text "Server Timeout"
                , div [] [ defaultClearSearchButton, editSearchButton, returnToSearchButton ]
                ]

        Http.NetworkError ->
            div []
                [ text "Network Error"
                , div [] [ defaultClearSearchButton, editSearchButton, returnToSearchButton ]
                ]

        Http.BadStatus int ->
            div []
                [ text (String.fromInt int ++ " Error: Bad Input")
                , div [] [ defaultClearSearchButton, editSearchButton, returnToSearchButton ]
                ]

        Http.BadBody body ->
            div []
                [ text ("Bad Body: " ++ body)
                , div [] [ defaultClearSearchButton, editSearchButton, returnToSearchButton ]
                ]


viewAggParam : String -> Html Msg
viewAggParam agg =
    div [ class "dropdown" ] [ text "Aggregation: ", button [ onClick AggOptionSelected ] [ text agg ] ]


background : List (Element Msg) -> Element Msg
background moreElements =
    Element.el
        [ Background.color (Element.rgb255 50 125 200)
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.row [ Font.color (Element.rgb255 0 255 255), Element.centerX ] moreElements)


dropdownHead : Element Msg
dropdownHead =
    Element.el [ Element.centerX, Element.padding 25 ] (Element.text ">Poli Graph Search<")


directedDropdownBody : Model -> List (Element Msg) -> Element Msg
directedDropdownBody model moreElements =
    case model.direction_selected of
        In ->
            dropdownBody model "vendor name" moreElements

        Out ->
            dropdownBody model "committee name" moreElements


dropdownBody : Model -> String -> List (Element Msg) -> Element Msg
dropdownBody model entityType moreElements =
    Element.column []
        ([ Input.search [ Element.width (Element.px 500) ]
            { onChange = SearchInput
            , text = model.vertex_name_search
            , placeholder = Nothing -- Just (Input.placeholder [] (Element.text "nada"))
            , label = Input.labelAbove [] (Element.text entityType)
            }
         ]
            ++ moreElements
        )


dropdownHeadAndBody : Model -> List (Html Msg) -> Element Msg
dropdownHeadAndBody model moreElements =
    Element.column []
        [ dropdownHead
        , directedDropdownBody model (List.map Element.html moreElements)
        ]


directionOptionButton : Direction -> Html Msg
directionOptionButton direction =
    case direction of
        In ->
            button [ onClick DirectionOptionSelected ] [ text "Vendors" ]

        Out ->
            button [ onClick DirectionOptionSelected ] [ text "Committees" ]


makeVertexIdsRequestButton : Html Msg
makeVertexIdsRequestButton =
    button [ class "button", onClick VertexIdsRequestMade ] [ text "Search" ]


almostClearSearchButton : List (Html Msg) -> Html Msg
almostClearSearchButton =
    button [ class "button", onClick ClearSearch ]


defaultClearSearchButton : Html Msg
defaultClearSearchButton =
    almostClearSearchButton [ text "Clear Search" ]


editSearchButton : Html Msg
editSearchButton =
    button [ class "button", onClick EditSearch ] [ text "Edit Search" ]


confirmSearchButton : Html Msg
confirmSearchButton =
    button [ class "button", onClick ConfirmSearch ] [ text "Confirm Search" ]


returnToSearchButton : Html Msg
returnToSearchButton =
    button [ class "button", onClick ConfirmSearch ] [ text "Return To Existing Search" ]


viewDirectedResponse : Model -> Direction -> Html Msg
viewDirectedResponse model direction =
    case direction of
        In ->
            viewDirectedResponseWithText model "Related Committees"

        Out ->
            viewDirectedResponseWithText model "Related Vendors"


viewDirectedResponseWithText : Model -> String -> Html Msg
viewDirectedResponseWithText model textToDisplay =
    div [ class "response" ]
        [ ul [ class "dropdown" ] ([ text "Searched: " ] ++ List.map fromVertexDataToHTMLNoButtons model.vertices_selected)
        , ul [] ([ text textToDisplay ] ++ List.map fromVertexDataToHTMLWithSearchButton model.vertex_data_response)
        ]



-- TODO: more fields


almostFromVertexDataToHTML : VertexData -> List (Html Msg) -> Html Msg
almostFromVertexDataToHTML vertexData html =
    li []
        (html
            ++ [ ul []
                    [ li []
                        [ text "uid:"
                        , ul [] [ li [] [ text vertexData.uid ] ]
                        ]
                    , li []
                        [ text "name:"
                        , ul [] [ li [] [ text vertexData.name ] ]
                        ]
                    , li []
                        [ text "is_comittee:"
                        , ul [] [ li [] [ text (printBool vertexData.is_committee) ] ]
                        ]
                    , li []
                        [ text "cities:"
                        , ul [] (List.map textListItem vertexData.cities)
                        ]
                    ]
               ]
        )


textListItem : String -> Html Msg
textListItem str =
    li [] [ text str ]


fromVertexDataToHTMLWithSelectVertexButton : VertexData -> Html Msg
fromVertexDataToHTMLWithSelectVertexButton vertexData =
    almostFromVertexDataToHTML vertexData [ button [ onClick (VertexSelected vertexData) ] [ text "Select" ] ]


fromVertexDataToHTMLWithDeleteVertexButton : VertexData -> Html Msg
fromVertexDataToHTMLWithDeleteVertexButton vertexData =
    almostFromVertexDataToHTML vertexData [ button [ onClick (DeleteVertexSelection vertexData) ] [ text "delete" ] ]


fromVertexDataToHTMLWithSearchButton : VertexData -> Html Msg
fromVertexDataToHTMLWithSearchButton vertexData =
    almostFromVertexDataToHTML vertexData [ button [ onClick (ChildVertexIdsRequestMade vertexData) ] [ text "Search" ] ]


fromVertexDataToHTMLNoButtons : VertexData -> Html Msg
fromVertexDataToHTMLNoButtons vertexData =
    almostFromVertexDataToHTML vertexData []
