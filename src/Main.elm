module Main exposing (main, updateWithVertexNamePrefixResponse)

import Browser
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
    , vertex_name_search : Maybe String
    , vertex_name_search_response : List VertexData
    , vertices_selected : List VertexData
    , aggregation_selected : String
    , vertex_ids_response : List String
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


getVertices : VertexPresence -> List VertexData
getVertices vertexPresence =
    vertexPresence.vertices


hasUID : String -> VertexData -> Bool
hasUID uid vertex =
    vertex.uid == uid


distinctVertices : List VertexData -> VertexPresence
distinctVertices vertices =
    List.foldl updateVertexPresence (VertexPresence Set.empty []) vertices


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
    | VertexRequestsSuccess Direction
    | RequestFailure Http.Error


initialModel : Model
initialModel =
    { state = BuildingRequest
    , vertex_name_search = Nothing
    , vertex_name_search_response = []
    , aggregation_selected = defaultAggregationInput
    , vertices_selected = []
    , vertex_ids_response = []
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
    | VertexSelected VertexData
    | DeleteVertexSelection VertexData
    | VertexIdsRequestMade Direction
    | VertexIdsPostReceived Direction (Result Http.Error VertexIdsResponse)
    | VertexDataPostReceived Direction (Result Http.Error VertexDataResponse)
    | VertexNamePrefixGetReceived (Result Http.Error VertexNamePrefixResponse)


type Direction
    = In
    | Out


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput prefix ->
            updateWithVertexNamePrefixRequest model prefix VertexNamePrefixGetReceived

        VertexNamePrefixGetReceived result ->
            updateWithVertexNamePrefixResponse model result

        VertexIdsRequestMade direction ->
            case direction of
                In ->
                    updateWithVertexIdRequest model (buildVertexIdsRequest "in") (VertexIdsPostReceived direction)

                Out ->
                    updateWithVertexIdRequest model (buildVertexIdsRequest "out") (VertexIdsPostReceived direction)

        VertexIdsPostReceived direction result ->
            updateWithVertexIdResponse model result direction

        VertexDataPostReceived direction result ->
            updateWithVertexDataResponse model result direction

        ClearSearch ->
            ( initialModel, Cmd.none )

        EditSearch ->
            ( { model | state = BuildingRequest }, Cmd.none )

        ConfirmSearch ->
            ( { model | state = SearchConfirmed }, Cmd.none )

        AggOptionSelected ->
            updateAggInputAndOptions model

        VertexSelected vertex ->
            ( { model | vertices_selected = updateVertexSelected vertex model.vertices_selected }, Cmd.none )

        DeleteVertexSelection vertex ->
            ( { model | vertices_selected = updateVertexDeleted vertex model.vertices_selected }, Cmd.none )


cleanVertexNameInput : String -> String
cleanVertexNameInput input =
    String.replace " " "" input


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
            ( { model | vertex_name_search = Nothing }, Cmd.none )

        True ->
            ( { model | vertex_name_search = Just prefix }
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
                    ( { model | vertex_name_search_response = vertices }, Cmd.none )

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


updateWithVertexIdRequest model buildRequestArg toMsg =
    ( { model | state = Loading }, vertexIdsPost (buildRequestArg model) toMsg )


updateWithVertexIdResponse : Model -> Result Http.Error VertexIdsResponse -> Direction -> ( Model, Cmd Msg )
updateWithVertexIdResponse model result direction =
    case result of
        Ok response ->
            ( { model | vertex_ids_response = response.response_vertex_ids }
            , vertexDataPost (buildVertexDataRequest model) (VertexDataPostReceived direction)
            )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


updateWithVertexDataResponse : Model -> Result Http.Error VertexDataResponse -> Direction -> ( Model, Cmd Msg )
updateWithVertexDataResponse model result direction =
    case result of
        Ok response ->
            ( { model | state = VertexRequestsSuccess direction, vertex_data_response = unpackVertexDataResponse response }
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


buildVertexIdsRequest : String -> Model -> VertexIdsRequest
buildVertexIdsRequest directionString model =
    VertexIdsRequest (List.map getVertexId model.vertices_selected) directionString model.aggregation_selected


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


buildVertexDataRequest : Model -> VertexDataRequest
buildVertexDataRequest model =
    VertexDataRequest
        (VertexDataInnerRequest
            (VertexDataInnerRequestKey (List.map buildVertexDataRequestInnerValues model.vertex_ids_response))
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
            viewBuildingRequest model

        SearchConfirmed ->
            viewSearchConfirmed model

        Loading ->
            viewLoading

        VertexRequestsSuccess direction ->
            viewRequestSuccess direction model

        RequestFailure error ->
            viewRequestFailure error


viewSearchConfirmed : Model -> Html Msg
viewSearchConfirmed model =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
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


viewBuildingRequest : Model -> Html Msg
viewBuildingRequest model =
    case model.vertex_name_search of
        Nothing ->
            viewBuildingRequestWithNoInputButMaybeSomeConfirmed model

        Just title ->
            case title of
                "" ->
                    viewBuildingRequestWithNoInputButMaybeSomeConfirmed model

                _ ->
                    case model.vertices_selected of
                        [] ->
                            div [ class "dropdown" ]
                                [ dropDownHeadAndBody [ viewVertexNamePrefixResponse model ] ]

                        _ ->
                            div [ class "dropdown" ]
                                [ dropDownHeadAndBody
                                    [ div [] [ confirmSearchButton ]
                                    , viewVertexIdsSelected model
                                    , viewVertexNamePrefixResponse model
                                    ]
                                ]


viewNoInput : Html Msg
viewNoInput =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [] ]


viewBuildingRequestWithNoInputButMaybeSomeConfirmed : Model -> Html Msg
viewBuildingRequestWithNoInputButMaybeSomeConfirmed model =
    case model.vertices_selected of
        [] ->
            viewNoInput

        _ ->
            div [ class "dropdown" ]
                [ dropDownHeadAndBody [ div [] [ confirmSearchButton ], viewVertexIdsSelected model ] ]


viewLoading : Html Msg
viewLoading =
    div [ class "dropdown" ] [ text "Loading . . ." ]


viewRequestSuccess : Direction -> Model -> Html Msg
viewRequestSuccess direction model =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
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


dropdownHead : Html Msg
dropdownHead =
    p [ class "header" ] [ text ">Poli Graph Search<" ]


dropdownBody : List (Html Msg) -> Html Msg
dropdownBody moreHtml =
    div [ class "dropdown-body" ]
        ([ input [ class "search-box", onInput SearchInput, placeholder "committee/vendor name" ] [] ]
            ++ moreHtml
        )


dropDownHeadAndBody : List (Html Msg) -> Html Msg
dropDownHeadAndBody moreHtml =
    div [ class "dropdown" ]
        [ dropdownHead
        , dropdownBody moreHtml
        ]


makeRequestInDirectionButton : Html Msg
makeRequestInDirectionButton =
    button [ class "button", onClick (VertexIdsRequestMade In) ] [ text "in" ]


makeRequestOutDirectionButton : Html Msg
makeRequestOutDirectionButton =
    button [ class "button", onClick (VertexIdsRequestMade Out) ] [ text "out" ]


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
            viewResponse model "Direction: In"

        Out ->
            viewResponse model "Direction: Out"


viewResponse : Model -> String -> Html Msg
viewResponse model textToDisplay =
    div [ class "response" ]
        [ ul [ class "dropdown" ] ([ text "Searched: " ] ++ List.map fromVertexDataToHTMLNoButtons model.vertices_selected)
        , ul [] ([ text textToDisplay ] ++ htmlListItems model.vertex_ids_response)
        ]


htmlListItems : List String -> List (Html Msg)
htmlListItems items =
    List.map htmlListItem items


htmlListItem : String -> Html Msg
htmlListItem uid =
    li [] [ text uid ]



-- TODO: more fields


almostFromVertexDataToHTML : VertexData -> List (Html Msg) -> Html Msg
almostFromVertexDataToHTML vertexData html =
    li []
        (html
            ++ [ ul []
                    [ li []
                        [ text "name:"
                        , ul [] [ li [] [ text vertexData.name ] ]
                        ]
                    , li []
                        [ text "is_comittee:"
                        , ul [] [ li [] [ text (printBool vertexData.is_committee) ] ]
                        ]
                    , li []
                        [ text "cities:"
                        , ul [] [ li [] (List.map text vertexData.cities) ]
                        ]
                    ]
               ]
        )


fromVertexDataToHTMLWithSelectVertexButton : VertexData -> Html Msg
fromVertexDataToHTMLWithSelectVertexButton vertexData =
    almostFromVertexDataToHTML vertexData [ button [ onClick (VertexSelected vertexData) ] [ text "Select" ] ]


fromVertexDataToHTMLWithDeleteVertexButton : VertexData -> Html Msg
fromVertexDataToHTMLWithDeleteVertexButton vertexData =
    almostFromVertexDataToHTML vertexData [ button [ onClick (DeleteVertexSelection vertexData) ] [ text "delete" ] ]


fromVertexDataToHTMLNoButtons : VertexData -> Html Msg
fromVertexDataToHTMLNoButtons vertexData =
    almostFromVertexDataToHTML vertexData []
