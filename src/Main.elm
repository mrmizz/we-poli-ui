module Main exposing (main, updateWithVertexNamePrefixResponse)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode



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
    , vertex_name_input : Maybe String
    , vertex_data_response: List VertexData
    , vertex_ids_response : List String
    , aggregation_selected : String
    , vertices_selected : List String
    }

type alias VertexData =
    { uid: String
    , name: String
    , is_committee: Bool
    , cities: List String
    , streets: List String
    , states: List String
    }

getVertexId: VertexData -> String
getVertexId vertexData =
    vertexData.uid

printVertexData: VertexData -> String
printVertexData vertexData =
    "name: " ++ vertexData.name ++ "\n"
     ++ "is_committee: " ++ printBool vertexData.is_committee ++ "\n"
     ++ "cities: " ++ String.join ", " vertexData.cities ++ "\n"
     ++ "streets" ++ String.join ", " vertexData.streets ++ "\n"
     ++ "states" ++ String.join ", " vertexData.states ++ "\n"

printBool: Bool -> String
printBool bool =
    case bool of
        True -> "true"
        False -> "false"

type State
    = BuildingRequest
    | SearchConfirmed
    | Loading
    | VertexIdsRequestSuccess VertexIdsResponse Direction
    | RequestFailure Http.Error


initialModel : Model
initialModel =
    { state = BuildingRequest
    , vertex_name_input = Nothing
    , vertex_data_response = []
    , vertex_ids_response = []
    , aggregation_selected = defaultAggregationInput
    , vertices_selected = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


defaultAggregationInput : String
defaultAggregationInput =
    "Or"



-- UPDATE


type Msg
    = SearchInput String
    | AggOptionInput
    | VertexIdsRequestMade Direction
    | VertexIdsPostReceivedIn (Result Http.Error VertexIdsResponse)
    | VertexIdsPostReceivedOut (Result Http.Error VertexIdsResponse)
    | VertexNamePrefixGetReceived (Result Http.Error VertexNamePrefixResponse)
    | VertexDataGetReceived (Result Http.Error (List VertexDataResponse))
    | ClearSearch
    | AddSearch
    | ConfirmSearch String


type Direction
    = In
    | Out


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput prefix ->
            updateWithVertexNamePrefixRequest model prefix VertexNamePrefixGetReceived

        VertexNamePrefixGetReceived result ->
            updateWithVertexNamePrefixResponse model result VertexDataGetReceived

        VertexDataGetReceived result ->
            updateWithVertexDataResponse model result

        VertexIdsRequestMade direction ->
            case direction of
                In ->
                    updateWithVertexIdRequest model (vertexIdsBuildRequest "in") VertexIdsPostReceivedIn

                Out ->
                    updateWithVertexIdRequest model (vertexIdsBuildRequest "out") VertexIdsPostReceivedOut

        VertexIdsPostReceivedIn result ->
            updateWithVertexIdResponse model result In

        VertexIdsPostReceivedOut result ->
            updateWithVertexIdResponse model result Out

        ClearSearch ->
            ( initialModel, Cmd.none )

        AddSearch ->
            ( { model | state = BuildingRequest }, Cmd.none )

        ConfirmSearch title ->
            ( { model | state = SearchConfirmed, vertices_selected =  model.vertices_selected ++ [ title ] }, Cmd.none )

        AggOptionInput ->
            updateAggInputAndOptions model


cleanVertexNameInput : String -> String
cleanVertexNameInput input =
    String.replace " " "" input


updateWithVertexNamePrefixRequest : Model -> String -> (Result Http.Error VertexNamePrefixResponse -> Msg) -> ( Model, Cmd Msg )
updateWithVertexNamePrefixRequest model prefix toMsg =
    case String.length (cleanVertexNameInput prefix) >= 3 of
        False ->
            ( { model | vertex_name_input = Nothing }, Cmd.none )

        True ->
            ( { model | vertex_name_input = Just prefix }
            , vertexNamePrefixGet (cleanVertexNameInput prefix) toMsg
            )

updateWithVertexNamePrefixResponse: Model -> (Result Http.Error VertexNamePrefixResponse) -> ((Result Http.Error (List VertexDataResponse)) -> Msg) -> (Model, Cmd Msg)
updateWithVertexNamePrefixResponse model result toMsg =
    case result of
        Ok response ->
            case (unpackVertexNamePrefixResponse response) of
                [] ->
                    ( { model | vertex_ids_response = [] }, Cmd.none )
                uids ->
                   ( { model | vertex_ids_response = uids }, Cmd.batch (List.map (vertexDataGet toMsg) uids) )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )

updateWithVertexDataResponse: Model -> (Result Http.Error (List VertexDataResponse)) -> (Model, Cmd Msg)
updateWithVertexDataResponse model result =
    case result of
        Ok response ->
            ( { model | vertex_data_response = (List.concatMap unpackVertexDataResponse response) }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )

unpackVertexDataResponse : VertexDataResponse -> List VertexData
unpackVertexDataResponse response =
    case List.head response.items of
        Just head ->
            [VertexData (unpackDynamoValue head.uid) (unpackDynamoValue head.name) (unpackDynamoBool head.is_committee) (List.map unpackDynamoValue head.cities.list) (List.map unpackDynamoValue head.streets.list) (List.map unpackDynamoValue head.states.list)]

        Nothing ->
            []


unpackVertexNamePrefixResponse : VertexNamePrefixResponse -> List String
unpackVertexNamePrefixResponse response =
    case List.head response.items of
        Just head ->
            List.map unpackDynamoValue head.uids.list

        Nothing ->
            []


unpackDynamoValue : DynamoValue -> String
unpackDynamoValue dynamoValue =
    dynamoValue.value

unpackDynamoBool : DynamoBool -> Bool
unpackDynamoBool dynamoBool =
    dynamoBool.value

updateWithVertexIdRequest model buildRequestArg toMsg =
    ( { model | state = Loading }, vertexIdsPost (buildRequestArg model) toMsg )


updateWithVertexIdResponse model result direction =
    case result of
        Ok response ->
            ( { model | state = VertexIdsRequestSuccess response direction }, Cmd.none )

        Err error ->
            ( {model | state = RequestFailure error }, Cmd.none )


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
        (Decode.field "request_vertex_ids" (Decode.list Decode.string))
        (Decode.field "response_vertex_ids" (Decode.list Decode.string))


vertexIdsBuildRequest : String -> Model -> VertexIdsRequest
vertexIdsBuildRequest directionString model =
    VertexIdsRequest model.vertices_selected directionString model.aggregation_selected


type alias VertexNamePrefixResponse =
    { items : List VertexNamePrefixInnerResponse }

type alias VertexNamePrefixInnerResponse =
    { uids : DynamoArrayValue
    , prefix_size : DynamoValue
    , prefix : DynamoValue
    }

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
        (Decode.field "uids" dynamoArrayNumberValueDecoder)
        (Decode.field "prefix_size" dynamoNumberValueDecoder)
        (Decode.field "prefix" dynamoStringValueDecoder)


type alias VertexDataResponse =
    { items: List VertexDataInnerResponse }

type alias VertexDataInnerResponse =
    { name: DynamoValue
    , uid: DynamoValue
    , is_committee: DynamoBool
    , cities: DynamoArrayValue
    , streets: DynamoArrayValue
    , states: DynamoArrayValue
    }

vertexDataGet : (Result Http.Error (List VertexDataResponse) -> Msg) -> String -> Cmd Msg
vertexDataGet toMsg uid =
    Http.get
        { url = "https://yf87qmn85l.execute-api.us-west-2.amazonaws.com/v1/poli/id/" ++ uid
        , expect = Http.expectJson toMsg (Decode.list vertexDataResponseDecoder)
        }

vertexDataResponseDecoder: Decode.Decoder VertexDataResponse
vertexDataResponseDecoder =
    Decode.map VertexDataResponse (Decode.field "Items" (Decode.list vertexDataInnerResponseDecoder))

vertexDataInnerResponseDecoder: Decode.Decoder VertexDataInnerResponse
vertexDataInnerResponseDecoder =
    Decode.map6 VertexDataInnerResponse
        (Decode.field "name" dynamoStringValueDecoder)
        (Decode.field "uid" dynamoNumberValueDecoder)
        (Decode.field "is_committee" dynamoBoolDecoder)
        (Decode.field "cities" dynamoArrayStringValueDecoder)
        (Decode.field "streets" dynamoArrayStringValueDecoder)
        (Decode.field "states" dynamoArrayStringValueDecoder)

dynamoArrayNumberValueDecoder : Decode.Decoder DynamoArrayValue
dynamoArrayNumberValueDecoder =
    Decode.map DynamoArrayValue (Decode.field "L" (Decode.list dynamoNumberValueDecoder))

dynamoArrayStringValueDecoder : Decode.Decoder DynamoArrayValue
dynamoArrayStringValueDecoder =
    Decode.map DynamoArrayValue (Decode.field "L" (Decode.list dynamoStringValueDecoder))

dynamoNumberValueDecoder : Decode.Decoder DynamoValue
dynamoNumberValueDecoder =
    Decode.map DynamoValue (Decode.field "N" Decode.string)

dynamoStringValueDecoder : Decode.Decoder DynamoValue
dynamoStringValueDecoder =
    Decode.map DynamoValue (Decode.field "S" Decode.string)

dynamoBoolDecoder: Decode.Decoder DynamoBool
dynamoBoolDecoder =
    Decode.map DynamoBool (Decode.field "BOOL" Decode.bool)


type alias DynamoArrayValue =
    { list : List DynamoValue }

type alias DynamoValue =
    { value : String }

type alias DynamoBool =
    { value: Bool }






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

        VertexIdsRequestSuccess response direction ->
            viewRequestSuccess response direction model.aggregation_selected

        RequestFailure error ->
            viewRequestFailure error


viewSearchConfirmed : Model -> Html Msg
viewSearchConfirmed model =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
        , defaultClearSearchButton
        , addSearchButton
        , viewAggParam model.aggregation_selected
        , viewConfirmations model
        ]


viewVertexNamePrefixResponse : Model -> Html Msg
viewVertexNamePrefixResponse model =
    ul [ class "dropdown" ]
        ([ text "Potential Search Matches:" ] ++ List.map buildPotentialSearchMatch model.vertex_data_response)


buildPotentialSearchMatch vertexData =
    li [] [ text (printVertexData vertexData) ]


viewConfirmations : Model -> Html Msg
viewConfirmations model =
    ul [ class "dropdown" ]
        ([ text "We're Searching For:" ] ++ List.map fromTitleToUrlHtml model.vertices_selected)


viewBuildingRequest : Model -> Html Msg
viewBuildingRequest model =
    case model.vertex_name_input of
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
                                [ dropDownHeadAndBody [ confirmSearchButton title, viewVertexNamePrefixResponse model ] ]

                        _ ->
                            div [ class "dropdown" ]
                                [ dropDownHeadAndBody
                                    [ confirmSearchButton title
                                    , viewVertexNamePrefixResponse model
                                    , viewConfirmations model
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
                [ dropDownHeadAndBody [ viewConfirmations model ] ]


viewLoading : Html Msg
viewLoading =
    div [ class "dropdown" ] [ text "Loading . . ." ]


viewRequestSuccess : VertexIdsResponse -> Direction -> String -> Html Msg
viewRequestSuccess response direction agg =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
        , defaultClearSearchButton
        , viewAggParam agg
        , viewTitlesSearched response.request_vertex_ids
        , viewDirectedResponse response direction
        ]


viewRequestFailure : Http.Error -> Html Msg
viewRequestFailure error =
    case error of
        Http.BadUrl string ->
            almostClearSearchButton [ text ("Bad Url: " ++ string ++ ", Try Again!") ]

        Http.Timeout ->
            almostClearSearchButton [ text "Server Timeout, Try Again!" ]

        Http.NetworkError ->
            almostClearSearchButton [ text "Network Error, Try Again!" ]

        Http.BadStatus int ->
            almostClearSearchButton [ text (String.fromInt int ++ " Error: Bad Input, Try Again!") ]

        Http.BadBody body ->
            almostClearSearchButton [ text ("Bad Body: " ++ body ++ ", Try Again!") ]


viewAggParam : String -> Html Msg
viewAggParam agg =
    div [ class "dropdown" ] [ text "Aggregation: ", button [ onClick AggOptionInput ] [ text agg ] ]


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


addSearchButton : Html Msg
addSearchButton =
    button [ class "button", onClick AddSearch ] [ text "Add Search" ]


confirmSearchButton : String -> Html Msg
confirmSearchButton title =
    button [ class "button", onClick (ConfirmSearch title) ] [ text "Confirm" ]


viewTitlesSearched : List String -> Html Msg
viewTitlesSearched titles =
    ul [ class "dropdown" ] ([ text "Titles Searched: " ] ++ List.map fromTitleToUrlHtml titles)


viewDirectedResponse : VertexIdsResponse -> Direction -> Html Msg
viewDirectedResponse response direction =
    case direction of
        In ->
            viewResponse response "Direction: In"

        Out ->
            viewResponse response "Direction: Out"


viewResponse : VertexIdsResponse -> String -> Html Msg
viewResponse response textToDisplay =
    ul [ class "response" ]
        [ ul [] ([ text textToDisplay ] ++ responseItems response.response_vertex_ids) ]


responseItems : List String -> List (Html Msg)
responseItems items =
    List.map fromTitleToUrlHtml items


cleanTitle : String -> String
cleanTitle title =
    title
        |> String.replace "[" ""
        |> String.replace "]" ""


fromTitleToUrl : String -> String
fromTitleToUrl title =
    "https://en.wikipedia.org/wiki/"
        ++ (title
                |> cleanTitle
                |> String.replace " " "_"
           )


fromTitleToUrlHtml : String -> Html Msg
fromTitleToUrlHtml title =
    li [] [ a [ Html.Attributes.target "_blank", Html.Attributes.href (fromTitleToUrl title) ] [ text title ] ]
