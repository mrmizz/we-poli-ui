module Main exposing (main)

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
    , vertex_id_input : Maybe String
    , aggregation_input : String
    , aggregation_options : List String
    , selected : List String
    }


type State
    = BuildingRequest
    | SearchConfirmed
    | Loading
    | RequestSuccess Response Direction
    | RequestFailure Http.Error


type alias Response =
    { request_vertex_ids : List String
    , response_vertex_ids : List String
    }


initialModel : Model
initialModel =
    { state = BuildingRequest
    , vertex_id_input = Nothing
    , aggregation_input = defaultAggregationInput
    , aggregation_options = defaultAggregationOptions
    , selected = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


defaultAggregationInput : String
defaultAggregationInput =
    "Or"


defaultAggregationOptions : List String
defaultAggregationOptions =
    [ "Or", "And" ]



-- UPDATE


type Msg
    = SearchInput String
    | AggOptionInput String
    | RequestMade Direction
    | PostReceivedIn (Result Http.Error Response)
    | PostReceivedOut (Result Http.Error Response)
    | ClearSearch
    | AddSearch
    | ConfirmSearch String


type Direction
    = In
    | Out


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | vertex_id_input = Just query }, Cmd.none )

        RequestMade direction ->
            case direction of
                In ->
                    updateWithRequest model (buildRequest "in") PostReceivedIn

                Out ->
                    updateWithRequest model (buildRequest "out") PostReceivedOut

        PostReceivedIn result ->
            updateWithResponse model result In

        PostReceivedOut result ->
            updateWithResponse model result Out

        ClearSearch ->
            ( initialModel, Cmd.none )

        AddSearch ->
            ( { model | state = BuildingRequest }, Cmd.none )

        ConfirmSearch title ->
            ( { model | state = SearchConfirmed, selected = model.selected ++ [ title ] }, Cmd.none )

        AggOptionInput opt ->
            updateAggInputAndOptions opt model


updateWithRequest model buildRequestArg toMsg =
    ( { model | state = Loading }, post (buildRequestArg model) toMsg )


updateWithResponse model result direction =
    case result of
        Ok response ->
            ( { model | state = RequestSuccess response direction }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


updateAggInputAndOptions : String -> Model -> ( Model, Cmd Msg )
updateAggInputAndOptions selected model =
    case selected of
        "Or" ->
            ( { model | aggregation_input = selected, aggregation_options = [ "Or", "And" ] }, Cmd.none )

        "And" ->
            ( { model | aggregation_input = selected, aggregation_options = [ "And", "Or" ] }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- HTTP


type alias Request =
    { vertex_ids : List String
    , direction : String
    , agg : String
    }


post : Request -> (Result Http.Error Response -> msg) -> Cmd msg
post request msg =
    Http.post
        { url = "https://7qfeute799.execute-api.us-west-2.amazonaws.com/default/v1/tap-in"
        , body = Http.jsonBody (requestEncoder request)
        , expect = Http.expectJson msg responseDecoder
        }


requestEncoder : Request -> Encode.Value
requestEncoder request =
    Encode.object
        [ ( "vertex_ids", Encode.list Encode.string request.vertex_ids )
        , ( "direction", Encode.string request.direction )
        , ( "agg", Encode.string request.agg )
        ]


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map2 Response
        (Decode.field "request_vertex_ids" (Decode.list Decode.string))
        (Decode.field "response_vertex_ids" (Decode.list Decode.string))


buildRequest : String -> Model -> Request
buildRequest directionString model =
    Request model.selected directionString model.aggregation_input



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

        RequestSuccess response direction ->
            viewRequestSuccess response direction model.aggregation_input

        RequestFailure error ->
            viewRequestFailure error


viewSearchConfirmed : Model -> Html Msg
viewSearchConfirmed model =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
        , defaultClearSearchButton
        , addSearchButton
        , viewAggOptions "Aggregation: " model.aggregation_options model.aggregation_input
        , viewConfirmations model
        ]


viewConfirmations : Model -> Html Msg
viewConfirmations model =
    ul [ class "dropdown" ]
        ([ text "We're Searching For:" ] ++ List.map fromTitleToUrlHtml model.selected)


viewBuildingRequest : Model -> Html Msg
viewBuildingRequest model =
    case model.vertex_id_input of
        Nothing ->
            viewBuildingRequestWithNoInputButMaybeSomeConfirmed model

        Just title ->
            case title of
                "" ->
                    viewBuildingRequestWithNoInputButMaybeSomeConfirmed model

                _ ->
                    case model.selected of
                        [] ->
                            div [ class "dropdown" ]
                                [ dropDownHeadAndBody [ confirmSearchButton title ] ]

                        _ ->
                            div [ class "dropdown" ]
                                [ dropDownHeadAndBody [ confirmSearchButton title, viewConfirmations model ] ]


viewNoInput : Html Msg
viewNoInput =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [] ]


viewBuildingRequestWithNoInputButMaybeSomeConfirmed model =
    case model.selected of
        [] ->
            viewNoInput

        _ ->
            div [ class "dropdown" ]
                [ dropDownHeadAndBody [ viewConfirmations model ] ]


viewLoading : Html Msg
viewLoading =
    div [ class "dropdown" ] [ text "Loading . . ." ]


viewRequestSuccess : Response -> Direction -> String -> Html Msg
viewRequestSuccess response direction agg =
    div [ class "dropdown" ]
        [ dropDownHeadAndBody [ makeRequestInDirectionButton, makeRequestOutDirectionButton ]
        , defaultClearSearchButton
        , viewAggregationUsed agg
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
            almostClearSearchButton [ text (String.fromInt int ++ " Error: Bad Title Input, Try Again!") ]

        Http.BadBody body ->
            almostClearSearchButton [ text ("Bad Body: " ++ body ++ ", Try Again!") ]


viewAggOptions : String -> List String -> String -> Html Msg
viewAggOptions title optionNames agg =
    div [ class "dropdown-options" ]
        [ text title
        , select [ onInput AggOptionInput ] (List.map viewAggOption optionNames)
        , ul [] [ text "Aggregation: ", li [] [ text agg ] ]
        ]


viewAggOption : String -> Html Msg
viewAggOption optionName =
    option [] [ text optionName ]


dropdownHead : Html Msg
dropdownHead =
    p [ class "header" ] [ text ">Poli Graph Search<" ]


dropdownBody : List (Html Msg) -> Html Msg
dropdownBody moreHtml =
    div [ class "dropdown-body" ]
        ([ input [ class "search-box", onInput SearchInput, placeholder "vertex id" ] [] ]
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
    button [ class "button", onClick (RequestMade In) ] [ text "in" ]


makeRequestOutDirectionButton : Html Msg
makeRequestOutDirectionButton =
    button [ class "button", onClick (RequestMade Out) ] [ text "out" ]


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


viewAggregationUsed : String -> Html Msg
viewAggregationUsed agg =
    ul [ class "dropdown" ] [ text "Aggregation: ", li [] [ text agg ] ]


viewTitlesSearched : List String -> Html Msg
viewTitlesSearched titles =
    ul [ class "dropdown" ] ([ text "Titles Searched: " ] ++ List.map fromTitleToUrlHtml titles)


viewDirectedResponse : Response -> Direction -> Html Msg
viewDirectedResponse response direction =
    case direction of
        In ->
            viewResponse response "Direction: In"

        Out ->
            viewResponse response "Direction: Out"


viewResponse : Response -> String -> Html Msg
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
