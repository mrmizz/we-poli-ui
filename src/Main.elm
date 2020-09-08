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



{- BACKEND URLs -}


graphDataURL : String
graphDataURL =
    "https://yf87qmn85l.execute-api.us-west-2.amazonaws.com/prod/poli/graph"


prefixURL : String
prefixURL =
    "https://yf87qmn85l.execute-api.us-west-2.amazonaws.com/prod/poli/prefix/"



-- Model


type alias Model =
    { state : State
    , vertex_name_search : String
    , vertex_name_search_response : List VertexData
    , vertices_selected : List VertexData
    , aggregation_selected : Aggregation
    , direction_selected : Direction
    , traversal_response : List Traversal
    , traversal_data_response : List VertexData
    , agg_traversal_data_response : List VertexData
    }


type alias Traversal =
    { src_id : String
    , dst_ids : List String
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


notUID : String -> VertexData -> Bool
notUID uid vertex =
    vertex.uid /= uid


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
    , aggregation_selected = Or
    , vertices_selected = []
    , direction_selected = Out
    , traversal_response = []
    , traversal_data_response = []
    , agg_traversal_data_response = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



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
    | TraversalRequestMade
    | ChildTraversalRequestMade VertexData
    | VertexDataPostReceived (Result Http.Error VertexDataResponse)
    | VertexNamePrefixGetReceived (Result Http.Error VertexNamePrefixResponse)
    | TraversalPostReceived (Result Http.Error TraversalResponse)


type Direction
    = In
    | Out


type Aggregation
    = And
    | Or


printAgg : Aggregation -> String
printAgg agg =
    case agg of
        And ->
            "And"

        Or ->
            "Or"


switchDirection : Direction -> Direction
switchDirection direction =
    case direction of
        In ->
            Out

        Out ->
            In


updateWithDirectionOption : Model -> ( Model, Cmd Msg )
updateWithDirectionOption model =
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

        TraversalRequestMade ->
            updateWithTraversalRequest model

        TraversalPostReceived result ->
            updateWithTraversalResponse model result

        ChildTraversalRequestMade vertexData ->
            updateWithChildTraversalRequest model vertexData

        VertexDataPostReceived result ->
            updateWithVertexDataResponse model result

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


cleanVertexNameInput : String -> Model -> String
cleanVertexNameInput input model =
    printBool (directionToIsCommittee model.direction_selected)
        |> String.append "_"
        |> String.append (String.replace " " "" input)
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
            List.filter (notUID vertex.uid) vertices


updateWithVertexNamePrefixRequest : Model -> String -> (Result Http.Error VertexNamePrefixResponse -> Msg) -> ( Model, Cmd Msg )
updateWithVertexNamePrefixRequest model prefix toMsg =
    case String.length prefix >= 3 of
        False ->
            ( { model | vertex_name_search = prefix }, Cmd.none )

        True ->
            ( { model | vertex_name_search = prefix }
            , vertexNamePrefixGet (cleanVertexNameInput prefix model) toMsg
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


updateWithTraversalRequest : Model -> ( Model, Cmd Msg )
updateWithTraversalRequest model =
    ( { model | state = Loading }
    , traversalPost
        (buildTraversalRequest (List.map (\vertexData -> TraversalPage (getVertexId vertexData) "1") model.vertices_selected))
        TraversalPostReceived
    )


updateWithChildTraversalRequest : Model -> VertexData -> ( Model, Cmd Msg )
updateWithChildTraversalRequest model vertexData =
    ( { model | state = Loading, vertices_selected = [ vertexData ], direction_selected = switchDirection model.direction_selected }
    , traversalPost
        (buildTraversalRequest [ TraversalPage (getVertexId vertexData) "1" ])
        TraversalPostReceived
    )


updateWithVertexDataResponse : Model -> Result Http.Error VertexDataResponse -> ( Model, Cmd Msg )
updateWithVertexDataResponse model result =
    case result of
        Ok response ->
            let
                unpacked : List VertexData
                unpacked =
                    unpackVertexDataResponse response
            in
            ( { model
                | state = VertexRequestsSuccess
                , traversal_data_response = unpacked
                , agg_traversal_data_response = aggregateVertices model unpacked
              }
            , Cmd.none
            )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


aggregateVertices : Model -> List VertexData -> List VertexData
aggregateVertices model vertices =
    case model.aggregation_selected of
        And ->
            case model.traversal_response of
                _ :: [] ->
                    vertices

                head :: tail ->
                    let
                        headSet : Set String
                        headSet =
                            Set.fromList head.dst_ids

                        tailSets : List (Set String)
                        tailSets =
                            List.map (\trv -> Set.fromList trv.dst_ids) tail

                        intersection : Set String
                        intersection =
                            List.foldl Set.intersect headSet tailSets
                    in
                    List.filter (\vertex -> Set.member (getVertexId vertex) intersection) vertices

                [] ->
                    vertices

        Or ->
            vertices



-- TODO: edge analytics


zipVerticesWithSrcId : Model -> List VertexData -> List ( String, VertexData )
zipVerticesWithSrcId model vertices =
    let
        clause : Traversal -> VertexData -> Maybe ( String, VertexData )
        clause traversal vertex =
            case List.member (getVertexId vertex) traversal.dst_ids of
                True ->
                    Just ( traversal.src_id, vertex )

                False ->
                    Nothing
    in
    List.concatMap
        (\traversal ->
            List.filterMap
                (\vertexData -> clause traversal vertexData)
                vertices
        )
        model.traversal_response


unpackVertexDataResponse : VertexDataResponse -> List VertexData
unpackVertexDataResponse vertexDataResponse =
    List.map unpackDynamoVertexData vertexDataResponse.responses.items


updateWithAggOption : Model -> ( Model, Cmd Msg )
updateWithAggOption model =
    let
        semiUpdate : Model
        semiUpdate =
            case model.aggregation_selected of
                And ->
                    { model | aggregation_selected = Or }

                Or ->
                    { model | aggregation_selected = And }

        agg : List VertexData
        agg =
            aggregateVertices semiUpdate semiUpdate.traversal_data_response
    in
    ( { semiUpdate | agg_traversal_data_response = agg }, Cmd.none )


updateWithTraversalResponse : Model -> Result Http.Error TraversalResponse -> ( Model, Cmd Msg )
updateWithTraversalResponse model result =
    case result of
        Ok response ->
            let
                traversals : List Traversal
                traversals =
                    unpackTraversalResponse response
            in
            ( { model | traversal_response = traversals }
            , vertexDataPost
                -- TODO: paginate requests
                (buildVertexDataRequest
                    (List.concatMap (\trv -> trv.dst_ids) traversals
                        |> Set.fromList
                        |> Set.toList
                        |> List.take 99
                    )
                )
                VertexDataPostReceived
            )

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



-- HTTP


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
        { url = prefixURL ++ prefix
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
    { poli_vertex : VertexDataInnerRequestKeys }


type alias VertexDataInnerRequestKeys =
    { keys : List VertexDataInnerRequestKey }


type alias VertexDataInnerRequestKey =
    { uid : DynamoValue }


vertexDataPost : VertexDataRequest -> (Result Http.Error VertexDataResponse -> Msg) -> Cmd Msg
vertexDataPost request toMsg =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (vertexDataRequestEncoder request)
        , expect = Http.expectJson toMsg vertexDataResponseDecoder
        }


buildVertexDataRequest : List String -> VertexDataRequest
buildVertexDataRequest uids =
    VertexDataRequest
        (VertexDataInnerRequest
            (VertexDataInnerRequestKeys (List.map buildVertexDataRequestInnerValues uids))
        )


buildVertexDataRequestInnerValues : String -> VertexDataInnerRequestKey
buildVertexDataRequestInnerValues uid =
    VertexDataInnerRequestKey (DynamoValue uid)


vertexDataRequestEncoder : VertexDataRequest -> Encode.Value
vertexDataRequestEncoder vertexDataRequest =
    Encode.object
        [ ( "RequestItems", vertexDataInnerRequestEncoder vertexDataRequest.request_items ) ]


vertexDataInnerRequestEncoder : VertexDataInnerRequest -> Encode.Value
vertexDataInnerRequestEncoder vertexDataInnerRequest =
    Encode.object
        [ ( "PoliVertex", vertexDataInnerRequestKeysEncoder vertexDataInnerRequest.poli_vertex ) ]


vertexDataInnerRequestKeysEncoder : VertexDataInnerRequestKeys -> Encode.Value
vertexDataInnerRequestKeysEncoder vertexDataInnerRequestKey =
    Encode.object
        [ ( "Keys", Encode.list vertexDataInnerRequestKeyEncoder vertexDataInnerRequestKey.keys ) ]


vertexDataInnerRequestKeyEncoder : VertexDataInnerRequestKey -> Encode.Value
vertexDataInnerRequestKeyEncoder vertexDataInnerRequestUID =
    Encode.object
        [ ( "uid", dynamoNumberValueEncoder vertexDataInnerRequestUID.uid ) ]


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


type alias TraversalRequest =
    { request_items : TraversalInnerRequest }


type alias TraversalInnerRequest =
    { poli_traversals_page : TraversalInnerRequestKeys }


type alias TraversalInnerRequestKeys =
    { keys : List TraversalInnerRequestKey }


type alias TraversalInnerRequestKey =
    { vertex_id : DynamoValue
    , page_num : DynamoValue
    }


traversalPost : TraversalRequest -> (Result Http.Error TraversalResponse -> Msg) -> Cmd Msg
traversalPost request toMsg =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (traversalRequestEncoder request)
        , expect = Http.expectJson toMsg traversalResponseDecoder
        }


type alias TraversalPage =
    { vertex_id : String
    , page_number : String
    }


buildTraversalRequest : List TraversalPage -> TraversalRequest
buildTraversalRequest traversalPages =
    TraversalRequest
        (TraversalInnerRequest
            (TraversalInnerRequestKeys (List.map buildTraversalRequestInnerValues traversalPages))
        )


buildTraversalRequestInnerValues : TraversalPage -> TraversalInnerRequestKey
buildTraversalRequestInnerValues traversalPage =
    TraversalInnerRequestKey (DynamoValue traversalPage.vertex_id) (DynamoValue traversalPage.page_number)


traversalRequestEncoder : TraversalRequest -> Encode.Value
traversalRequestEncoder traversalRequest =
    Encode.object
        [ ( "RequestItems", traversalInnerRequestEncoder traversalRequest.request_items ) ]


traversalInnerRequestEncoder : TraversalInnerRequest -> Encode.Value
traversalInnerRequestEncoder traversalInnerRequest =
    Encode.object
        [ ( "PoliTraversalsPage", traversalInnerRequestKeysEncoder traversalInnerRequest.poli_traversals_page ) ]


traversalInnerRequestKeysEncoder : TraversalInnerRequestKeys -> Encode.Value
traversalInnerRequestKeysEncoder traversalInnerRequestKeys =
    Encode.object
        [ ( "Keys", Encode.list traversalInnerRequestKeyEncoder traversalInnerRequestKeys.keys ) ]


traversalInnerRequestKeyEncoder : TraversalInnerRequestKey -> Encode.Value
traversalInnerRequestKeyEncoder traversalInnerRequestKey =
    Encode.object
        [ ( "vertex_id", dynamoNumberValueEncoder traversalInnerRequestKey.vertex_id )
        , ( "page_num", dynamoNumberValueEncoder traversalInnerRequestKey.page_num )
        ]


type alias TraversalResponse =
    { responses : PoliTraversalsPageTable }


type alias PoliTraversalsPageTable =
    { items : List DynamoTraversal }


type alias DynamoTraversal =
    { vertex_id : DynamoValue
    , page_num : DynamoValue
    , related_vertex_ids : DynamoArrayValue
    }


traversalResponseDecoder : Decode.Decoder TraversalResponse
traversalResponseDecoder =
    Decode.map TraversalResponse (Decode.field "Responses" poliTraversalsPageTable)


poliTraversalsPageTable : Decode.Decoder PoliTraversalsPageTable
poliTraversalsPageTable =
    Decode.map PoliTraversalsPageTable (Decode.field "PoliTraversalsPage" (Decode.list traversalInnerResponseDecoder))


traversalInnerResponseDecoder : Decode.Decoder DynamoTraversal
traversalInnerResponseDecoder =
    Decode.map3 DynamoTraversal
        (Decode.field "vertex_id" dynamoNumberValueDecoder)
        (Decode.field "page_num" dynamoNumberValueDecoder)
        (Decode.field "related_vertex_ids" dynamoArrayNumberValueDecoder)


type alias EdgeRequest =
    { request_items : EdgeInnerRequest }


type alias EdgeInnerRequest =
    { poli_edge : EdgeInnerRequestKeys }


type alias EdgeInnerRequestKeys =
    { keys : List EdgeInnerRequestKey }


type alias EdgeInnerRequestKey =
    { src_id : DynamoValue
    , dst_id : DynamoValue
    }


edgePost : EdgeRequest -> (Result Http.Error EdgeResponse -> Msg) -> Cmd Msg
edgePost request toMsg =
    Http.post
        { url = graphDataURL
        , body = Http.jsonBody (edgeRequestEncoder request)
        , expect = Http.expectJson toMsg edgeResponseDecoder
        }


type alias Edge =
    { src_id : String
    , dst_id : String
    }


buildEdgeRequest : List Edge -> EdgeRequest
buildEdgeRequest edges =
    EdgeRequest
        (EdgeInnerRequest
            (EdgeInnerRequestKeys (List.map buildEdgeRequestInnerValues edges))
        )


buildEdgeRequestInnerValues : Edge -> EdgeInnerRequestKey
buildEdgeRequestInnerValues edge =
    EdgeInnerRequestKey (DynamoValue edge.src_id) (DynamoValue edge.dst_id)


edgeRequestEncoder : EdgeRequest -> Encode.Value
edgeRequestEncoder edgeRequest =
    Encode.object
        [ ( "RequestItems", edgeInnerRequestEncoder edgeRequest.request_items ) ]


edgeInnerRequestEncoder : EdgeInnerRequest -> Encode.Value
edgeInnerRequestEncoder edgeInnerRequest =
    Encode.object
        [ ( "PoliEdge", edgeInnerRequestKeysEncoder edgeInnerRequest.poli_edge ) ]


edgeInnerRequestKeysEncoder : EdgeInnerRequestKeys -> Encode.Value
edgeInnerRequestKeysEncoder edgeInnerRequestKeys =
    Encode.object
        [ ( "Keys", Encode.list edgeInnerRequestKeyEncoder edgeInnerRequestKeys.keys ) ]


edgeInnerRequestKeyEncoder : EdgeInnerRequestKey -> Encode.Value
edgeInnerRequestKeyEncoder edgeInnerRequestKey =
    Encode.object
        [ ( "src_id", dynamoNumberValueEncoder edgeInnerRequestKey.src_id )
        , ( "dst_id", dynamoNumberValueEncoder edgeInnerRequestKey.dst_id )
        ]


type alias EdgeResponse =
    { responses : PoliEdgeTable }


type alias PoliEdgeTable =
    { items : List DynamoEdge }


type alias DynamoEdge =
    { src_id : DynamoValue
    , dst_id : DynamoValue
    , num_transactions : DynamoValue
    , total_spend : DynamoValue
    , avg_spend : DynamoValue
    , max_spend : DynamoValue
    , min_spend : DynamoValue
    }


edgeResponseDecoder : Decode.Decoder EdgeResponse
edgeResponseDecoder =
    Decode.map EdgeResponse (Decode.field "Responses" poliEdgeTableDecoder)


poliEdgeTableDecoder : Decode.Decoder PoliEdgeTable
poliEdgeTableDecoder =
    Decode.map PoliEdgeTable (Decode.field "PoliEdge" (Decode.list edgeInnerResponseDecoder))


edgeInnerResponseDecoder : Decode.Decoder DynamoEdge
edgeInnerResponseDecoder =
    Decode.map7 DynamoEdge
        (Decode.field "src_id" dynamoNumberValueDecoder)
        (Decode.field "dst_id" dynamoNumberValueDecoder)
        (Decode.field "num_transactions" dynamoNumberValueDecoder)
        (Decode.field "total_spend" dynamoNumberValueDecoder)
        (Decode.field "avg_spend" dynamoNumberValueDecoder)
        (Decode.field "max_spend" dynamoNumberValueDecoder)
        (Decode.field "min_spend" dynamoNumberValueDecoder)


type alias DynamoArrayValue =
    { list : List DynamoValue }


type alias DynamoValue =
    { value : String }


type alias DynamoBool =
    { value : Bool }


dynamoArrayStringValueDecoder : Decode.Decoder DynamoArrayValue
dynamoArrayStringValueDecoder =
    Decode.map DynamoArrayValue (Decode.field "L" (Decode.list dynamoStringValueDecoder))


dynamoArrayNumberValueDecoder : Decode.Decoder DynamoArrayValue
dynamoArrayNumberValueDecoder =
    Decode.map DynamoArrayValue (Decode.field "L" (Decode.list dynamoNumberValueDecoder))


dynamoNumberValueDecoder : Decode.Decoder DynamoValue
dynamoNumberValueDecoder =
    Decode.map DynamoValue (Decode.field "N" Decode.string)


dynamoNumberValueEncoder : DynamoValue -> Encode.Value
dynamoNumberValueEncoder dynamoValue =
    Encode.object
        [ ( "N", Encode.string dynamoValue.value ) ]


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
    Element.layout [] (elementView model)


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


viewLoading : Element Msg
viewLoading =
    Element.el [] (Element.text "Loading . . .")


viewRequestSuccess : Direction -> Model -> Element Msg
viewRequestSuccess direction model =
    almostDropdownHeadAndBody (dropdownHead model)
        [ makeVertexIdsRequestButton
        , editSearchButton
        , clearSearchButton
        , viewAggParam model.aggregation_selected
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
    Input.button [] { onPress = Just AggOptionSelected, label = buttonStyle (Element.text (printAgg agg)) }


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
    , Element.scrollbarX
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
            , fromVerticesToTableWithSearchButton model.agg_traversal_data_response
            ]
        ]



-- TODO: more fields


fromVerticesToTable : List VertexData -> Element Msg
fromVerticesToTable vertices =
    Element.column tableStyle
        (List.map fromVertexDataToRow vertices)


tableStyle : List (Element.Attribute Msg)
tableStyle =
    [ Element.spacing 10 ]


fromVertexDataToRow : VertexData -> Element Msg
fromVertexDataToRow vertex =
    almostFromVertexDataToRow vertex Element.none


almostFromVertexDataToRow : VertexData -> Element Msg -> Element Msg
almostFromVertexDataToRow vertex anotherElement =
    Element.row [ Element.spacing 10 ]
        [ anotherElement
        , uidColumn vertex
        , isCommitteeColumn vertex
        , nameColumn vertex
        , citiesColumn vertex
        , statesColumn vertex
        ]


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


almostFromVerticesToTable : List VertexData -> (VertexData -> Msg) -> String -> Element Msg
almostFromVerticesToTable vertices buttonMsg buttonName =
    Element.column tableStyle
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
    almostFromVerticesToTable vertices ChildTraversalRequestMade "Search"


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
