module Update.Update exposing (update)

import Model.Model exposing (Model, initialModel)
import Model.State exposing (State(..))
import Msg.Msg exposing (Msg(..))
import Update.Configure exposing (updateWithConfiguration)
import Update.Edge exposing (updateWithEdgeDataResponse)
import Update.Misc exposing (updateVertexDeleted, updateVertexSelected, updateWithAggOption, updateWithDirectionOption, updateWithSortByOption)
import Update.NamePrefix exposing (updateWithVertexNamePrefixRequest, updateWithVertexNamePrefixResponse)
import Update.Traversal exposing (updateWithChildPageCountRequest, updateWithPageCountRequest, updateWithPageCountResponse, updateWithTraversalResponse)
import Update.Vertex exposing (updateWithVertexDataResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedAbout ->
            ( { initialModel | state = About }, Cmd.none )

        ClickedTool ->
            ( { initialModel | state = BuildingSearch False }, Cmd.none )

        ConfigureSearch ->
            updateWithConfiguration model

        VertexNameSearchInput prefix ->
            updateWithVertexNamePrefixRequest model prefix

        VertexNamePrefixGetReceived prefix result ->
            updateWithVertexNamePrefixResponse model prefix result

        TraversalRequestMade ->
            updateWithPageCountRequest model

        ChildTraversalRequestMade vertexData ->
            updateWithChildPageCountRequest model vertexData

        PageCountPostReceived result ->
            updateWithPageCountResponse model result

        TraversalPostReceived pageCount result ->
            updateWithTraversalResponse model pageCount result

        VertexDataPostReceived client result ->
            updateWithVertexDataResponse model client result

        EdgeDataPostReceived result ->
            updateWithEdgeDataResponse model result

        ClearSearch ->
            ( initialModel, Cmd.none )

        AggOptionSelected ->
            updateWithAggOption model

        DirectionOptionSelected ->
            updateWithDirectionOption model

        VertexSelected vertex ->
            updateVertexSelected model vertex

        DeleteVertexSelection vertex ->
            updateVertexDeleted model vertex

        SortByOptionSelected sortBy ->
            updateWithSortByOption model sortBy
