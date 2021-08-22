module Update.Vertex exposing (updateWithVertexDataResponse)

import Http
import Http.Vertex exposing (VertexDataResponse)
import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Model.Traversal as Traversal exposing (Traversal(..))
import Model.VertexData exposing (VertexData)
import Model.VertexNameSearch as VertexNameSearch
import Model.Zipped as Zipped
import Msg.Msg exposing (Msg(..), VertexDataClient(..))
import Update.Generic exposing (unpackDynamoVertexData)


updateWithVertexDataResponse : Model -> VertexDataClient -> Result Http.Error VertexDataResponse -> ( Model, Cmd Msg )
updateWithVertexDataResponse model client result =
    case result of
        Ok response ->
            let
                unpack : VertexDataResponse -> List VertexData
                unpack vertexDataResponse =
                    List.map unpackDynamoVertexData vertexDataResponse.responses.items

                vertices : List VertexData
                vertices =
                    unpack response
            in
            case client of
                ForNameSearch prefix sortedVertexIds ->
                    updateForNameSearch model prefix sortedVertexIds vertices

                ForTraversal ->
                    updateForTraversal model vertices

        Err error ->
            ( { model | state = TraversalFailure error }, Cmd.none )


updateForNameSearch : Model -> String -> List String -> List VertexData -> ( Model, Cmd Msg )
updateForNameSearch model prefix sortedVertexIds unsortedVertices =
    let
        sortedVertices : List VertexData
        sortedVertices =
            VertexNameSearch.sort sortedVertexIds unsortedVertices
    in
    ( { model
        | vertex_name_search = VertexNameSearch.Waiting prefix sortedVertices
      }
    , Cmd.none
    )


updateForTraversal : Model -> List VertexData -> ( Model, Cmd Msg )
updateForTraversal model vertices =
    case model.traversal of
        Waiting pageCount ->
            ( { model | traversal = WaitingForEdges pageCount vertices }
            , Cmd.none
            )

        WaitingForVertices pageCount edges ->
            let
                zipped =
                    Zipped.zip model.direction_selected vertices edges

                sorted =
                    Zipped.sortBy model.sort_by_selected zipped
            in
            ( { model
                | state = TraversalSuccess False
                , traversal = Traversal.Done pageCount
                , zipped = sorted
              }
            , Cmd.none
            )

        _ ->
            ( { model | state = DataIntegrityFailure }, Cmd.none )
