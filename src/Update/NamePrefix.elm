module Update.NamePrefix exposing (updateWithVertexNamePrefixRequest, updateWithVertexNamePrefixResponse)

import Http
import Http.NamePrefix exposing (VertexNamePrefixResponse, vertexNamePrefixGet)
import Http.Vertex exposing (buildVertexDataRequest, vertexDataPost)
import Model.Direction as Direction
import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Model.VertexNameSearch as VertexNameSearch
import Msg.Msg exposing (Msg(..), VertexDataClient(..))
import Update.Generic exposing (unpackDynamoArrayNumber)
import Util.Util exposing (printBool)


updateWithVertexNamePrefixRequest : Model -> String -> ( Model, Cmd Msg )
updateWithVertexNamePrefixRequest model prefix =
    let
        clean : String -> String
        clean input =
            printBool (Direction.toIsCommittee model.direction_selected)
                |> String.append "_"
                |> String.append (String.replace " " "" input)
                -- TODO: reverse order ^ ?
                |> String.toLower

        new : Model
        new =
            { model | vertex_name_search = VertexNameSearch.Input prefix }
    in
    case String.length prefix >= 3 of
        False ->
            ( new, Cmd.none )

        True ->
            ( new
            , vertexNamePrefixGet (clean prefix) (VertexNamePrefixGetReceived prefix)
            )


updateWithVertexNamePrefixResponse : Model -> String -> Result Http.Error VertexNamePrefixResponse -> ( Model, Cmd Msg )
updateWithVertexNamePrefixResponse model prefix result =
    case result of
        Ok response ->
            case response.items of
                head :: [] ->
                    let
                        unpack =
                            unpackDynamoArrayNumber head.vertexIds
                    in
                    ( model
                    , vertexDataPost
                        (buildVertexDataRequest unpack)
                        (VertexDataPostReceived (ForNameSearch prefix unpack))
                    )

                _ ->
                    -- keep typing
                    ( model, Cmd.none )

        Err error ->
            ( { model | state = TraversalFailure error }, Cmd.none )
