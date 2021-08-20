module Update.Configure exposing (updateWithConfiguration)

import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Msg.Msg exposing (Msg)


updateWithConfiguration : Model -> ( Model, Cmd Msg )
updateWithConfiguration model =
    case model.state of
        BuildingRequest bool ->
            case bool of
                True ->
                    ( { model | state = BuildingRequest False }, Cmd.none )

                False ->
                    ( { model | state = BuildingRequest True }, Cmd.none )

        VertexRequestsSuccess bool ->
            case bool of
                True ->
                    ( { model | state = VertexRequestsSuccess False }, Cmd.none )

                False ->
                    ( { model | state = VertexRequestsSuccess True }, Cmd.none )

        _ ->
            ( { model | state = BuildingRequest False }, Cmd.none )
