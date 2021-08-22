module Update.Configure exposing (updateWithConfiguration)

import Model.Model exposing (Model)
import Model.State exposing (State(..))
import Msg.Msg exposing (Msg)


updateWithConfiguration : Model -> ( Model, Cmd Msg )
updateWithConfiguration model =
    case model.state of
        BuildingSearch bool ->
            case bool of
                True ->
                    ( { model | state = BuildingSearch False }, Cmd.none )

                False ->
                    ( { model | state = BuildingSearch True }, Cmd.none )

        TraversalSuccess bool ->
            case bool of
                True ->
                    ( { model | state = TraversalSuccess False }, Cmd.none )

                False ->
                    ( { model | state = TraversalSuccess True }, Cmd.none )

        _ ->
            ( { model | state = BuildingSearch False }, Cmd.none )
