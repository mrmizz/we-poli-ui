module Main exposing (main)

import Browser
import Html exposing (Html)
import Model.Model as Model exposing (Model)
import Model.State exposing (State(..))
import Msg.Msg exposing (Msg(..))
import Update.Update
import View.About
import View.BuildingSearch
import View.Loading
import View.TraversalFailure
import View.TraversalSuccess



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = Model.init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Update.Update.update msg model



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        About ->
            View.About.view

        BuildingSearch isModalActive ->
            View.BuildingSearch.view model isModalActive

        Loading ->
            View.Loading.view

        TraversalSuccess isModalActive ->
            View.TraversalSuccess.view model isModalActive

        TraversalFailure error ->
            View.TraversalFailure.view error

        DataIntegrityFailure ->
            Html.div [] []
