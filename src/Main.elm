module Main exposing (main)

import Browser
import Html exposing (Html)
import Model.Model as Model exposing (Model)
import Model.State exposing (State(..))
import Msg.Msg exposing (Msg(..))
import Update.Update
import View.About
import View.BuildingRequest
import View.Loading
import View.RequestFailure
import View.VertexRequestSuccess



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

        BuildingRequest isModalActive ->
            View.BuildingRequest.view model isModalActive

        SearchConfirmed ->
            Html.div [] []

        Loading ->
            View.Loading.view

        VertexRequestsSuccess isModalActive ->
            View.VertexRequestSuccess.view model isModalActive

        RequestFailure error ->
            View.RequestFailure.view error

        DataIntegrityFailure ->
            Html.div [] []
