module View.BuildingRequest exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Msg.Msg exposing (Msg)
import View.Hero


view : Html Msg
view =
    View.Hero.view body


body : Html Msg
body =
    Html.div
        [ class "container has-text-centered"
        ]
        [ Html.input
            [ class "input is-input is-large"
            , type_ "text"
            , placeholder "Search for a Committee..."
            ]
            []
        ]
