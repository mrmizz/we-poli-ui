module View.BuildingRequest exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)
import Model.Model exposing (Model)
import Msg.Msg exposing (Msg(..))
import View.Hero
import View.VertexData


view : Model -> Html Msg
view model =
    View.Hero.view (body model)


body : Model -> Html Msg
body model =
    let
        response : Html Msg
        response =
            case String.length model.vertex_name_search >= 3 of
                True ->
                    Html.div
                        []
                        [ View.VertexData.view model.vertex_name_search_response
                        ]

                False ->
                    Html.div [] []
    in
    Html.div
        [ class "container"
        ]
        [ Html.div
            [ class "field"
            ]
            [ Html.div
                [ class "control has-icons-left"
                ]
                [ Html.input
                    [ class "input is-input is-large"
                    , type_ "text"
                    , placeholder "Search for a Committee..."
                    , onInput SearchInput
                    ]
                    []
                , Html.span
                    [ class "icon is-left"
                    ]
                    [ Html.i
                        [ class "fab fa-searchengin"
                        ]
                        []
                    ]
                ]
            , response
            ]
        ]
