module View.BuildingRequest exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Model.Model exposing (Model)
import Model.VertexData exposing (VertexData)
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
                        (List.map selectable model.vertex_name_search_response)

                False ->
                    Html.div [] []

        selected_ : Html Msg
        selected_ =
            case model.vertices_selected of
                [] ->
                    Html.div [] []

                nel ->
                    Html.div
                        [ class "pb-6"
                        ]
                        (List.map selected nel)
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
            , selected_
            , response
            ]
        ]


selectable : VertexData -> Html Msg
selectable vertex =
    let
        select =
            Html.button
                [ class "button is-link is-light is-fullwidth"
                , onClick (VertexSelected vertex)
                ]
                [ Html.text "Add to search"
                ]
    in
    Html.div
        [ class "box"
        ]
        [ select
        , View.VertexData.view vertex
        ]


selected : VertexData -> Html Msg
selected vertex =
    let
        delete : Html Msg
        delete =
            Html.button
                [ class "button is-link is-light is-fullwidth"
                , onClick (DeleteVertexSelection vertex)
                ]
                [ Html.text "Delete from search"
                ]
    in
    Html.div
        [ class "box"
        ]
        [ delete
        , View.VertexData.viewMin vertex
        ]
