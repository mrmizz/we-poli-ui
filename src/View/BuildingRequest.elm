module View.BuildingRequest exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Model.Model exposing (Model)
import Model.VertexData exposing (VertexData)
import Msg.Msg exposing (Msg(..))
import View.Configure
import View.Hero
import View.VertexData


view : Model -> Bool -> Html Msg
view model isModalActive =
    View.Hero.view (body model isModalActive)


body : Model -> Bool -> Html Msg
body model isModalActive =
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

        buttons : Html Msg
        buttons =
            case model.vertices_selected of
                [] ->
                    Html.div
                        [ class "columns is-mobile is-pulled-right"
                        ]
                        [ Html.div
                            [ class "column"
                            ]
                            [ configure
                            ]
                        ]

                _ ->
                    Html.div
                        [ class "columns is-mobile is-pulled-right"
                        ]
                        [ Html.div
                            [ class "column"
                            ]
                            [ configure
                            ]
                        , Html.div
                            [ class "column"
                            ]
                            [ search
                            ]
                        ]
    in
    Html.div
        [ class "container"
        ]
        [ View.Configure.view isModalActive model.direction_selected model.aggregation_selected model.sort_by_selected
        , buttons
        , Html.div
            [ class "field"
            ]
            [ Html.div
                [ class "control has-icons-left"
                ]
                [ Html.input
                    [ class "input is-input is-large"
                    , type_ "text"
                    , placeholder "Search for a Committee..." -- TODO: 
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
            ]
        , Html.div
            []
            [ selected_
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


configure : Html Msg
configure =
    Html.a
        [ class "button"
        , onClick ConfigureSearch
        ]
        [ Html.text "Configure"
        ]


search : Html Msg
search =
    Html.button
        [ class "button"
        , onClick TraversalRequestMade
        ]
        [ Html.text "Search"
        ]
