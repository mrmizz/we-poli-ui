module View.VertexRequestSuccess exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model.Model exposing (Model)
import Model.VertexData exposing (VertexData)
import Model.Zipped exposing (Zipped)
import Msg.Msg exposing (Msg(..))
import View.Configure
import View.Hero
import View.VertexData
import View.Zipped


view : Model -> Bool -> Html Msg
view model isModalActive =
    View.Hero.view (body model isModalActive)


body : Model -> Bool -> Html Msg
body model isModalActive =
    let
        selected_ : Html Msg
        selected_ =
            case model.vertices_selected of
                [] ->
                    Html.div [] []

                nel ->
                    Html.div
                        [ class "box"
                        ]
                        ([ Html.h2
                            [ class "title is-5"
                            ]
                            [ Html.text "Searched for"
                            ]
                         ]
                            ++ List.map selected nel
                        )

        selectable_ : Html Msg
        selectable_ =
            case model.zipped of
                [] ->
                    Html.div [] []

                nel ->
                    Html.div
                        [ class "pb-6"
                        ]
                        (List.map selectable nel)

        buttons : Html Msg
        buttons =
            Html.div
                [ class "columns is-pulled-right pt-3 pr-5"
                ]
                [ Html.div
                    [ class "column"
                    ]
                    [ configure
                    ]
                , Html.div
                    [ class "column"
                    ]
                    [ clear
                    ]
                ]
    in
    Html.div
        [ class "container"
        ]
        [ View.Configure.view isModalActive model.direction_selected model.aggregation_selected model.sort_by_selected
        , buttons
        , Html.div
            []
            [ selected_
            , selectable_
            ]
        ]


selected : VertexData -> Html Msg
selected vertex =
    View.VertexData.viewMin vertex


selectable : Zipped -> Html Msg
selectable zipped =
    let
        select =
            Html.button
                [ class "button is-link is-light is-fullwidth"
                , onClick (ChildTraversalRequestMade (Tuple.second zipped))
                ]
                [ Html.text "Search"
                ]
    in
    Html.div
        [ class "box"
        ]
        [ select
        , View.Zipped.view zipped
        ]


configure : Html Msg
configure =
    Html.a
        [ class "button"
        , onClick ConfigureSearch
        ]
        [ Html.text "Configure"
        ]


clear : Html Msg
clear =
    Html.a
        [ class "button"
        , onClick ClearSearch
        ]
        [ Html.text "Clear search"
        ]
