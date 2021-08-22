module View.VertexData exposing (view, viewMin)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Model.VertexData exposing (VertexData)
import Msg.Msg exposing (Msg)
import Util.Util as Util


view : VertexData -> Html Msg
view vertexData =
    Html.table
        [ class "table is-bordered is-hoverable is-fullwidth"
        ]
        [ body vertexData
        ]


viewMin : VertexData -> Html Msg
viewMin vertexData =
    Html.table
        [ class "table is-bordered is-hoverable is-fullwidth"
        ]
        [ bodyMin vertexData
        ]


body : VertexData -> Html Msg
body vertexData =
    Html.tbody
        []
        [ Html.tr
            []
            [ Html.td
                []
                [ Html.text "uid"
                ]
            , Html.td
                []
                [ Html.text vertexData.uid
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "name"
                ]
            , Html.td
                []
                [ Html.text vertexData.name
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "is-committee"
                ]
            , Html.td
                []
                [ Html.text (Util.printBool vertexData.is_committee)
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "city"
                ]
            , Html.td
                []
                [ Html.text (Maybe.withDefault "null" vertexData.address.city)
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "street"
                ]
            , Html.td
                []
                [ Html.text (Maybe.withDefault "null" vertexData.address.street)
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "state"
                ]
            , Html.td
                []
                [ Html.text (Maybe.withDefault "null" vertexData.address.state)
                ]
            ]
        ]


bodyMin : VertexData -> Html Msg
bodyMin vertexData =
    Html.tbody
        []
        [ Html.tr
            []
            [ Html.td
                []
                [ Html.text "uid"
                ]
            , Html.td
                []
                [ Html.text vertexData.uid
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "name"
                ]
            , Html.td
                []
                [ Html.text vertexData.name
                ]
            ]
        ]
