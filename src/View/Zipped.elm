module View.Zipped exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Model.EdgeData as EdgeData
import Model.VertexData exposing (VertexData)
import Model.Zipped exposing (Zipped)
import Msg.Msg exposing (Msg)
import Util.Util as Util


view : Zipped -> Html Msg
view zipped =
    Html.table
        [ class "table is-bordered is-hoverable is-fullwidth"
        ]
        [ body zipped
        ]


body : Zipped -> Html Msg
body zipped =
    let
        edgeData =
            EdgeData.format (Tuple.first zipped)

        vertexData =
            Tuple.second zipped
    in
    Html.tbody
        []
        [ Html.tr
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
                [ Html.text "state"
                ]
            , Html.td
                []
                [ Html.text (Maybe.withDefault "null" vertexData.address.state)
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "total transaction count"
                ]
            , Html.td
                []
                [ Html.text edgeData.num_transactions
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "total spend"
                ]
            , Html.td
                []
                [ Html.text edgeData.total_spend
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "average spend"
                ]
            , Html.td
                []
                [ Html.text edgeData.avg_spend
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "max spend"
                ]
            , Html.td
                []
                [ Html.text edgeData.max_spend
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "minimum spend"
                ]
            , Html.td
                []
                [ Html.text edgeData.min_spend
                ]
            ]
        ]
