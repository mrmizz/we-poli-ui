module View.Zipped exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
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
            Tuple.first zipped

        vertexData =
            Tuple.second zipped
    in
    Html.tbody
        []
        [ Html.tr
            []
            [ Html.td
                []
                [ Html.text "vertex_id"
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
                [ Html.text "src_id"
                ]
            , Html.td
                []
                [ Html.text edgeData.src_id
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "dst_id"
                ]
            , Html.td
                []
                [ Html.text edgeData.dst_id
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
