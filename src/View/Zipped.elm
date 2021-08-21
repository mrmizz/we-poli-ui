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
                [ Html.text "src_id"
                ]
            , Html.td
                []
                [ Html.text (String.fromInt edgeData.src_id)
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
                [ Html.text (String.fromInt edgeData.dst_id)
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
                [ Html.text (String.fromInt edgeData.num_transactions)
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
                [ Html.text (String.fromInt edgeData.total_spend)
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
                [ Html.text (String.fromInt edgeData.avg_spend)
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
                [ Html.text (String.fromInt edgeData.max_spend)
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
                [ Html.text (String.fromInt edgeData.min_spend)
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
                [ Html.text "cities"
                ]
            , Html.td
                []
                [ Html.text (Util.printList vertexData.cities)
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "streets"
                ]
            , Html.td
                []
                [ Html.text (Util.printList vertexData.streets)
                ]
            ]
        , Html.tr
            []
            [ Html.td
                []
                [ Html.text "states"
                ]
            , Html.td
                []
                [ Html.text (Util.printList vertexData.states)
                ]
            ]
        ]
