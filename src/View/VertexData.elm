module View.VertexData exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Model.VertexData exposing (VertexData)
import Msg.Msg exposing (Msg)
import Util.Util as Util



-- TODO: Handle "bad" input gracefully
-- example: inputting /// throws an error
-- TODO: city, state, etc all to lower
-- TODO: aggregate city, state, etc to address object


view : List VertexData -> Html Msg
view vertices =
    Html.div
        []
        (List.map table vertices)


table : VertexData -> Html Msg
table vertexData =
    Html.div
        [ class "box"
        ]
        [ Html.table
            [ class "table is-bordered is-hoverable is-fullwidth"
            ]
            [ header
            , body vertexData
            ]
        ]


header : Html msg
header =
    Html.thead
        []
        [ Html.tr
            []
            [ Html.th
                []
                [ Html.text "key"
                ]
            , Html.th
                []
                [ Html.text "value"
                ]
            ]
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
