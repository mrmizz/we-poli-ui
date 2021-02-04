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
    Html.table
        [ class "table is-bordered is-hoverable"
        ]
        ([ header ] ++ List.map body vertices)


header : Html msg
header =
    Html.thead
        []
        [ Html.tr
            []
            [ Html.th
                []
                [ Html.text "uid"
                ]
            , Html.th
                []
                [ Html.text "name"
                ]
            , Html.th
                []
                [ Html.text "is_committee"
                ]
            , Html.th
                []
                [ Html.text "cities"
                ]
            , Html.th
                []
                [ Html.text "streets"
                ]
            , Html.th
                []
                [ Html.text "states"
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
                [ Html.text vertexData.uid
                ]
            , Html.td
                []
                [ Html.text vertexData.name
                ]
            , Html.td
                []
                [ Html.text (Util.printBool vertexData.is_committee)
                ]
            , Html.td
                []
                [ Html.text (Util.printList vertexData.cities)
                ]
            , Html.td
                []
                [ Html.text (Util.printList vertexData.streets)
                ]
            , Html.td
                []
                [ Html.text (Util.printList vertexData.states)
                ]
            ]
        ]
