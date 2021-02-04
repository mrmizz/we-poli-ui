module View.Header exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Msg.Msg exposing (Msg)


view : Html Msg
view =
    Html.div
        [ class "tabs is-boxed is-medium has-background-white-bis"
        ]
        [ Html.ul
            []
            [ Html.li
                []
                [ Html.a
                    []
                    [ Html.text "About"
                    ]
                ]
            , Html.li
                []
                [ Html.a
                    []
                    [ Html.text "Tool"
                    ]
                ]
            ]
        ]
