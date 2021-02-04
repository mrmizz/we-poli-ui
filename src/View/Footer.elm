module View.Footer exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Msg.Msg exposing (Msg)


view : Html Msg
view =
    Html.footer
        [ class "footer has-background-white-ter"
        ]
        [ Html.div
            [ class "content has-text-centered is-family-secondary"
            ]
            [ Html.h1
                [ class "title is-3"
                ]
                [ Html.text "Campaign Cash"
                ]
            , Html.h2
                [ class "subtitle is-6"
                ]
                [ Html.text "Providing Transparency, Now"
                ]
            ]
        ]
