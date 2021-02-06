module View.Header exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Msg.Msg exposing (Msg)


view : Html Msg
view =
    Html.nav
        [ class "level has-background-white-bis"
        ]
        [ Html.div
            [ class "level-left"
            ]
            [ Html.div
                [ class "level-item"
                ]
                [ Html.div
                    [ class "tabs is-boxed is-large"
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
                ]
            ]
        , Html.div
            [ class "level-right"
            ]
            [ Html.div
                [ class "level-item"
                ]
                [ Html.div
                    [ class "title is-family-secondary is-3 px-2 pb-2"
                    ]
                    [ Html.text "Campaign Cash"
                    ]
                ]
            ]
        ]
