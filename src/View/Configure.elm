module View.Configure exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onClick)
import Model.Direction exposing (Direction(..))
import Msg.Msg exposing (Msg(..))


view : Bool -> Direction -> Html Msg
view bool directionArg =
    let
        isActive =
            case bool of
                True ->
                    " is-active"

                False ->
                    ""
    in
    Html.div
        [ class ("modal" ++ isActive)
        ]
        [ Html.div
            [ class "modal-background"
            ]
            []
        , Html.div
            [ class "modal-content"
            ]
            [ Html.div
                [ class "container"
                ]
                [ Html.article
                    [ class "message"
                    ]
                    [ Html.div
                        [ class "message-header"
                        ]
                        [ Html.text "Configuration"
                        ]
                    , Html.div
                        [ class "message-body"
                        ]
                        [ direction directionArg
                        ]
                    ]
                ]
            ]
        , Html.button
            [ class "modal-close is-large"
            , onClick ConfigureSearch
            ]
            []
        ]


direction : Direction -> Html Msg
direction directionArg =
    let
        -- TODO
        flags =
            case directionArg of
                In ->
                    ( True, False )

                Out ->
                    ( False, True )
    in
    Html.div
        [ class "box"
        ]
        [ Html.h2
            [ class "title is-5 mb-2"
            ]
            [ Html.text "Search For"
            ]
        , Html.div
            [ class "field"
            ]
            [ Html.div
                [ class "control"
                ]
                [ Html.label
                    [ class "radio" --TODO Margin
                    ]
                    [ Html.input
                        [ type_ "radio"
                        , checked True
                        , onClick DirectionOptionSelected
                        ]
                        []
                    , Html.text "Committees"
                    ]
                ]
            ]
        ]
