module View.Configure exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onClick)
import Model.Aggregation exposing (Aggregation(..))
import Model.Direction exposing (Direction(..))
import Msg.Msg exposing (Msg(..))


view : Bool -> Direction -> Aggregation -> Html Msg
view bool directionArg aggregationArg =
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
                        , aggregation aggregationArg
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
        flags =
            case directionArg of
                In ->
                    ( False, True )

                Out ->
                    ( True, False )
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
            [ class "control"
            ]
            [ Html.label
                [ class "radio"
                ]
                [ Html.input
                    [ type_ "radio"
                    , checked (Tuple.first flags)
                    , onClick DirectionOptionSelected
                    ]
                    []
                , Html.text " Committees"
                ]
            , Html.label
                [ class "radio"
                ]
                [ Html.input
                    [ type_ "radio"
                    , checked (Tuple.second flags)
                    , onClick DirectionOptionSelected
                    ]
                    []
                , Html.text " Vendors"
                ]
            ]
        ]


aggregation : Aggregation -> Html Msg
aggregation aggregationArg =
    let
        flags =
            case aggregationArg of
                And ->
                    ( True, False )

                Or ->
                    ( False, True )
    in
    Html.div
        [ class "box"
        ]
        [ Html.h2
            [ class "title is-5 mb-2"
            ]
            [ Html.text "Aggregate with"
            ]
        , Html.div
            [ class "control"
            ]
            [ Html.label
                [ class "radio"
                ]
                [ Html.input
                    [ type_ "radio"
                    , checked (Tuple.first flags)
                    , onClick AggOptionSelected
                    ]
                    []
                , Html.text " And"
                ]
            , Html.label
                [ class "radio"
                ]
                [ Html.input
                    [ type_ "radio"
                    , checked (Tuple.second flags)
                    , onClick AggOptionSelected
                    ]
                    []
                , Html.text " Or"
                ]
            ]
        ]
