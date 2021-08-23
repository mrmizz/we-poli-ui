module View.Pagination exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model.Model exposing (Model)
import Model.Traversal exposing (PageCount, Traversal(..))
import Msg.Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        baseButtonClass : String
        baseButtonClass =
            "button is-link is-light"

        ellipses : Html Msg
        ellipses =
            Html.li
                []
                [ Html.span
                    [ class "pagination-ellipsis"
                    ]
                    [ Html.text ". . ."
                    ]
                ]

        first : PageCount -> Html Msg
        first pageCount =
            Html.div
                [ class (baseButtonClass ++ " pagination-previous")
                , onClick (PaginatedTraversalRequestMade { pageCount | current_page = 1 })
                ]
                [ Html.div
                    [ class "icon-text"
                    ]
                    [ Html.span
                        [ class "span"
                        ]
                        [ Html.text "first"
                        ]
                    , Html.span
                        [ class "icon"
                        ]
                        [ Html.i
                            [ class "fas fa-angle-left"
                            ]
                            []
                        ]
                    ]
                ]

        last : PageCount -> Html Msg
        last pageCount =
            Html.div
                [ class (baseButtonClass ++ " pagination-next")
                , onClick (PaginatedTraversalRequestMade { pageCount | current_page = pageCount.total_pages })
                ]
                [ Html.div
                    [ class "icon-text"
                    ]
                    [ Html.span
                        [ class "icon"
                        ]
                        [ Html.i
                            [ class "fas fa-angle-right"
                            ]
                            []
                        ]
                    , Html.span
                        [ class "span"
                        ]
                        [ Html.text ("last (" ++ String.fromInt pageCount.total_pages ++ ")")
                        ]
                    ]
                ]

        button : PageCount -> Int -> Bool -> Html Msg
        button pageCount pageNumber isActive =
            let
                class_ =
                    case isActive of
                        True ->
                            baseButtonClass ++ " pagination-link is-active"

                        False ->
                            baseButtonClass ++ " pagination-link"
            in
            Html.li
                [ onClick (PaginatedTraversalRequestMade { pageCount | current_page = pageNumber })
                ]
                [ Html.div
                    [ class class_
                    ]
                    [ Html.text (String.fromInt pageNumber)
                    ]
                ]

        pagination : List (Html Msg) -> Html Msg
        pagination list =
            Html.nav
                [ class "pagination is-centered is-mobile"
                ]
                list

        paginationList : List (Html Msg) -> Html Msg
        paginationList list =
            Html.ul
                [ class "pagination-list"
                ]
                list

        html : Html Msg
        html =
            case model.traversal of
                Done pageCount ->
                    let
                        button_ =
                            button pageCount

                        first_ =
                            first pageCount

                        last_ =
                            last pageCount
                    in
                    if pageCount.total_pages == 1 then
                        pagination
                            [ paginationList
                                [ button_ 1 True
                                ]
                            ]

                    else if pageCount.total_pages == 2 then
                        if pageCount.current_page == 1 then
                            pagination
                                [ paginationList
                                    [ button_ 1 True
                                    , button_ 2 False
                                    ]
                                ]

                        else
                            pagination
                                [ paginationList
                                    [ button_ 1 False
                                    , button_ 2 True
                                    ]
                                ]

                    else if pageCount.current_page == 1 then
                        pagination
                            [ first_
                            , paginationList
                                [ ellipses
                                , button_ 1 True
                                , button_ 2 False
                                , button_ 3 False
                                , ellipses
                                ]
                            , last_
                            ]

                    else if pageCount.current_page == pageCount.total_pages then
                        pagination
                            [ first_
                            , paginationList
                                [ ellipses
                                , button_ (pageCount.total_pages - 2) False
                                , button_ (pageCount.total_pages - 1) False
                                , button_ pageCount.total_pages True
                                , ellipses
                                ]
                            , last_
                            ]

                    else
                        pagination
                            [ first_
                            , paginationList
                                [ ellipses
                                , button_ (pageCount.current_page - 1) False
                                , button_ pageCount.current_page True
                                , button_ (pageCount.current_page + 1) False
                                , ellipses
                                ]
                            , last_
                            ]

                _ ->
                    Html.div [] []
    in
    html
