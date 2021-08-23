module View.Pagination exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Model.Model exposing (Model)
import Model.Traversal exposing (Traversal(..))
import Msg.Msg exposing (Msg)


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

        first : Html Msg
        first =
            Html.div
                [ class (baseButtonClass ++ " pagination-previous")
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

        last : Int -> Html Msg
        last pageNumber =
            Html.div
                [ class (baseButtonClass ++ " pagination-next")
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
                        [ Html.text ("last (" ++ String.fromInt pageNumber ++ ")")
                        ]
                    ]
                ]

        button : Int -> Bool -> Html Msg
        button pageNumber isActive =
            let
                class_ =
                    case isActive of
                        True ->
                            baseButtonClass ++ " pagination-link is-active"

                        False ->
                            baseButtonClass ++ " pagination-link"
            in
            Html.li
                []
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
                    if pageCount.total_pages == 1 then
                        pagination
                            [ paginationList
                                [ button 1 True
                                ]
                            ]

                    else if pageCount.total_pages == 2 then
                        if pageCount.current_page == 1 then
                            pagination
                                [ paginationList
                                    [ button 1 True
                                    , button 2 False
                                    ]
                                ]

                        else
                            pagination
                                [ paginationList
                                    [ button 1 False
                                    , button 2 True
                                    ]
                                ]

                    else if pageCount.current_page == 1 then
                        pagination
                            [ first
                            , paginationList
                                [ ellipses
                                , button 1 True
                                , button 2 False
                                , button 3 False
                                , ellipses
                                ]
                            , last pageCount.total_pages
                            ]

                    else if pageCount.current_page == pageCount.total_pages then
                        pagination
                            [ first
                            , paginationList
                                [ ellipses
                                , button (pageCount.total_pages - 2) True
                                , button (pageCount.total_pages - 1) False
                                , button pageCount.total_pages True
                                , ellipses
                                ]
                            , last pageCount.total_pages
                            ]

                    else
                        pagination
                            [ first
                            , paginationList
                                [ ellipses
                                , button (pageCount.current_page - 1) True
                                , button pageCount.current_page False
                                , button (pageCount.current_page + 1) True
                                , ellipses
                                ]
                            , last pageCount.total_pages
                            ]

                _ ->
                    Html.div [] []
    in
    html



--            case model.traversal of
--                Done pageCount ->
--                    if pageCount.total_pages == 1 then
--                        pagination
--                            [ button "1" True
--                            ]
--
--                    else if pageCount.total_pages == 2 then
--                        if pageCount.current_page == 1 then
--                            pagination
--                                [ button "1" True
--                                , button "2" False
--                                ]
--                        else
--                            pagination
--                                [ button "1" False
--                                , button "2" True
--                                ]
--
--                    else if pageCount.total_pages == 3 then
--                        if pageCount.current_page == 1 then
--                            pagination
--                                [ button "1" True
--                                , button "2" False
--                                , button "3" False
--                                ]
--                        else if pageCount.current_page == 2 then
--                            pagination
--                                [ button "1" False
--                                , button "2" True
--                                , button "3" False
--                                ]
--                        else
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" True
--                                ]
--
--                    else if pageCount.total_pages == 4 then
--                        if pageCount.current_page == 1 then
--                            pagination
--                                [ button "1" True
--                                , button "2" False
--                                , button "3" False
--                                , button "4" False
--                                ]
--                        else if pageCount.current_page == 2 then
--                            pagination
--                                [ button "1" False
--                                , button "2" True
--                                , button "3" False
--                                , button "4" False
--                                ]
--                        else if pageCount.current_page == 3 then
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" True
--                                , button "4" False
--                                ]
--                        else
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" False
--                                , button "4" True
--                                ]
--
--                    else if pageCount.total_pages == 5 then
--                        if pageCount.current_page == 1 then
--                            pagination
--                                [ button "1" True
--                                , button "2" False
--                                , button "3" False
--                                , button "4" False
--                                , button "5" False
--                                ]
--                        else if pageCount.current_page == 2 then
--                            pagination
--                                [ button "1" False
--                                , button "2" True
--                                , button "3" False
--                                , button "4" False
--                                , button "5" False
--                                ]
--                        else if pageCount.current_page == 3 then
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" True
--                                , button "4" False
--                                , button "5" False
--                                ]
--                        else if pageCount.current_page == 4 then
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" False
--                                , button "4" True
--                                , button "5" False
--                                ]
--
--                        else
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" False
--                                , button "4" False
--                                , button "5" True
--                                ]
--
--                    else if pageCount.total_pages == 6 then
--                        if pageCount.current_page == 1 then
--                            pagination
--                                [ button "1" True
--                                , button "2" False
--                                , button "3" False
--                                , button "4" False
--                                , button "5" False
--                                , button "6" False
--                                ]
--                        else if pageCount.current_page == 2 then
--                            pagination
--                                [ button "1" False
--                                , button "2" True
--                                , button "3" False
--                                , button "4" False
--                                , button "5" False
--                                , button "6" False
--                                ]
--                        else if pageCount.current_page == 3 then
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" True
--                                , button "4" False
--                                , button "5" False
--                                , button "6" False
--                                ]
--                        else if pageCount.current_page == 4 then
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" False
--                                , button "4" True
--                                , button "5" False
--                                , button "6" False
--                                ]
--
--                        else if pageCount.current_page == 5 then
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" False
--                                , button "4" False
--                                , button "5" True
--                                , button "6" False
--                                ]
--
--                        else
--                            pagination
--                                [ button "1" False
--                                , button "2" False
--                                , button "3" False
--                                , button "4" False
--                                , button "5" False
--                                , button "6" True
--                                ]
--                    else
--                        if pageCount.current_page == 1 then
--                            let
--                                mid =
--                                    pageCount.total_pages // 2
--                            in
--
--                            pagination
--                                [ button "1" True
--                                , ellipses
--                                , button (String.fromInt (mid - 1)) False
--                                , button (String.fromInt (mid)) False
--                                , button (String.fromInt (mid + 1)) False
--                                , ellipses
--                                , button (String.fromInt pageCount.total_pages) False
--                                ]
--                        else
--                            Html.div [] []
