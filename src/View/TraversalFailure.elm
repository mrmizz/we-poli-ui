module View.TraversalFailure exposing (view)

import Html exposing (Html)
import Http
import Msg.Msg exposing (Msg)


view : Http.Error -> Html Msg
view error =
    case error of
        Http.BadUrl string ->
            Html.div
                []
                [ Html.text ("Bad URL: " ++ string)
                ]

        Http.Timeout ->
            Html.div
                []
                [ Html.text "Server Timeout"
                ]

        Http.NetworkError ->
            Html.div
                []
                [ Html.text "Network Error"
                ]

        Http.BadStatus int ->
            Html.div
                []
                [ Html.text (String.fromInt int ++ " Error: Bad Input")
                ]

        Http.BadBody body ->
            Html.div
                []
                [ Html.text ("Bad Body: " ++ body)
                ]
