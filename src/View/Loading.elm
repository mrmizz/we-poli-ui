module View.Loading exposing (view)

import Element exposing (Element)
import Msg.Msg exposing (Msg)

view : Element Msg
view =
    Element.el [] (Element.text "Loading . . .")
