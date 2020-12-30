module Util.Util exposing (printBool)


printBool : Bool -> String
printBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"
