module Util.Util exposing (printBool, printList)


printBool : Bool -> String
printBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


printList : List String -> String
printList list =
    let
        reduce left right =
            left ++ " " ++ right
    in
    List.foldl reduce "" list
