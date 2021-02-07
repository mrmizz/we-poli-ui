module Model.Direction exposing (Direction(..), switch, toIsCommittee, toString)


type Direction
    = In
    | Out


toString : Direction -> String
toString direction =
    case direction of
        In ->
            "Vendors"

        Out ->
            "Committees"


toIsCommittee : Direction -> Bool
toIsCommittee direction =
    case direction of
        In ->
            False

        Out ->
            True


switch : Direction -> Direction
switch direction =
    case direction of
        In ->
            Out

        Out ->
            In
