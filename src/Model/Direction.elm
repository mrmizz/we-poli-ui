module Model.Direction exposing (Direction(..), switch, toIsCommittee)


type Direction
    = In
    | Out


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
