module Models.Direction exposing (Direction(..), directionToIsCommittee, switchDirection)


type Direction
    = In
    | Out


directionToIsCommittee : Direction -> Bool
directionToIsCommittee direction =
    case direction of
        In ->
            False

        Out ->
            True


switchDirection : Direction -> Direction
switchDirection direction =
    case direction of
        In ->
            Out

        Out ->
            In
