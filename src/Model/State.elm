module Model.State exposing (State(..))

import Http


type State
    = About
    | BuildingRequest Bool
    | SearchConfirmed
    | Loading
    | VertexRequestsSuccess
    | RequestFailure Http.Error
    | DataIntegrityFailure
