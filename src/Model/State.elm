module Model.State exposing (State(..))

import Http


type State
    = BuildingRequest
    | SearchConfirmed
    | Loading
    | VertexRequestsSuccess
    | RequestFailure Http.Error
    | DataIntegrityFailure
