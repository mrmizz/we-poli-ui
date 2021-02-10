module Model.State exposing (State(..))

import Http


type State
    = About
    | BuildingRequest Bool
    | Loading
    | VertexRequestsSuccess Bool
    | RequestFailure Http.Error
    | DataIntegrityFailure
