module Model.State exposing (State(..))

import Http


type State
    = About
    | BuildingRequest Bool -- ADT for ModalOpen
    | Loading
    | VertexRequestsSuccess Bool -- ADT for ModalOpen
    | RequestFailure Http.Error
    | DataIntegrityFailure
