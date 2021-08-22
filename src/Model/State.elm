module Model.State exposing (State(..))

import Http


type State
    = About
    | BuildingSearch Bool -- ADT for ModalOpen
    | Loading
    | TraversalSuccess Bool -- ADT for ModalOpen
    | TraversalFailure Http.Error
    | DataIntegrityFailure
