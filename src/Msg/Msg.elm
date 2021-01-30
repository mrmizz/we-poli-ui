module Msg.Msg exposing (Msg(..))

import Http
import Http.Edge exposing (EdgeDataResponse)
import Http.NamePrefix exposing (VertexNamePrefixResponse)
import Http.PageCount exposing (PageCountResponse)
import Http.Traversal exposing (TraversalResponse)
import Http.Vertex exposing (VertexDataResponse)
import Model.SortBy exposing (SortBy)
import Model.VertexData exposing (VertexData)


type Msg
    = ClearSearch
    | EditSearch
    | ConfirmSearch
    | SearchInput String
    | AggOptionSelected
    | DirectionOptionSelected
    | VertexSelected VertexData
    | DeleteVertexSelection VertexData
    | TraversalRequestMade
    | ChildTraversalRequestMade VertexData
    | VertexDataPostReceived (Result Http.Error VertexDataResponse)
    | EdgeDataPostReceived (Result Http.Error EdgeDataResponse)
    | VertexNamePrefixGetReceived (Result Http.Error VertexNamePrefixResponse)
    | TraversalPostReceived (Result Http.Error TraversalResponse)
    | PageCountPostReceived (Result Http.Error PageCountResponse)
    | SortByOptionSelected SortBy
