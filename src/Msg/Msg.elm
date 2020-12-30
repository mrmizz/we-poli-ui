module Msg.Msg exposing (Msg(..))

import HTTP.Edge exposing (EdgeDataResponse)
import HTTP.NamePrefix exposing (VertexNamePrefixResponse)
import HTTP.PageCount exposing (PageCountResponse)
import HTTP.Traversal exposing (TraversalResponse)
import HTTP.Vertex exposing (VertexDataResponse)
import Http
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
