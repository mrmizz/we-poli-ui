module Msg.Msg exposing (Msg(..), resetViewport)

import Browser.Dom as Dom
import Http
import Http.Edge exposing (EdgeDataResponse)
import Http.NamePrefix exposing (VertexNamePrefixResponse)
import Http.PageCount exposing (PageCountResponse)
import Http.Traversal exposing (TraversalResponse)
import Http.Vertex exposing (VertexDataResponse)
import Model.SortBy exposing (SortBy)
import Model.VertexData exposing (VertexData)
import Task


type Msg
    = NoOp
    | ClickedAbout
    | ClickedTool
    | ConfigureSearch
    | ClearSearch
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


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)
