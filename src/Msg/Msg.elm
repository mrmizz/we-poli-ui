module Msg.Msg exposing (Msg(..), VertexDataClient(..), resetViewport)

import Browser.Dom as Dom
import Http
import Http.Edge exposing (EdgeDataResponse)
import Http.NamePrefix exposing (VertexNamePrefixResponse)
import Http.PageCount exposing (PageCountResponse)
import Http.Traversal exposing (TraversalResponse)
import Http.Vertex exposing (VertexDataResponse)
import Model.SortBy exposing (SortBy)
import Model.Traversal exposing (PageCount)
import Model.VertexData exposing (VertexData)
import Task


type Msg
    = NoOp
      -- Clicks
    | ClickedAbout -- TODO: href
    | ClickedTool -- TODO: href
    | ConfigureSearch
    | ClearSearch
    | VertexSelected VertexData
    | DeleteVertexSelection VertexData
      -- Configuration Options
    | AggOptionSelected
    | DirectionOptionSelected
    | SortByOptionSelected SortBy
      -- Vertex Name Auto Complete Search
    | VertexNameSearchInput String
    | VertexNamePrefixGetReceived (Result Http.Error VertexNamePrefixResponse)
      -- Vertex Data Request
    | VertexDataPostReceived VertexDataClient (Result Http.Error VertexDataResponse)
      -- Traversal Request
    | TraversalRequestMade
    | ChildTraversalRequestMade VertexData
    | PageCountPostReceived (Result Http.Error PageCountResponse)
    | TraversalPostReceived PageCount (Result Http.Error TraversalResponse)
    | EdgeDataPostReceived (Result Http.Error EdgeDataResponse)


type VertexDataClient
    = ForNameSearch
    | ForTraversal


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)
