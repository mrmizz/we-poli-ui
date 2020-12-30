module Model.Model exposing (Model)

import Model.State exposing (State)
import Model.Aggregation exposing (Aggregation)
import Model.Direction exposing (Direction)
import Model.EdgeData exposing (EdgeData)
import Model.PageCount exposing (PageCount)
import Model.SortBy exposing (SortBy)
import Model.Traversal exposing (Traversal)
import Model.VertexData exposing (VertexData)
import Model.Zipped exposing (Zipped)


type alias Model =
    { state : State
    , vertex_name_search : String
    , vertex_name_search_response : List VertexData
    , vertices_selected : List VertexData
    , aggregation_selected : Aggregation
    , direction_selected : Direction
    , sort_by_selected : SortBy
    , traversal_response : List Traversal
    , traversal_data_response : List VertexData
    , edge_data_response : List EdgeData
    , zipped : List Zipped
    , page_count : Maybe PageCount
    }
