module Model.Traversal exposing (PageCount, Traversal(..))

import Model.EdgeData exposing (EdgeData)
import Model.VertexData exposing (VertexData)


type Traversal
    = Pending -- still building search
    | Waiting PageCount -- async requests made
    | WaitingForEdges PageCount (List VertexData) -- vertices arrived first
    | WaitingForVertices PageCount (List EdgeData) -- edges arrived first
    | Done PageCount -- all async requests received


type alias PageCount =
    { src_id : Int
    , total_pages : Int
    , current_page : Int
    }
