module Models.Traversal exposing (Traversal, TraversalPage)


type alias Traversal =
    { src_id : String
    , dst_ids : List String
    }


type alias TraversalPage =
    { vertex_id : String
    , page_number : String
    }
