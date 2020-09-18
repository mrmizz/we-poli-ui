module Models.EdgeData exposing (EdgeData)

type alias EdgeData =
    { src_id : String
    , dst_id : String
    , num_transactions : String
    , total_spend : String
    , avg_spend : String
    , max_spend : String
    , min_spend : String
    }