module Models.Aggregation exposing (Aggregation(..), printAgg)


type Aggregation
    = And
    | Or



-- TODO: rename to print ?


printAgg : Aggregation -> String
printAgg agg =
    case agg of
        And ->
            "And"

        Or ->
            "Or"
