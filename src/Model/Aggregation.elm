module Model.Aggregation exposing (Aggregation(..), print)


type Aggregation
    = And
    | Or


print : Aggregation -> String
print agg =
    case agg of
        And ->
            "And"

        Or ->
            "Or"
