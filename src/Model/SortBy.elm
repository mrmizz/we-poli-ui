module Model.SortBy exposing (SortBy(..), toString)


type SortBy
    = Count
    | TotalSpend
    | AvgSpend
    | MaxSpend
    | MinSpend


toString : SortBy -> String
toString sortBy =
    case sortBy of
        Count ->
            "1"

        TotalSpend ->
            "2"

        AvgSpend ->
            "3"

        MaxSpend ->
            "5"

        MinSpend ->
            "4"
