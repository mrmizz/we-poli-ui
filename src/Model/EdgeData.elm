module Model.EdgeData exposing (EdgeData, format)

import FormatNumber
import FormatNumber.Locales as Locales exposing (Decimals(..))


type alias EdgeData =
    { src_id : String
    , dst_id : String
    , num_transactions : String
    , total_spend : String
    , avg_spend : String
    , max_spend : String
    , min_spend : String
    }


format : EdgeData -> EdgeData
format edgeData =
    let
        base : Locales.Locale
        base =
            Locales.base

        currency : String -> String
        currency str =
            case String.toFloat str of
                Just float ->
                    "$" ++ FormatNumber.format Locales.usLocale float

                Nothing ->
                    str

        total : String -> String
        total str =
            case String.toFloat str of
                Just float ->
                    FormatNumber.format { base | decimals = Exact 0, thousandSeparator = "," } float

                Nothing ->
                    str
    in
    { edgeData
        | num_transactions = total edgeData.num_transactions
        , total_spend = currency edgeData.total_spend
        , avg_spend = currency edgeData.avg_spend
        , max_spend = currency edgeData.max_spend
        , min_spend = currency edgeData.min_spend
    }
