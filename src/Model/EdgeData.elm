module Model.EdgeData exposing (EdgeData, format)

import FormatNumber
import FormatNumber.Locales as Locales


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
        f : String -> String
        f str =
            case String.toFloat str of
                Just float ->
                    "$" ++ FormatNumber.format Locales.usLocale float

                Nothing ->
                    str
    in
    { edgeData
        | total_spend = f edgeData.total_spend
        , avg_spend = f edgeData.avg_spend
        , max_spend = f edgeData.max_spend
        , min_spend = f edgeData.min_spend
    }
