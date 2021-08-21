module Model.EdgeData exposing (EdgeData)

import FormatNumber
import FormatNumber.Locales as Locales


type alias EdgeData =
    { src_id : Int
    , dst_id : Int
    , num_transactions : Int
    , total_spend : Int
    , avg_spend : Int
    , max_spend : Int
    , min_spend : Int
    }



--format : EdgeData -> EdgeData
--format edgeData =
--    let
--        f : String -> String
--        f str =
--            case String.toFloat str of
--                Just float ->
--                    "$" ++ FormatNumber.format Locales.usLocale float
--
--                Nothing ->
--                    str
--    in
--    { edgeData
--        | total_spend = f edgeData.total_spend
--        , avg_spend = f edgeData.avg_spend
--        , max_spend = f edgeData.max_spend
--        , min_spend = f edgeData.min_spend
--    }
--
