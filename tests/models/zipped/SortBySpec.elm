module Models.Zipped.SortBySpec exposing (suite1)

import Expect
import Models.EdgeData exposing (EdgeData)
import Models.SortBy exposing (SortBy(..))
import Models.VertexData exposing (VertexData)
import Models.Zipped as Zipped exposing (Zipped)
import Test exposing (Test, describe, test)


suite1 : Test
suite1 =
    describe "Sort Zipped"
        [ test "should sort by Transactions Count" <|
            \_ ->
                let
                    actual : List Zipped
                    actual =
                        Zipped.sortBy
                            Count
                            [ ( edgeA1, vendor1 )
                            , ( edgeA3, vendor1 )
                            , ( edgeA2, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeA3, vendor1 )
                    , ( edgeA2, vendor1 )
                    , ( edgeA1, vendor1 )
                    ]
        , test "should sort by Total Spend" <|
            \_ ->
                let
                    actual : List Zipped
                    actual =
                        Zipped.sortBy
                            TotalSpend
                            [ ( edgeB1, vendor1 )
                            , ( edgeB3, vendor1 )
                            , ( edgeB2, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeB3, vendor1 )
                    , ( edgeB2, vendor1 )
                    , ( edgeB1, vendor1 )
                    ]
        , test "should sort by Average Spend" <|
            \_ ->
                let
                    actual : List Zipped
                    actual =
                        Zipped.sortBy
                            AvgSpend
                            [ ( edgeC1, vendor1 )
                            , ( edgeC3, vendor1 )
                            , ( edgeC2, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeC3, vendor1 )
                    , ( edgeC2, vendor1 )
                    , ( edgeC1, vendor1 )
                    ]
        , test "should sort by Max Spend" <|
            \_ ->
                let
                    actual : List Zipped
                    actual =
                        Zipped.sortBy
                            MaxSpend
                            [ ( edgeD1, vendor1 )
                            , ( edgeD3, vendor1 )
                            , ( edgeD2, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeD3, vendor1 )
                    , ( edgeD2, vendor1 )
                    , ( edgeD1, vendor1 )
                    ]
        , test "should sort by Min Spend" <|
            \_ ->
                let
                    actual : List Zipped
                    actual =
                        Zipped.sortBy
                            MinSpend
                            [ ( edgeE1, vendor1 )
                            , ( edgeE3, vendor1 )
                            , ( edgeE2, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeE3, vendor1 )
                    , ( edgeE2, vendor1 )
                    , ( edgeE1, vendor1 )
                    ]
        ]


edgeA1 : EdgeData
edgeA1 =
    EdgeData
        "1C"
        "1V"
        "10"
        "$100"
        "$10"
        "$10"
        "$10"


edgeA2 : EdgeData
edgeA2 =
    { edgeA1 | num_transactions = "20" }


edgeA3 : EdgeData
edgeA3 =
    { edgeA1 | num_transactions = "30" }


edgeB1 : EdgeData
edgeB1 =
    { edgeA1 | total_spend = "$1" }


edgeB2 : EdgeData
edgeB2 =
    { edgeA1 | total_spend = "$2" }


edgeB3 : EdgeData
edgeB3 =
    { edgeA1 | total_spend = "$3" }


edgeC1 : EdgeData
edgeC1 =
    { edgeA1 | avg_spend = "$1" }


edgeC2 : EdgeData
edgeC2 =
    { edgeA1 | avg_spend = "$2" }


edgeC3 : EdgeData
edgeC3 =
    { edgeA1 | avg_spend = "$3" }


edgeD1 : EdgeData
edgeD1 =
    { edgeA1 | max_spend = "$1" }


edgeD2 : EdgeData
edgeD2 =
    { edgeA1 | max_spend = "$2" }


edgeD3 : EdgeData
edgeD3 =
    { edgeA1 | max_spend = "$3" }


edgeE1 : EdgeData
edgeE1 =
    { edgeA1 | min_spend = "$1" }


edgeE2 : EdgeData
edgeE2 =
    { edgeA1 | min_spend = "$2" }


edgeE3 : EdgeData
edgeE3 =
    { edgeA1 | min_spend = "$3" }


vendor1 : VertexData
vendor1 =
    VertexData
        "1V"
        "Vendor1"
        True
        [ "Santa Barbara" ]
        [ "Milpas" ]
        [ "CA" ]
