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
                            , ( edgeB4, vendor1 )
                            , ( edgeB6, vendor1 )
                            , ( edgeB5, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeB3, vendor1 )
                    , ( edgeB2, vendor1 )
                    , ( edgeB1, vendor1 )
                    , ( edgeB6, vendor1 )
                    , ( edgeB5, vendor1 )
                    , ( edgeB4, vendor1 )
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
                            , ( edgeC4, vendor1 )
                            , ( edgeC6, vendor1 )
                            , ( edgeC5, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeC3, vendor1 )
                    , ( edgeC2, vendor1 )
                    , ( edgeC1, vendor1 )
                    , ( edgeC6, vendor1 )
                    , ( edgeC5, vendor1 )
                    , ( edgeC4, vendor1 )
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
                            , ( edgeD4, vendor1 )
                            , ( edgeD6, vendor1 )
                            , ( edgeD5, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeD3, vendor1 )
                    , ( edgeD2, vendor1 )
                    , ( edgeD1, vendor1 )
                    , ( edgeD6, vendor1 )
                    , ( edgeD5, vendor1 )
                    , ( edgeD4, vendor1 )
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
                            , ( edgeE4, vendor1 )
                            , ( edgeE6, vendor1 )
                            , ( edgeE5, vendor1 )
                            ]
                in
                Expect.equal
                    actual
                    [ ( edgeE3, vendor1 )
                    , ( edgeE2, vendor1 )
                    , ( edgeE1, vendor1 )
                    , ( edgeE6, vendor1 )
                    , ( edgeE5, vendor1 )
                    , ( edgeE4, vendor1 )
                    ]
        ]


edgeA1 : EdgeData
edgeA1 =
    EdgeData
        "1C"
        "1V"
        "13"
        "$100"
        "$10"
        "$10"
        "$10"


edgeA2 : EdgeData
edgeA2 =
    { edgeA1 | num_transactions = "129" }


edgeA3 : EdgeData
edgeA3 =
    { edgeA1 | num_transactions = "130" }


edgeB1 : EdgeData
edgeB1 =
    { edgeA1 | total_spend = "$13" }


edgeB2 : EdgeData
edgeB2 =
    { edgeA1 | total_spend = "$129" }


edgeB3 : EdgeData
edgeB3 =
    { edgeA1 | total_spend = "$130" }


edgeB4 : EdgeData
edgeB4 =
    { edgeA1 | total_spend = "$-130" }


edgeB5 : EdgeData
edgeB5 =
    { edgeA1 | total_spend = "$-129" }


edgeB6 : EdgeData
edgeB6 =
    { edgeA1 | total_spend = "$-13" }


edgeC1 : EdgeData
edgeC1 =
    { edgeA1 | avg_spend = "$13" }


edgeC2 : EdgeData
edgeC2 =
    { edgeA1 | avg_spend = "$129" }


edgeC3 : EdgeData
edgeC3 =
    { edgeA1 | avg_spend = "$130" }


edgeC4 : EdgeData
edgeC4 =
    { edgeA1 | avg_spend = "$-130" }


edgeC5 : EdgeData
edgeC5 =
    { edgeA1 | avg_spend = "$-129" }


edgeC6 : EdgeData
edgeC6 =
    { edgeA1 | avg_spend = "$-13" }


edgeD1 : EdgeData
edgeD1 =
    { edgeA1 | max_spend = "$13" }


edgeD2 : EdgeData
edgeD2 =
    { edgeA1 | max_spend = "$129" }


edgeD3 : EdgeData
edgeD3 =
    { edgeA1 | max_spend = "$130" }


edgeD4 : EdgeData
edgeD4 =
    { edgeA1 | max_spend = "$-130" }


edgeD5 : EdgeData
edgeD5 =
    { edgeA1 | max_spend = "$-129" }


edgeD6 : EdgeData
edgeD6 =
    { edgeA1 | max_spend = "$-13" }


edgeE1 : EdgeData
edgeE1 =
    { edgeA1 | min_spend = "$13" }


edgeE2 : EdgeData
edgeE2 =
    { edgeA1 | min_spend = "$129" }


edgeE3 : EdgeData
edgeE3 =
    { edgeA1 | min_spend = "$130" }


edgeE4 : EdgeData
edgeE4 =
    { edgeA1 | min_spend = "$-130" }


edgeE5 : EdgeData
edgeE5 =
    { edgeA1 | min_spend = "$-129" }


edgeE6 : EdgeData
edgeE6 =
    { edgeA1 | min_spend = "$-13" }


vendor1 : VertexData
vendor1 =
    VertexData
        "1V"
        "Vendor1"
        True
        [ "Santa Barbara" ]
        [ "Milpas" ]
        [ "CA" ]
