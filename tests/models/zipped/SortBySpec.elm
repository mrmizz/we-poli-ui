module Models.Zipped.SortBySpec exposing (suite1)

import Expect
import Models.EdgeData exposing (EdgeData)
import Models.SortBy exposing (SortBy(..))
import Models.VertexData exposing (VertexData)
import Models.Zipped as Zipped exposing (Zipped)
import Test exposing (Test, describe, test)



-- TODO


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


vendor1 : VertexData
vendor1 =
    VertexData
        "1V"
        "Vendor1"
        True
        [ "Santa Barbara" ]
        [ "Milpas" ]
        [ "CA" ]
