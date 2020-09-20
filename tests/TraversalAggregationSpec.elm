module TraversalAggregationSpec exposing (suite)

import Expect exposing (Expectation)
import Models.Aggregation exposing (Aggregation(..))
import Models.EdgeData exposing (EdgeData)
import Models.Traversal exposing (Traversal)
import Models.VertexData exposing (VertexData)
import Models.Zipped exposing (Zipped, aggregateZipped)
import Test exposing (..)


suite : Test
suite =
    describe "Traversal Aggregations"
        [ describe "should aggregate with OR logic"
            [ test "with one traversal" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                Or
                                [ traversal1 ]
                                zipped1
                    in
                    Expect.equal
                        actual
                        zipped1
            , test "with two traversals" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                Or
                                [ traversal1, traversal2 ]
                                zipped2
                    in
                    Expect.equal
                        actual
                        zipped2
            , test "with two or more traversals" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                Or
                                [ traversal1, traversal2, traversal3 ]
                                zipped3
                    in
                    Expect.equal
                        actual
                        zipped3
            ]
        , describe "should aggregate with AND logic"
            [ test "with one traversal" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                And
                                [ traversal1 ]
                                zipped1
                    in
                    Expect.equal
                        actual
                        zipped1
            , test "with two traversals" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                And
                                [ traversal1, traversal2 ]
                                zipped2
                    in
                    Expect.equal
                        actual
                        [ ( edge2, vendor2 )
                        , ( edge3, vendor3 )
                        , ( edge4, vendor2 )
                        , ( edge5, vendor3 )
                        ]
            , test "with two or more traversals" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                And
                                [ traversal1, traversal2, traversal3 ]
                                zipped3
                    in
                    Expect.equal
                        actual
                        []
            ]
        ]


zipped1 : List Zipped
zipped1 =
    [ ( edge1, vendor1 )
    , ( edge2, vendor2 )
    , ( edge3, vendor3 )
    ]


zipped2 : List Zipped
zipped2 =
    [ ( edge1, vendor1 )
    , ( edge2, vendor2 )
    , ( edge3, vendor3 )
    , ( edge4, vendor2 )
    , ( edge5, vendor3 )
    , ( edge6, vendor4 )
    ]


zipped3 : List Zipped
zipped3 =
    [ ( edge1, vendor1 )
    , ( edge2, vendor2 )
    , ( edge3, vendor3 )
    , ( edge4, vendor2 )
    , ( edge5, vendor3 )
    , ( edge6, vendor4 )
    , ( edge7, vendor4 )
    , ( edge8, vendor5 )
    , ( edge9, vendor6 )
    ]


traversal1 : Traversal
traversal1 =
    Traversal
        "1C"
        [ "1V", "2V", "3V" ]


traversal2 : Traversal
traversal2 =
    Traversal
        "2C"
        [ "2V", "3V", "4V" ]


traversal3 : Traversal
traversal3 =
    Traversal
        "3C"
        [ "4V", "5V", "6V" ]


edge1 : EdgeData
edge1 =
    EdgeData
        "1C"
        "1V"
        "10 transactions"
        "$100"
        "$10"
        "$10"
        "$10"


edge2 : EdgeData
edge2 =
    { edge1 | dst_id = "2V" }


edge3 : EdgeData
edge3 =
    { edge1 | dst_id = "3V" }


edge4 : EdgeData
edge4 =
    EdgeData
        "2C"
        "2V"
        "10 transactions"
        "$400"
        "$40"
        "$40"
        "$40"


edge5 : EdgeData
edge5 =
    { edge2 | dst_id = "3V" }


edge6 : EdgeData
edge6 =
    { edge2 | dst_id = "4V" }


edge7 : EdgeData
edge7 =
    EdgeData
        "3C"
        "4V"
        "10 transactions"
        "$700"
        "$70"
        "$70"
        "$70"


edge8 : EdgeData
edge8 =
    { edge3 | dst_id = "5V" }


edge9 : EdgeData
edge9 =
    { edge3 | dst_id = "6V" }


vendor1 : VertexData
vendor1 =
    VertexData
        "1V"
        "Vendor1"
        True
        [ "Santa Barbara" ]
        [ "Milpas" ]
        [ "CA" ]


vendor2 : VertexData
vendor2 =
    VertexData
        "2V"
        "Vendor2"
        True
        [ "Santa Barbara" ]
        [ "San Andres" ]
        [ "CA" ]


vendor3 : VertexData
vendor3 =
    VertexData
        "3V"
        "Vendor3"
        True
        [ "Santa Barbara" ]
        [ "W. Haley" ]
        [ "CA" ]


vendor4 : VertexData
vendor4 =
    VertexData
        "4V"
        "Vendor4"
        True
        [ "Santa Barbara" ]
        [ "E. Haley" ]
        [ "CA" ]


vendor5 : VertexData
vendor5 =
    VertexData
        "5V"
        "Vendor5"
        True
        [ "Santa Barbara" ]
        [ "Garden" ]
        [ "CA" ]


vendor6 : VertexData
vendor6 =
    VertexData
        "6V"
        "Vendor6"
        True
        [ "Santa Barbara" ]
        [ "De La Guerra" ]
        [ "CA" ]
