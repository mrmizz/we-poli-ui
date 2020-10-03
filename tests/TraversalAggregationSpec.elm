module TraversalAggregationSpec exposing (suite1, suite2)

import Expect exposing (Expectation)
import Models.Aggregation exposing (Aggregation(..))
import Models.EdgeData exposing (EdgeData)
import Models.Traversal exposing (Traversal)
import Models.VertexData exposing (VertexData)
import Models.Zipped exposing (Zipped, aggregateZipped, groupBySrcId)
import Test exposing (..)


suite1 : Test
suite1 =
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
            , test "with two or more traversals from the same src_id" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                Or
                                [ traversal3, traversal4 ]
                                zipped4
                    in
                    Expect.equal
                        actual
                        zipped4
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
            , test "with two or more traversals from the same src_id" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            aggregateZipped
                                And
                                [ traversal3, traversal4 ]
                                zipped4
                    in
                    Expect.equal
                        actual
                        zipped4
            ]
        ]


suite2 : Test
suite2 =
    describe "Traversal groupBy src_id"
        [ test "with multiple traversals from the same src_id" <|
            \_ ->
                let
                    actual : List Traversal
                    actual =
                        groupBySrcId [ traversal3, traversal4 ]
                in
                Expect.equal
                    actual
                    [ Traversal "3C" [ "4V", "5V", "6V", "7V", "8V" ] ]
        , test "with multiple traversals from distinct src_id" <|
            \_ ->
                let
                    actual : List Traversal
                    actual =
                        groupBySrcId [ traversal2, traversal3 ]
                in
                Expect.equal
                    actual
                    [ traversal2, traversal3 ]
        , test "with a single traversal" <|
            \_ ->
                let
                    actual : List Traversal
                    actual =
                        groupBySrcId [ traversal1 ]
                in
                Expect.equal
                    actual
                    [ traversal1 ]
        , test "with empty traversal" <|
            \_ ->
                let
                    actual : List Traversal
                    actual =
                        groupBySrcId []
                in
                Expect.equal
                    actual
                    []
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


zipped4 : List Zipped
zipped4 =
    [ ( edge7, vendor4 )
    , ( edge8, vendor5 )
    , ( edge9, vendor6 )
    , ( edge10, vendor7 )
    , ( edge11, vendor8 )
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


traversal4 : Traversal
traversal4 =
    Traversal
        "3C"
        [ "7V", "8V" ]


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


edge10 : EdgeData
edge10 =
    { edge3 | dst_id = "7V" }


edge11 : EdgeData
edge11 =
    { edge3 | dst_id = "8V" }


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
    { vendor1 | uid = "2V" }


vendor3 : VertexData
vendor3 =
    { vendor1 | uid = "3V" }


vendor4 : VertexData
vendor4 =
    { vendor1 | uid = "4V" }


vendor5 : VertexData
vendor5 =
    { vendor1 | uid = "5V" }


vendor6 : VertexData
vendor6 =
    { vendor1 | uid = "6V" }


vendor7 : VertexData
vendor7 =
    { vendor1 | uid = "7V" }


vendor8 : VertexData
vendor8 =
    { vendor1 | uid = "8V" }
