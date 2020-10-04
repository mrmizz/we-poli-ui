module TraversalAggregationSpec exposing (suite1, suite2)

import Expect exposing (Expectation)
import Models.Aggregation exposing (Aggregation(..))
import Models.Traversal exposing (Traversal)
import Models.Zipped exposing (Zipped, aggregateZipped, groupBySrcId)
import Models.Zipped.Fixtures exposing (..)
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
