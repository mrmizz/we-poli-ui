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
        [ test "should aggregate with OR logic" <|
            \_ ->
                let
                    actual : List Zipped
                    actual = aggregateZipped Or [traversal1] [(edge1, vendor1), (edge2, vendor2), (edge3, vendor3)]

                in
                Expect.equal
                    actual
                    [(edge1, vendor1), (edge2, vendor2), (edge3, vendor3)]
        ]


traversal1: Traversal
traversal1 =
    Traversal
        "1C"
        ["1V", "2V", "3V"]


traversal2: Traversal
traversal2 =
    Traversal
        "2C"
        ["2V", "3V", "4V"]


traversal3: Traversal
traversal3 =
    Traversal
        "3C"
        ["4V", "5V", "6V"]


edge1: EdgeData
edge1 =
    EdgeData
        "1C"
        "1V"
        "10 transactions"
        "$100"
        "$10"
        "$10"
        "$10"

edge2: EdgeData
edge2 =
    EdgeData
        "1C"
        "2V"
        "10 transactions"
        "$200"
        "$20"
        "$20"
        "$20"

edge3: EdgeData
edge3 =
    EdgeData
        "1C"
        "3V"
        "10 transactions"
        "$300"
        "$30"
        "$30"
        "$30"


committee1: VertexData
committee1 =
    VertexData
        "1C"
        "Committee1"
        True
        ["Los Angeles"]
        ["Eagle Rock"]
        ["CA"]


committee2: VertexData
committee2 =
    VertexData
        "2C"
        "Committee2"
        True
        ["Los Angeles"]
        ["Belmont"]
        ["CA"]


committee3: VertexData
committee3 =
    VertexData
        "3C"
        "Committee3"
        True
        ["Los Angeles"]
        ["York St."]
        ["CA"]


vendor1: VertexData
vendor1 =
    VertexData
        "1V"
        "Vendor1"
        True
        ["Santa Barbara"]
        ["Milpas"]
        ["CA"]


vendor2: VertexData
vendor2 =
    VertexData
        "2V"
        "Vendor2"
        True
        ["Santa Barbara"]
        ["San Andres"]
        ["CA"]


vendor3: VertexData
vendor3 =
    VertexData
        "3V"
        "Vendor3"
        True
        ["Santa Barbara"]
        ["W. Haley"]
        ["CA"]


vendor4: VertexData
vendor4 =
    VertexData
        "4V"
        "Vendor4"
        True
        ["Santa Barbara"]
        ["E. Haley"]
        ["CA"]


vendor5: VertexData
vendor5 =
    VertexData
        "5V"
        "Vendor5"
        True
        ["Santa Barbara"]
        ["Garden"]
        ["CA"]


vendor6: VertexData
vendor6 =
    VertexData
        "6V"
        "Vendor6"
        True
        ["Santa Barbara"]
        ["De La Guerra"]
        ["CA"]
