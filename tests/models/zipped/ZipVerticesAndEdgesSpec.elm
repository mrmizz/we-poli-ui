module Models.Zipped.ZipVerticesAndEdgesSpec exposing (suite1)

import Expect
import Models.Direction exposing (Direction(..))
import Models.Zipped exposing (Zipped, zipVerticesAndEdges)
import Models.Zipped.Fixtures exposing (..)
import Test exposing (Test, describe, test)


suite1 : Test
suite1 =
    describe "Zip Vertices and Edges"
        [ describe "should handle multiple traversals from different src ids"
            [ test "direction in" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            zipVerticesAndEdges
                                In
                                [ traversal1 ]
                                [ vendor1, vendor2, vendor3 ]
                                [ edge1, edge2, edge3 ]
                    in
                    Expect.equal
                        actual
                        zipped1
            ]
        ]
