module Models.Zipped.ZipVerticesAndEdgesSpec exposing (suite1)

import Expect
import Models.Direction exposing (Direction(..))
import Models.Zipped exposing (Zipped, zipVerticesAndEdges)
import Models.Zipped.Fixtures exposing (..)
import Test exposing (Test, describe, test)


suite1 : Test
suite1 =
    describe "Zip Vertices and Edges"
        [ describe "should handle a single traversal"
            [ test "that is pre-sorted" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            zipVerticesAndEdges
                                Out
                                [ traversal1 ]
                                [ vendor1, vendor2, vendor3 ]
                                [ edge1, edge2, edge3 ]
                    in
                    Expect.equal
                        actual
                        zipped1
            , test "that is not pre-sorted" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            zipVerticesAndEdges
                                Out
                                [ traversal1 ]
                                [ vendor1, vendor3, vendor2 ]
                                [ edge3, edge2, edge1 ]
                    in
                    Expect.equal
                        actual
                        zipped1
            ]
        , describe "should handle multiple traversals from different src ids"
            [ test "that is pre-sorted" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            zipVerticesAndEdges
                                Out
                                [ traversal1, traversal2 ]
                                [ vendor1, vendor2, vendor3, vendor4 ]
                                [ edge1, edge2, edge3, edge4, edge5, edge6 ]
                    in
                    Expect.equal
                        actual
                        zipped2
            , test "that is not pre-sorted" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            zipVerticesAndEdges
                                Out
                                [ traversal1, traversal2 ]
                                [ vendor1, vendor3, vendor2, vendor4 ]
                                [ edge6, edge2, edge4, edge3, edge5, edge1 ]
                    in
                    Expect.equal
                        actual
                        zipped2
            ]
        ]
