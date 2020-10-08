module Models.Zipped.SortBySpec exposing (suite1)

import Expect
import Models.Zipped exposing (Zipped)
import Test exposing (Test, describe, test)



-- TODO


suite1 : Test
suite1 =
    describe "Sort Zipped"
        [ describe "should sort by count"
            [ test "that is pre-sorted" <|
                \_ ->
                    let
                        actual : List Zipped
                        actual =
                            []
                    in
                    Expect.equal
                        actual
                        []
            ]
        ]
