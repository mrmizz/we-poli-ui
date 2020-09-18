module Models.Zipped exposing (..)

import Models.EdgeData exposing (EdgeData)
import Models.VertexData exposing (VertexData)

-- TODO: move methods here
type alias Zipped =
    ( EdgeData, VertexData )
