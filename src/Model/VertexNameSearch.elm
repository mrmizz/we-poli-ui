module Model.VertexNameSearch exposing (VertexNameSearch(..), sort)

import Dict
import Model.VertexData exposing (VertexData)


type VertexNameSearch
    = Input String
    | Waiting String (List VertexData)


sort : List String -> List VertexData -> List VertexData
sort sortedVertexIds unsortedVertices =
    let
        dict : Dict.Dict String VertexData
        dict =
            Dict.fromList (List.map (\v -> ( v.uid, v )) unsortedVertices)

        get : String -> List VertexData
        get key =
            case Dict.get key dict of
                Just vertex ->
                    [ vertex ]

                Nothing ->
                    []
    in
    List.concatMap get sortedVertexIds
