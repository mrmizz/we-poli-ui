module Model.Model exposing (Model, init, initialModel, initialModelWithParams)

import Model.Aggregation exposing (Aggregation(..))
import Model.Direction exposing (Direction(..))
import Model.SortBy exposing (SortBy(..))
import Model.State exposing (State(..))
import Model.Traversal as Traversal exposing (PageCount, Traversal)
import Model.VertexData exposing (VertexData)
import Model.VertexNameSearch exposing (VertexNameSearch)
import Model.Zipped exposing (Zipped)
import Msg.Msg exposing (Msg)


type alias Model =
    { state : State
    -- Building Search Parameters
    , aggregation_selected : Aggregation
    , direction_selected : Direction
    , sort_by_selected : SortBy
    , vertices_selected : List VertexData
    -- Vertex Name Auto Complete Search
    , vertex_name_search : VertexNameSearch
    -- Traversal
    , traversal: Traversal
    , zipped : List Zipped
    }


initialModel : Model
initialModel =
    { state = BuildingRequest False
    -- Building Search Parameters
    , aggregation_selected = Or
    , direction_selected = Out
    , sort_by_selected = Count
    , vertices_selected = []
    -- Vertex Name Auto Complete Search
    , vertex_name_search = { input = "", vertices = [] }
    -- Traversal
    , traversal = Traversal.Pending
    , zipped = []
    }


initialModelWithParams : Model -> Model
initialModelWithParams model =
    { initialModel
        | aggregation_selected = model.aggregation_selected
        , direction_selected = model.direction_selected
        , sort_by_selected = model.sort_by_selected
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
