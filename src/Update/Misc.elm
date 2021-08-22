module Update.Misc exposing (..)

import Model.Aggregation exposing (Aggregation(..))
import Model.Direction exposing (Direction(..))
import Model.Model exposing (Model, initialModel)
import Model.SortBy exposing (SortBy)
import Model.VertexData as VertexData exposing (VertexData)
import Msg.Msg exposing (Msg, resetViewport)


updateWithDirectionOption : Model -> ( Model, Cmd Msg )
updateWithDirectionOption model =
    case model.direction_selected of
        In ->
            ( { initialModel
                | direction_selected = Out
              }
            , Cmd.none
            )

        Out ->
            ( { initialModel
                | direction_selected = In
              }
            , Cmd.none
            )


updateWithAggOption : Model -> ( Model, Cmd Msg )
updateWithAggOption model =
    case model.aggregation_selected of
        And ->
            ( { model | aggregation_selected = Or }, Cmd.none )

        Or ->
            ( { model | aggregation_selected = And }, Cmd.none )


-- TODO: new traversal
updateWithSortByOption : Model -> SortBy -> ( Model, Cmd Msg )
updateWithSortByOption model sortBy =
    ( { model
        | sort_by_selected = sortBy
      }
    , Cmd.none
    )


updateVertexSelected : Model -> VertexData -> ( Model, Cmd Msg )
updateVertexSelected model vertex =
    let
        update : List VertexData
        update =
            (\gp -> gp.vertices) (VertexData.distinct (List.singleton vertex ++ model.vertices_selected))
    in
    ( { model | vertices_selected = update }, resetViewport )


updateVertexDeleted : Model -> VertexData -> ( Model, Cmd Msg )
updateVertexDeleted model vertex =
    let
        update =
            case model.vertices_selected of
                _ :: [] ->
                    []

                _ ->
                    List.filter (VertexData.notUID vertex.uid) model.vertices_selected
    in
    ( { model | vertices_selected = update }, Cmd.none )
