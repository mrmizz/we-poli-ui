module Update.Traversal exposing (updateWithChildPageCountRequest, updateWithPageCountRequest, updateWithPageCountResponse, updateWithPaginatedTraversalRequest, updateWithTraversalResponse)

import Http
import Http.Edge exposing (buildEdgeDataRequest, edgeDataPost)
import Http.PageCount exposing (DynamoPageCount, PageCountResponse, buildPageCountRequest, pageCountPost)
import Http.Traversal exposing (DynamoTraversal, TraversalResponse, buildTraversalRequest, traversalPost)
import Http.Vertex exposing (buildVertexDataRequest, vertexDataPost)
import Model.Direction as Direction
import Model.Model exposing (Model, initialModelWithParams)
import Model.State exposing (State(..))
import Model.Traversal as Traversal exposing (PageCount, Traversal)
import Model.VertexData exposing (VertexData)
import Msg.Msg exposing (Msg(..), VertexDataClient(..), resetViewport)
import Update.Generic exposing (unpackDynamoArrayNumber, unpackDynamoNumber, unpackDynamoNumberAsInt)


updateWithPageCountRequest : Model -> ( Model, Cmd Msg )
updateWithPageCountRequest model =
    let
        new =
            initialModelWithParams model
    in
    case List.head model.vertices_selected of
        Just head ->
            ( { new
                | state = Loading
                , vertices_selected = model.vertices_selected
              }
            , pageCountPost PageCountPostReceived (buildPageCountRequest ((\v -> v.uid) head))
            )

        Nothing ->
            ( { new
                | state = DataIntegrityFailure
              }
            , Cmd.none
            )


updateWithChildPageCountRequest : Model -> VertexData -> ( Model, Cmd Msg )
updateWithChildPageCountRequest model vertexData =
    let
        new =
            initialModelWithParams model
    in
    ( { new
        | state = Loading
        , vertices_selected = [ vertexData ]
        , direction_selected = Direction.switch model.direction_selected
      }
    , pageCountPost
        PageCountPostReceived
        (buildPageCountRequest ((\v -> v.uid) vertexData))
    )


updateWithPaginatedTraversalRequest : Model -> PageCount -> ( Model, Cmd Msg )
updateWithPaginatedTraversalRequest model pageCount =
    ( { model
        | state = Loading
        , traversal = Traversal.Waiting pageCount
      }
    , Cmd.batch
        [ traversalPost
            (buildTraversalRequest model.sort_by_selected pageCount.src_id pageCount.current_page)
            (TraversalPostReceived pageCount)
        , resetViewport
        ]
    )


updateWithPageCountResponse : Model -> Result Http.Error PageCountResponse -> ( Model, Cmd Msg )
updateWithPageCountResponse model result =
    case result of
        Ok response ->
            let
                unpack : PageCount
                unpack =
                    { src_id = unpackDynamoNumber response.item.vertex_id
                    , total_pages = unpackDynamoNumberAsInt response.item.page_count
                    , current_page = 1
                    }
            in
            ( { model | traversal = Traversal.Waiting unpack }
            , traversalPost
                (buildTraversalRequest model.sort_by_selected unpack.src_id unpack.current_page)
                (TraversalPostReceived unpack)
            )

        Err error ->
            ( { model | state = TraversalFailure error }, Cmd.none )


updateWithTraversalResponse : Model -> PageCount -> Result Http.Error TraversalResponse -> ( Model, Cmd Msg )
updateWithTraversalResponse model pageCount result =
    case result of
        Ok response ->
            let
                unpack =
                    unpackDynamoArrayNumber response.item.related_vertex_ids

                vertexRequest : Cmd Msg
                vertexRequest =
                    vertexDataPost (buildVertexDataRequest unpack) (VertexDataPostReceived ForTraversal)

                edgeRequest : Cmd Msg
                edgeRequest =
                    edgeDataPost (buildEdgeDataRequest model.direction_selected ( pageCount.src_id, unpack )) EdgeDataPostReceived
            in
            ( model
            , Cmd.batch [ vertexRequest, edgeRequest ]
            )

        Err error ->
            ( { model | state = TraversalFailure error }, Cmd.none )
