module Update.Traversal exposing (updateWithChildPageCountRequest, updateWithPageCountRequest, updateWithPageCountResponse, updateWithTraversalResponse)

import Http
import Http.PageCount exposing (DynamoPageCount, PageCountResponse, buildPageCountRequest, pageCountPost)
import Http.Traversal exposing (DynamoTraversal, TraversalResponse, buildTraversalRequest, traversalPost)
import Http.Vertex exposing (buildVertexDataRequest, vertexDataPost)
import Model.Direction as Direction
import Model.Model exposing (Model, initialModelWithParams)
import Model.PageCount exposing (PageCount)
import Model.State exposing (State(..))
import Model.Traversal exposing (Traversal, TraversalPage)
import Model.VertexData exposing (VertexData)
import Msg.Msg exposing (Msg(..))
import Update.Generic exposing (unpackDynamoValue)


updateWithPageCountRequest : Model -> ( Model, Cmd Msg )
updateWithPageCountRequest model =
    let
        new =
            initialModelWithParams model
    in
    ( { new
        | state = Loading
        , vertices_selected = model.vertices_selected
      }
    , pageCountPost PageCountPostReceived (buildPageCountRequest (List.map (\v -> v.uid) model.vertices_selected))
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
    , pageCountPost PageCountPostReceived (buildPageCountRequest [ (\v -> v.uid) vertexData ])
    )


updateWithPageCountResponse : Model -> Result Http.Error PageCountResponse -> ( Model, Cmd Msg )
updateWithPageCountResponse model result =
    case result of
        Ok response ->
            let
                unpack : PageCountResponse -> PageCount
                unpack pageCountResponse =
                    let
                        unpackDynamoPageCount : DynamoPageCount -> List TraversalPage
                        unpackDynamoPageCount dynamoPageCount =
                            case String.toInt (unpackDynamoValue dynamoPageCount.page_count) of
                                Just int ->
                                    List.range 1 int
                                        |> List.map String.fromInt
                                        |> List.map (\pageNum -> TraversalPage (unpackDynamoValue dynamoPageCount.vertex_id) pageNum)

                                Nothing ->
                                    []
                    in
                    PageCount
                        (Model.PageCount.TraversalsPageCount [] (List.concatMap unpackDynamoPageCount pageCountResponse.responses.items))
                        (Model.PageCount.VertexDataPageCount [] [])
                        (Model.PageCount.EdgeDataPageCount [] [])

                pageCount : PageCount
                pageCount =
                    unpack response
            in
            case pageCount.traversals.pending of
                head :: tail ->
                    ( { model | page_count = Just { pageCount | traversals = Model.PageCount.TraversalsPageCount [ head ] tail } }
                    , traversalPost
                        (buildTraversalRequest [ head ])
                        TraversalPostReceived
                    )

                [] ->
                    ( { model | page_count = Just pageCount, state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )


updateWithTraversalResponse : Model -> Result Http.Error TraversalResponse -> ( Model, Cmd Msg )
updateWithTraversalResponse model result =
    case result of
        Ok response ->
            let
                unpack : TraversalResponse -> List Traversal
                unpack traversalResponse =
                    let
                        unpack_ : DynamoTraversal -> Traversal
                        unpack_ dynamoTraversal =
                            Traversal
                                (unpackDynamoValue dynamoTraversal.vertex_id)
                                (List.map unpackDynamoValue dynamoTraversal.related_vertex_ids.list)
                    in
                    List.map unpack_ traversalResponse.responses.items

                traversals : List Traversal
                traversals =
                    unpack response

                incrementedTotalTraversals : List Traversal
                incrementedTotalTraversals =
                    model.traversal_response ++ traversals
            in
            case model.page_count of
                Just pageCount ->
                    case pageCount.traversals.pending of
                        head :: tail ->
                            ( { model
                                | traversal_response = incrementedTotalTraversals
                                , page_count =
                                    Just
                                        { pageCount
                                            | traversals = Model.PageCount.TraversalsPageCount (pageCount.traversals.made ++ [ head ]) tail
                                        }
                              }
                            , traversalPost
                                (buildTraversalRequest [ head ])
                                TraversalPostReceived
                            )

                        [] ->
                            ( { model
                                | traversal_response = incrementedTotalTraversals
                                , page_count =
                                    Just
                                        { pageCount
                                            | vertex_data = Model.PageCount.VertexDataPageCount traversals model.traversal_response
                                        }
                              }
                            , vertexDataPost
                                (buildVertexDataRequest (List.concatMap (\trv -> trv.dst_ids) traversals))
                                VertexDataPostReceived
                            )

                Nothing ->
                    ( { model | state = DataIntegrityFailure }, Cmd.none )

        Err error ->
            ( { model | state = RequestFailure error }, Cmd.none )
