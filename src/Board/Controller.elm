module Board.Controller exposing (..)

import Debug
import Maybe
import Task
import Html exposing (..)
import Html.Events exposing (on)
import Html.Attributes exposing (id, style, property, class)
import Board.Model
import Style.Color exposing (Color(..))
import Box.Controller as Box
import Connection.Controller as Connection
import Connection.Controller exposing (leftOf)
import Connection.Model
import DomUtils exposing (getTargetId, extractBoxId, getMouseSelectionEvent, styleProperty, DragEvent)
import Html exposing (Html)
import List
import List exposing ((::))
import List.Extra exposing (find)
import Geometry.Types as Geometry
import Dom
import Json.Decode exposing (map, succeed)


type alias Board =
    Board.Model.Model


type Msg
    = NoOp
    | BoxAction Box.Msg
    | ClearBoard
    | UpdateBox Box.Model String
    | NewBox
    | MoveBox Box.MoveType Box.MoveDirection
    | DeselectBoxes
    | EditingBox Box.BoxKey Bool
    | EditingSelectedBox Bool
    | SelectBox Box.BoxKey
    | SelectBoxMulti Box.BoxKey
    | CancelEditingBox Box.BoxKey
    | ConnectSelections
    | ReconnectSelections
    | DisconnectSelections
    | DeleteSelections
    | SelectNextBox
    | SelectPreviousBox
    | DraggingBox Box.BoxKey
    | UpdateBoxColor Color
    | ResizeBox Box.ResizeMode
    | Drop Box.BoxKey DragEvent


view channel model height =
    let
        ( boxes, connections ) =
            widgets model
    in
        div
            [ style
                [ styleProperty "height" <| Geometry.toPx height
                ]
            , class "board"
            , id "container"
            , on "dblclick" (getTargetId |> Json.Decode.map buildEditingAction)
            , on "mousedown" (getMouseSelectionEvent |> Json.Decode.map buildSelectAction)
            ]
            (List.concatMap identity [ boxes, connections ])


entersEditMode update =
    Debug.log "entersEditMode" <|
        case update of
            EditingBox _ toggle ->
                toggle

            EditingSelectedBox toggle ->
                toggle

            BoxAction a ->
                Box.entersEditMode a

            otherwise ->
                False


encode =
    Board.Model.encode


decode =
    Board.Model.decode


renderConnections : List Connection.Model.Model -> List (Html Msg)
renderConnections =
    List.map Connection.renderConnection


buildSelectAction event =
    let
        boxIdM =
            extractBoxId event.id
    in
        case boxIdM of
            Just key ->
                if event.shiftKey then
                    SelectBoxMulti key
                else
                    SelectBox key

            Nothing ->
                Debug.log "deselect" DeselectBoxes


buildEditingAction : String -> Msg
buildEditingAction id =
    let
        boxIdM =
            extractBoxId id
    in
        case boxIdM of
            Just key ->
                EditingBox key True

            Nothing ->
                NoOp


widgets : Board -> ( List (Html Msg), List (Html Msg) )
widgets board =
    ( (List.map (\b -> (Box.view b |> Html.map BoxAction)) board.boxes)
    , (renderConnections board.connections)
    )


toSelector : Int -> String
toSelector domId =
    ("#box-" ++ toString id ++ "-label")


moveBoxAction : DragEvent -> Msg
moveBoxAction event =
    let
        boxKeyM =
            extractBoxId event.id
    in
        case boxKeyM of
            Just key ->
                if event.isStart then
                    DraggingBox key
                else
                    Drop key event

            Nothing ->
                NoOp


startingState =
    { boxes = []
    , connections = []
    , nextIdentifier = 1
    }


isEditing =
    List.any (.isEditing)


updateStateSelections box state =
    let
        updateBox lastBox _ =
            Box.step (Box.SetSelected <| lastBox.selectedIndex + 1) box
    in
        { state | boxes = List.foldl updateBox state.boxes }


contains obj =
    List.any (\b -> b == obj)


containsEither obj1 obj2 =
    List.any (\b -> b == obj1 || b == obj2)


sortRightToLeft =
    List.reverse << sortLeftToRight


sortLeftToRight : List Box.Model -> List Box.Model
sortLeftToRight boxes =
    List.sortBy (Tuple.first << (.position)) <|
        List.sortBy (Tuple.second << (.position)) boxes


boxForKey : Box.BoxKey -> List Box.Model -> Maybe Box.Model
boxForKey key boxes =
    List.head (List.filter (\b -> b.key == key) boxes)


makeBox : Box.BoxKey -> Box.Model
makeBox identifier =
    let
        style =
            { color = White }
    in
        { position = ( 0, 0 )
        , size = ( 100, 50 )
        , label = "New Box"
        , originalLabel = "New Box"
        , key = identifier
        , isEditing = False
        , isDragging = False
        , selectedIndex = 1
        , style = style
        }


replaceBox : List Box.Model -> Box.Model -> List Box.Model
replaceBox boxes withBox =
    List.map
        (\box ->
            if box.key == withBox.key then
                withBox
            else
                box
        )
        boxes


updateBoxInState : Box.BoxKey -> Box.Msg -> Box.Model -> Box.Model
updateBoxInState boxKey update box =
    if box.key == boxKey then
        Box.step update box
    else
        box


step : Msg -> Board -> ( Board, Cmd Msg )
step update state =
    let
        updateSelectedBoxes update iterator =
            (if iterator.selectedIndex > -1 then
                Box.step update iterator
             else
                iterator
            )

        performActionOnAllBoxes update =
            (Box.step update)

        nextSelectedIndex boxes =
            List.foldl
                (\last index ->
                    if index > last then
                        index + 1
                    else
                        last + 1
                )
                0
            <|
                List.map (.selectedIndex) boxes

        selectedBoxes boxes =
            List.sortBy (.selectedIndex) <|
                List.filter (\b -> b.selectedIndex > -1) boxes

        deselectBoxes =
            List.map (\box -> { box | selectedIndex = -1 })
    in
        case Debug.log "Performing update" update of
            NewBox ->
                if isEditing state.boxes then
                    ( state, Cmd.none )
                else
                    let
                        newBox =
                            makeBox state.nextIdentifier
                    in
                        ( Debug.log "newBox"
                            { state
                                | boxes = newBox :: (deselectBoxes state.boxes)
                                , nextIdentifier = state.nextIdentifier + 1
                            }
                        , Cmd.none
                        )

            BoxAction (Box.CancelEditingBox box) ->
                let
                    box_ =
                        Box.step Box.CancelEditing box

                    boxes_ =
                        replaceBox state.boxes box_
                in
                    ( Debug.log "Canceling edit" { state | boxes = deselectBoxes boxes_ }, Cmd.none )

            DeselectBoxes ->
                let
                    cancelEditing =
                        List.map (Box.step Box.CancelEditing)

                    updateBoxes =
                        cancelEditing >> deselectBoxes
                in
                    ( { state | boxes = updateBoxes state.boxes }, Cmd.none )

            SelectBoxMulti id ->
                ( Debug.log "adding box to selection"
                    { state
                        | boxes =
                            state.boxes
                                |> List.map
                                    (\box ->
                                        updateBoxInState id (nextSelectedIndex state.boxes |> Box.SetSelected) box
                                    )
                    }
                , Cmd.none
                )

            DraggingBox id ->
                let
                    box =
                        boxForKey id state.boxes

                    selectedBox boxes =
                        List.map (\box -> updateBoxInState id (Box.SetSelected <| nextSelectedIndex boxes) box) boxes

                    draggingBox =
                        List.map (updateSelectedBoxes Box.Dragging)

                    updateBoxes =
                        selectedBox >> draggingBox
                in
                    ( Debug.log "started dragging" { state | boxes = updateBoxes state.boxes }, Cmd.none )

            SelectBox id ->
                let
                    selectedBox boxes =
                        List.map (\box -> updateBoxInState id (Box.SetSelected <| nextSelectedIndex boxes) box) boxes

                    updateBoxes =
                        deselectBoxes >> selectedBox
                in
                    ( Debug.log "selecting box" { state | boxes = updateBoxes state.boxes }, Cmd.none )

            SelectNextBox ->
                let
                    selections =
                        List.filter Box.isSelected <| sortLeftToRight state.boxes

                    nextBox =
                        let
                            sortedBoxes =
                                sortLeftToRight state.boxes
                        in
                            case Debug.log "selections" selections of
                                [] ->
                                    List.head <| Debug.log "all sorted" sortedBoxes

                                current :: _ ->
                                    let
                                        rightBoxes =
                                            List.filter (\box -> leftOf current.position box.position) sortedBoxes
                                    in
                                        case rightBoxes of
                                            [] ->
                                                List.head sortedBoxes

                                            current :: _ ->
                                                Just current

                    newState =
                        nextBox
                            |> Maybe.map
                                (\next ->
                                    { state
                                        | boxes =
                                            List.map
                                                (\box -> updateBoxInState next.key (Box.SetSelected 0) box)
                                                state.boxes
                                    }
                                )
                in
                    ( Maybe.withDefault state newState
                    , Cmd.none
                    )

            SelectPreviousBox ->
                let
                    selections =
                        List.filter Box.isSelected <| sortRightToLeft state.boxes

                    nextBox =
                        let
                            sortedBoxes =
                                sortRightToLeft state.boxes
                        in
                            case Debug.log "selections" selections of
                                [] ->
                                    List.head <| Debug.log "all sorted" sortedBoxes

                                current :: _ ->
                                    let
                                        firstRight =
                                            find (\box -> leftOf box.position current.position) sortedBoxes
                                    in
                                        case firstRight of
                                            Nothing ->
                                                List.head sortedBoxes

                                            _ ->
                                                firstRight

                    newState =
                        nextBox
                            |> Maybe.map
                                (\next ->
                                    { state
                                        | boxes =
                                            deselectBoxes state.boxes
                                                |> List.map
                                                    (\box -> updateBoxInState next.key (Box.SetSelected 0) box)
                                    }
                                )
                in
                    ( Maybe.withDefault state newState
                    , Cmd.none
                    )

            EditingBox boxKey toggle ->
                let
                    box =
                        boxForKey boxKey state.boxes |> Maybe.map (\box -> Box.step (Box.Editing toggle) box)

                    newState =
                        Maybe.map (\box -> { state | boxes = replaceBox state.boxes box }) box
                in
                    ( Debug.log "Canceling edit" (Maybe.withDefault state newState), toSelector boxKey |> Dom.focus |> (Task.attempt (\_ -> NoOp)) )

            BoxAction (Box.EditingBox box toggle) ->
                let
                    box_ =
                        Box.step (Box.Editing toggle) box
                in
                    ( Debug.log "editing box" { state | boxes = replaceBox state.boxes <| box_ }, Cmd.none )

            EditingSelectedBox toggle ->
                let
                    selectedBox =
                        List.filter (\b -> b.selectedIndex /= -1) state.boxes |> List.head

                    newState =
                        selectedBox
                            |> Maybe.map
                                (\box ->
                                    { state
                                        | boxes =
                                            Box.step (Box.Editing toggle) box |> replaceBox state.boxes
                                    }
                                )
                in
                    ( Maybe.withDefault state newState, Cmd.none )

            BoxAction (Box.UpdateBox box label) ->
                ( Debug.log "Box.UpdateBox" { state | boxes = replaceBox state.boxes <| Box.step (Box.Update label) box }, Cmd.none )

            Drop key event ->
                let
                    draggingBox =
                        List.map (updateSelectedBoxes Box.Dragging)

                    moveAllSelectedBoxes boxes =
                        List.map (updateSelectedBoxes (Box.Drop event)) boxes

                    updateBoxes =
                        moveAllSelectedBoxes >> draggingBox
                in
                    Debug.log "moved box"
                        step
                        ReconnectSelections
                        { state | boxes = updateBoxes state.boxes }

            ReconnectSelections ->
                ( state, Cmd.none )

            -- TODO: Fix reconnect selections!
            -- { state
            --     | connections =
            --         Connection.boxMap
            --             Connection.connectBoxes
            --             state.boxes
            --             state.connections
            -- }
            ConnectSelections ->
                if List.length (selectedBoxes state.boxes) < 2 then
                    ( state, Cmd.none )
                else
                    ( state
                    , Cmd.none
                    )

            -- TODO: Fix connect selections
            -- Debug.log "Connecting Selections"
            --     { state
            --         | connections =
            --             Connection.buildConnections state.connections <|
            --                 Debug.log "Selected Boxes" <|
            --                     selectedBoxes state.boxes
            --     }
            DisconnectSelections ->
                let
                    selectedBoxes =
                        List.filter Box.isSelected state.boxes

                    connectionish =
                        case selectedBoxes of
                            [ box1, box2 ] ->
                                Just <| List.filter (Connection.onBoxes box1 box2) state.connections

                            _ ->
                                Nothing

                    filtered =
                        Maybe.map
                            (\c ->
                                case c of
                                    [] ->
                                        state.connections

                                    connection :: rest ->
                                        List.filter ((/=) connection) state.connections
                            )
                            connectionish
                in
                    case filtered of
                        Just f ->
                            ( { state | connections = f }, Cmd.none )

                        Nothing ->
                            ( state, Cmd.none )

            DeleteSelections ->
                let
                    isSelected boxKey =
                        List.length (Box.filterKey (not << Box.isSelected) boxKey state.boxes)
                            == 1
                in
                    ( Debug.log "Deleting Selections"
                        { state
                            | boxes = List.filter (not << Box.isSelected) state.boxes
                            , connections =
                                List.filter
                                    (\c ->
                                        (isSelected c.startBox)
                                            && (isSelected c.endBox)
                                    )
                                    state.connections
                        }
                    , Cmd.none
                    )

            ResizeBox mode ->
                let
                    updateBoxes =
                        List.map (updateSelectedBoxes (Box.Resize mode))
                in
                    step ReconnectSelections { state | boxes = updateBoxes state.boxes }

            UpdateBoxColor color ->
                ( { state | boxes = List.map (updateSelectedBoxes (Box.UpdateColor color)) state.boxes }, Cmd.none )

            MoveBox mode direction ->
                let
                    updateBoxes =
                        List.map (updateSelectedBoxes (Box.Move mode direction))
                in
                    step ReconnectSelections { state | boxes = updateBoxes state.boxes }

            ClearBoard ->
                ( startingState, Cmd.none )

            _ ->
                ( state, Cmd.none )
