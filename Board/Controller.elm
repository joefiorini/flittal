module Board.Controller (renderBoard, checkFocus) where

import Keyboard
import Debug
import Graphics.Input as Input

import Board.View (draw)
import Board.State (..)

import Board.Action (..)
import Box.Controller as Box

import Connection.Controller as Connection

import DomUtils (DragEvent, DnDPort, extractBoxId)

import Html (Html)

import String (split, toInt)
import Either (..)

import Native.Custom.Html

import Box.Action

renderBoard : DnDPort -> DnDPort -> Signal Html
renderBoard dropP dragstartP = lift render <| state dropP dragstartP

renderConnections : [Connection.State] -> [Html]
renderConnections = map Connection.renderConnection

widgets : Board -> [Html]
widgets board = concatMap identity [ (map (Box.renderBox actions.handle) board.boxes)
                , (renderConnections board.connections)
                ]

render : Board -> Html
render board = draw board actions.handle <| widgets board

actions : Input.Input Action
actions = Input.input NoOp

checkFocus =
    let needsFocus act =
            case act of
              EditingBox key bool -> bool
              _ -> False

        toSelector (EditingBox id _) = ("#box-" ++ show id ++ "-label")
    in
        toSelector <~ keepIf needsFocus (EditingBox 0 True) actions.signal

keyboardRequestAction = lift keyboardRequest Keyboard.lastPressed

keyboardRequest keyCode = case keyCode of
  65 -> NewBox
  67 -> ConnectSelections
  _ -> NoOp

moveBoxAction : DragEvent -> Action
moveBoxAction event = let boxKeyM = extractBoxId event.id in
    case boxKeyM of
      Just key ->
       if | event.isStart -> DraggingBox key
          | otherwise -> MoveBox key event
      Nothing -> NoOp

state : DnDPort -> DnDPort -> Signal Board
state dropP dragstartP = foldp step startingState (merges [
                                           keyboardRequestAction
                                         , actions.signal
                                         , moveBoxAction <~ dropP
                                         , moveBoxAction <~ dragstartP
                                         ])

isEditing = any (.isEditing)

updateStateSelections box state =
  let updateBox = Box.step Box.Action.Selected in
  { state | boxes <- map updateBox state.boxes }

contains obj = any (\b -> b == obj)

sortLeftToRight : [Box.State] -> [Box.State]
sortLeftToRight boxes = sortBy (snd << (.position)) <| sortBy (fst << (.position)) boxes

step : Action -> Board -> Board
step action state =
  let updateBoxInState action box iterator = (if iterator.key == box.key then Box.step action iterator else iterator)
      updateSelectedBoxes action iterator = (if iterator.isSelected then Box.step action iterator else iterator)
      performActionOnAllBoxes action = (Box.step action)
      selectedBoxes = filter (.isSelected)
      deselectBoxes = map (\box -> { box | isSelected <- False }) in
    case action of
      NewBox ->
        if | isEditing state.boxes -> state
           | True ->
            let newBox = makeBox state.nextIdentifier in
              Debug.log "newBox" { state | boxes <- newBox :: state.boxes
                                         , nextIdentifier <- state.nextIdentifier + 1 }
      CancelEditingBox key ->
        let box = boxForKey key state.boxes
            cancelEditing = map (updateBoxInState Box.Action.CancelEditing box)
            updateBoxes = cancelEditing >> deselectBoxes in
          Debug.log "Canceling edit" { state | boxes <- updateBoxes state.boxes }
      DeselectBoxes ->
        let cancelEditing = map (Box.step Box.Action.CancelEditing)
            updateBoxes = cancelEditing >> deselectBoxes in
          { state | boxes <- updateBoxes state.boxes }
      SelectBoxMulti id ->
        let box = boxForKey id state.boxes
            updateBoxes = map (updateBoxInState Box.Action.Selected box) in
          Debug.log "adding box to selection" { state | boxes <- updateBoxes state.boxes }

      DraggingBox id ->
        let box = boxForKey id state.boxes
            selectedBox = map (updateBoxInState (Box.Action.SetSelected True) box)
            draggingBox = map (updateSelectedBoxes Box.Action.Dragging)
            updateBoxes = selectedBox >> draggingBox in
            Debug.log "started dragging" { state | boxes <- updateBoxes state.boxes }

      SelectBox id ->
        let box = boxForKey id state.boxes
            selectedBox = map (updateBoxInState Box.Action.Selected box)
            updateBoxes = deselectBoxes >> selectedBox in
          if | box.isSelected -> state
             | otherwise ->
          Debug.log "selecting box" { state | boxes <- updateBoxes state.boxes }
      EditingBox id toggle ->
        let box = boxForKey id state.boxes in
          Debug.log "editing box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Editing toggle) box }
      UpdateBox box label ->
        { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Update label) box }
      MoveBox key event ->
        let draggingBox = map (updateSelectedBoxes Box.Action.Dragging)
            moveAllSelectedBoxes boxes = map (updateSelectedBoxes (Box.Action.Move event)) boxes
            updateBoxes = moveAllSelectedBoxes >> draggingBox
        in
          Debug.log "moved box" { state | boxes <- updateBoxes state.boxes }
      ConnectSelections ->
        let makeConnections : [Connection.State] -> [Box.State] -> (Box.State, [Connection.State])
            makeConnections connections boxes =
              (foldl Connection.connectBoxes (head boxes, connections) (tail boxes))
        in
        if | length (selectedBoxes state.boxes) < 2 -> state
           | otherwise ->
             Debug.log "Connecting Selections"
             { state | connections <- snd <| makeConnections state.connections
                                          <| sortLeftToRight
                                          <| selectedBoxes state.boxes }

      NoOp -> Debug.log "NoOp" state

