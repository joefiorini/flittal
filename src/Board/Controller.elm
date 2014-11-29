module Board.Controller (renderBoard, checkFocus) where

import Keyboard
import Debug

import Signal
import Signal (Channel, (<~))

import Board.View (draw)
import Board.State (..)

import Board.Action (..)
import Box.Controller as Box

import Connection.Controller as Connection

import DomUtils (DragEvent, DnDPort, extractBoxId)

import Html (Html)

import String (split, toInt)
import List
import List ((::))

import Native.Custom.Html

import Box.Action

renderBoard : DnDPort -> DnDPort -> Signal.Signal Html
renderBoard dropP dragstartP = Signal.map render <| state dropP dragstartP

renderConnections : List Connection.State -> List Html
renderConnections = List.map Connection.renderConnection

widgets : Board -> List Html
widgets board = List.concatMap identity [ (List.map (Box.renderBox actions) board.boxes)
                , (renderConnections board.connections)
                ]

render : Board -> Html
render board = draw board actions <| widgets board

actions : Channel Action
actions = Signal.channel NoOp

checkFocus =
    let needsFocus act =
            case act of
              EditingBox key bool -> bool
              _ -> False

        toSelector (EditingBox id _) = ("#box-" ++ toString id ++ "-label")
    in
        toSelector <~ Signal.keepIf needsFocus (EditingBox 0 True) (Signal.subscribe actions)

keyboardRequestAction = Signal.map keyboardRequest Keyboard.lastPressed

keyboardRequest keyCode = case keyCode of
  65 -> NewBox
  67 -> ConnectSelections
  _ -> NoOp

moveBoxAction : DragEvent -> Action
moveBoxAction event = let boxKeyM = extractBoxId event.id in
    case boxKeyM of
      Ok key ->
       if | event.isStart -> DraggingBox key
          | otherwise -> MoveBox key event
      Err s -> NoOp

state : DnDPort -> DnDPort -> Signal.Signal Board
state dropP dragstartP = Signal.foldp step startingState (Signal.mergeMany [
                                           keyboardRequestAction
                                         , Signal.subscribe actions
                                         , moveBoxAction <~ dropP
                                         , moveBoxAction <~ dragstartP
                                         ])

isEditing = List.any (.isEditing)

updateStateSelections box state =
  let updateBox = Box.step Box.Action.Selected in
  { state | boxes <- List.map updateBox state.boxes }

contains obj = List.any (\b -> b == obj)
containsEither obj1 obj2 = List.any (\b -> b == obj1 || b == obj2)

sortLeftToRight : List Box.State -> List Box.State
sortLeftToRight boxes = List.sortBy (snd << (.position)) <| List.sortBy (fst << (.position)) boxes

step : Action -> Board -> Board
step action state =
  let updateBoxInState action box iterator = (if iterator.key == box.key then Box.step action iterator else iterator)
      updateSelectedBoxes action iterator = (if iterator.isSelected then Box.step action iterator else iterator)
      performActionOnAllBoxes action = (Box.step action)
      selectedBoxes = List.filter (.isSelected)
      deselectBoxes = List.map (\box -> { box | isSelected <- False }) in
    case action of
      NewBox ->
        if | isEditing state.boxes -> state
           | True ->
            let newBox = makeBox state.nextIdentifier in
              Debug.log "newBox" { state | boxes <- newBox :: state.boxes
                                         , nextIdentifier <- state.nextIdentifier + 1 }
      CancelEditingBox key ->
        let box = boxForKey key state.boxes
            cancelEditing = List.map (updateBoxInState Box.Action.CancelEditing box)
            updateBoxes = cancelEditing >> deselectBoxes in
          Debug.log "Canceling edit" { state | boxes <- updateBoxes state.boxes }
      DeselectBoxes ->
        let cancelEditing = List.map (Box.step Box.Action.CancelEditing)
            updateBoxes = cancelEditing >> deselectBoxes in
          { state | boxes <- updateBoxes state.boxes }
      SelectBoxMulti id ->
        let box = boxForKey id state.boxes
            updateBoxes = List.map (updateBoxInState Box.Action.Selected box) in
          Debug.log "adding box to selection" { state | boxes <- updateBoxes state.boxes }

      DraggingBox id ->
        let box = boxForKey id state.boxes
            selectedBox = List.map (updateBoxInState (Box.Action.SetSelected True) box)
            draggingBox = List.map (updateSelectedBoxes Box.Action.Dragging)
            updateBoxes = selectedBox >> draggingBox in
            Debug.log "started dragging" { state | boxes <- updateBoxes state.boxes }

      SelectBox id ->
        let box = boxForKey id state.boxes
            selectedBox = List.map (updateBoxInState Box.Action.Selected box)
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
        let draggingBox = List.map (updateSelectedBoxes Box.Action.Dragging)
            moveAllSelectedBoxes boxes = List.map (updateSelectedBoxes (Box.Action.Move event)) boxes
            updateBoxes = moveAllSelectedBoxes >> draggingBox
        in
          Debug.log "moved box"
            step ReconnectSelections { state | boxes <- updateBoxes state.boxes }
      ReconnectSelections ->
        { state | connections <- Connection.rebuildConnections
                                  <| List.map (Connection.updateBoxes state.boxes)
                                  state.connections }
      ConnectSelections ->
        if | List.length (selectedBoxes state.boxes) < 2 -> state
           | otherwise ->
             Debug.log "Connecting Selections"
             { state | connections <- Connection.buildConnections state.connections
                                          <| sortLeftToRight
                                          <| selectedBoxes state.boxes }

      NoOp -> Debug.log "NoOp" state

