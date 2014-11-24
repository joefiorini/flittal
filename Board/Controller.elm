module Board.Controller (renderBoard, checkFocus) where

import Keyboard
import Debug
import Graphics.Input as Input

import Board.View (draw)
import Board.State (..)

import Board.Action (..)
import Box.Controller as Box

import DomUtils (DragEvent, DnDPort, extractBoxId)

import Html (Html)

import String (split, toInt)
import Either (..)

import Native.Custom.Html

import Box.Action

renderBoard : DnDPort -> DnDPort -> Signal Html
renderBoard dropP dragstartP = lift render <| state dropP dragstartP

render : Board -> Html
render board = draw actions.handle (map (Box.renderBox actions.handle) board.boxes)

actions : Input.Input Action
actions = Input.input NoOp

checkFocus =
    let needsFocus act =
            case Debug.log "checking focus for" act of
              EditingBox key bool -> bool
              _ -> False

        toSelector (EditingBox id _) = ("#box-" ++ show id ++ "-label")
    in
        toSelector <~ keepIf needsFocus (EditingBox 0 True) actions.signal

eitherToAction = (either (\id -> NewBox id) (\id -> NoOp))
extractEither = (either (\id -> id) (\id -> id))

addBoxAction = eitherToAction <~ (foldp (\k i -> if | k == 65 -> Left ((extractEither i) + 1)
                                                    | True -> Right (extractEither i))
                                        (Left 0) Keyboard.lastPressed)

moveBoxAction : DragEvent -> Action
moveBoxAction event = let boxKeyM = extractBoxId event.id in
    case boxKeyM of
      Just key ->
       if | event.isStart -> DraggingBox key
          | otherwise -> MoveBox key event
      Nothing -> NoOp

state : DnDPort -> DnDPort -> Signal Board
state dropP dragstartP = foldp step startingState (merges [
                                           addBoxAction
                                         , actions.signal
                                         , moveBoxAction <~ dropP
                                         , moveBoxAction <~ dragstartP
                                         ])

isEditing = any (.isEditing)

updateStateSelections box state =
  let updateBox = Box.step Box.Action.Selected in
  { state | boxes <- map updateBox state.boxes }

contains obj = any (\b -> b == obj)

step : Action -> Board -> Board
step action state =
  let updateBoxInState action box iterator = (if iterator.key == box.key then Box.step action iterator else iterator)
      updateSelectedBoxes action iterator = (if iterator.isSelected then Box.step action iterator else iterator)
      performActionOnAllBoxes action = (Box.step action)
      selectedBoxes = filter (.isSelected)
      deselectBoxes = map (\box -> { box | isSelected <- False }) in
    case action of
      NewBox i ->
        if | isEditing state.boxes -> state
           | True ->
            let newBox = makeBox i in
              Debug.log "newBox" { state | boxes <- newBox :: state.boxes }
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
      NoOp -> state

