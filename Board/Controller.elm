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
       if | event.isStart -> SelectBox key
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
  { state | boxes <- replaceBox state.boxes <| Box.step Box.Action.Selected box
           , selectedBoxes <- filter (.isSelected) state.boxes }

contains obj = any (\b -> b == obj)
step : Action -> Board -> Board
step action state =
  case action of
    NewBox i ->
      if | isEditing state.boxes -> state
         | True ->
          let newBox = makeBox i in
            Debug.log "newBox" { state | boxes <- newBox :: state.boxes }
    CancelEditingBox key ->
      let box = boxForKey key state.boxes
          updateBox = Box.step Box.Action.CancelEditing in
        Debug.log "Canceling edit" { state | boxes <- replaceBox state.boxes <| updateBox box }
    CancelEditingBoxes ->
      let updateBox = Box.step Box.Action.CancelEditing in
        { state | boxes <- map updateBox state.boxes }
    DeselectBoxes ->
      let state_ = step CancelEditingBoxes state in
        { state_ | boxes <- map (\box -> { box | isSelected <- False }) state_.boxes
                , selectedBoxes <- [] }
    SelectBoxMulti id ->
      let box = boxForKey id state.boxes in
        Debug.log "adding box to selection" updateStateSelections box state
    SelectBox id ->
      let box = boxForKey id state.boxes
          state_ = step DeselectBoxes state in
        if | box.isSelected -> state
           | otherwise ->
        Debug.log "selecting box" updateStateSelections box state_
    EditingBox id toggle ->
      let box = boxForKey id state.boxes in
        Debug.log "editing box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Editing toggle) box }
    UpdateBox box label ->
      { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Update label) box }
    MoveBox key event ->
      let selectedBoxes = filter (.isSelected) state.boxes
          updateBox box = (if contains box selectedBoxes then Box.step (Box.Action.Move event) box else box) in
        Debug.log "moved box" { state | boxes <- map updateBox state.boxes }
    NoOp -> state

