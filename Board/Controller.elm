module Board.Controller (renderBoard) where

import Keyboard
import Debug
import Graphics.Input as Input

import Board.View (draw)
import Board.State (..)

import Board.Action (..)
import Box.Controller as Box

import DomUtils (DragEvent, DropPort, extractBoxId)

import Html (Html)

import String (split, toInt)
import Either (..)

import Native.Custom.Html

import Box.Action

renderBoard : DropPort -> Signal Html
renderBoard p = lift render <| state p

render : Board -> Html
render board = draw actions.handle (map (Box.renderBox actions.handle) board.boxes)

actions : Input.Input Action
actions = Input.input NoOp

eitherToAction = (either (\id -> NewBox id) (\id -> NoOp))
extractEither = (either (\id -> id) (\id -> id))

addBoxAction = eitherToAction <~ (foldp (\k i -> if | k == 65 -> Left ((extractEither i) + 1)
                                                    | True -> Right (extractEither i))
                                        (Left 0) Keyboard.lastPressed)

moveBoxAction : DragEvent -> Action
moveBoxAction event = let boxKeyM = extractBoxId event.id in
    case boxKeyM of
      Just key -> MoveBox key event
      Nothing -> NoOp

state : DropPort -> Signal Board
state dropPort = foldp step startingState (merges [
                                           addBoxAction
                                         , actions.signal
                                         , lift moveBoxAction dropPort
                                         ])

isEditing = any (.isEditing)

updateStateSelections box state =
  { state | boxes <- replaceBox state.boxes <| Box.step Box.Action.Selected box
           , selectedBoxes <- filter (.isSelected) state.boxes }

step : Action -> Board -> Board
step action state =
  case action of
    NewBox i ->
      if | isEditing state.boxes -> state
         | True ->
          let newBox = makeBox i in
            Debug.log "newBox" { state | boxes <- newBox :: state.boxes }
    DeselectBoxes ->
      { state | boxes <- map (\box -> { box | isSelected <- False }) state.boxes
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
      let box = boxForKey key state.boxes in
        Debug.log "moved box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Move event) box }
    NoOp -> state

