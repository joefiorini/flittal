module Board.Controller (renderBoard, Action(..)) where

import Keyboard
import Debug
import Graphics.Input as Input

import Board.View (draw)
import Board.State (..)

import Box.Controller as Box

import Html (Html)
import DomUtils (DragEvent, DropPort)

import String (split, toInt)


data Action = NoOp |
  Add |
  NewBox Int |
  OtherKeyboard Int |
  BoxAction Box.Action |
  Drop DragEvent
  -- DragStart DragEvent |
  -- DragEnd DragEvent

renderBoard : DropPort -> Signal Html
renderBoard p = lift render <| state p

render : Board -> Html
render board = draw (map Box.renderBox board.boxes)

keyboardActions : Signal Action
keyboardActions = foldp (\k a -> if k == 65 then Add else NoOp) NoOp Keyboard.lastPressed

actions : Input.Input Action
actions = Input.input NoOp

addBoxAction = foldp (\k a ->
  if (Debug.log "k" k) == 65 then
    case Debug.log "a" a of
      NewBox i -> Debug.log "NewBox" <| NewBox (i + 1)
      OtherKeyboard i -> NewBox (i + 1)
  else
    case a of
      NewBox i -> OtherKeyboard i
      OtherKeyboard i -> OtherKeyboard i)
  (OtherKeyboard 0) Keyboard.lastPressed

state : DropPort -> Signal Board
state dropPort = foldp step startingState (merges [
                                           addBoxAction
                                         , actions.signal
                                         , BoxAction <~ (Box.Move <~ dropPort)
                                         ])

step : Action -> Board -> Board
step action state =
  case Debug.log "action" action of
    NewBox i ->
      let newBox = makeBox i in
        Debug.log "newBox" { state | boxes <- newBox :: state.boxes }
    BoxAction (Box.Move event) ->
      let box = selectedBox event.id state.boxes in
        Debug.log "moved box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Move event) box }
    OtherKeyboard i -> state
    NoOp -> state


