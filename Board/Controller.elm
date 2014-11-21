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
import Either (..)

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

eitherToAction = (either (\id -> NewBox id) (\id -> NoOp))
extractEither = (either (\id -> id) (\id -> id))

addBoxAction = eitherToAction <~ (foldp (\k i -> if | k == 65 -> Left ((extractEither i) + 1)
                                                    | True -> Right (extractEither i))
                                        (Left 0) Keyboard.lastPressed)

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


