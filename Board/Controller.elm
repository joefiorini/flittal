module Board.Controller (renderBoard) where

import Keyboard
import Debug
import Graphics.Input as Input

import Board.View (draw)
import Board.State (..)

import Board.Action (..)
import Box.Controller as Box

import DomUtils (DragEvent, DropPort)

import Html (Html)

import String (split, toInt)
import Either (..)

import Native.Custom.Html

renderBoard : DropPort -> Signal Html
renderBoard p = lift render <| state p

render : Board -> Html
render board = let editBoxAction id = EditBox id in
                draw actions.handle (map Box.renderBox board.boxes)

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
  case action of
    NewBox i ->
      let newBox = makeBox i in
        Debug.log "newBox" { state | boxes <- newBox :: state.boxes }
    EditBox id ->
      let box = selectedBox id state.boxes in
        Debug.log "editing box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Edit) box }
    BoxAction (Box.Move event) ->
      let box = selectedBox event.id state.boxes in
        Debug.log "moved box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Move event) box }
    NoOp -> state

