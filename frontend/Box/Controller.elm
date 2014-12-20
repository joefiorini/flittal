module Box.Controller (moveBox, step, renderBox, Box, BoxKey) where

import DomUtils
import DomUtils (DragEvent)

import Box.Model
import Box.Action as Action
import Box.View (draw)
import Board.Action as Board

import Html (Html)

import Signal
import Signal (Channel)

import Graphics.Input as Input

import Debug

type alias Box = Box.Model.Model
type alias BoxKey = Box.Model.BoxKey

renderBox : Channel Board.Action -> Box -> Html
renderBox handle box = draw handle box

moveBox : DragEvent -> Box -> Box
moveBox { id, isStart, isEnd, isDrop, startX, startY, endX, endY } box =
  let offsetX = (fst box.position) - startX
      offsetY = (snd box.position) - startY
      newX = endX + offsetX
      newY = endY + offsetY in
    { box | position <- (newX, newY) }

labelSelector : Box -> String
labelSelector box = "#box-" ++ toString box.key ++ "-label"

step : Action.Action -> Box -> Box
step action box = case action of
  Action.Move event ->
    Debug.log "Moved a box" <| moveBox event box
  Action.SetSelected index ->
    { box | selectedIndex <- index }
  Action.CancelEditing ->
    { box | label <- box.originalLabel
          , isEditing <- False }
  Action.Editing toggle ->
       let focusedBox = Debug.log "focusing box" <| DomUtils.setFocus (labelSelector box) box in
         { focusedBox | isEditing <- toggle, originalLabel <- box.label }
  Action.Update newLabel ->
    { box | label <- newLabel }
  Action.Dragging ->
    { box | isDragging <- if box.isDragging then False else True }
  Action.NoOp -> box
