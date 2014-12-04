module Box.Controller (moveBox, step, renderBox, State) where

import DomUtils (DragEvent)

import Box.State (..)
import Box.View (draw)
import Box.Action (..)
import Board.Action as Board

import Html (Html)

import Signal (Channel)

import Graphics.Input as Input

import Debug

type alias State = Box

renderBox : Channel Board.Action -> Box -> Html
renderBox handle box = draw handle box

moveBox : DragEvent -> Box -> Box
moveBox { id, isStart, isEnd, isDrop, startX, startY, endX, endY } box =
  let offsetX = (fst box.position) - startX
      offsetY = (snd box.position) - startY
      newX = endX + offsetX
      newY = endY + offsetY in
    { box | position <- (newX, newY) }

step : Action -> Box -> Box
step action box = case action of
  Move event ->
    Debug.log "Moved a box" <| moveBox event box
  SetSelected index ->
    { box | selectedIndex <- index }
  CancelEditing ->
    { box | label <- box.originalLabel
          , isEditing <- False }
  Editing toggle ->
       { box | isEditing <- toggle, originalLabel <- box.label }
  Update newLabel ->
    { box | label <- newLabel }
  Dragging ->
    { box | isDragging <- if box.isDragging then False else True }
  NoOp -> box
