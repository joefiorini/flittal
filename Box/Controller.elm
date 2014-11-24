module Box.Controller (moveBox, step, renderBox) where

import DomUtils (DragEvent)

import Box.State (..)
import Box.View (draw)
import Box.Action (..)
import Board.Action as Board

import Html (Html)

import Graphics.Input as Input

renderBox : Input.Handle Board.Action -> Box -> Html
renderBox handle box = draw handle box

moveBox : DragEvent -> Box -> Box
moveBox { id, startX, startY, endX, endY } box =
  let offsetX = (fst box.position) - startX
      offsetY = (snd box.position) - startY
      newX = endX + offsetX
      newY = endY + offsetY in
    { box | position <- (newX, newY) }

step : Action -> Box -> Box
step action box = case action of
  Move event ->
    moveBox event box
  Selected ->
    { box | isSelected <- if box.isSelected then False else True }
  Editing toggle ->
    { box | isEditing <- toggle }
  Update newLabel ->
    { box | label <- newLabel }
  NoOp -> box
