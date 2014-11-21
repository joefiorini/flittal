module Box.Controller (moveBox, step, Action(..), renderBox) where

import DomUtils (DragEvent)

import Box.State (..)
import Box.View (draw)

import Html (Html)

data Action = Move DragEvent
            | NoOp

renderBox : Box -> Html
renderBox box = draw box


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
  NoOp -> box
