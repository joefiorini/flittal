module Board.Action where

import Box.Action
import Box.State (Box)
import DomUtils (DragEvent, DropPort)

data Action = NoOp |
  RequestedAdd |
  UpdateBox Box String |
  NewBox Int |
  MoveBox Int DragEvent |
  DeselectBoxes |
  EditingBox Int Bool |
  SelectBox Int |
  SelectBoxMulti Int |
  Drop DragEvent

