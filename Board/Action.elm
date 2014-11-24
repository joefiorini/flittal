module Board.Action where

import Box.Action
import Box.State (Box)
import DomUtils (DragEvent, DropPort)

data Action = NoOp |
  RequestedAdd |
  UpdateBox Box String |
  NewBox Int |
  MoveBox Int DragEvent |
  EditingBox Int Bool |
  Drop DragEvent

