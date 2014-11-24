module Board.Action where

import Box.Controller as Box
import DomUtils (DragEvent, DropPort)

data Action = NoOp |
  RequestedAdd |
  NewBox Int |
  BoxAction Box.Action |
  EditBox String |
  Drop DragEvent


