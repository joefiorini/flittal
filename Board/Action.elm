module Board.Action where

import Box.Action
import Box.State (Box, BoxKey)
import DomUtils (DragEvent, DropPort)

data Action = NoOp |
  RequestedAdd |
  UpdateBox Box String |
  NewBox BoxKey |
  MoveBox BoxKey DragEvent |
  DeselectBoxes |
  CancelEditingBoxes |
  EditingBox BoxKey Bool |
  SelectBox BoxKey |
  SelectBoxMulti BoxKey |
  CancelEditingBox BoxKey |
  Drop DragEvent

