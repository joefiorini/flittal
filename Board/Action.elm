module Board.Action where

import Box.Action
import Box.State (Box, BoxKey)
import DomUtils (DragEvent)

data Action = NoOp |
  RequestedAdd |
  UpdateBox Box String |
  NewBox BoxKey |
  MoveBox BoxKey DragEvent |
  DeselectBoxes |
  EditingBox BoxKey Bool |
  SelectBox BoxKey |
  SelectBoxMulti BoxKey |
  CancelEditingBox BoxKey |
  DraggingBox BoxKey |
  Drop DragEvent

