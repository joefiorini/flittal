module Board.Action where

import Box.Action
import Board.State (BoardMode)
import Box.State (Box, BoxKey)
import DomUtils (DragEvent)

data Action = NoOp |
  RequestedAdd |
  UpdateBox Box String |
  NewBox |
  MoveBox BoxKey DragEvent |
  DeselectBoxes |
  EditingBox BoxKey Bool |
  SelectBox BoxKey |
  SelectBoxMulti BoxKey |
  CancelEditingBox BoxKey |
  ConnectSelections |
  DraggingBox BoxKey |
  ToggleMode BoardMode |
  Drop DragEvent

