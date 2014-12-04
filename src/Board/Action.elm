module Board.Action where

import Box.Action
import Box.State (Box, BoxKey)
import DomUtils (DragEvent)

type Action = NoOp |
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
  ReconnectSelections |
  DraggingBox BoxKey |
  Drop DragEvent

