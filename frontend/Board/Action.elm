module Board.Action where

import Box.Model
import DomUtils (DragEvent)

type alias Box = Box.Model.Model
type alias BoxKey = Box.Model.BoxKey

type Action = NoOp |
  RequestedAdd |
  UpdateBox Box String |
  NewBox |
  MoveBox BoxKey DragEvent |
  DeselectBoxes |
  EditingBox BoxKey Bool |
  EditingSelectedBox Bool |
  SelectBox BoxKey |
  SelectBoxMulti BoxKey |
  CancelEditingBox BoxKey |
  ConnectSelections |
  ReconnectSelections |
  DeleteSelections |
  DraggingBox BoxKey |
  Drop DragEvent

