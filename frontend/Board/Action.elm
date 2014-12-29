module Board.Action where

import Box.Model
import Box.Action
import DomUtils (DragEvent)

type alias Box = Box.Model.Model
type alias BoxKey = Box.Model.BoxKey

type Action = NoOp |
  BoxAction Box.Action.Action |
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

