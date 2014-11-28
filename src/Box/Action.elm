module Box.Action where

import Box.State (BoxKey, Box)

import DomUtils (DragEvent)

type Action = Move DragEvent
            | Editing Bool
            | CancelEditing
            | Selected
            | SetSelected Bool
            | Dragging
            | NoOp
            | Update String

