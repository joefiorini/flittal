module Box.Action where

import Box.State (BoxKey, Box)

import DomUtils (DragEvent)

type Action = Move DragEvent
            | Editing Bool
            | CancelEditing
            | SetSelected Int
            | Dragging
            | NoOp
            | Update String

