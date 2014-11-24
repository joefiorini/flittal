module Box.Action where

import Box.State (BoxKey, Box)

import DomUtils (DragEvent)

data Action = Move DragEvent
            | Editing Bool
            | NoOp
            | Update String

