module Box.Action where

import Signal (Channel)

import Box.Model

import DomUtils (DragEvent)

type alias Box = Box.Model.Model

type Action = Move DragEvent
            | Editing Bool
            | CancelEditing
            | SetSelected Int
            | Dragging
            | NoOp
            | Update String

