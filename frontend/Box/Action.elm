module Box.Action where

import Signal (Channel)

import Box.Model

import DomUtils (DragEvent)

type alias Box = Box.Model.Model

type Action = Move DragEvent
            | Editing Bool
            | EditingBox Int Bool
            | CancelEditingBox Int
            | CancelEditing
            | SetSelected Int
            | Dragging
            | UpdateBox Int String
            | NoOp
            | Update String

