module Box.Msg exposing (..)

import Box.Types exposing (MoveDirection, MoveType, ResizeMode, Model)
import Dom.Types exposing (DragEvent)
import Style


type Msg
    = Drop DragEvent
    | Editing Bool
    | EditingBox Model Bool
    | CancelEditingBox Model
    | CancelEditing
    | SetSelected Int
    | Dragging
    | UpdateBox Model String
    | UpdateColor Style.Color
    | NoOp
    | Resize ResizeMode
    | Update String
    | Move MoveType MoveDirection
