module Board.Msg exposing (..)

import Box.Msg
import Box.Types exposing (BoxKey, MoveDirection, MoveType, ResizeMode)
import Dom.Types exposing (..)
import Style exposing (Color)


type Msg
    = NoOp
    | BoxAction Box.Msg.Msg
    | ClearBoard
    | NewBox
    | MoveBox MoveType MoveDirection
    | DeselectBoxes
    | EditingBox BoxKey Bool
    | EditingSelectedBox Bool
    | SelectBox BoxKey
    | SelectBoxMulti BoxKey
    | ConnectSelections
    | ReconnectSelections
    | DisconnectSelections
    | DeleteSelections
    | SelectNextBox
    | SelectPreviousBox
    | DraggingBox BoxKey
    | UpdateBoxColor Color
    | ResizeBox ResizeMode
    | Drop BoxKey DragEvent
