module Box.Types exposing (..)

import Geometry.Types exposing (Geometric)
import Style


type ResizeMode
    = ResizeUpAll
    | ResizeDownAll
    | ResizeUpNS
    | ResizeDownNS
    | ResizeUpEW
    | ResizeDownEW


type MoveType
    = Nudge
    | Push
    | Jump


type MoveDirection
    = Up
    | Down
    | Left
    | Right


type alias BoxKey =
    Int


type alias Model =
    Geometric
        { key : BoxKey
        , label : String
        , originalLabel : String
        , isEditing : Bool
        , isDragging : Bool
        , selectedIndex : Int
        , style : Style.Model
        }
