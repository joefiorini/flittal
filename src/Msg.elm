module Msg exposing (..)

import Board.Msg
import Navigation
import Keyboard.Combo
import Window exposing (Size)


type Msg
    = NoOp
    | BoardUpdate Board.Msg.Msg
    | Undo
    | Redo
    | LoadedState String
    | UrlChange Navigation.Location
    | NewPage String
    | KeyCombo Keyboard.Combo.Msg
    | ToggleHelp
    | ResizeWindow Size
    | ShareBoard
    | ClearBoard
