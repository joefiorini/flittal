module Msg exposing (..)

import Board.Msg
import Partials.Toolbar as Toolbar
import Navigation
import Keyboard.Combo


type Msg
    = NoOp
    | BoardUpdate Board.Msg.Msg
    | ToolbarUpdate Toolbar.Msg
    | Undo
    | Redo
    | SerializeState
    | LoadedState String
    | UrlChange Navigation.Location
    | NewPage String
    | KeyCombo Keyboard.Combo.Msg
    | ToggleHelp
