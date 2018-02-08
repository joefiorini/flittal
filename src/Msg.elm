module Msg exposing (..)

import Routes exposing (RouteName)
import Board.Msg
import Partials.Toolbar as Toolbar
import Navigation


type Msg
    = NoOp
    | BoardUpdate Board.Msg.Msg
    | ToolbarUpdate Toolbar.Msg
    | Undo
    | Redo
    | SerializeState
    | LoadedState String
    | UrlChange Navigation.Location
    | NewPage RouteName
