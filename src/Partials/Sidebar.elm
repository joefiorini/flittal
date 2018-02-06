module Partials.Sidebar exposing (..)

import Html exposing (aside)
import Html.Attributes exposing (class)
import DomUtils exposing (linkTo)
import Routes


view child channel =
    aside
        [ class "sidebar" ]
        [ linkTo "x" "#" (LC.send channel Routes.Root)
        , child
        ]
