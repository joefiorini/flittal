module Partials.Sidebar where

import Signal

import LocalChannel as LC
import Html (aside)
import Html.Attributes (class)

import DomUtils (linkTo)
import Routes

view child channel =
  aside
    [ class "sidebar" ]
    [ linkTo "x" "#" (LC.send channel Routes.Root)
    , child
    ]
