module Partials.Header where

import Html (header, nav, text, a, Html)
import Html.Attributes (href, class)
import DomUtils (linkTo)
import LocalChannel as LC
import Routes
import List

navLinks : LC.LocalChannel Routes.RouteName -> List Html
navLinks channel =
  List.concat
    [ [ linkTo "Diagrammer" "#" (LC.send channel Routes.Root) ]
    , [ linkTo "About" "#" (LC.send channel Routes.About) ]
    ]

view channel =
  header
    []
    [ nav
        [ class "nav-bar" ]
        (navLinks channel)
    ]
