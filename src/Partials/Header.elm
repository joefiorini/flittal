module Partials.Header where

import Html (header, nav, text, a, Html)
import Html.Attributes (href, class)
import DomUtils (linkTo)
import LocalChannel as LC
import Routes
import List

type RouteUpdate = SignUp
                 | SignIn
                 | Demo
                 | Home
                 | NoOp

navLinks : LC.LocalChannel Routes.RouteName -> List Html
navLinks channel =
  List.concat
    [ [ linkTo "Diagrammer" "#" (LC.send channel Routes.Root) ]
    , [ linkTo "Demo" "#" (LC.send channel Routes.Demo) ]
    ]

view channel =
  header
    []
    [ nav
        [ class "nav-bar" ]
        (navLinks channel)
    ]
