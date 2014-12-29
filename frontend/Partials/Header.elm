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

authLinks : LC.LocalChannel Routes.RouteName -> List Html
authLinks channel =
  [ linkTo "Sign Up" "#" (LC.send channel Routes.SignUp)
  , linkTo "Sign In" "#" (LC.send channel Routes.SignIn)
  ]

navLinks : LC.LocalChannel Routes.RouteName -> List Html
navLinks channel =
  List.concat
    [ [ linkTo "Diagrammer" "#" (LC.send channel Routes.Root) ]
    , [ linkTo "Demo" "#" (LC.send channel Routes.Demo) ]
    , authLinks channel
    ]

view channel =
  header
    []
    [ nav
        [ class "nav-bar" ]
        (navLinks channel)
    ]
