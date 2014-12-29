module Partials.Header where

import Html (header, nav, text, a, Html)
import Html.Attributes (href, class)
import DomUtils (linkTo)
import LocalChannel as LC
import List

type Update = SignUp
            | SignIn
            | Demo
            | Home
            | NoOp

authLinks : LC.LocalChannel Update -> List Html
authLinks channel =
  [ linkTo "Sign Up" "#" (LC.send channel SignUp)
  , linkTo "Sign In" "#" (LC.send channel SignIn)
  ]

navLinks : LC.LocalChannel Update -> List Html
navLinks channel =
  List.concat
    [ [ linkTo "Diagrammer" "#" (LC.send channel Home) ]
    , [ linkTo "Demo" "#" (LC.send channel Demo) ]
    , authLinks channel
    ]

view channel =
  header
    []
    [ nav
        [ class "nav-bar" ]
        (navLinks channel)
    ]
