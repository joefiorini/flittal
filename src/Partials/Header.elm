module Partials.Header where

import Html (header, nav, text, a, Html, img, div)
import Html.Attributes (href, class, src)
import DomUtils (linkTo)
import LocalChannel as LC
import Routes
import List

navLinks : LC.LocalChannel Routes.RouteName -> List Html
navLinks channel =
  List.concat
    [ [ div
          [ class "nav-bar__logo-wrapper" ]
          [ a
            [ href "/", class "logo nav-bar__logo" ]
            [ text " " ]
          ]
      ]
    , [ div
        [ class "nav-bar__links" ]
        [
           linkTo "About" "#" (LC.send channel Routes.About) 
        ,  linkTo "Colophon" "#" (LC.send channel Routes.Colophon) 
        ]
      ]
    ]

view channel =
  header
    [class "l-container"]
    [ nav
        [ class "nav-bar header__nav-bar" ]
        (navLinks channel)
    ]
