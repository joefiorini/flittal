module Partials.Header exposing (..)

import Html exposing (header, nav, text, a, Html, img, div)
import Html.Attributes exposing (href, class, src)
import DomUtils exposing (linkTo)
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
                [ linkTo "About" "#" (LC.send channel Routes.About)
                , linkTo "Colophon" "#" (LC.send channel Routes.Colophon)
                , linkTo "What's New" "#" (LC.send channel Routes.Releases)
                , linkTo "Help" "#" (LC.send channel Routes.Help)
                ]
          ]
        ]


view channel =
    header
        [ class "l-container" ]
        [ nav
            [ class "nav-bar header__nav-bar" ]
            (navLinks channel)
        ]
