module Partials.Header exposing (..)

import Html exposing (header, nav, text, a, Html, img, div)
import Html.Attributes exposing (href, class, src)
import DomUtils exposing (linkTo)
import Routes exposing (RouteName)
import List


navLinks : List (Html RouteName)
navLinks =
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
                [ linkTo "About" "#" Routes.About
                , linkTo "Colophon" "#" Routes.Colophon
                , linkTo "What's New" "#" Routes.Releases
                , linkTo "Help" "#" Routes.Help
                ]
          ]
        ]


view channel =
    header
        [ class "l-container" ]
        [ nav
            [ class "nav-bar header__nav-bar" ]
            navLinks
        ]
