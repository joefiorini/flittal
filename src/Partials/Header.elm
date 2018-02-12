module Partials.Header exposing (..)

import DomUtils exposing (linkTo)
import Html exposing (Html, a, div, header, img, nav, text)
import Html.Attributes exposing (class, href, src)
import List
import Msg exposing (Msg)
import Routes exposing (RouteName)


navLinks : List (Html Msg)
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


view : Html Msg
view =
    header
        [ class "l-container" ]
        [ nav
            [ class "nav-bar header__nav-bar" ]
            navLinks
        ]
