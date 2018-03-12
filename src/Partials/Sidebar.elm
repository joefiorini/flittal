module Partials.Sidebar exposing (..)

import Html exposing (aside, Html)
import Html.Attributes exposing (class)
import DomUtils exposing (linkTo)
import Routes
import Msg exposing (Msg)


view : Html Msg -> Html Msg
view child =
    aside
        [ class "sidebar" ]
        [ linkTo "x" "/"
        , child
        ]
