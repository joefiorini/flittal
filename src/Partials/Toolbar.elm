module Partials.Toolbar exposing (..)

import Html exposing (input, div, text, section, button, img, Html)
import Html.Attributes exposing (class, placeholder, src, width, type_, readonly, required, title)
import Html.Events exposing (onClick)


type Msg
    = ShareBoard
    | ClearBoard
    | NoOp


clearButton : Html Msg
clearButton =
    div
        [ class "clear-board" ]
        [ button
            [ onClick ClearBoard
            , title "Clear the board"
            ]
            [ img
                [ src "/images/icon-clear.svg" ]
                []
            ]
        ]


shareButton : Html Msg
shareButton =
    div
        [ class "share" ]
        [ input
            [ class "share__url"
            , placeholder "Share this board"
            , onClick ShareBoard
            , type_ "text"
            , readonly True
            ]
            []
        , button
            [ class "button-pseudo" ]
            [ img
                [ src "/images/icon-share.svg"
                , width 25
                ]
                []
            ]
        ]


view : Html Msg
view =
    section
        [ class "l-container" ]
        [ clearButton
        , shareButton
        ]
