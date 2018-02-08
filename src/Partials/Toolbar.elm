module Partials.Toolbar exposing (..)

import Html exposing (input, div, text, section, button, img, Html)
import Html.Attributes exposing (class, placeholder, src, width, type_, readonly, required, title)
import Html.Events exposing (onClick)


type Msg
    = ShareBoard
    | ClearBoard
    | NoOp


clearButton : (Msg -> msg) -> Html msg
clearButton tx =
    div
        [ class "clear-board" ]
        [ button
            [ onClick <| tx ClearBoard
            , title "Clear the board"
            ]
            [ img
                [ src "/images/icon-clear.svg" ]
                []
            ]
        ]


shareButton : (Msg -> msg) -> Html msg
shareButton tx =
    div
        [ class "share" ]
        [ input
            [ class "share__url"
            , placeholder "Share this board"
            , onClick <| tx ShareBoard
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


view : (Msg -> msg) -> Html msg
view tx =
    section
        [ class "l-container" ]
        [ clearButton tx
        , shareButton tx
        ]
