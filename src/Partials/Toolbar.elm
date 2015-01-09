module Partials.Toolbar where

import Html (input, div, text, section, button, img)
import Html.Attributes (class, placeholder, src, width, type', readonly, required, title)
import Html.Events (onClick)
import LocalChannel as LC

type Update =
    ShareBoard
  | ClearBoard
  | NoOp

clearButton channel =
  div
    [ class "clear-board" ]
    [ button
      [ onClick (LC.send channel ClearBoard)
      , title "Clear the board"
      ]
      [ img
        [ src "/images/icon-clear.svg" ]
        [ ]
      ]
    ]

shareButton channel =
  div
    [ class "share" ]
    [ input
        [ class "share__url"
        , placeholder "Share this board"
        , onClick (LC.send channel ShareBoard)
        , type' "text"
        , readonly True
        ]
        [ ]
    , button
      [ class "button-pseudo" ]
      [ img
        [ src "/images/icon-share.svg"
        , width 25
        ]
        []
      ]
    ]

view channel share =
  section
    [class "l-container"]
    [ clearButton channel
    , shareButton share
    ]
