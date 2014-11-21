module Board.View (draw) where

import Debug
import Html (..)
import Html.Tags (div)
import Html.Attributes (id)

draw : [Html] -> Html
draw widgets = Debug.log "draw" <| div [ style
      [ prop "position" "relative"
      , prop "width" "900px"
      , prop "height" "600px"
      , prop "border" "solid thin blue"
      , prop "overflow" "hidden"
      ]
      , id "container"
    ] widgets

