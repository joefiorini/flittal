module Box.View (draw) where

import Html (..)
import Html.Tags (div)
import Html.Attributes (id)

import Box.State (..)

toPxPoint : Point -> (String, String)
toPxPoint point = (show (fst point) ++ "px", show (snd point) ++ "px")

draw : Box -> Html
draw box = div [style
    [ prop "position" "absolute"
    , prop "width" (fst <| toPxPoint box.size)
    , prop "height" (snd <| toPxPoint box.size)
    , prop "border" "solid thin black"
    , prop "left" (fst <| toPxPoint box.position)
    , prop "top" (snd <| toPxPoint box.position)
    ]
    , attr "draggable" "true"
    , id <| "box-" ++ (show box.key)
  ]
  [ text box.label ]
