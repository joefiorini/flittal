module Box.View (draw) where

import Html (..)
import Html.Tags (div, input)
import Html.Attributes (id)
import Html.Events (onkeypress)

import Graphics.Input as Input

import Debug

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
  [ if box.isEditing then labelField box.label else text box.label ]

labelField : String -> Html
labelField label = input [
    attr "type" "text"
  , attr "value" label
  ] []
