module Box.View (draw) where

import Html (..)
import Html.Tags (div, input)
import Html.Attributes (id, class)
import Html.Events (onkeypress)

import Graphics.Input as Input

import Debug

import Box.State (..)
import Board.Action as Board

toPxPoint : Point -> (String, String)
toPxPoint point = (show (fst point) ++ "px", show (snd point) ++ "px")

onEnter handle value = on "keydown" (when (\k -> k.keyCode == 13) getKeyboardEvent) handle (always value)

draw : Input.Handle Board.Action -> Box -> Html
draw handle box = div [style
    [ prop "position" "absolute"
    , prop "width" (fst <| toPxPoint box.size)
    , prop "height" (snd <| toPxPoint box.size)
    , if box.isSelected then prop "border" "dashed thin green" else prop "border" "solid thin black"
    , prop "left" (fst <| toPxPoint box.position)
    , prop "top" (snd <| toPxPoint box.position)
    ]
    , toggle "draggable" True
    , id <| "box-" ++ (show box.key)
    , on "input" (Debug.log "value" getValue) handle (Board.UpdateBox box)
    , onEnter handle (Board.EditingBox box.key False)
  ]
  [ if box.isEditing then labelField box.label else text box.label ]

labelField : String -> Html
labelField label = input [
    attr "type" "text"
  , attr "value" label
  ] []
