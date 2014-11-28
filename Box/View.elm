module Box.View (draw) where

import Html (..)
import Html.Tags (div, input)
import Html.Attributes (id, class, autofocus)
import Html.Events (onkeypress)

import Graphics.Input as Input

import Debug

import Box.State (..)
import Board.Action as Board

import DomUtils (stopPropagation)

import Geometry.Types (toPxPoint)

onKeyDown handle boxKey = let checkKeyCode ke = (case ke.keyCode of
                            13 -> Board.EditingBox boxKey False
                            27 -> Board.CancelEditingBox boxKey
                            _ -> Board.NoOp) in
                          on "keydown" getKeyboardEvent handle checkKeyCode


draw : Input.Handle Board.Action -> Box -> Html
draw handle box = div [style
    [ prop "position" "absolute"
    , prop "width" (fst <| toPxPoint box.size)
    , prop "height" (snd <| toPxPoint box.size)
    , if box.isSelected && not box.isDragging then prop "border" "dashed 2px green" else prop "border" "solid 2px black"
    , prop "left" (fst <| toPxPoint box.position)
    , prop "top" (snd <| toPxPoint box.position)
    , prop "text-align" "center"
    , prop "line-height" (show (toFloat <| snd box.size)  ++ "px")
    ]
    , autofocus True
    , toggle "draggable" True
    , id <| "box-" ++ (show box.key)
    , on "input" getValue handle (Board.UpdateBox box)
    , class "box"
    , onKeyDown handle box.key
  ]
  [ if box.isEditing then labelField box.key box.label else text box.label ]

labelField : BoxKey -> String -> Html
labelField key label = let nullHandler = (Input.input ()).handle in
  input [
    id <| "box-" ++ show key ++ "-label"
  , attr "type" "text"
  , attr "value" label
  , on "click" stopPropagation nullHandler (always ())
  ] []
