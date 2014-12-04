module Box.View (draw) where

import Html (..)
import Html.Attributes (id, class, autofocus, style, property, boolProperty, type', value)
import Html.Events (on, keyCode, targetValue)

import Graphics.Input as Input

import Signal (Channel, send, channel)

import Debug

import Box.State (..)
import Board.Action as Board

import DomUtils (stopPropagation, styleProperty)

import Geometry.Types (toPxPoint)

onKeyDown handle boxKey = let checkKeyCode keyCode = (case keyCode of
                            13 -> Board.EditingBox boxKey False
                            27 -> Board.CancelEditingBox boxKey
                            _ -> Board.NoOp) in
                          on "keydown" keyCode (\v -> send handle <| checkKeyCode v)


draw : Channel Board.Action -> Box -> Html
draw handle box = div [style
    [ styleProperty "position" "absolute"
    , styleProperty "width" (fst <| toPxPoint box.size)
    , styleProperty "height" (snd <| toPxPoint box.size)
    , if box.selectedIndex > -1 && not box.isDragging then styleProperty "border" "dashed 2px green" else styleProperty "border" "solid 2px black"
    , styleProperty "left" (fst <| toPxPoint box.position)
    , styleProperty "top" (snd <| toPxPoint box.position)
    , styleProperty "text-align" "center"
    , styleProperty "line-height" (toString (toFloat <| snd box.size)  ++ "px")
    ]
    , autofocus True
    , boolProperty "draggable" True
    , id <| "box-" ++ (toString box.key)
    , on "input" targetValue (\v -> send handle (Board.UpdateBox box v))
    , class "box"
    , onKeyDown handle box.key
  ]
  [ if box.isEditing then labelField box.key box.label else text box.label ]

labelField : BoxKey -> String -> Html
labelField key label = let nullHandler v = send (channel ()) () in
  input [
    id <| "box-" ++ toString key ++ "-label"
  , type' "text"
  , value label
  , on "mousedown" stopPropagation nullHandler
  ] []
