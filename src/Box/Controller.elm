module Box.Controller where

import DomUtils
import DomUtils (DragEvent, stopPropagation, styleProperty, class')
import Geometry.Types (toPxPoint)

import Html (..)
import Html.Attributes (id, class, autofocus, style, property, boolProperty, type', value)
import Html.Events (on, keyCode, onKeyPress, targetValue)


import Signal
import Signal (Channel)

import Graphics.Input as Input

import Box.Model as Model
import Debug

import LocalChannel as LC

import Json.Decode ((:=))
import Json.Decode as Json

import Style.Color (..)

type alias Model = Model.Model
type alias BoxKey = Model.BoxKey


type Update = Move DragEvent
            | Editing Bool
            | EditingBox Model Bool
            | CancelEditingBox Model
            | CancelEditing
            | SetSelected Int
            | Dragging
            | UpdateBox Model String
            | UpdateColor Color
            | NoOp
            | Update String

filterKey = Model.filterKey
isSelected = Model.isSelected

view : LC.LocalChannel Update -> Model -> Html
view channel box =
  div [style
    [ styleProperty "position" "absolute"
    , styleProperty "width" (fst <| toPxPoint box.size)
    , styleProperty "height" (snd <| toPxPoint box.size)
    , if box.selectedIndex > -1 && not box.isDragging then styleProperty "border" "dashed 2px" else styleProperty "border" "solid 2px"
    , styleProperty "left" (fst <| toPxPoint box.position)
    , styleProperty "top" (snd <| toPxPoint box.position)
    , styleProperty "text-align" "center"
    , styleProperty "line-height" (toString (toFloat <| snd box.size)  ++ "px")
    ]
    , autofocus True
    , boolProperty "draggable" True
    , id <| "box-" ++ (toString box.key)
    , on "input" targetValue (\v -> LC.send channel (UpdateBox box v))
    , class' <| boxClasses box
    , onKeyDown channel box
  ]
  [ if box.isEditing then labelField channel box box.label else text box.label ]

boxClassForColor color =
  case color of
    Dark1 -> "box--dark1"
    Dark2 -> "box--dark2"
    Dark3 -> "box--dark3"
    Dark4 -> "box--dark4"
    Light1 -> "box--light1"
    Light2 -> "box--light2"
    Light3 -> "box--light3"
    Light4 -> "box--light4"
    Black -> "box--black"
    White -> "box--white"

boxClasses box =
  [ "box"
  , boxClassForColor box.style.color
  ]

entersEditMode update =
  case update of
    EditingBox _ toggle ->
      toggle
    CancelEditingBox _ ->
      False
    CancelEditing ->
      False
    otherwise ->
      False

encode = Model.encode

onKeyDown channel box = let checkKeyCode keyCode = (case keyCode of
                            -- TODO: Kill at least the 13 in this one
                            13 -> EditingBox box False
                            27 -> CancelEditingBox box
                            _ -> NoOp) in
                          on "keydown" keyCode (\v -> Debug.log "box:keydown" <| LC.send channel <| checkKeyCode v)

keyCodeAndValue : Json.Decoder (Int, String)
keyCodeAndValue =
  Json.object2 (,)
    ("keyCode" := Json.int)
    (Json.at ["target", "value"] Json.string)

extractLabelUpdate box (keyCode, value) =
  if | keyCode == 13 ->
    CancelEditingBox box
     | otherwise ->
    Debug.log "UpdateBox" <| UpdateBox box value

labelField : LC.LocalChannel Update -> Model -> String -> Html
labelField channel box label =
  let nullHandler v = LC.send channel NoOp
  in
    input
      [ id <| "box-" ++ toString box.key ++ "-label"
      , type' "text"
      , value label
      -- , on "input" keyCodeAndValue (\a -> Debug.log "input:keydown" LC.send channel <| extractLabelUpdate box a)
      , on "mousedown" stopPropagation nullHandler
      ] []

moveBox : DragEvent -> Model -> Model
moveBox { id, isStart, isEnd, isDrop, startX, startY, endX, endY } box =
  let offsetX = (fst box.position) - startX
      offsetY = (snd box.position) - startY
      newX = endX + offsetX
      newY = endY + offsetY in
    { box | position <- (newX, newY) }

labelSelector : Model -> String
labelSelector box = "#box-" ++ toString box.key ++ "-label"

step : Update -> Model -> Model
step update box = case update of
  Move event ->
    Debug.log "Moved a box" <| moveBox event box
  SetSelected index ->
    { box | selectedIndex <- index }
  CancelEditing ->
    { box | label <- box.originalLabel
          , isEditing <- False }
  Editing toggle ->
       let focusedBox = Debug.log "focusing box" <| DomUtils.setFocus (labelSelector box) box in
         { focusedBox | isEditing <- toggle, originalLabel <- box.label }
  Update newLabel ->
    { box | label <- Debug.log "got new label" newLabel }
  Dragging ->
    { box | isDragging <- if box.isDragging then False else True }
  UpdateColor color ->
    let style = box.style
        style' = { style | color <- color }
    in
       Debug.log "updated color" { box | style <- style' }
  NoOp -> box
