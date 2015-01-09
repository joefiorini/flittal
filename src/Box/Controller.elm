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
import Geometry.Types as Geometry

type alias Model = Model.Model
type alias BoxKey = Model.BoxKey

type ResizeMode = ResizeUpAll
                | ResizeDownAll
                | ResizeUpNS
                | ResizeDownNS
                | ResizeUpEW
                | ResizeDownEW

type MoveType =
    Nudge
  | Push
  | Jump

type MoveDirection =
    Up
  | Down
  | Left
  | Right


type Update = Drop DragEvent
            | Editing Bool
            | EditingBox Model Bool
            | CancelEditingBox Model
            | CancelEditing
            | SetSelected Int
            | Dragging
            | UpdateBox Model String
            | UpdateColor Color
            | NoOp
            | Resize ResizeMode
            | Update String
            | Move MoveType MoveDirection

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
    , styleProperty "line-height" "2"
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

moveBox : Int -> MoveDirection -> Model -> Geometry.Point
moveBox amount direction box =
  let (x,y) = box.position
  in
     case direction of
       Up ->
         (x, y-amount)
       Down ->
         (x, y+amount)
       Left ->
         (x-amount, y)
       Right ->
         (x+amount, y)

moveBoxDrag : DragEvent -> Model -> Model
moveBoxDrag { id, isStart, isEnd, isDrop, startX, startY, endX, endY } box =
  let offsetX = (fst box.position) - startX
      offsetY = (snd box.position) - startY
      newX = endX + offsetX
      newY = endY + offsetY in
    { box | position <- (newX, newY) }

labelSelector : Model -> String
labelSelector box = "#box-" ++ toString box.key ++ "-label"

resize : ResizeMode -> Model -> (Geometry.Point, Geometry.Size)
resize mode box =
  let (width,height) = box.size
      (left,top) = box.position
      maxWidth = 280
      minWidth = 90
      maxHeight = 180
      minHeight = 40
  in
    case mode of
      ResizeUpAll ->
        if (width >= maxWidth) || (height >= maxHeight) then
          (box.position, box.size)
        else
          ( (left - 5, top - 5)
          , (10 + width, 10 + height)
          )
      ResizeDownAll ->
        if (width <= minWidth) || (height <= minHeight) then
          (box.position, box.size)
        else
          ( (left + 5, top + 5)
          , (width - 10, height - 10)
          )
      ResizeUpNS ->
        if height >= maxHeight then
           (box.position, box.size)
        else
          ( (left, top - 5)
          , (width, 10 + height)
          )
      ResizeDownNS ->
        if height <= minHeight then
          (box.position, box.size)
        else
          ( (left, top + 5)
          , (width, height - 10)
          )
      ResizeUpEW ->
        if width >= maxWidth then
           (box.position, box.size)
        else
          ( (left - 5, top)
          , (width + 10, height)
          )
      ResizeDownEW ->
        if width <= minWidth then
           (box.position, box.size)
        else
          ( (left + 5, top)
          , (width - 10, height)
          )


step : Update -> Model -> Model
step update box = case update of
  Drop event ->
    Debug.log "Moved a box" <| moveBoxDrag event box
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
  Resize mode ->
    let (position',size') = resize mode box
    in
       { box | size <- size', position <- position' }
  Move mode direction ->
    let moveBox' =
      case mode of
        Nudge ->
          moveBox 10
        Push ->
          moveBox 100
        Jump ->
          moveBox 300
    in
       { box | position <- moveBox' direction box }

  NoOp -> box
