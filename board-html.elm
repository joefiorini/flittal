module Canvas where

import Html (..)
import Html.Tags (div)
import Html.Attributes (id)
import Html.Events (onkeyup)

import Graphics.Input as Input

import Window
import Debug
import Keyboard
import String (split, toInt)

type BoxKey = Int

type Position b = { b | position: Point }
type Size b = { b | size: Point }
type Labelled b = { b | label: String }
type Keyed b = { b | key: Int }

type Point = (Int, Int)

type Box = Position (Size (Labelled (Keyed {})))

type CanvasState =
  { boxes: [Box]
  , selectedBox: Maybe Box
  , nextIdentifier: BoxKey
  }

type DragEvent =
  { id: String
  , startX: Int
  , endX: Int
  , startY: Int
  , endY: Int
  }

data Action = NoOp |
  Add |
  DragStart DragEvent |
  Drop DragEvent |
  DragEnd DragEvent

toPxPoint : Point -> (String, String)
toPxPoint point = (show (fst point) ++ "px", show (snd point) ++ "px")
mkBox : BoxKey -> Box
mkBox identifier =
  { position = (0,0)
  , size = (100, 50)
  , label = "New Box"
  , key = identifier
  }

port dragstart : Signal DragEvent
port dragend : Signal DragEvent
port drop : Signal DragEvent

onAkey : Input.Handle a -> a -> Attribute
onAkey handle value = on "keydown" (when (\k -> k.keyCode == 65) (Debug.log "keyboardEvent" getKeyboardEvent)) handle (always value)

boxWidget : Box -> Html
boxWidget box = div [style
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

view : CanvasState -> Html
view state = div [ style
      [ prop "position" "relative"
      , prop "width" "900px"
      , prop "height" "600px"
      , prop "border" "solid thin blue"
      , prop "overflow" "hidden"
      ]
      , id "container"
    ] <| Debug.log "widgets" <| map boxWidget state.boxes

main : Signal Element
main = lift2 scene state Window.dimensions

scene : CanvasState -> (Int, Int) -> Element
scene state (w,h) =
  toElement 900 600 <| view (Debug.watch "state" state)

actions : Input.Input Action
actions = Input.input NoOp

keyboardActions : Signal Action
keyboardActions = foldp (\k a -> if k == 65 then Add else NoOp) NoOp Keyboard.lastPressed

boxForId : String -> [Box] -> Box
boxForId id boxes = let keyM = toInt << last <| (split "-" id) in
  case keyM of
    Just key -> head (filter (\b -> b.key == key) boxes)
    Nothing -> Debug.crash "expected box key to be a number"

moveBox : DragEvent -> Box -> Box
moveBox { id, startX, startY, endX, endY } box =
  let offsetX = (fst box.position) - startX
      offsetY = (snd box.position) - startY
      newX = endX + offsetX
      newY = endY + offsetY in
    { box | position <- (newX, newY) }

performOnSelectedBox : (Box -> Box) -> CanvasState -> [Box]
performOnSelectedBox f state = case state.selectedBox of
  Just selectedBox ->
    map (\b -> if selectedBox == b then (f b) else b) state.boxes
  Nothing -> state.boxes

step : Action -> CanvasState -> CanvasState
step action state =
  case action of
    Add ->
      { state | boxes <- mkBox state.nextIdentifier :: state.boxes,
                nextIdentifier <- state.nextIdentifier + 1 }
    DragStart event ->
      Debug.log "DragStart" { state | selectedBox <- Just <| boxForId event.id state.boxes }
    DragEnd event ->
      Debug.log "DragEnd" { state | selectedBox <- Nothing }
    Drop event -> Debug.log "Drop" { state | boxes <- performOnSelectedBox (moveBox event) state }
    NoOp -> state

state : Signal CanvasState
state = foldp step startingState (merges [ keyboardActions
                                         , actions.signal
                                         , DragStart <~ dragstart
                                         , DragEnd <~ dragend
                                         , Drop <~ drop ])

startingState : CanvasState
startingState =
  { boxes = []
  , selectedBox = Nothing
  , nextIdentifier = 1
  }
