module Canvas where

import Html (..)
import Html.Tags (div)
import Html.Events (onkeyup)

import Graphics.Input as Input

import Window
import Debug
import Keyboard

type Position b = { b | position: (Int, Int) }
type Size b = { b | size: (Int, Int) }
type Labelled b = { b | label: String }

type Box = Position (Size (Labelled {}))

type CanvasState =
  { boxes: [Box]
  }

data Action = NoOp | Add

mkBox =
  { position = (0,0)
  , size = (100, 50)
  , label = "New Box"
  }

onAkey : Input.Handle a -> a -> Attribute
onAkey handle value = on "keydown" (when (\k -> k.keyCode == 65) (Debug.log "keyboardEvent" getKeyboardEvent)) handle (always value)

boxWidget : Box -> Html
boxWidget box = div [style
    [ prop "position" "absolute"
    , prop "width" (show (fst box.size) ++ "px")
    , prop "height" (show  (snd box.size) ++ "px")
    , prop "border" "solid thin black"
    ]
  ]
  [ text box.label ]

view : CanvasState -> Html
view state = div [ style
      [ prop "position" "relative"
      , prop "width" "900px"
      , prop "height" "600px"
      , prop "border" "solid thin black"
      ]
    ] <| map boxWidget state.boxes

main : Signal Element
main = lift2 scene state Window.dimensions

scene : CanvasState -> (Int, Int) -> Element
scene state (w,h) =
  container w h middle (toElement 600 600 <| view state)

actions : Input.Input Action
actions = Input.input NoOp

keyboardActions : Signal Action
keyboardActions = foldp (\k a -> if k == 65 then Add else NoOp) NoOp Keyboard.lastPressed

step : Action -> CanvasState -> CanvasState
step action state =
  case action of
    Add ->
      Debug.watch "state" { state | boxes <- mkBox :: state.boxes  }
    NoOp -> state

state : Signal CanvasState
state = foldp step startingState keyboardActions

startingState : CanvasState
startingState =
  { boxes = []
  }
