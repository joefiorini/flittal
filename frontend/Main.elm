module Main where
import Graphics.Element (Element)
import Html
import Html (Html, toElement, div)
import Html.Attributes (class)
import Board.Controller as Board
import Board.Action as BoardAction
import Board.Controller (checkFocus)
import DomUtils (DragEvent)
import Router
import Router.Types (RouteHandler(..))
import Router.Renderers (renderTopLevel)
import LocalChannel as LC
import Signal
import Signal (Signal, (<~))
import Keyboard
import Window

port drop : Signal DragEvent
port dragstart : Signal DragEvent
port dragend : Signal DragEvent

port focus : Signal String
port focus = checkFocus

type alias AppState =
  { currentBoard        : Board.Board
  }

main : Signal Element
main = Signal.map (\h -> toElement 900 600 <| container h) state

keyboardRequestAction = Signal.map convertKeyboardOperation Keyboard.lastPressed

startingState =
  { currentBoard = Board.startingState
  }


type Update = NoOp
            | BoardUpdate Board.Action

updateChannel : Signal.Channel Board.Action
updateChannel = Signal.channel BoardAction.NoOp

updates : Signal.Channel Update
updates = Signal.channel NoOp

userInput = Signal.mergeMany [drop, dragstart, dragend]

state : Signal.Signal AppState
state =
  Signal.foldp step startingState
    (Signal.mergeMany
      [ keyboardRequestAction
      , Signal.subscribe updates
      , convertDragOperation <~ Signal.mergeMany [drop, dragstart]
      ])

convertKeyboardOperation keyboardE =
  BoardUpdate <| Board.keyboardRequest keyboardE

convertDragOperation dragE =
  BoardUpdate <| Board.moveBoxAction dragE

step update state =
  case update of
    BoardUpdate u ->
      let updatedBoard = Board.step u state.currentBoard
      in
      { state | currentBoard <- updatedBoard }
    _ -> state

container : AppState -> Html.Html
container state =
  let boardChannel' = LC.create BoardUpdate updates
  in
    div [ class "container" ]
      [ Board.view boardChannel' state.currentBoard ]

