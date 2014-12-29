module Main where
import Graphics.Element (Element)
import Html
import Html (Html, toElement, div, main', body, text)
import Html.Attributes (class)
import Board.Controller as Board
import Board.Controller (checkFocus)
import DomUtils (DragEvent)
import Mousetrap
import LocalChannel as LC
import Partials.Header as Header
import Partials.Footer as Footer
import Routes
import Routes (Route)
import Signal
import Signal (Signal, (<~))
import Keyboard
import Window
import Debug

port drop : Signal DragEvent
port dragstart : Signal DragEvent
port dragend : Signal DragEvent

port focus : Signal String
port focus = checkFocus

port globalKeyDown : Signal Int

type alias AppState =
  { currentBoard        : Board.Board
  }

main : Signal Element
main = Signal.map2 (\h r -> toElement 900 1200 <| container h r) state routeHandler

keyboardRequestAction = Signal.map convertKeyboardOperation (Signal.dropWhen inEditingMode 0 Keyboard.lastPressed)

globalKeyboardShortcuts : String -> Update
globalKeyboardShortcuts keyCommand =
  case keyCommand of
    "a"     -> BoardUpdate Board.NewBox
    "c"     -> BoardUpdate Board.ConnectSelections
    "d"     -> BoardUpdate Board.DeleteSelections
    "enter" -> BoardUpdate (Board.EditingSelectedBox True)
    _       -> NoOp


startingState =
  { currentBoard = Board.startingState
  }

routesMap routeName =
  case routeName of
    Routes.Root -> ("/", Routes.Root)
    Routes.SignUp -> ("/register", Routes.SignUp)
    Routes.SignIn -> ("/sign-in", Routes.SignIn)
    Routes.Demo -> ("/demo", Routes.Demo)

port transitionToRoute : Signal Routes.Url
port transitionToRoute =
  Routes.sendToPort routeHandler

routeHandler =
  Routes.map routesMap
    <| Signal.subscribe routeChannel

type RouteUpdate = Default
                 | NavigationRouteUpdate Header.RouteUpdate

type Update = NoOp
            | BoardUpdate Board.Update

updates : Signal.Channel Update
updates = Signal.channel NoOp

routeChannel : Signal.Channel Routes.RouteName
routeChannel = Signal.channel Routes.Root

userInput = Signal.mergeMany [drop, dragstart, dragend]

state : Signal.Signal AppState
state =
  Signal.foldp step startingState
    (Signal.mergeMany
      [ Signal.subscribe updates
      , Signal.map globalKeyboardShortcuts Mousetrap.keydown
      , convertDragOperation <~ Signal.mergeMany [drop, dragstart]
      ])

convertKeyboardOperation keyboardE =
  Debug.log "main:keydown" <| BoardUpdate <| Board.keyboardRequest keyboardE

entersEditMode update =
  case update of
    BoardUpdate a ->
      Board.entersEditMode a
    otherwise ->
      False

inEditingMode : Signal Bool
inEditingMode = Signal.map entersEditMode (Signal.subscribe updates)

convertDragOperation dragE =
  BoardUpdate <| Board.moveBoxAction dragE

step update state =
  case Debug.log "update" update of
    BoardUpdate u ->
      let updatedBoard = Board.step u state.currentBoard
      in
      { state | currentBoard <- updatedBoard }
    _ -> state

container : AppState -> Routes.Route -> Html.Html
container state (url,route) =
  let headerChannel = LC.create identity routeChannel
      boardChannel = LC.create BoardUpdate updates
  in
    body []
      [ Header.view headerChannel
      , main' []
          [ case route of
              Routes.Root ->
                text "Welcome to Diagrammer"
              Routes.Demo ->
                Board.view boardChannel state.currentBoard
              Routes.SignUp ->
                text "Sign Up"
              Routes.SignIn ->
                text "Sign In"
          ]
      , Footer.view
      ]
