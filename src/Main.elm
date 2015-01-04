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
import Native.App as App
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode ((:=))
import Signal
import Result
import Routes
import Signal (Signal, (<~), (~))
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
main = (\s r (w,h) -> toElement w h  <| container s r h)
        <~ state
         ~ routeHandler
         ~ Window.dimensions

keyboardRequestAction = Signal.map convertKeyboardOperation (Signal.dropWhen inEditingMode 0 Keyboard.lastPressed)

globalKeyboardShortcuts : String -> Update
globalKeyboardShortcuts keyCommand =
  case keyCommand of
    "a"     -> BoardUpdate Board.NewBox
    "c"     -> BoardUpdate Board.ConnectSelections
    "d"     -> BoardUpdate Board.DeleteSelections
    "enter" -> BoardUpdate (Board.EditingSelectedBox True)
    _       -> NoOp


startingState : AppState
startingState =
  { currentBoard = Board.startingState
  }

routesMap routeName =
  let url =
    case routeName of
      Routes.Root -> "/"
      Routes.About -> "/about"
  in
     (url, routeName)

encodeAppState : AppState -> Encode.Value
encodeAppState state =
  Encode.object
    [ ("currentBoard", Board.encode state.currentBoard)
    ]

decodeAppState : Decode.Decoder AppState
decodeAppState =
  Decode.object1 AppState
    ("currentBoard" := Board.decode)

extractAppState : Result.Result String AppState -> AppState
extractAppState result =
  case result of
    Result.Ok state -> state
    Result.Err s -> Debug.crash s

deserializedState : Signal Update
deserializedState =
  let deserializeAppState = Decode.decodeString decodeAppState
      loadedState' = Signal.keepIf ((/=) "")
        (Encode.encode 0 <| encodeAppState startingState) loadedState
  in
    (HydrateAppState << extractAppState << deserializeAppState) <~ loadedState'

port loadedState : Signal String

port serializeState : Signal String
port serializeState =
  let serializeAppState = (Encode.encode 0) << encodeAppState
  in
    serializeAppState <~ state


port transitionToRoute : Signal Routes.Url
port transitionToRoute =
  Routes.sendToPort routeHandler

routeHandler =
  Routes.map routesMap
    <| Signal.subscribe routeChannel

type Update = NoOp
            | HydrateAppState AppState
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
      , deserializedState
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

step : Update -> AppState -> AppState
step update state =
  case Debug.log "update" update of
    HydrateAppState state' -> Debug.log "Updated State" state'
    BoardUpdate u ->
      let updatedBoard = Board.step u state.currentBoard
      in
      { state | currentBoard <- updatedBoard }
    _ -> state

container : AppState -> Routes.Route -> Int -> Html.Html
container state (url,route) screenHeight =
  let headerChannel = LC.create identity routeChannel
      boardChannel = LC.create BoardUpdate updates
  in
    body []
      [ Header.view headerChannel
      , main' []
          [ case route of
              Routes.Root ->
                Board.view boardChannel state.currentBoard (screenHeight - 36)
              Routes.About ->
                text "About Diagrammer"
          ]
      , Footer.view
      ]
