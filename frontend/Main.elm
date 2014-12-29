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
import Http
import Signal
import Result
import Signal (Signal, (<~))
import Keyboard
import Window
import Debug
import User.SignUp as SignUp

port drop : Signal DragEvent
port dragstart : Signal DragEvent
port dragend : Signal DragEvent

port focus : Signal String
port focus = checkFocus

port globalKeyDown : Signal Int

type AjaxAction =
    SubmitSignUpForm (Http.Request String)
  | None

type alias AppState =
  { currentBoard        : Board.Board
  , signUpForm          : SignUp.Model
  , ajaxAction          : AjaxAction
  , ajaxResult          : Result String String
  }

main : Signal Element
main = Signal.map2 (\h r -> toElement 900 1200 <| container h r) (processActions state) routeHandler

processActions : Signal AppState -> Signal AppState
processActions stateSignal =
  let actionSignal f = Signal.map f stateSignal
  in
    ajaxActions (actionSignal .ajaxAction) stateSignal

ajaxActions : Signal AjaxAction -> Signal AppState -> Signal AppState
ajaxActions action state =
  let request = Signal.map requestForAction
      requestForAction a =
        case a of
          SubmitSignUpForm r -> r
          _ -> Http.request "" "" "" []
  in
    Signal.map2
      (\r state ->
        case Debug.log "http result" r of
          Http.Success s -> { state | ajaxResult <- (Result.Ok s) }
          Http.Waiting -> state
          Http.Failure code s -> { state | ajaxResult <- (Result.Err s) })
      (Http.send <| request action)
      state


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
  , signUpForm = SignUp.startingState
  , ajaxAction = None
  , ajaxResult = (Result.Ok "")
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
            | SignUpUpdate SignUp.Update

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

step : Update -> AppState -> AppState
step update state =
  case Debug.log "update" update of
    SignUpUpdate (SignUp.SubmitForm) ->
      let request = SignUp.buildRequest state.signUpForm
      in
        { state | ajaxAction <- SubmitSignUpForm request }
    SignUpUpdate u ->
      let updatedSignUp = SignUp.step u state.signUpForm
      in
         { state | signUpForm <- updatedSignUp }
    BoardUpdate u ->
      let updatedBoard = Board.step u state.currentBoard
      in
      { state | currentBoard <- updatedBoard }
    _ -> state

container : AppState -> Routes.Route -> Html.Html
container state (url,route) =
  let headerChannel = LC.create identity routeChannel
      signUpChannel = LC.create SignUpUpdate updates
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
                SignUp.view signUpChannel state.signUpForm
              Routes.SignIn ->
                text "Sign In"
          ]
      , Footer.view
      ]
