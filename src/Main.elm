module Main exposing (..)

import Board.Controller as Board
import Board.Model exposing (Model)
import Box.Controller as Box
import DomUtils exposing (DragEvent, class_, linkTo, styleProperty)
import Geometry.Types as Geometry
import Html exposing (Html, aside, body, div, main_, section, text)
import Html.Attributes exposing (class, style)
import Interop
import Json.Decode exposing (Decoder, decodeString, field, map)
import Json.Encode as Encode
import Keyboard.Extra exposing (Key)
import Keyboard.Extra exposing (..)
import List
import Mousetrap
import Native.App as App
import Partials.About as About
import Partials.Colophon as Colophon
import Partials.Footer as Footer
import Partials.Header as Header
import Partials.Help as Help
import Partials.Releases as Releases
import Partials.Sidebar as Sidebar
import Partials.Toolbar as Toolbar
import Result
import Routes
import Style.Color exposing (Color(..))
import TimeMachine


type alias AppState =
    { currentBoard : Board.Board
    , boardHistory : TimeMachine.History Board.Board
    }


main : Program Never Model Msg
main =
    Html.program
        { init = startingState
        , view = container
        , update = step
        , subscriptions = subscriptions
        }



{-
   Use http://package.elm-lang.org/packages/ohanhi/keyboard-extra/latest to get
   the keys that were pressed in a list, so use those lists for the case clauses
   instead of mousetrap strings; then in the update function, we'll call this
   as part of the (a -> Msg) in Task.perform (might have to use Task.succeed
   with the list of pressed keys to get them into the function)
-}


globalKeyboardShortcuts : List Key -> Msg
globalKeyboardShortcuts keyCommand =
    case Debug.log "keyCommand" keyCommand of
        [ Tab ] ->
            BoardUpdate Board.SelectNextBox

        [ Shift, Tab ] ->
            BoardUpdate Board.SelectPreviousBox

        [ CharA ] ->
            BoardUpdate Board.NewBox

        [ CharC ] ->
            BoardUpdate Board.ConnectSelections

        [ CharX ] ->
            BoardUpdate Board.DisconnectSelections

        [ CharD ] ->
            BoardUpdate Board.DeleteSelections

        [ Number1 ] ->
            BoardUpdate <| Board.UpdateBoxColor Dark1

        [ Number2 ] ->
            BoardUpdate <| Board.UpdateBoxColor Dark2

        [ Number3 ] ->
            BoardUpdate <| Board.UpdateBoxColor Dark3

        -- "4" ->
        --     BoardUpdate <| Board.UpdateBoxColor Dark4
        --
        -- "shift+1" ->
        --     BoardUpdate <| Board.UpdateBoxColor Light1
        --
        -- "shift+2" ->
        --     BoardUpdate <| Board.UpdateBoxColor Light2
        --
        -- "shift+3" ->
        --     BoardUpdate <| Board.UpdateBoxColor Light3
        --
        -- "shift+4" ->
        --     BoardUpdate <| Board.UpdateBoxColor Light4
        --
        -- "0" ->
        --     BoardUpdate <| Board.UpdateBoxColor Black
        --
        -- "shift+0" ->
        --     BoardUpdate <| Board.UpdateBoxColor White
        --
        -- "shift+=" ->
        --     BoardUpdate <| Board.ResizeBox Box.ResizeUpAll
        --
        -- "-" ->
        --     BoardUpdate <| Board.ResizeBox Box.ResizeDownAll
        --
        -- "ctrl+shift+=" ->
        --     BoardUpdate <| Board.ResizeBox Box.ResizeUpNS
        --
        -- "ctrl+-" ->
        --     BoardUpdate <| Board.ResizeBox Box.ResizeDownNS
        --
        -- "alt+shift+=" ->
        --     BoardUpdate <| Board.ResizeBox Box.ResizeUpEW
        --
        -- "alt+-" ->
        --     BoardUpdate <| Board.ResizeBox Box.ResizeDownEW
        --
        -- "enter" ->
        --     BoardUpdate (Board.EditingSelectedBox True)
        --
        -- "h" ->
        --     BoardUpdate <| Board.MoveBox Box.Nudge Box.Left
        --
        -- "j" ->
        --     BoardUpdate <| Board.MoveBox Box.Nudge Box.Down
        --
        -- "k" ->
        --     BoardUpdate <| Board.MoveBox Box.Nudge Box.Up
        --
        -- "l" ->
        --     BoardUpdate <| Board.MoveBox Box.Nudge Box.Right
        --
        -- "u" ->
        --     Undo
        --
        -- "ctrl+r" ->
        --     Redo
        --
        -- "shift+h" ->
        --     BoardUpdate <| Board.MoveBox Box.Push Box.Left
        --
        -- "shift+j" ->
        --     BoardUpdate <| Board.MoveBox Box.Push Box.Down
        --
        -- "shift+k" ->
        --     BoardUpdate <| Board.MoveBox Box.Push Box.Up
        --
        -- "shift+l" ->
        --     BoardUpdate <| Board.MoveBox Box.Push Box.Right
        --
        -- "alt+shift+h" ->
        --     BoardUpdate <| Board.MoveBox Box.Jump Box.Left
        --
        -- "alt+shift+j" ->
        --     BoardUpdate <| Board.MoveBox Box.Jump Box.Down
        --
        -- "alt+shift+k" ->
        --     BoardUpdate <| Board.MoveBox Box.Jump Box.Up
        --
        -- "alt+shift+l" ->
        --     BoardUpdate <| Board.MoveBox Box.Jump Box.Right
        _ ->
            NoOp


startingState : AppState
startingState =
    { currentBoard = Board.startingState
    , boardHistory = TimeMachine.initialize Board.startingState
    }


routesMap routeName =
    let
        url =
            case routeName of
                Routes.Root ->
                    "/"

                Routes.About ->
                    "/about"

                Routes.Colophon ->
                    "/colophon"

                Routes.Releases ->
                    "/releases"

                Routes.Help ->
                    "/help"
    in
        ( url, routeName )


encodeAppState : AppState -> Encode.Value
encodeAppState state =
    Encode.object
        [ ( "currentBoard", Board.encode state.currentBoard )
        ]


mkState board =
    { currentBoard = board
    , boardHistory = TimeMachine.initialize Board.startingState
    }


decodeAppState : Decoder AppState
decodeAppState =
    map mkState
        (field "currentBoard" Board.decode)


extractAppState : Result.Result String AppState -> AppState
extractAppState result =
    case result of
        Result.Ok state ->
            state

        Result.Err s ->
            Debug.crash s


subscriptions : model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Interop.serializeState SerializeState
        , Interop.dragstart (\e -> Board.moveBoxAction e |> BoardUpdate)
        , Interop.drop (\e -> Board.moveBoxAction e |> BoardUpdate)
        , Interop.loadedState LoadedState
        ]


toggleHelp =
    Signal.map
        (\k ->
            case k of
                "shift+/" ->
                    Routes.Help

                "w" ->
                    Routes.Root

                _ ->
                    Routes.Root
        )
        Mousetrap.keydown


routeHandler =
    Routes.map routesMap <|
        Signal.mergeMany
            [ Signal.subscribe routeChannel
            , toggleHelp
            ]


type Msg
    = NoOp
    | HydrateAppState AppState
    | BoardUpdate Board.Msg
    | ToolbarUpdate Toolbar.Msg
    | Undo
    | Redo
    | SerializeState
    | LoadedState String



-- state : Signal.Signal AppState
-- state =
--     Signal.foldp step
--         startingState
--         (Signal.mergeMany
--             [ Signal.subscribe updates
--             , deserializedState
--             , Signal.map globalKeyboardShortcuts Mousetrap.keydown
--             ]
--         )


entersEditMode update =
    case update of
        BoardUpdate a ->
            Board.entersEditMode a

        otherwise ->
            False


initTimeMachine : AppState -> AppState
initTimeMachine appState =
    let
        history =
            TimeMachine.initialize appState.currentBoard
    in
        { appState | boardHistory = history }


step : Msg -> AppState -> AppState
step update state =
    case Debug.log "update" update of
        LoadedState deserializedState ->
            deserializedState
                |> decodeString decodeAppState
                |> extractAppState
                |> initTimeMachine

        BoardUpdate u ->
            let
                updatedBoard =
                    Board.step u state.currentBoard

                recordedHistory =
                    TimeMachine.record updatedBoard state.boardHistory

                history_ =
                    case u of
                        Board.NewBox ->
                            recordedHistory

                        Board.MoveBox _ _ ->
                            recordedHistory

                        Board.UpdateBoxColor _ ->
                            recordedHistory

                        Board.DeleteSelections ->
                            recordedHistory

                        Board.ConnectSelections ->
                            recordedHistory

                        Board.DisconnectSelections ->
                            recordedHistory

                        Board.Drop _ _ ->
                            recordedHistory

                        Board.ResizeBox _ ->
                            recordedHistory

                        Board.BoxAction (Box.EditingBox _ _) ->
                            recordedHistory

                        _ ->
                            state.boardHistory
            in
                { state
                    | currentBoard = updatedBoard
                    , boardHistory = Debug.log "new history" history_
                }

        ToolbarUpdate u ->
            let
                updatedBoard =
                    Board.step Board.ClearBoard state.currentBoard
            in
                { state | currentBoard = updatedBoard }

        Undo ->
            let
                history_ =
                    TimeMachine.travelBackward state.boardHistory
            in
                case history_.current of
                    Just b ->
                        { state
                            | currentBoard = b
                            , boardHistory = history_
                        }

                    Nothing ->
                        state

        Redo ->
            let
                history_ =
                    TimeMachine.travelForward state.boardHistory
            in
                case history_.current of
                    Just b ->
                        { state
                            | currentBoard = b
                            , boardHistory = history_
                        }

                    Nothing ->
                        state

        SerializeState ->
            ( state
            , state
                |> (Encode.encode 0)
                << encodeAppState
                |> Interop.serializeState
            )

        _ ->
            state


container : AppState -> Routes.Route -> Int -> Html Msg
container state ( url, route ) screenHeight =
    let
        sidebar h =
            Sidebar.view h

        offsetHeight =
            screenHeight - 52

        board =
            Board.view state.currentBoard offsetHeight

        ( sidebar_, extraClass, sidebarHeight ) =
            case route of
                Routes.About ->
                    ( sidebar <| About.view, "l-board--compressed", offsetHeight )

                Routes.Colophon ->
                    ( sidebar <| Colophon.view, "l-board--compressed", offsetHeight )

                Routes.Help ->
                    ( sidebar <| Help.view, "l-board--compressed", offsetHeight )

                Routes.Releases ->
                    ( sidebar <| Releases.view, "l-board--compressed", offsetHeight )

                _ ->
                    ( text "", "", 0 )
    in
        div []
            [ Header.view
            , Toolbar.view
            , main_
                [ class "l-container" ]
                [ section
                    [ class_ [ "l-board", extraClass ]
                    ]
                    [ board ]
                , section
                    [ class "l-content"
                    , style
                        [ styleProperty "height" <| Geometry.toPx sidebarHeight
                        ]
                    ]
                    [ sidebar_
                    ]
                ]
            , section
                [ class "l-container" ]
                [ Footer.view ]
            ]
