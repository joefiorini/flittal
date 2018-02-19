module Main exposing (..)

import Board.Controller as Board
import Board.Model exposing (Model)
import Board.Msg as BoardMsg
import Box.Msg
import Box.Types as Box
import Html exposing (Html, aside, body, div, main_, section, text)
import Html.Attributes exposing (..)
import Interop
import Json.Decode as Decode exposing (Decoder, decodeString, field)
import Json.Encode as Encode
import Task
import Dom
import List
import Navigation exposing (Location)
import Partials.About as About
import Partials.Colophon as Colophon
import Partials.Help as Help
import Partials.Releases as Releases
import Partials.Sidebar as Sidebar
import Partials.Header as Header
import Partials.Footer as Footer
import Partials.Toolbar as Toolbar
import Result
import Routes
import Style.Color exposing (Color(..))
import Msg exposing (Msg(..))
import UndoList exposing (UndoList)
import Geometry.Types as Geometry
import Keyboard.Combo as Keys


type alias AppState =
    { currentBoard : Board.Board
    , boardHistory : UndoList Board.Board
    , navigationHistory : List Location
    , currentRoute : Routes.RouteName
    , keys : Keys.Model Msg
    }


main : Program Never AppState Msg
main =
    Navigation.program UrlChange
        { init = startingState
        , view = container
        , update = step
        , subscriptions = subscriptions
        }


parseLocation : Location -> Routes.RouteName
parseLocation location =
    case location.pathname of
        "/" ->
            Routes.Root

        "/about" ->
            Routes.About

        "/colophon" ->
            Routes.Colophon

        "/releases" ->
            Routes.Releases

        "/help" ->
            Routes.Help

        _ ->
            Routes.Root


boardUpdateCombo1 : Keys.Key -> BoardMsg.Msg -> Keys.KeyCombo Msg
boardUpdateCombo1 key msg =
    Keys.combo1 key (BoardUpdate msg)


movementKeys : List ( Keys.Key, Box.MoveDirection )
movementKeys =
    [ ( Keys.h, Box.Left )
    , ( Keys.j, Box.Down )
    , ( Keys.k, Box.Up )
    , ( Keys.l, Box.Right )
    ]


styleCombos : List (Keys.KeyCombo Msg)
styleCombos =
    let
        updateColor color =
            BoardUpdate <| BoardMsg.UpdateBoxColor color
    in
        [ Keys.combo1 Keys.one <| updateColor Dark1
        , Keys.combo1 Keys.two <| updateColor Dark2
        , Keys.combo1 Keys.three <| updateColor Dark3
        , Keys.combo1 Keys.four <| updateColor Dark4
        , Keys.combo2 ( Keys.one, Keys.shift ) <| updateColor Light1
        , Keys.combo2 ( Keys.two, Keys.shift ) <| updateColor Light2
        , Keys.combo2 ( Keys.three, Keys.shift ) <| updateColor Light3
        , Keys.combo2 ( Keys.four, Keys.shift ) <| updateColor Light4
        , Keys.combo1 Keys.zero <| updateColor White
        , Keys.combo2 ( Keys.zero, Keys.shift ) <| updateColor Black
        ]


movementCombos : List (Keys.KeyCombo Msg)
movementCombos =
    let
        moveAction movement direction =
            BoardUpdate (BoardMsg.MoveBox movement direction)
    in
        List.concatMap
            (\( key, direction ) ->
                [ Keys.combo1 key <| moveAction Box.Nudge direction
                , Keys.combo2 ( Keys.shift, key ) <| moveAction Box.Push direction
                , Keys.combo3 ( Keys.shift, Keys.alt, key ) <| moveAction Box.Jump direction
                ]
            )
            movementKeys


selectionCombos : List (Keys.KeyCombo Msg)
selectionCombos =
    [ Keys.combo1 Keys.tab <| BoardUpdate BoardMsg.SelectNextBox
    , Keys.combo2 ( Keys.tab, Keys.shift ) <| BoardUpdate BoardMsg.SelectPreviousBox
    , Keys.combo1 Keys.d <| BoardUpdate BoardMsg.DeleteSelections
    , Keys.combo1 Keys.c <| BoardUpdate BoardMsg.ConnectSelections
    , Keys.combo1 Keys.x <| BoardUpdate BoardMsg.DisconnectSelections
    ]


boardCombos : List (Keys.KeyCombo Msg)
boardCombos =
    [ Keys.combo1 Keys.a <| BoardUpdate BoardMsg.NewBox
    , Keys.combo1 Keys.enter <| BoardUpdate (BoardMsg.EditingSelectedBox True)
    , Keys.combo1 Keys.u Undo
    , Keys.combo2 ( Keys.r, Keys.control ) Redo
    ]


sizingCombos : List (Keys.KeyCombo Msg)
sizingCombos =
    let
        updateSize size =
            BoardUpdate <| BoardMsg.ResizeBox size
    in
        [ Keys.combo2 ( Keys.equals, Keys.shift ) <| updateSize Box.ResizeUpAll
        , Keys.combo3 ( Keys.equals, Keys.shift, Keys.control ) <| updateSize Box.ResizeUpNS
        , Keys.combo3 ( Keys.equals, Keys.shift, Keys.alt ) <| updateSize Box.ResizeUpEW
        , Keys.combo1 Keys.minus <| updateSize Box.ResizeDownAll
        , Keys.combo2 ( Keys.minus, Keys.control ) <| updateSize Box.ResizeDownNS
        , Keys.combo2 ( Keys.minus, Keys.alt ) <| updateSize Box.ResizeDownEW
        ]


keyboardCombos : List (Keys.KeyCombo Msg)
keyboardCombos =
    movementCombos
        ++ styleCombos
        ++ selectionCombos
        ++ boardCombos
        ++ sizingCombos
        ++ [ Keys.combo2 ( Keys.shift, Keys.forwardSlash ) ToggleHelp ]


startingState : Location -> ( AppState, Cmd msg )
startingState location =
    let
        currentRoute =
            parseLocation location
    in
        mkState [ location ] Board.startingState
            ! []


encodeAppState : AppState -> Encode.Value
encodeAppState state =
    Encode.object
        [ ( "currentBoard", Board.Model.encode state.currentBoard )
        ]


mkState : List Location -> Model -> AppState
mkState navigationHistory board =
    { currentBoard = board
    , boardHistory = UndoList.fresh Board.startingState
    , currentRoute = Routes.Root
    , navigationHistory = navigationHistory
    , keys = Keys.init keyboardCombos KeyCombo
    }


decodeAppState : Decoder AppState
decodeAppState =
    Decode.map (mkState [])
        (field "currentBoard" Board.Model.decode)


extractAppState : Result.Result String AppState -> AppState
extractAppState result =
    case result of
        Result.Ok state ->
            state

        Result.Err s ->
            Debug.crash s


subscriptions : AppState -> Sub Msg
subscriptions model =
    Sub.batch
        [ Interop.dragstart (\e -> Board.moveBoxAction e |> BoardUpdate)
        , Interop.drop (\e -> Board.moveBoxAction e |> BoardUpdate)
        , Interop.loadedState LoadedState
        , Keys.subscriptions model.keys
        ]


entersEditMode : Msg -> Bool
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
            UndoList.fresh appState.currentBoard
    in
        { appState | boardHistory = history }



-- restoreStateFromHistory : TimeMachine.History ->


step : Msg -> AppState -> ( AppState, Cmd Msg )
step update state =
    case Debug.log "update" update of
        LoadedState deserializedState ->
            deserializedState
                |> decodeString decodeAppState
                |> extractAppState
                |> initTimeMachine
                |> flip (!) [ Cmd.none ]

        NewPage path ->
            state ! [ Navigation.newUrl path ]

        UrlChange location ->
            let
                newRoute =
                    parseLocation location
            in
                { state | currentRoute = newRoute, navigationHistory = location :: state.navigationHistory } ! []

        BoardUpdate u ->
            let
                recordedHistory =
                    UndoList.mapPresent (Board.step u) state.boardHistory

                history_ =
                    case u of
                        BoardMsg.NewBox ->
                            recordedHistory

                        BoardMsg.MoveBox _ _ ->
                            recordedHistory

                        BoardMsg.UpdateBoxColor _ ->
                            recordedHistory

                        BoardMsg.DeleteSelections ->
                            recordedHistory

                        BoardMsg.ConnectSelections ->
                            recordedHistory

                        BoardMsg.DisconnectSelections ->
                            recordedHistory

                        BoardMsg.Drop _ _ ->
                            recordedHistory

                        BoardMsg.ResizeBox _ ->
                            recordedHistory

                        BoardMsg.BoxAction (Box.Msg.EditingBox _ _) ->
                            recordedHistory

                        _ ->
                            state.boardHistory

                cmd =
                    case u of
                        BoardMsg.EditingBox boxKey toggle ->
                            Board.toSelector boxKey |> Dom.focus |> (Task.attempt (\_ -> NoOp))

                        _ ->
                            Cmd.none
            in
                { state
                    | currentBoard = (Board.step u history_.present)
                    , boardHistory = Debug.log "new history" history_
                }
                    ! [ cmd ]

        ToolbarUpdate u ->
            let
                updatedBoard =
                    Board.step BoardMsg.ClearBoard state.currentBoard
            in
                { state | currentBoard = updatedBoard } ! []

        Undo ->
            let
                history =
                    UndoList.undo state.boardHistory
            in
                history.present
                    |> (\board ->
                            { state
                                | currentBoard = board
                                , boardHistory = history
                            }
                       )
                    |> flip (!) []

        Redo ->
            let
                history =
                    UndoList.redo state.boardHistory
            in
                history.present
                    |> (\board ->
                            { state
                                | currentBoard = board
                                , boardHistory = history
                            }
                       )
                    |> flip (!) []

        SerializeState ->
            ( state
            , state
                |> (Encode.encode 0)
                << encodeAppState
                |> Interop.serializeState
            )

        KeyCombo combo ->
            let
                ( keys, cmd ) =
                    Keys.update combo state.keys
            in
                ( { state | keys = keys }, cmd )

        ToggleHelp ->
            case state.currentRoute of
                Routes.Help ->
                    state ! [ Navigation.newUrl "/" ]

                _ ->
                    state ! [ Navigation.newUrl "/help" ]

        NoOp ->
            state ! []


container : AppState -> Html Msg
container state =
    let
        sidebar h =
            Sidebar.view h

        offsetHeight =
            -- TODO: Don't hard code this. get the actual height, if needed?
            1024 - 52

        board =
            Board.view BoardUpdate state.currentBoard offsetHeight

        ( sidebar_, extraClass, sidebarHeight ) =
            case state.currentRoute of
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
        section []
            [ Header.view
            , Toolbar.view ToolbarUpdate
            , main_
                [ class "l-container" ]
                [ section
                    [ classList [ ( "l-board", True ), ( extraClass, extraClass /= "" ) ]
                    ]
                    [ board ]
                , section
                    [ class "l-content"
                    , style
                        [ ( "height", (Geometry.toPx sidebarHeight) )
                        ]
                    ]
                    [ sidebar_
                    ]
                ]
            , section
                [ class "l-container" ]
                [ Footer.view ]
            ]
