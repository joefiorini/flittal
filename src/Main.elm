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
import List.Extra exposing (find)
import Window
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
import Base64
import UrlParser
import Result.Extra as ResultExtra


type alias Flags =
    { windowSize : Window.Size
    }


type alias AppState =
    { currentBoard : Board.Board
    , boardHistory : UndoList Board.Board
    , navigationHistory : List Location
    , currentLocation : Location
    , currentRoute : Routes.RouteName
    , keys : Keys.Model Msg
    , windowSize : Window.Size
    , encodedBoard : Maybe String
    }


main : Program Flags AppState Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
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

        movementKeys =
            [ ( Keys.h, Box.Left )
            , ( Keys.j, Box.Down )
            , ( Keys.k, Box.Up )
            , ( Keys.l, Box.Right )
            ]
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
        ++ [ Keys.combo2 ( Keys.shift, Keys.forwardSlash ) ToggleHelp
           , Keys.combo1 Keys.w (NewPage "/")
           ]


getEncodedState : Location -> Maybe String
getEncodedState location =
    UrlParser.parseHash UrlParser.string location


init : Flags -> Location -> ( AppState, Cmd msg )
init flags location =
    let
        currentRoute =
            parseLocation location

        boardState =
            getEncodedState location
                |> Maybe.map
                    (\str ->
                        let
                            decoded s =
                                Decode.decodeString Board.Model.decode s
                                    |> ResultExtra.orElse (decodeAppState s)

                            decodeBoard s =
                                Base64.decode s |> Result.andThen (\s -> decoded s) |> Debug.log "decoded"
                        in
                            Result.withDefault Board.init <| decodeBoard str
                    )
                |> Maybe.withDefault Board.init
    in
        { currentBoard = boardState
        , boardHistory = UndoList.fresh boardState
        , currentRoute = Routes.Root
        , navigationHistory = [ location ]
        , currentLocation = location
        , keys = Keys.init keyboardCombos KeyCombo
        , windowSize = flags.windowSize
        , encodedBoard = Nothing
        }
            ! []


serializeBoardState : Model -> String
serializeBoardState board =
    Board.Model.encode board
        |> Encode.encode 0


decodeAppState : String -> Result String Model
decodeAppState s =
    field "currentBoard" Board.Model.decode
        |> flip Decode.decodeString s


subscriptions : AppState -> Sub Msg
subscriptions model =
    Sub.batch
        [ Interop.dragstart (\e -> Board.moveBoxAction e |> BoardUpdate)
        , Interop.drop (\e -> Board.moveBoxAction e |> BoardUpdate)
        , Keys.subscriptions model.keys
        , Window.resizes ResizeWindow
        ]


update : Msg -> AppState -> ( AppState, Cmd Msg )
update update state =
    case update of
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
                isRecordable =
                    case u of
                        BoardMsg.NewBox ->
                            True

                        BoardMsg.DeleteSelections ->
                            True

                        BoardMsg.ConnectSelections ->
                            True

                        BoardMsg.DisconnectSelections ->
                            True

                        BoardMsg.Drop _ _ ->
                            True

                        BoardMsg.ResizeBox _ ->
                            True

                        BoardMsg.BoxAction (Box.Msg.EditingBox _ _) ->
                            True

                        _ ->
                            False

                focusBox boxKey =
                    let
                        boxDomId =
                            Board.toSelector boxKey

                        doNothing task =
                            Task.attempt (\_ -> NoOp) task
                    in
                        [ (doNothing <| Dom.focus boxDomId), (Interop.selectInputText boxDomId) ]

                cmd =
                    case u of
                        BoardMsg.EditingSelectedBox True ->
                            let
                                selectedBox =
                                    find (\b -> b.selectedIndex /= -1) state.currentBoard.boxes
                            in
                                case selectedBox of
                                    Just { key } ->
                                        focusBox key

                                    Nothing ->
                                        []

                        BoardMsg.EditingBox boxKey toggle ->
                            if toggle then
                                focusBox boxKey
                            else
                                []

                        _ ->
                            []

                newBoard =
                    Board.update u state.currentBoard
            in
                { state
                    | currentBoard = newBoard
                    , boardHistory =
                        if isRecordable then
                            UndoList.new newBoard state.boardHistory
                        else
                            state.boardHistory
                }
                    ! cmd

        ClearBoard ->
            let
                updatedBoard =
                    Board.update BoardMsg.ClearBoard state.currentBoard
            in
                { state | currentBoard = updatedBoard } ! []

        ShareBoard ->
            let
                serializeAndEncodeBoard =
                    serializeBoardState >> Base64.encode
            in
                { state | encodedBoard = Just (serializeAndEncodeBoard state.currentBoard) } ! [ Interop.selectInputText "share-url" ]

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

        KeyCombo combo ->
            let
                ( keys, cmd ) =
                    Keys.update combo state.keys
            in
                ( { state | keys = keys }, cmd )

        ToggleHelp ->
            case Debug.log "help" state.currentRoute of
                Routes.Help ->
                    state ! [ Navigation.newUrl "/" ]

                _ ->
                    state ! [ Navigation.newUrl "/help" ]

        ResizeWindow size ->
            { state | windowSize = size } ! []

        NoOp ->
            state ! []


view : AppState -> Html Msg
view state =
    let
        sidebar h =
            Sidebar.view h

        offsetHeight =
            state.windowSize.height - 52

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

        currentLocation =
            case state.navigationHistory of
                location :: _ ->
                    location

                _ ->
                    Debug.crash "No navigation history!"
    in
        section []
            [ Header.view
            , Toolbar.view state.encodedBoard currentLocation
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
