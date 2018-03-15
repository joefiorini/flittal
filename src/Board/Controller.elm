module Board.Controller exposing (..)

import Board.Model
import Board.Msg exposing (..)
import Box.Controller as Box
import Box.Types
import Box.Msg
import Box.Types
import Connection.Controller as Connection exposing (leftOf)
import Dom.Types exposing (DragEvent)
import DomUtils exposing (extractBoxId, getMouseSelectionEvent, getTargetId, styleProperty)
import Geometry.Types as Geometry
import Html exposing (..)
import Html.Attributes exposing (class, id, property, style)
import Html.Events exposing (on)
import Json.Decode exposing (map, succeed)
import List exposing ((::))
import List.Extra exposing (find)
import Maybe
import Style.Color exposing (Color(..))
import Box.Model
import Mouse
import Json.Decode as Decode exposing (Decoder)


type alias Model =
    Board.Model.Model


view : (Msg -> msg) -> Model -> Int -> Html msg
view tx model height =
    let
        boxes =
            List.map (\b -> (Box.view b |> Html.map (\a -> tx (BoxAction a)))) model.boxes

        connections =
            List.map Connection.renderConnection model.connections
    in
        div
            [ style
                [ styleProperty "height" <| Geometry.toPx height
                ]
            , class "board"
            , id "container"
            , on "dblclick" (getTargetId |> Json.Decode.map (\s -> tx <| buildEditingAction s))
            , on "mousedown" (Decode.map (\event -> buildSelectAction event |> tx) decodeWithTarget)
            ]
            (List.concatMap identity [ boxes, connections ])


type alias MouseWithTarget =
    { mouseEvent : Mouse.Event
    , targetId : String
    }


decodeWithTarget : Decoder MouseWithTarget
decodeWithTarget =
    Decode.map2 MouseWithTarget
        Mouse.eventDecoder
        targetDecoder


targetDecoder : Decoder String
targetDecoder =
    Decode.at [ "target", "id" ] Decode.string


buildSelectAction : MouseWithTarget -> Msg
buildSelectAction event =
    let
        boxIdM =
            extractBoxId event.targetId
    in
        case boxIdM of
            Just key ->
                if event.mouseEvent.keys.shift then
                    SelectBoxMulti key
                else
                    SelectBox key

            Nothing ->
                DeselectBoxes


buildEditingAction : String -> Msg
buildEditingAction id =
    let
        boxIdM =
            extractBoxId id
    in
        case boxIdM of
            Just key ->
                EditingBox key True

            Nothing ->
                NoOp


toSelector : Int -> String
toSelector domId =
    ("box-" ++ toString domId ++ "-label")


moveBoxAction : DragEvent -> Msg
moveBoxAction event =
    let
        boxKeyM =
            extractBoxId event.id
    in
        case boxKeyM of
            Just key ->
                if event.isStart then
                    DraggingBox key
                else
                    Drop key event

            Nothing ->
                NoOp


init : Model
init =
    { boxes = []
    , connections = []
    , nextIdentifier = 1
    }


isEditing : List Box.Types.Model -> Bool
isEditing =
    List.any (.isEditing)


contains : a -> List a -> Bool
contains obj =
    List.any (\b -> b == obj)


containsEither : a -> a -> List a -> Bool
containsEither obj1 obj2 =
    List.any (\b -> b == obj1 || b == obj2)


sortRightToLeft : List Box.Types.Model -> List Box.Types.Model
sortRightToLeft =
    List.reverse << sortLeftToRight


sortLeftToRight : List Box.Types.Model -> List Box.Types.Model
sortLeftToRight boxes =
    List.sortBy (Tuple.first << (.position)) <|
        List.sortBy (Tuple.second << (.position)) boxes


boxForKey : Box.Types.BoxKey -> List Box.Types.Model -> Maybe Box.Types.Model
boxForKey key boxes =
    List.head (List.filter (\b -> b.key == key) boxes)


makeBox : Box.Types.BoxKey -> Box.Types.Model
makeBox identifier =
    let
        style =
            { color = White }
    in
        { position = ( 0, 0 )
        , size = ( 100, 50 )
        , label = "New Box"
        , originalLabel = "New Box"
        , key = identifier
        , isEditing = False
        , isDragging = False
        , selectedIndex = 1
        , style = style
        }


replaceBox : List Box.Types.Model -> Box.Types.Model -> List Box.Types.Model
replaceBox boxes withBox =
    List.map
        (\box ->
            if box.key == withBox.key then
                withBox
            else
                box
        )
        boxes


updateBoxInState : Box.Types.BoxKey -> Box.Msg.Msg -> Box.Types.Model -> Box.Types.Model
updateBoxInState boxKey msg box =
    if box.key == boxKey then
        Box.update msg box
    else
        box


update : Msg -> Model -> Model
update msg model =
    let
        updateSelectedBoxes msg iterator =
            (if iterator.selectedIndex > -1 then
                Box.update msg iterator
             else
                iterator
            )

        performActionOnAllBoxes msg =
            (Box.update msg)

        nextSelectedIndex boxes =
            List.foldl
                (\last index ->
                    if index > last then
                        index + 1
                    else
                        last + 1
                )
                0
            <|
                List.map (.selectedIndex) boxes

        selectedBoxes boxes =
            List.sortBy (.selectedIndex) <|
                List.filter (\b -> b.selectedIndex > -1) boxes

        deselectBoxes =
            List.map (\box -> { box | selectedIndex = -1 })
    in
        case msg of
            NewBox ->
                if isEditing model.boxes then
                    model
                else
                    let
                        newBox =
                            makeBox model.nextIdentifier
                    in
                        { model
                            | boxes = newBox :: (deselectBoxes model.boxes)
                            , nextIdentifier = model.nextIdentifier + 1
                        }

            BoxAction (Box.Msg.CancelEditingBox box) ->
                let
                    box_ =
                        Box.update Box.Msg.CancelEditing box

                    boxes_ =
                        replaceBox model.boxes box_
                in
                    { model | boxes = deselectBoxes boxes_ }

            DeselectBoxes ->
                let
                    cancelEditing =
                        List.map (Box.update Box.Msg.CancelEditing)

                    updateBoxes =
                        cancelEditing >> deselectBoxes
                in
                    { model | boxes = updateBoxes model.boxes }

            SelectBoxMulti id ->
                { model
                    | boxes =
                        model.boxes
                            |> List.map
                                (\box ->
                                    updateBoxInState id (nextSelectedIndex model.boxes |> Box.Msg.SetSelected) box
                                )
                }

            DraggingBox id ->
                let
                    box =
                        boxForKey id model.boxes

                    selectedBox boxes =
                        List.map (\box -> updateBoxInState id (Box.Msg.SetSelected <| nextSelectedIndex boxes) box) boxes

                    draggingBox =
                        List.map (updateSelectedBoxes Box.Msg.Dragging)

                    updateBoxes =
                        selectedBox >> draggingBox
                in
                    { model | boxes = updateBoxes model.boxes }

            SelectBox id ->
                let
                    selectedBox boxes =
                        List.map (\box -> updateBoxInState id (Box.Msg.SetSelected <| nextSelectedIndex boxes) box) boxes

                    updateBoxes =
                        deselectBoxes >> selectedBox
                in
                    { model | boxes = updateBoxes model.boxes }

            SelectNextBox ->
                let
                    selections =
                        List.filter Box.Model.isSelected <| sortLeftToRight model.boxes

                    nextBox =
                        let
                            sortedBoxes =
                                sortLeftToRight model.boxes
                        in
                            case selections of
                                [] ->
                                    List.head <| sortedBoxes

                                current :: _ ->
                                    let
                                        rightBoxes =
                                            List.filter (\box -> leftOf current.position box.position) sortedBoxes
                                    in
                                        case rightBoxes of
                                            [] ->
                                                List.head sortedBoxes

                                            current :: _ ->
                                                Just current

                    newState =
                        nextBox
                            |> Maybe.map
                                (\next ->
                                    { model
                                        | boxes =
                                            List.map
                                                (\box -> updateBoxInState next.key (Box.Msg.SetSelected 0) box)
                                                model.boxes
                                    }
                                )
                in
                    Maybe.withDefault model newState

            SelectPreviousBox ->
                let
                    selections =
                        List.filter Box.Model.isSelected <| sortRightToLeft model.boxes

                    nextBox =
                        let
                            sortedBoxes =
                                sortRightToLeft model.boxes
                        in
                            case selections of
                                [] ->
                                    List.head <| sortedBoxes

                                current :: _ ->
                                    let
                                        firstRight =
                                            find (\box -> leftOf box.position current.position) sortedBoxes
                                    in
                                        case firstRight of
                                            Nothing ->
                                                List.head sortedBoxes

                                            _ ->
                                                firstRight

                    newState =
                        nextBox
                            |> Maybe.map
                                (\next ->
                                    { model
                                        | boxes =
                                            deselectBoxes model.boxes
                                                |> List.map
                                                    (\box -> updateBoxInState next.key (Box.Msg.SetSelected 0) box)
                                    }
                                )
                in
                    Maybe.withDefault model newState

            EditingBox boxKey toggle ->
                let
                    box =
                        boxForKey boxKey model.boxes |> Maybe.map (\box -> Box.update (Box.Msg.Editing toggle) box)

                    newState =
                        Maybe.map (\box -> { model | boxes = replaceBox model.boxes box }) box
                in
                    (Maybe.withDefault model newState)

            BoxAction (Box.Msg.EditingBox box toggle) ->
                let
                    box_ =
                        Box.update (Box.Msg.Editing toggle) box
                in
                    { model | boxes = replaceBox model.boxes <| box_ }

            EditingSelectedBox toggle ->
                let
                    selectedBox =
                        List.filter (\b -> b.selectedIndex /= -1) model.boxes |> List.head

                    newState =
                        selectedBox
                            |> Maybe.map
                                (\box ->
                                    { model
                                        | boxes =
                                            Box.update (Box.Msg.Editing toggle) box |> replaceBox model.boxes
                                    }
                                )
                in
                    Maybe.withDefault model newState

            BoxAction (Box.Msg.UpdateBox box label) ->
                { model | boxes = replaceBox model.boxes <| Box.update (Box.Msg.Update label) box }

            Drop key event ->
                let
                    draggingBox =
                        List.map (updateSelectedBoxes Box.Msg.Dragging)

                    moveAllSelectedBoxes boxes =
                        List.map (updateSelectedBoxes (Box.Msg.Drop event)) boxes

                    updateBoxes =
                        moveAllSelectedBoxes >> draggingBox
                in
                    update
                        ReconnectSelections
                        { model | boxes = updateBoxes model.boxes }

            ReconnectSelections ->
                { model
                    | connections =
                        Connection.boxMap
                            Connection.connectBoxes
                            model.boxes
                            model.connections
                }

            ConnectSelections ->
                if List.length (selectedBoxes model.boxes) < 2 then
                    model
                else
                    { model
                        | connections =
                            Connection.buildConnections model.connections <|
                                selectedBoxes model.boxes
                    }

            DisconnectSelections ->
                let
                    selectedBoxes =
                        List.filter Box.Model.isSelected model.boxes

                    connectionish =
                        case selectedBoxes of
                            [ box1, box2 ] ->
                                Just <| List.filter (Connection.onBoxes box1 box2) model.connections

                            _ ->
                                Nothing

                    filtered =
                        Maybe.map
                            (\c ->
                                case c of
                                    [] ->
                                        model.connections

                                    connection :: rest ->
                                        List.filter ((/=) connection) model.connections
                            )
                            connectionish
                in
                    case filtered of
                        Just f ->
                            { model | connections = f }

                        Nothing ->
                            model

            DeleteSelections ->
                let
                    isSelected boxKey =
                        List.length (Box.Model.filterKey (not << Box.Model.isSelected) boxKey model.boxes)
                            == 1
                in
                    { model
                        | boxes = List.filter (not << Box.Model.isSelected) model.boxes
                        , connections =
                            List.filter
                                (\c ->
                                    (isSelected c.startBox)
                                        && (isSelected c.endBox)
                                )
                                model.connections
                    }

            ResizeBox mode ->
                let
                    updateBoxes =
                        List.map (updateSelectedBoxes (Box.Msg.Resize mode))
                in
                    update ReconnectSelections { model | boxes = updateBoxes model.boxes }

            UpdateBoxColor color ->
                { model | boxes = List.map (updateSelectedBoxes (Box.Msg.UpdateColor color)) model.boxes }

            MoveBox mode direction ->
                let
                    updateBoxes =
                        List.map (updateSelectedBoxes (Box.Msg.Move mode direction))
                in
                    update ReconnectSelections { model | boxes = updateBoxes model.boxes }

            ClearBoard ->
                init

            BoxAction _ ->
                model

            NoOp ->
                model
