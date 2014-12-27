module Board.Controller (checkFocus, startingState, Board, Action, view, keyboardRequest, moveBoxAction, step) where

import Keyboard
import Debug

import Signal
import Signal (Channel, (<~))

import Html (..)
import Html.Events (on)
import Html.Attributes (id, style, property)

import Board.View (draw)
import Board.Model

import Box.Action
import Box.Controller as Box

import Board.Action
import Board.Action (Action(..))


import Connection.Controller as Connection
import Connection.Model

import DomUtils (getTargetId, extractBoxId, getMouseSelectionEvent, styleProperty, DragEvent, DnDPort)

import Html (Html)

import LocalChannel as LC

import String (split, toInt)
import List
import List ((::))

import Result

import Native.Custom.Html

type alias Board = Board.Model.Model
type alias Action = Board.Action.Action

view channel model =
  div [ style
      [ styleProperty "position" "relative"
      , styleProperty "width" "900px"
      , styleProperty "height" "600px"
      , styleProperty "border" "solid thin blue"
      , styleProperty "overflow" "hidden"
      ]
      , id "container"
      , on "dblclick" getTargetId (\v -> LC.send channel <| buildEditingAction v)
      , on "mousedown" getMouseSelectionEvent (\v -> LC.send channel <| buildSelectAction v)
    ] <| widgets model

renderConnections : List Connection.Model.Model -> List Html
renderConnections = List.map Connection.renderConnection

buildSelectAction event = let boxIdM = extractBoxId event.id in
                    case boxIdM of
                      Result.Ok key ->
                        if | event.metaKey -> SelectBoxMulti key
                           | otherwise -> SelectBox key
                      Result.Err s -> Debug.log "deselect" DeselectBoxes

buildEditingAction : String -> Action
buildEditingAction id = let boxIdM = extractBoxId id in
                   case boxIdM of
                     Result.Ok key ->
                       EditingBox key True
                     Result.Err s -> NoOp

widgets : Board -> List Html
widgets board =
  List.concatMap identity
    [ (List.map (Box.renderBox actions) board.boxes)
    , (renderConnections board.connections)
    ]

actions : Channel Action
actions = Signal.channel NoOp

checkFocus =
    let needsFocus act =
            case act of
              EditingBox key bool -> bool
              _ -> False

        toSelector (EditingBox id _) = ("#box-" ++ toString id ++ "-label")
    in
        toSelector <~ Signal.keepIf needsFocus (EditingBox 0 True) (Signal.subscribe actions)

keyboardRequest keyCode = case keyCode of
  65 -> NewBox
  67 -> ConnectSelections
  68 -> DeleteSelections
  13 -> EditingSelectedBox True
  _ -> NoOp

moveBoxAction : DragEvent -> Action
moveBoxAction event = let boxKeyM = extractBoxId event.id in
    case boxKeyM of
      Ok key ->
       if | event.isStart -> DraggingBox key
          | otherwise -> MoveBox key event
      Err s -> NoOp

startingState =
  { boxes = []
  , connections = []
  , nextIdentifier = 1
  }

isEditing = List.any (.isEditing)

updateStateSelections box state =
  let updateBox lastBox _ = Box.step (Box.Action.SetSelected <| lastBox.selectedIndex + 1) box in
  { state | boxes <- List.foldl updateBox state.boxes }

contains obj = List.any (\b -> b == obj)
containsEither obj1 obj2 = List.any (\b -> b == obj1 || b == obj2)

sortLeftToRight : List Box.Box -> List Box.Box
sortLeftToRight boxes = List.sortBy (snd << (.position)) <| List.sortBy (fst << (.position)) boxes

boxForKey : Box.BoxKey -> List Box.Box -> Box.Box
boxForKey key boxes = List.head (List.filter (\b -> b.key == key) boxes)

makeBox : Box.BoxKey -> Box.Box
makeBox identifier =
  { position = (0,0)
  , size = (100, 50)
  , label = "New Box"
  , originalLabel = "New Box"
  , key = identifier
  , isEditing = False
  , isSelected = False
  , isDragging = False
  , selectedIndex = 1
  , borderSize = 2
  }

replaceBox : List Box.Box -> Box.Box -> List Box.Box
replaceBox boxes withBox = List.map (\box ->
      if box.key == withBox.key then withBox else box) boxes

step : Action -> Board -> Board
step action state =
  let updateBoxInState action box iterator = (if iterator.key == box.key then Box.step action iterator else iterator)
      updateSelectedBoxes action iterator = (if iterator.selectedIndex > -1 then Box.step action iterator else iterator)
      performActionOnAllBoxes action = (Box.step action)
      nextSelectedIndex boxes = List.foldl (\last index -> if index > last then index + 1 else last + 1) 0 <| List.map (.selectedIndex) boxes
      selectedBoxes boxes = List.sortBy (.selectedIndex)
                        <| List.filter (\b -> b.selectedIndex > -1) boxes
      deselectBoxes = List.map (\box -> { box | selectedIndex <- -1 }) in
    case action of
      NewBox ->
        if | isEditing state.boxes -> state
           | True ->
            let newBox = makeBox state.nextIdentifier in
              Debug.log "newBox" { state | boxes <- newBox :: (deselectBoxes state.boxes)
                                         , nextIdentifier <- state.nextIdentifier + 1 }
      CancelEditingBox key ->
        let box = boxForKey key state.boxes
            cancelEditing = List.map (updateBoxInState Box.Action.CancelEditing box)
            updateBoxes = cancelEditing >> deselectBoxes in
          Debug.log "Canceling edit" { state | boxes <- updateBoxes state.boxes }
      DeselectBoxes ->
        let cancelEditing = List.map (Box.step Box.Action.CancelEditing)
            updateBoxes = cancelEditing >> deselectBoxes in
          { state | boxes <- updateBoxes state.boxes }
      SelectBoxMulti id ->
        let box = boxForKey id state.boxes
            updateBoxes boxes = List.map (updateBoxInState (Box.Action.SetSelected <| nextSelectedIndex boxes) box) boxes in
          Debug.log "adding box to selection" { state | boxes <- updateBoxes state.boxes }

      DraggingBox id ->
        let box = boxForKey id state.boxes
            selectedBox boxes = List.map (updateBoxInState (Box.Action.SetSelected <| nextSelectedIndex boxes) box) boxes
            draggingBox = List.map (updateSelectedBoxes Box.Action.Dragging)
            updateBoxes = selectedBox >> draggingBox in
            Debug.log "started dragging" { state | boxes <- updateBoxes state.boxes }

      SelectBox id ->
        let box = boxForKey id state.boxes
            selectedBox boxes = List.map (updateBoxInState (Box.Action.SetSelected <| nextSelectedIndex boxes) box) boxes
            updateBoxes = deselectBoxes >> selectedBox in
          if | box.isSelected -> state
             | otherwise ->
          Debug.log "selecting box" { state | boxes <- updateBoxes state.boxes }
      EditingBox id toggle ->
        let box = boxForKey id state.boxes in
          Debug.log "editing box" { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Editing toggle) box }
      EditingSelectedBox toggle ->
          let selectedBoxes = List.filter (\b -> b.selectedIndex /= -1) state.boxes in
            if | List.length selectedBoxes == 1 ->
              { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Editing toggle)
                                                        <| List.head selectedBoxes }
               | otherwise ->
              state
      UpdateBox box label ->
        { state | boxes <- replaceBox state.boxes <| Box.step (Box.Action.Update label) box }
      MoveBox key event ->
        let draggingBox = List.map (updateSelectedBoxes Box.Action.Dragging)
            moveAllSelectedBoxes boxes = List.map (updateSelectedBoxes (Box.Action.Move event)) boxes
            updateBoxes = moveAllSelectedBoxes >> draggingBox
        in
          Debug.log "moved box"
            step ReconnectSelections { state | boxes <- updateBoxes state.boxes }
      ReconnectSelections ->
        { state | connections <- Connection.rebuildConnections
                                  <| List.map (Connection.updateBoxes state.boxes)
                                  state.connections }
      ConnectSelections ->
        if | List.length (selectedBoxes state.boxes) < 2 -> state
           | otherwise ->
             Debug.log "Connecting Selections"
             { state | connections <- Connection.buildConnections state.connections
                                          <| Debug.log "Selected Boxes" <| selectedBoxes state.boxes }

      DeleteSelections ->
        Debug.log "Deleting Selections"
          { state | boxes <- List.filter (\b -> b.selectedIndex == -1) state.boxes,
                    connections <- List.filter
                      (\c -> c.startBox.selectedIndex == -1 &&
                        c.endBox.selectedIndex == -1)
                      state.connections }
      NoOp -> Debug.log "NoOp" state

