module Board.View (draw) where

import Debug
import Html (..)
import Html.Tags (div)
import Html.Events (ondblclick)
import Html.Attributes (id)

import Graphics.Input (Handle)

import Board.Action (Action(..))
import Board.State (Board, BoardMode(..))

import DomUtils (getTargetId, extractBoxId, getMouseSelectionEvent)

type SelectionEvent =
  { id: String
  , metaKey: Bool
  }

buildEditingAction : String -> Action
buildEditingAction id = let boxIdM = extractBoxId id in
                   case boxIdM of
                     Just key ->
                       EditingBox key True
                     Nothing -> NoOp

buildSelectAction event = let boxIdM = extractBoxId event.id in
                    case boxIdM of
                      Just key ->
                        if | event.metaKey -> SelectBoxMulti key
                           | otherwise -> SelectBox key
                      Nothing -> DeselectBoxes

draw : Board -> Handle Action -> [Html] -> Html
draw board handle widgets = div [ style
      [ prop "position" "relative"
      , prop "width" "900px"
      , prop "height" "600px"
      , prop "border" "solid thin blue"
      , prop "overflow" "hidden"
      , case board.mode of
          Connect -> prop "background-color" "#ccc"
          Normal -> prop "background-color" "white"
      ]
      , id "container"
      , on "dblclick" getTargetId handle buildEditingAction
      , on "mousedown" getMouseSelectionEvent handle buildSelectAction
    ] widgets

