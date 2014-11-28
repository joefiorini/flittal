module Board.View (draw) where

import Debug
import Html (..)
import Html.Events (on)
import Html.Attributes (id, style, property)

import Signal (Channel, send)

import Board.Action (Action(..))
import Board.State (Board, BoardMode(..))

import Result

import DomUtils (getTargetId, extractBoxId, getMouseSelectionEvent, styleProperty)

type alias SelectionEvent =
  { id: String
  , metaKey: Bool
  }

buildEditingAction : String -> Action
buildEditingAction id = let boxIdM = extractBoxId id in
                   case boxIdM of
                     Result.Ok key ->
                       EditingBox key True
                     Result.Err s -> NoOp

buildSelectAction event = let boxIdM = extractBoxId event.id in
                    case boxIdM of
                      Result.Ok key ->
                        if | event.metaKey -> SelectBoxMulti key
                           | otherwise -> SelectBox key
                      Result.Err s -> Debug.log "deselect" DeselectBoxes

draw : Board -> Channel Action -> List Html -> Html
draw board handle widgets = div [ style
      [ styleProperty "position" "relative"
      , styleProperty "width" "900px"
      , styleProperty "height" "600px"
      , styleProperty "border" "solid thin blue"
      , styleProperty "overflow" "hidden"
      , case board.mode of
          Connect -> styleProperty "background-color" "#ccc"
          Normal ->  styleProperty "background-color" "white"
      ]
      , id "container"
      , on "dblclick" getTargetId (\v -> send handle <| buildEditingAction v)
      , on "mousedown" getMouseSelectionEvent (\v -> send handle <| buildSelectAction v)
    ] widgets

