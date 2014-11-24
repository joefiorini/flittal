module Board.View (draw) where

import Debug
import Html (..)
import Html.Tags (div)
import Html.Events (ondblclick)
import Html.Attributes (id)

import Graphics.Input (Handle)

import Board.Action (Action(..))

import DomUtils (getTargetId, extractBoxId)

buildAction : String -> Action
buildAction id = let boxIdM = extractBoxId id in
                   case boxIdM of
                     Just key -> EditingBox key True
                     Nothing -> NoOp

draw : Handle Action -> [Html] -> Html
draw handle widgets = Debug.log "draw" <| div [ style
      [ prop "position" "relative"
      , prop "width" "900px"
      , prop "height" "600px"
      , prop "border" "solid thin blue"
      , prop "overflow" "hidden"
      ]
      , id "container"
      , on "dblclick" getTargetId handle buildAction
    ] widgets

