module Board.View (draw) where

import Debug
import Html (..)
import Html.Tags (div)
import Html.Events (ondblclick)
import Html.Attributes (id)

import Graphics.Input (Handle)

import Board.Action (Action(..))

import DomUtils (getTargetId)

buildAction : String -> Action
buildAction id = EditBox id

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

