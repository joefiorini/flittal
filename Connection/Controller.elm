module Connection.Controller where

import Html (..)
import Html.Tags (div)
import Html.Attributes (class)

import Geometry.Types (Geometric, Point, toPxPoint)

import Connection.State (Connection, Line, LineLayout(..))
import Box.Controller as Box

import Debug

type State = Connection

data Port = Right Point
            | Bottom Point
            | Left Point
            | Top Point

renderConnection : Connection -> Html
renderConnection connection =
  div [class "connection"] <| map drawSegment connection.segments

drawSegment : Line -> Html
drawSegment line =
  div [style
      [ prop "position" "absolute"
      , prop "width" <| fst
                     <| toPxPoint line.size
      , prop "height" <| snd
                      <| toPxPoint line.size
      , prop "background-color" "black"
      , prop "top" <| snd
                   <| toPxPoint line.position
      , prop "left" <| fst
                    <| toPxPoint line.position
      ]
  ] []

below : Point -> Point -> Bool
(x1,y1) `below` (x2,y2) = y1 > y2

leftOf : Point -> Point -> Bool
(x1,y1) `leftOf` (x2,y2) = x1 < x2

midPoint : Int -> Int
midPoint c = round <| (toFloat c) / 2.0

rightPort : Box.State -> Port
rightPort {position,size} =
  let (x,y) = position
      (w,h) = size in
      Right (w + x, y + midPoint h)


leftPort : Box.State -> Port
leftPort {position,size} =
  let (x,y) = position
      (w,h) = size in
      Left (x, y + midPoint h)


bottomPort : Box.State -> Port
bottomPort {position,size} =
  let (x,y) = position
      (w,h) = size in
      Bottom (x + midPoint w, y + h)

topPort : Box.State -> Port
topPort {position,size} =
  let (x,y) = position
      (w,h) = size in
      Top (x + midPoint w, y)

portLocations : Box.State -> Box.State -> (Port, Port)
portLocations leftBox rightBox =
  let p1 = leftBox.position
      p2 = rightBox.position
      (x1,y1) = p1
      (x2,y2) = p2
      (w1,h1) = leftBox.size
      (w2,h2) = rightBox.size in
  if | p1 `below` p2 && p1 `leftOf` p2 ->
    (rightPort leftBox, bottomPort rightBox)
     | p2 `below` p1 && p1 `leftOf` p2 ->
       (bottomPort leftBox, leftPort rightBox)
     | p2 `below` p1 && p2 `leftOf` p1 ->
       (rightPort rightBox, bottomPort leftBox)
     | p1 `leftOf` p2 ->
       (rightPort leftBox, leftPort rightBox)
     | p1 `below` p2 ->
       (topPort rightBox, bottomPort leftBox)
     | p2 `below` p1 ->
       (bottomPort leftBox, topPort rightBox)

buildSegments : (Port, Port) -> [Line]
buildSegments ports =
  let horizontalSegment (x1,y1) (x2,y2) =
        { position = (x1 + 2, y1)
        , size = (x2 - x1,2)
        , layout = Horizontal }
      verticalSegment (x1,y1) (x2,y2) =
        { position = if (x1,y1) `below` (x2,y2) then (x2,y2 + 2) else (x1,y1 + 2)
        , size = (2, if (x1,y1) `below` (x2,y2) then y1 - y2 else  y2 - y1)
        , layout = Vertical
        } in
  case Debug.log "buildSegments from" ports of
    (Right p1, Left p2) ->
      if | p1 `below` p2 ->
          [horizontalSegment p1 p2, verticalSegment p1 p2, horizontalSegment p1 p2]
         | otherwise ->
          [horizontalSegment p1 p2]
    (Right p1, Bottom p2) ->
      [horizontalSegment p1 p2, verticalSegment p1 p2]
    (Bottom p1, Left p2) ->
      let (x1,y1) = p1
          (x2,y2) = p2 in
      [verticalSegment p1 p2, horizontalSegment (x1, y2) p2]
    (Bottom p1, Top p2) ->
      -- if | p1 `leftOf` p2 ->
      --   let (x1,y1) = p1
      --       (x2,y2) = p2 in
      --   [ verticalSegment (x1, y1) (midPoint x1, midPoint y1)
      --   , horizontalSegment (midPoint x1, midPoint y1) (x1, midPoint y1)
      --   , verticalSegment (midPoint x1, midPoint y1) (x2, y2) ]
      -- | otherwise ->
        [verticalSegment p1 p2]

connectBoxes : Box.State -> (Box.State, [Connection]) -> (Box.State, [Connection])
connectBoxes rightBox (leftBox, connections)  =
  let newConnection = { segments = buildSegments <| portLocations leftBox rightBox,
                        startBox = leftBox,
                        endBox = rightBox } in
  (rightBox, newConnection :: connections)
