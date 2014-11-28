module Connection.Controller where

import Html (..)
import Html.Attributes (class, style)

import Geometry.Types (Geometric, Point, toPxPoint)

import Connection.State (Connection, Line, LineLayout(..))
import Box.Controller as Box

import Debug

import DomUtils (styleProperty)

import List
import List ((::))

type alias State = Connection

type Port = Right Point
            | Bottom Point
            | Left Point
            | Top Point

renderConnection : Connection -> Html
renderConnection connection =
  div [class "connection"] <| List.map drawSegment connection.segments

drawSegment : Line -> Html
drawSegment line =
  div [style
      [ styleProperty "position" "absolute"
      , styleProperty "width" <| fst
                     <| toPxPoint line.size
      , styleProperty "height" <| snd
                      <| toPxPoint line.size
      , styleProperty "background-color" "black"
      , styleProperty "top" <| snd
                   <| toPxPoint line.position
      , styleProperty "left" <| fst
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

buildSegments : (Port, Port) -> List Line
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

buildConnections : List Connection -> List Box.State -> List Connection
buildConnections connections boxes =
  snd (List.foldl connectBoxes (List.head boxes, connections) (List.tail boxes))

connectBoxes : Box.State -> (Box.State, List Connection) -> (Box.State, List Connection)
connectBoxes rightBox (leftBox, connections)  =
  let newConnection = { segments = buildSegments <| portLocations leftBox rightBox,
                        startBox = leftBox,
                        endBox = rightBox } in
  (rightBox, newConnection :: connections)
