module Connection.Controller where

import Html (..)
import Html.Attributes (class, style)

import Geometry.Types (Geometric, Point, toPxPoint, toPx)

import Box.Model
import Connection.Model (Model, Line, LineLayout(..), ConnectionPort, PortLocation(..), PortOrder(..))
import Box.Controller

import Debug

import DomUtils (styleProperty)

import List
import List ((::))

type alias Box = Box.Model.Model

renderConnection : Model -> Html
renderConnection connection =
  div [class "connection"] <| drawEndpoint connection.endPort :: List.map drawSegment connection.segments

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

endpointWidth = 10
endpointHeight = 10

offsetMidpoint x = if x % 2 /= 0 then x - 1 else x
endpointMidX = offsetMidpoint <| endpointWidth // 2
endpointMidY = offsetMidpoint <| endpointHeight // 2

drawEndpoint : PortLocation -> Html
drawEndpoint p =
  let points = case p of
                Top (x,y) ->
                  (x - endpointMidX, y - endpointHeight)
                Right (x,y) ->
                  (x, y - endpointMidY)
                Bottom (x,y) ->
                  (x - endpointMidX, y)
                Left (x,y) ->
                  (x - endpointWidth, y - endpointMidY)
  in
  div [style
        [ styleProperty "position" "absolute"
        , styleProperty "background-color" "black"
        , styleProperty "width" <| toPx endpointWidth
        , styleProperty "height" <| toPx endpointHeight
        , styleProperty "top" <| snd
                              <| toPxPoint points
        , styleProperty "left" <| fst
                               <| toPxPoint points
        ]
      ] []

below : Point -> Point -> Bool
(x1,y1) `below` (x2,y2) = y1 > y2

leftOf : Point -> Point -> Bool
(x1,y1) `leftOf` (x2,y2) = x1 < x2

midPoint : Int -> Int
midPoint c =
  let offset n = if n % 2 == 0 then n - 1 else n in
     offset <| c // 2

rightPort : Box -> PortLocation
rightPort {position,size,borderSize} =
  let offset = borderSize * 2
      (x,y) = position
      (w,h) = size in
      Right (w + x + offset, y + midPoint h)


leftPort : Box -> PortLocation
leftPort {position,size} =
  let (x,y) = position
      (w,h) = size in
      Left (x, y + midPoint h)


bottomPort : Box -> PortLocation
bottomPort {position,size,borderSize} =
  let offset = borderSize * 2
      (x,y) = position
      (w,h) = size in
      Bottom (x + midPoint w, y + h + offset)

topPort : Box -> PortLocation
topPort {position,size} =
  let (x,y) = position
      (w,h) = size in
      Top (x + midPoint w, y)

portLocations : Box -> Box -> ConnectionPort
portLocations leftBox rightBox =
  let p1 = leftBox.position
      p2 = rightBox.position
      (x1,y1) = p1
      (x2,y2) = p2
      output = Debug.log "p1,p2" (p1,p2)
      (w1,h1) = leftBox.size
      (w2,h2) = rightBox.size in
  if | p1 `below` p2 && p1 `leftOf` p2 ->
      {start=rightPort leftBox, end=bottomPort rightBox, order=StartEnd}
     | p1 `below` p2 && p2 `leftOf` p1 ->
       {start=leftPort leftBox, end=bottomPort rightBox, order=EndStart}
     | p2 `below` p1 && p1 `leftOf` p2 ->
       {start=bottomPort leftBox, end=leftPort rightBox, order=StartEnd}
     | p2 `below` p1 && p2 `leftOf` p1 ->
       {start=bottomPort leftBox, end=rightPort rightBox, order=EndStart}
     | p1 `leftOf` p2 ->
       {start=rightPort leftBox, end=leftPort rightBox, order=StartEnd}
     | p1 `below` p2 ->
       {start=topPort leftBox, end=bottomPort rightBox,order=EndStart}
     | p2 `below` p1 ->
       {start=bottomPort leftBox, end=topPort rightBox, order=StartEnd}

lineSize = 2

buildSegments : ConnectionPort -> List Line
buildSegments {start,end,order} =
  let horizontalSegment (x1,y1) (x2,y2) =
        { position = (x1, y1)
        , size = (x2 - x1, lineSize)
        , layout = Horizontal }
      verticalSegment (x1,y1) (x2,y2) =
        { position = if (x1,y1) `below` (x2,y2) then (x2,y2) else (x1,y1)
        , size = (lineSize, ((+) lineSize <| if (x1,y1) `below` (x2,y2) then y1 - y2 else  y2 - y1))
        , layout = Vertical
        } in
  case Debug.log "buildSegments from" (start,end) of
    (Right p1, Left p2) ->
      if | p1 `below` p2 ->
          [horizontalSegment p1 p2, verticalSegment p1 p2, horizontalSegment p1 p2]
         | otherwise ->
          [horizontalSegment p1 p2]
    (Right p1, Bottom p2) ->
      [horizontalSegment p1 p2, verticalSegment p1 p2]
    (Bottom p1, Right p2) ->
      let (x1,y1) = p1
          (x2,y2) = p2 in
      [verticalSegment p1 p2, horizontalSegment (x2, y2) (x1, y2)]
    (Bottom p1, Left p2) ->
      let (x1,y1) = p1
          (x2,y2) = p2 in
      [verticalSegment p1 p2, horizontalSegment (x1, y2) p2]
    (Left p1, Bottom p2) ->
      let (x1,y1) = p1
          (x2,y2) = p2 in
      [horizontalSegment (x2, y1) p1, verticalSegment (x2, y1) p2]
    (Bottom p1, Top p2) ->
      -- if | p1 `leftOf` p2 ->
      --   let (x1,y1) = p1
      --       (x2,y2) = p2 in
      --   [ verticalSegment (x1, y1) (midPoint x1, midPoint y1)
      --   , horizontalSegment (midPoint x1, midPoint y1) (x1, midPoint y1)
      --   , verticalSegment (midPoint x1, midPoint y1) (x2, y2) ]
      -- | otherwise ->
        [verticalSegment p1 p2]
    (Top p1, Bottom p2) ->
      [verticalSegment p1 p2]


boxMap : (Box -> Box -> Model) -> List Box -> List Model -> List Model
boxMap f boxes connections =
  List.map (\c ->
    let startBox = List.head
        <| List.filter (\b -> b.key == c.startBox) boxes
        endBox = List.head
        <| List.filter (\b -> b.key == c.endBox) boxes
    in
       f startBox endBox
     ) connections



buildConnections : List Model -> List Box -> List Model
buildConnections connections boxes =
  snd (List.foldl connectBoxesFold (List.head boxes, connections) (List.tail boxes))


connectBoxes : Box -> Box -> Model
connectBoxes startBox endBox =
  { segments = buildSegments <| portLocations startBox endBox
  , startPort = (.start) <| portLocations startBox endBox
  , endPort = (.end) <| portLocations startBox endBox
  , startBox = startBox.key
  , endBox = endBox.key }


connectBoxesFold : Box -> (Box, List Model) -> (Box, List Model)
connectBoxesFold rightBox (leftBox, connections)  =
  let newConnection = { segments = buildSegments <| portLocations leftBox rightBox
                      , startPort = (.start) <| portLocations leftBox rightBox
                      , endPort = (.end) <| portLocations leftBox rightBox
                      , startBox = leftBox.key
                      , endBox = rightBox.key } in
  (rightBox, newConnection :: connections)
