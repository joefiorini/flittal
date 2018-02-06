module Connection.Controller exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Geometry.Types exposing (Geometric, Point, toPxPoint, toPx)
import Box.Model
import Connection.Model exposing (Model, Line, LineLayout(..), ConnectionPort, PortLocation(..), PortOrder(..))
import Debug
import DomUtils exposing (styleProperty)
import List
import List exposing ((::))


type alias Box =
    Box.Model.Model


renderConnection : Model -> Html msg
renderConnection connection =
    div [ class "connection" ] <| drawEndpoint connection.endPort :: List.map drawSegment connection.segments


drawSegment : Line -> Html msg
drawSegment line =
    div
        [ style
            [ styleProperty "position" "absolute"
            , styleProperty "width" <|
                Tuple.first <|
                    toPxPoint line.size
            , styleProperty "height" <|
                Tuple.second <|
                    toPxPoint line.size
            , styleProperty "background-color" "black"
            , styleProperty "top" <|
                Tuple.second <|
                    toPxPoint line.position
            , styleProperty "left" <|
                Tuple.first <|
                    toPxPoint line.position
            ]
        ]
        []


endpointWidth =
    10


endpointHeight =
    10


offsetMidpoint x =
    if x % 2 /= 0 then
        x - 1
    else
        x


endpointMidX =
    offsetMidpoint <| endpointWidth // 2


endpointMidY =
    offsetMidpoint <| endpointHeight // 2


drawEndpoint : PortLocation -> Html msg
drawEndpoint p =
    let
        points =
            case p of
                Top ( x, y ) ->
                    ( x - endpointMidX, y - endpointHeight )

                Right ( x, y ) ->
                    ( x, y - endpointMidY )

                Bottom ( x, y ) ->
                    ( x - endpointMidX, y )

                Left ( x, y ) ->
                    ( x - endpointWidth, y - endpointMidY )
    in
        div
            [ style
                [ styleProperty "position" "absolute"
                , styleProperty "background-color" "black"
                , styleProperty "width" <| toPx endpointWidth
                , styleProperty "height" <| toPx endpointHeight
                , styleProperty "top" <|
                    Tuple.second <|
                        toPxPoint points
                , styleProperty "left" <|
                    Tuple.first <|
                        toPxPoint points
                ]
            ]
            []


below : Point -> Point -> Bool
below ( x1, y1 ) ( x2, y2 ) =
    y1 > y2


leftOf : Point -> Point -> Bool
leftOf ( x1, y1 ) ( x2, y2 ) =
    x1 < x2



-- (isPort <| ((leftBox `leftOf2` rightBox) `within` heightThreshold))


type alias Calculation =
    { result : Int -> Int -> Bool
    , clampedResult : Int -> Int -> Int -> Int -> Bool
    , location1 : Int
    , location2 : Int
    , lowerClamp : Maybe Int
    , upperClamp : Maybe Int
    }


map2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 f ma mb =
    case ma of
        Just a_ ->
            case mb of
                Just b_ ->
                    Just <| f a_ b_

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


mlt : Maybe Int -> Maybe Int -> Maybe Bool
mlt a b =
    map2 (\n1 n2 -> n1 < n2) a b


within : Calculation -> Int -> Calculation
within calculation threshold =
    { calculation
        | lowerClamp = Just <| Debug.log "lowerClamp" (calculation.location1 - threshold)
        , upperClamp = Just <| Debug.log "upperClamp" (calculation.location2 + threshold)
    }


onBoxes box1 box2 connection =
    (connection.startBox == box1.key || connection.endBox == box1.key)
        && (connection.startBox == box2.key || connection.endBox == box2.key)


isPort : Calculation -> Bool
isPort { result, clampedResult, lowerClamp, upperClamp, location1, location2 } =
    let
        result_ =
            map2 (clampedResult location1 location2) lowerClamp upperClamp
    in
        case result_ of
            Just n ->
                n

            Nothing ->
                result location1 location2


midPoint : Int -> Int
midPoint c =
    let
        offset n =
            if n % 2 == 0 then
                n - 1
            else
                n
    in
        offset <| c // 2


rightPort : Box -> PortLocation
rightPort { position, size } =
    let
        ( x, y ) =
            position

        ( w, h ) =
            size
    in
        Right ( w + x, y + midPoint h )


leftPort : Box -> PortLocation
leftPort { position, size } =
    let
        ( x, y ) =
            position

        ( w, h ) =
            size
    in
        Left ( x, y + midPoint h )


bottomPort : Box -> PortLocation
bottomPort { position, size } =
    let
        ( x, y ) =
            position

        ( w, h ) =
            size
    in
        Bottom ( x + midPoint w, y + h )


topPort : Box -> PortLocation
topPort { position, size } =
    let
        ( x, y ) =
            position

        ( w, h ) =
            size
    in
        Top ( x + midPoint w, y )


portLocations : Box -> Box -> ConnectionPort
portLocations leftBox rightBox =
    let
        p1 =
            leftBox.position

        p2 =
            rightBox.position

        ( x1, y1 ) =
            p1

        ( x2, y2 ) =
            p2

        output =
            ( p1, p2 )

        leftHeight =
            Tuple.second leftBox.size

        rightHeight =
            Tuple.second rightBox.size

        leftWidth =
            Tuple.first leftBox.size

        rightWidth =
            Tuple.first rightBox.size

        heightThreshold =
            if leftHeight > rightHeight then
                leftHeight
            else
                rightHeight

        widthThreshold =
            if leftWidth > rightWidth then
                leftWidth
            else
                rightWidth

        ( w1, h1 ) =
            leftBox.size

        ( w2, h2 ) =
            rightBox.size

        heightDiff =
            abs <| y1 - y2

        widthDiff =
            abs <| x1 - x2

        threshold =
            25

        maxHeight =
            (max h1 h2)

        maxWidth =
            (max w1 w2)
    in
        if below p1 p2 && leftOf p1 p2 then
            Debug.log "p1 below p2 && p1 leftOf p2" <|
                (if widthDiff <= maxWidth then
                    { start = topPort leftBox, end = bottomPort rightBox, order = StartEnd }
                 else if heightDiff > maxHeight then
                    { start = rightPort leftBox, end = bottomPort rightBox, order = StartEnd }
                 else
                    { start = rightPort leftBox, end = leftPort rightBox, order = StartEnd }
                )
        else if below p1 p2 && leftOf p2 p1 then
            -- left leftBox - bottom rightBox
            -- left leftBox - right rightBox
            Debug.log "p1 below p2 && p2 leftOf p1" <|
                (if widthDiff <= maxWidth then
                    { start = topPort leftBox, end = bottomPort rightBox, order = EndStart }
                 else if heightDiff > maxHeight then
                    { start = leftPort leftBox, end = bottomPort rightBox, order = EndStart }
                 else
                    { start = rightPort rightBox, end = leftPort leftBox, order = EndStart }
                )
        else if below p2 p1 && leftOf p1 p2 then
            Debug.log "p2 below p1 && p1 leftOf p2" <|
                (if widthDiff <= maxWidth then
                    { start = bottomPort leftBox, end = topPort rightBox, order = StartEnd }
                 else if heightDiff > maxHeight then
                    { start = bottomPort leftBox, end = leftPort rightBox, order = StartEnd }
                 else
                    { start = rightPort leftBox, end = leftPort rightBox, order = StartEnd }
                )
        else if below p2 p1 && leftOf p2 p1 then
            Debug.log "p2 below p1 && p2 leftOf p1" <|
                (if widthDiff <= maxWidth then
                    { start = bottomPort leftBox, end = topPort rightBox, order = EndStart }
                 else if heightDiff > maxHeight then
                    { start = bottomPort leftBox, end = rightPort rightBox, order = EndStart }
                 else
                    { start = rightPort rightBox, end = leftPort leftBox, order = EndStart }
                )
        else if leftOf p1 p2 then
            Debug.log "p1 leftOf p2" { start = rightPort leftBox, end = leftPort rightBox, order = StartEnd }
        else if leftOf p2 p1 then
            Debug.log "p2 leftOf p1" { start = rightPort rightBox, end = leftPort leftBox, order = EndStart }
        else if below p1 p2 then
            Debug.log "p1 below p2" { start = topPort leftBox, end = bottomPort rightBox, order = EndStart }
        else if below p2 p1 then
            Debug.log "p2 below p1" { start = bottomPort leftBox, end = topPort rightBox, order = StartEnd }
        else
            Debug.crash ("cases exhausted in portLocations:\np1:" ++ (toString p1) ++ "\np2:" ++ (toString p2))


lineSize =
    2


buildSegments : ConnectionPort -> List Line
buildSegments { start, end, order } =
    let
        horizontalSegment ( x1, y1 ) ( x2, y2 ) =
            { position = ( x1, y1 )
            , size = ( abs <| x2 - x1, lineSize )
            , layout = Horizontal
            }

        verticalSegment ( x1, y1 ) ( x2, y2 ) =
            { position =
                if below ( x1, y1 ) ( x2, y2 ) then
                    ( x2, y2 )
                else
                    ( x1, y1 )
            , size =
                ( lineSize
                , ((+) lineSize <|
                    if below ( x1, y1 ) ( x2, y2 ) then
                        y1 - y2
                    else
                        y2 - y1
                  )
                )
            , layout = Vertical
            }
    in
        Debug.log "drawing segments" <|
            case Debug.log "buildSegments from" ( start, end ) of
                ( Right p1, Left p2 ) ->
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2

                        midx =
                            midPoint <| x2 + x1

                        yVals =
                            case order of
                                StartEnd ->
                                    ( y1, y2 )

                                EndStart ->
                                    ( y2, y1 )
                    in
                        Debug.log "StartEnd" <|
                            if y1 == y2 then
                                [ horizontalSegment p1 p2 ]
                            else if y1 > y2 then
                                [ horizontalSegment p1 ( midx, y1 )
                                , verticalSegment ( midx, Tuple.first yVals ) ( midx, Tuple.second yVals )
                                , horizontalSegment ( midx, y2 ) p2
                                ]
                            else
                                [ horizontalSegment p1 ( midx, y1 )
                                , verticalSegment ( midx, Tuple.second yVals ) ( midx, Tuple.first yVals )
                                , horizontalSegment ( midx, y2 ) p2
                                ]

                ( Right p1, Bottom p2 ) ->
                    [ horizontalSegment p1 p2, verticalSegment p1 p2 ]

                ( Bottom p1, Right p2 ) ->
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2
                    in
                        [ verticalSegment p1 p2, horizontalSegment ( x2, y2 ) ( x1, y2 ) ]

                ( Bottom p1, Left p2 ) ->
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2
                    in
                        [ verticalSegment p1 p2, horizontalSegment ( x1, y2 ) p2 ]

                ( Left p1, Bottom p2 ) ->
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2
                    in
                        [ horizontalSegment ( x2, y1 ) p1, verticalSegment ( x2, y1 ) p2 ]

                ( Bottom p1, Top p2 ) ->
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2

                        midy =
                            midPoint <| y2 + y1
                    in
                        if x1 == x2 then
                            [ verticalSegment p1 p2 ]
                        else if x1 > x2 then
                            [ verticalSegment p1 ( x1, midy )
                            , horizontalSegment ( x2, midy ) ( x1, midy )
                            , verticalSegment ( x2, midy ) p2
                            ]
                        else
                            [ verticalSegment p1 ( x1, midy )
                            , horizontalSegment ( x1, midy ) ( x2, midy )
                            , verticalSegment ( x2, midy ) p2
                            ]

                ( Top p1, Bottom p2 ) ->
                    let
                        ( x1, y1 ) =
                            p1

                        ( x2, y2 ) =
                            p2

                        midy =
                            midPoint <| y2 + y1
                    in
                        if x1 == x2 then
                            [ verticalSegment p1 p2 ]
                        else if x1 > x2 then
                            [ verticalSegment p1 ( x1, midy )
                            , horizontalSegment ( x2, midy ) ( x1, midy )
                            , verticalSegment ( x2, midy ) p2
                            ]
                        else
                            [ verticalSegment p1 ( x1, midy )
                            , horizontalSegment ( x1, midy ) ( x2, midy )
                            , verticalSegment ( x2, midy ) p2
                            ]

                otherwise ->
                    Debug.crash ("cases exhausted in buildSegments" ++ toString ( start, end ))



-- TODO: Fix the type error in "f startBox endBox"
-- boxMap : (Box -> Box -> Model) -> List Box -> List Model -> List Model
-- boxMap f boxes connections =
--     List.map
--         (\c ->
--             let
--                 startBox =
--                     List.head <|
--                         List.filter (\b -> b.key == c.startBox) boxes
--                 endBox =
--                     List.head <|
--                         List.filter (\b -> b.key == c.endBox) boxes
--             in
--                 f startBox endBox
--         )
--         connections
-- TODO: Fix type error in List.foldl
-- buildConnections : List Model -> List Box -> List Model
-- buildConnections connections boxes =
--     Tuple.second (List.foldl connectBoxesFold ( List.head boxes, connections ) (List.tail boxes))


connectBoxes : Box -> Box -> Model
connectBoxes startBox endBox =
    { segments = buildSegments <| portLocations startBox endBox
    , startPort = (.start) <| portLocations startBox endBox
    , endPort = (.end) <| portLocations startBox endBox
    , startBox = startBox.key
    , endBox = endBox.key
    }


connectBoxesFold : Box -> ( Box, List Model ) -> ( Box, List Model )
connectBoxesFold rightBox ( leftBox, connections ) =
    let
        newConnection =
            { segments = buildSegments <| portLocations leftBox rightBox
            , startPort = (.start) <| portLocations leftBox rightBox
            , endPort = (.end) <| portLocations leftBox rightBox
            , startBox = leftBox.key
            , endBox = rightBox.key
            }
    in
        ( rightBox, newConnection :: connections )
