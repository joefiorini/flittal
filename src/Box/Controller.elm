module Box.Controller exposing (..)

import Box.Msg exposing (..)
import Box.Types exposing (..)
import Dom.Types exposing (DragEvent)
import DomUtils exposing (boolProperty, class_, styleProperty)
import Geometry.Types as Geometry
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, id, property, style, type_, value)
import Html.Events exposing (keyCode, on, onInput, onWithOptions, targetValue)
import Json.Decode as Json exposing (field)
import Style.Color exposing (..)


view : Model -> Html Msg
view box =
    div
        [ style
            [ styleProperty "position" "absolute"
            , styleProperty "width" (Tuple.first <| Geometry.toPxPoint box.size)
            , styleProperty "height" (Tuple.second <| Geometry.toPxPoint box.size)
            , if box.selectedIndex > -1 && not box.isDragging then
                styleProperty "border" "dashed 2px"
              else
                styleProperty "border" "solid 2px"
            , styleProperty "left" (Tuple.first <| Geometry.toPxPoint box.position)
            , styleProperty "top" (Tuple.second <| Geometry.toPxPoint box.position)
            , styleProperty "text-align" "center"
            , styleProperty "line-height" "2"
            ]
        , autofocus True
        , boolProperty "draggable" True
        , id <| "box-" ++ (toString box.key)
        , onInput (\v -> (UpdateBox box v))
        , class_ <| boxClasses box
        , onKeyDown box
        ]
        [ if box.isEditing then
            labelField box box.label
          else
            text box.label
        ]


boxClassForColor : Color -> String
boxClassForColor color =
    case color of
        Dark1 ->
            "box--dark1"

        Dark2 ->
            "box--dark2"

        Dark3 ->
            "box--dark3"

        Dark4 ->
            "box--dark4"

        Light1 ->
            "box--light1"

        Light2 ->
            "box--light2"

        Light3 ->
            "box--light3"

        Light4 ->
            "box--light4"

        Black ->
            "box--black"

        White ->
            "box--white"


boxClasses : Model -> List String
boxClasses box =
    [ "box"
    , boxClassForColor box.style.color
    ]


entersEditMode : Msg -> Bool
entersEditMode update =
    case update of
        EditingBox _ toggle ->
            toggle

        CancelEditingBox _ ->
            False

        CancelEditing ->
            False

        otherwise ->
            False


onKeyDown : Model -> Attribute Msg
onKeyDown box =
    let
        checkKeyCode keyCode =
            (case keyCode of
                -- TODO: Kill at least the 13 in this one
                13 ->
                    EditingBox box False

                27 ->
                    CancelEditingBox box

                _ ->
                    NoOp
            )
    in
        on "keydown" (Json.map checkKeyCode keyCode)


keyCodeAndValue : Json.Decoder ( Int, String )
keyCodeAndValue =
    Json.map2 (,)
        (field "keyCode" Json.int)
        (Json.at [ "target", "value" ] Json.string)


extractLabelUpdate : Model -> ( Int, String ) -> Msg
extractLabelUpdate box ( keyCode, value ) =
    if keyCode == 13 then
        CancelEditingBox box
    else
        UpdateBox box value


labelField : Model -> String -> Html Msg
labelField box label =
    let
        nullHandler =
            NoOp
    in
        input
            [ id <| "box-" ++ toString box.key ++ "-label"
            , type_ "text"
            , value label
            , onWithOptions "mousedown" { stopPropagation = True, preventDefault = True } (Json.succeed nullHandler)
            ]
            []


moveBox : Int -> MoveDirection -> Model -> Geometry.Point
moveBox amount direction box =
    let
        ( x, y ) =
            box.position
    in
        case direction of
            Up ->
                ( x, y - amount )

            Down ->
                ( x, y + amount )

            Left ->
                ( x - amount, y )

            Right ->
                ( x + amount, y )


moveBoxDrag : DragEvent -> Model -> Model
moveBoxDrag { id, isStart, isEnd, isDrop, startX, startY, endX, endY } box =
    let
        offsetX =
            (Tuple.first box.position) - startX

        offsetY =
            (Tuple.second box.position) - startY

        newX =
            endX + offsetX

        newY =
            endY + offsetY
    in
        { box | position = ( newX, newY ) }


labelSelector : Model -> String
labelSelector box =
    "#box-" ++ toString box.key ++ "-label"


resize : ResizeMode -> Model -> ( Geometry.Point, Geometry.Size )
resize mode box =
    let
        ( width, height ) =
            box.size

        ( left, top ) =
            box.position

        maxWidth =
            280

        minWidth =
            90

        maxHeight =
            180

        minHeight =
            40
    in
        case mode of
            ResizeUpAll ->
                if (width >= maxWidth) || (height >= maxHeight) then
                    ( box.position, box.size )
                else
                    ( ( left - 5, top - 5 )
                    , ( 10 + width, 10 + height )
                    )

            ResizeDownAll ->
                if (width <= minWidth) || (height <= minHeight) then
                    ( box.position, box.size )
                else
                    ( ( left + 5, top + 5 )
                    , ( width - 10, height - 10 )
                    )

            ResizeUpNS ->
                if height >= maxHeight then
                    ( box.position, box.size )
                else
                    ( ( left, top - 5 )
                    , ( width, 10 + height )
                    )

            ResizeDownNS ->
                if height <= minHeight then
                    ( box.position, box.size )
                else
                    ( ( left, top + 5 )
                    , ( width, height - 10 )
                    )

            ResizeUpEW ->
                if width >= maxWidth then
                    ( box.position, box.size )
                else
                    ( ( left - 5, top )
                    , ( width + 10, height )
                    )

            ResizeDownEW ->
                if width <= minWidth then
                    ( box.position, box.size )
                else
                    ( ( left + 5, top )
                    , ( width - 10, height )
                    )


step : Msg -> Model -> Model
step update box =
    case update of
        Drop event ->
            moveBoxDrag event box

        SetSelected index ->
            { box | selectedIndex = index }

        CancelEditing ->
            { box
                | label = box.originalLabel
                , isEditing = False
            }

        Editing toggle ->
            { box | isEditing = toggle, originalLabel = box.label }

        Update newLabel ->
            { box | label = newLabel }

        Dragging ->
            { box
                | isDragging =
                    if box.isDragging then
                        False
                    else
                        True
            }

        UpdateColor color ->
            let
                style =
                    box.style

                style_ =
                    { style | color = color }
            in
                { box | style = style_ }

        Resize mode ->
            let
                ( position_, size_ ) =
                    resize mode box
            in
                { box | size = size_, position = position_ }

        Move mode direction ->
            let
                moveBox_ =
                    case mode of
                        Nudge ->
                            moveBox 10

                        Push ->
                            moveBox 100

                        Jump ->
                            moveBox 300
            in
                { box | position = moveBox_ direction box }

        CancelEditingBox _ ->
            box

        UpdateBox _ _ ->
            box

        EditingBox _ _ ->
            box

        NoOp ->
            box
