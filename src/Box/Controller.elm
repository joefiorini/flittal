module Box.Controller exposing (..)

import DomUtils
import DomUtils exposing (DragEvent, stopPropagation, styleProperty, class_, boolProperty)
import Geometry.Types exposing (toPxPoint)
import Html exposing (..)
import Html.Attributes exposing (id, class, autofocus, style, property, type_, value)
import Html.Events exposing (on, keyCode, targetValue, onWithOptions, onInput)
import Box.Model as Model
import Debug
import Json.Decode exposing (field)
import Json.Decode as Json
import Style.Color exposing (..)
import Geometry.Types as Geometry


type alias Model =
    Model.Model


type alias BoxKey =
    Model.BoxKey


type ResizeMode
    = ResizeUpAll
    | ResizeDownAll
    | ResizeUpNS
    | ResizeDownNS
    | ResizeUpEW
    | ResizeDownEW


type MoveType
    = Nudge
    | Push
    | Jump


type MoveDirection
    = Up
    | Down
    | Left
    | Right


type Msg
    = Drop DragEvent
    | Editing Bool
    | EditingBox Model Bool
    | CancelEditingBox Model
    | CancelEditing
    | SetSelected Int
    | Dragging
    | UpdateBox Model String
    | UpdateColor Color
    | NoOp
    | Resize ResizeMode
    | Update String
    | Move MoveType MoveDirection


filterKey =
    Model.filterKey


isSelected =
    Model.isSelected


view : Model -> Html Msg
view box =
    div
        [ style
            [ styleProperty "position" "absolute"
            , styleProperty "width" (Tuple.first <| toPxPoint box.size)
            , styleProperty "height" (Tuple.second <| toPxPoint box.size)
            , if box.selectedIndex > -1 && not box.isDragging then
                styleProperty "border" "dashed 2px"
              else
                styleProperty "border" "solid 2px"
            , styleProperty "left" (Tuple.first <| toPxPoint box.position)
            , styleProperty "top" (Tuple.second <| toPxPoint box.position)
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


boxClasses box =
    [ "box"
    , boxClassForColor box.style.color
    ]


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


encode =
    Model.encode


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
        on "keydown" (Json.map (Debug.log "box:keydown" <| checkKeyCode) keyCode)


keyCodeAndValue : Json.Decoder ( Int, String )
keyCodeAndValue =
    Json.map2 (,)
        (field "keyCode" Json.int)
        (Json.at [ "target", "value" ] Json.string)


extractLabelUpdate box ( keyCode, value ) =
    if keyCode == 13 then
        CancelEditingBox box
    else
        Debug.log "UpdateBox" <| UpdateBox box value


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

            -- , on "input" keyCodeAndValue (\a -> Debug.log "input:keydown" LC.send channel <| extractLabelUpdate box a)
            , onWithOptions "mousedown" stopPropagation (Json.succeed nullHandler)
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
            Debug.log "Moved a box" <| moveBoxDrag event box

        SetSelected index ->
            { box | selectedIndex = index }

        CancelEditing ->
            { box
                | label = box.originalLabel
                , isEditing = False
            }

        Editing toggle ->
            let
                focusedBox =
                    Debug.log "focusing box" <| DomUtils.setFocus (labelSelector box) box
            in
                { focusedBox | isEditing = toggle, originalLabel = box.label }

        Update newLabel ->
            { box | label = Debug.log "got new label" newLabel }

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
                Debug.log "updated color" { box | style = style_ }

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
