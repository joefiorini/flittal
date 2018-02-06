module DomUtils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Native.Custom.Html
import Json.Decode as Json
import Json.Decode exposing (string, Decoder, at, field, map5, bool)
import List exposing (head, reverse)
import String exposing (split, toInt, join)


type alias DragEvent =
    { id : String
    , isStart : Bool
    , isEnd : Bool
    , isDrop : Bool
    , isMulti : Bool
    , startX : Int
    , endX : Int
    , startY : Int
    , endY : Int
    }


type alias MouseSelectionEvent =
    { id : String
    , metaKey : Bool
    , altKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    }


styleProperty =
    (,)


type alias DnDPort =
    Signal DragEvent


getTargetId : Json.Decoder String
getTargetId =
    at [ "target", "id" ] string


getMouseSelectionEvent : Json.Decoder MouseSelectionEvent
getMouseSelectionEvent =
    object5 MouseSelectionEvent
        (at [ "target", "id" ] string)
        ("metaKey" := bool)
        ("altKey" := bool)
        ("ctrlKey" := bool)
        ("shiftKey" := bool)


stopPropagation =
    Native.Custom.Html.stopPropagation


setFocus =
    Native.Custom.Html.setFocus


on =
    Native.Custom.Html.on


extractBoxId : String -> Result String Int
extractBoxId id =
    toInt << head << reverse <| split "-" id


linkTo : String -> String -> Signal.Message -> Html
linkTo title url handle =
    a
        [ href url
        , onClick handle
        ]
        [ text title ]


class_ : List String -> Attribute
class_ names =
    class <| join " " names
