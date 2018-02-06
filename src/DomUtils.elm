module DomUtils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, property)
import Html.Events exposing (onClick)
import Native.Custom.Html
import Json.Encode as Json
import Json.Decode exposing (string, Decoder, at, field, map5, bool)
import List exposing (head, reverse)
import String exposing (split, toInt, join)
import Routes exposing (RouteName)
import Result exposing (fromMaybe, andThen, map)


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


getTargetId : Decoder String
getTargetId =
    at [ "target", "id" ] string


getMouseSelectionEvent : Decoder MouseSelectionEvent
getMouseSelectionEvent =
    map5 MouseSelectionEvent
        (at [ "target", "id" ] string)
        (field "metaKey" bool)
        (field "altKey" bool)
        (field "ctrlKey" bool)
        (field "shiftKey" bool)


stopPropagation =
    { stopPropagation = True
    , preventDefault = False
    }


setFocus =
    Native.Custom.Html.setFocus


on =
    Native.Custom.Html.on


extractBoxId : String -> Maybe Int
extractBoxId domId =
    -- toInt << head << reverse <| split "-" id
    let
        firstItem =
            split "-" domId |> reverse |> head
    in
        firstItem |> Maybe.andThen (\s -> (toInt s) |> Result.toMaybe)


linkTo : String -> String -> RouteName -> Html RouteName
linkTo title url routeName =
    a
        [ href url
        , onClick routeName
        ]
        [ text title ]


class_ : List String -> Attribute msg
class_ names =
    class <| join " " names


boolProperty : String -> Bool -> Attribute msg
boolProperty key b =
    property key (Json.bool b)
