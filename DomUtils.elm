module DomUtils where

import Html (..)
import Native.Custom.Html

import String (split, toInt)

type DragEvent =
  { id: String
  , isStart: Bool
  , isEnd: Bool
  , isDrop: Bool
  , isMulti: Bool
  , startX: Int
  , endX: Int
  , startY: Int
  , endY: Int
  }

type DnDPort = Signal DragEvent

getTargetId : Get String
getTargetId = Native.Custom.Html.getTargetId

getMouseSelectionEvent = Native.Custom.Html.getMouseSelectionEvent

stopPropagation = Native.Custom.Html.stopPropagation

extractBoxId : String -> Maybe Int
extractBoxId id = toInt << last <| split "-" id
