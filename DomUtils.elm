module DomUtils where

import Html (..)
import Native.Custom.Html

type DragEvent =
  { id: String
  , startX: Int
  , endX: Int
  , startY: Int
  , endY: Int
  }

type DropPort = Signal DragEvent

getTargetId : Get String
getTargetId = Native.Custom.Html.getTargetId

