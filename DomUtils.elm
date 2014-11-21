module DomUtils where

type DragEvent =
  { id: String
  , startX: Int
  , endX: Int
  , startY: Int
  , endY: Int
  }


type DropPort = Signal DragEvent
