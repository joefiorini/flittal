module Connection.State where

import Box.State (Box)
import Geometry.Types (Point, Size)

type LineLayout = Vertical | Horizontal

type alias Line =
  { position: Point
  , size: Size
  , layout: LineLayout
  }

type alias Connection =
  { segments: List Line
  , startBox: Box
  , endBox: Box
  }
