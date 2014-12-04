module Connection.State where

import Box.State (Box)
import Geometry.Types (Point, Size)

type LineLayout = Vertical | Horizontal

type PortOrder = StartEnd | EndStart

type alias ConnectionPort =
  { start: PortLocation
  , end: PortLocation
  , order: PortOrder
  }

type PortLocation = Right Point
                  | Bottom Point
                  | Left Point
                  | Top Point

type alias Line =
  { position: Point
  , size: Size
  , layout: LineLayout
  }

type alias Connection =
  { segments: List Line
  , startPort: PortLocation
  , endPort: PortLocation
  , startBox: Box
  , endBox: Box
  }
