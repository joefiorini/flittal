module Connection.State where

import Box.State (Box)
import Geometry.Types (Point, Size)

data LineLayout = Vertical | Horizontal

type Line =
  { position: Point
  , width: Size
  , height: Size
  , layout: LineLayout
  }

type Connection =
  { segments: [Line]
  , startBox: Box
  , endBox: Box
  }
