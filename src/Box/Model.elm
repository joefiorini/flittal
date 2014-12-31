module Box.Model (Model, BoxKey) where

import Geometry.Types (Geometric)

-- type Position b = { b | position: Point }
-- type Size b = { b | size: Point }
-- type Labelled b = { b | label: String }
-- type Keyed b = { b | key: Int }

-- type BoxModel b = { b | isEditing: Bool
--                       , isSelected: Bool
--                       , originalLabel: String }

type alias BoxKey = Int

type alias Model = Geometric
  { key: BoxKey
  , label: String
  , originalLabel: String
  , isEditing: Bool
  , isSelected: Bool
  , isDragging: Bool
  , selectedIndex: Int
  , borderSize: Int
  }

