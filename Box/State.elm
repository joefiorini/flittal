module Box.State (Box, BoxKey, Point) where

-- type Position b = { b | position: Point }
-- type Size b = { b | size: Point }
-- type Labelled b = { b | label: String }
-- type Keyed b = { b | key: Int }

-- type BoxModel b = { b | isEditing: Bool
--                       , isSelected: Bool
--                       , originalLabel: String }

type Point = (Int, Int)

type Box =
  { key: BoxKey
  , label: String
  , originalLabel: String
  , position: Point
  , size: Point
  , isEditing: Bool
  , isSelected: Bool
  }

type BoxKey = Int
