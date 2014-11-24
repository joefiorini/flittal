module Box.State (Box, BoxKey, Point) where

type Position b = { b | position: Point }
type Size b = { b | size: Point }
type Labelled b = { b | label: String }
type Keyed b = { b | key: Int }

type BoxModel b = { b | isEditing: Bool }

type Point = (Int, Int)

type Box = Position (Size (Labelled (Keyed (BoxModel {}))))

type BoxKey = Int
