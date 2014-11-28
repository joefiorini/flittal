module Geometry.Types where

type Size = (Int, Int)
type Point = (Int, Int)

type Geometric a = { a | position: Point, size: Size }

toPxPoint : Point -> (String, String)
toPxPoint point = (show (fst point) ++ "px", show (snd point) ++ "px")

