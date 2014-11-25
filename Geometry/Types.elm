module Geometry.Types where

type Size = (Int, Int)
type Point = (Int, Int)

type Geometric a = { a | position: Point, size: Size }
