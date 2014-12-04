module Geometry.Types where

type alias Size = (Int, Int)
type alias Point = (Int, Int)

type alias Geometric a = { a | position: Point, size: Size }

toPxPoint : Point -> (String, String)
toPxPoint point = (toString (fst point) ++ "px", toString (snd point) ++ "px")

toPx : Int -> String
toPx n = (toString n) ++ "px"
