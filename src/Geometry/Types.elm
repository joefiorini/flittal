module Geometry.Types where

import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode ((:=))

type alias Size = (Int, Int)
type alias Point = (Int, Int)

type alias Geometric a = { a | position: Point, size: Size }

toPxPoint : Point -> (String, String)
toPxPoint point = (toString (fst point) ++ "px", toString (snd point) ++ "px")

toPx : Int -> String
toPx n = (toString n) ++ "px"

encodePoint : Point -> Encode.Value
encodePoint point =
  Encode.object
    [ ("x", Encode.int <| fst point)
    , ("y", Encode.int <| snd point)
    ]

decodePoint : Decode.Decoder Point
decodePoint =
  Decode.object2 (,)
    ("x" := Decode.int)
    ("y" := Decode.int)

encodeSize : Size -> Encode.Value
encodeSize size =
  Encode.object
    [ ("width", Encode.int <| fst size)
    , ("height", Encode.int <| snd size)
    ]

decodeSize : Decode.Decoder Size
decodeSize =
  Decode.object2 (,)
    ("width" := Decode.int)
    ("height" := Decode.int)

