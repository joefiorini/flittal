module Geometry.Types exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing ((:=))


type alias Size =
    ( Int, Int )


type alias Point =
    ( Int, Int )


type alias Geometric a =
    { a | position : Point, size : Size }


toPxPoint : Point -> ( String, String )
toPxPoint point =
    ( toString (Tuple.first point) ++ "px", toString (Tuple.second point) ++ "px" )


toPx : Int -> String
toPx n =
    (toString n) ++ "px"


encodePoint : Point -> Encode.Value
encodePoint point =
    Encode.object
        [ ( "x", Encode.int <| Tuple.first point )
        , ( "y", Encode.int <| Tuple.second point )
        ]


decodePoint : Decode.Decoder Point
decodePoint =
    Decode.object2 (,)
        ("x" := Decode.int)
        ("y" := Decode.int)


encodeSize : Size -> Encode.Value
encodeSize size =
    Encode.object
        [ ( "width", Encode.int <| Tuple.first size )
        , ( "height", Encode.int <| Tuple.second size )
        ]


decodeSize : Decode.Decoder Size
decodeSize =
    Decode.object2 (,)
        ("width" := Decode.int)
        ("height" := Decode.int)
