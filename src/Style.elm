module Style where

import Style.Color as InternalColor
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode ((:=))

type alias Color = InternalColor.Color

type alias Model =
  { color: Color
  }

decode : Decode.Decoder Model
decode =
  Decode.object1 Model
    ("color" := InternalColor.decode)

encode style =
  Encode.object
    [ ("color", InternalColor.encode style.color)
    ]

