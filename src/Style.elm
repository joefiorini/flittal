module Style exposing (..)

import Style.Color as InternalColor
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing (field)


type alias Color =
    InternalColor.Color


type alias Model =
    { color : Color
    }


decode : Decode.Decoder Model
decode =
    Decode.map Model
        (field "color" InternalColor.decode)


encode style =
    Encode.object
        [ ( "color", InternalColor.encode style.color )
        ]
