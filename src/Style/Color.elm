module Style.Color exposing (..)

import Json.Encode as Encode
import Json.Decode exposing (succeed, fail, andThen, Decoder, string)
import Result


type Color
    = Dark1
    | Dark2
    | Dark3
    | Dark4
    | Light1
    | Light2
    | Light3
    | Light4
    | Black
    | White


encode color =
    Encode.string <|
        case color of
            Dark1 ->
                "dark1"

            Dark2 ->
                "dark2"

            Dark3 ->
                "dark3"

            Dark4 ->
                "dark4"

            Light1 ->
                "light1"

            Light2 ->
                "light2"

            Light3 ->
                "light3"

            Light4 ->
                "light4"

            Black ->
                "black"

            White ->
                "white"


decode : Decode.Decoder Color
decode =
    Decode.customDecoder Decode.string
        (\color ->
            case color of
                "dark1" ->
                    Result.Ok Dark1

                "dark2" ->
                    Result.Ok Dark2

                "dark3" ->
                    Result.Ok Dark3

                "dark4" ->
                    Result.Ok Dark4

                "light1" ->
                    Result.Ok Light1

                "light2" ->
                    Result.Ok Light2

                "light3" ->
                    Result.Ok Light3

                "light4" ->
                    Result.Ok Light4

                "white" ->
                    Result.Ok White

                "black" ->
                    Result.Ok Black

                _ ->
                    Result.Err ("color value of \"" ++ color ++ "\" is invalid.")
        )
