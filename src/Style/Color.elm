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


decode : Decoder Color
decode =
    string
        |> andThen
            (\color ->
                case color of
                    "dark1" ->
                        succeed Dark1

                    "dark2" ->
                        succeed Dark2

                    "dark3" ->
                        succeed Dark3

                    "dark4" ->
                        succeed Dark4

                    "light1" ->
                        succeed Light1

                    "light2" ->
                        succeed Light2

                    "light3" ->
                        succeed Light3

                    "light4" ->
                        succeed Light4

                    "white" ->
                        succeed White

                    "black" ->
                        succeed Black

                    _ ->
                        fail ("color value of \"" ++ color ++ "\" is invalid.")
            )
