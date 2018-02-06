module Board.Model exposing (..)

import List exposing (head, filter, map)
import Box.Model as Box
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing ((:=))
import Connection.Model as Connection
import Debug


type alias BoxKey =
    Box.BoxKey


type alias Model =
    { boxes : List Box.Model
    , connections : List Connection.Model
    , nextIdentifier : BoxKey
    }


encode : Model -> Encode.Value
encode board =
    let
        encodedBoxes =
            map Box.encode board.boxes

        encodedConnections =
            map Connection.encode board.connections
    in
        Encode.object
            [ ( "boxes", Encode.list encodedBoxes )
            , ( "connections", Encode.list encodedConnections )
            , ( "nextIdentifier", Encode.int board.nextIdentifier )
            ]


decode : Decode.Decoder Model
decode =
    Decode.object3 Model
        ("boxes" := Decode.list Box.decode)
        ("connections" := Decode.list Connection.decode)
        ("nextIdentifier" := Decode.int)
