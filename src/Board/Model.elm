module Board.Model exposing (..)

import Box.Types as Box
import Box.Model
import Connection.Model as Connection
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
import List exposing (filter, head, map)


type alias Model =
    { boxes : List Box.Model
    , connections : List Connection.Model
    , nextIdentifier : Box.BoxKey
    }


encode : Model -> Encode.Value
encode board =
    let
        encodedBoxes =
            map Box.Model.encode board.boxes

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
    Decode.map3 Model
        (Decode.list Box.Model.decode |> field "boxes")
        (Decode.list Connection.decode |> field "connections")
        (field "nextIdentifier" Decode.int)
