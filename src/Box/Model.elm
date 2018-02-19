module Box.Model exposing (..)

import Geometry.Types as Geometry
import Geometry.Types exposing (Geometric)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing (field)
import List
import Style
import Box.Types exposing (Model, BoxKey)


mkBox : Geometry.Point -> Geometry.Size -> String -> BoxKey -> Style.Model -> Model
mkBox position size label key style =
    { position = position
    , size = size
    , label = label
    , originalLabel = ""
    , key = key
    , isEditing = False
    , isDragging = False
    , selectedIndex = 0
    , style = style
    }


encode : Model -> Encode.Value
encode box =
    Encode.object
        [ ( "position", Geometry.encodePoint box.position )
        , ( "size", Geometry.encodeSize box.size )
        , ( "key", Encode.int box.key )
        , ( "label", Encode.string box.label )
        , ( "style", Style.encode box.style )
        ]


decode : Decode.Decoder Model
decode =
    Decode.map5 mkBox
        (field "position" Geometry.decodePoint)
        (field "size" Geometry.decodeSize)
        (field "label" Decode.string)
        (field "key" Decode.int)
        (field "style" Style.decode)


isSelected : Model -> Bool
isSelected box =
    box.selectedIndex /= -1


filterKey : (Model -> Bool) -> BoxKey -> List Model -> List Model
filterKey pred key boxes =
    List.filter
        (\b ->
            b.key == key && pred b
        )
        boxes
