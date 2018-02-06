module Box.Model exposing (..)

import Geometry.Types as Geometry
import Geometry.Types exposing (Geometric)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing ((:=))
import Json.Ext as JsonExt
import Result
import Result exposing (andThen)
import List
import Style


type alias BoxKey =
    Int


type alias Model =
    Geometric
        { key : BoxKey
        , label : String
        , originalLabel : String
        , isEditing : Bool
        , isDragging : Bool
        , selectedIndex : Int
        , style : Style.Model
        }


mkBox position size label originalLabel key isEditing isDragging selectedIndex style =
    { position = position
    , size = size
    , label = label
    , originalLabel = originalLabel
    , key = key
    , isEditing = isEditing
    , isDragging = isDragging
    , selectedIndex = selectedIndex
    , style = style
    }


encode : Model -> Encode.Value
encode box =
    Encode.object
        [ ( "position", Geometry.encodePoint box.position )
        , ( "size", Geometry.encodeSize box.size )
        , ( "key", Encode.int box.key )
        , ( "label", Encode.string box.label )
        , ( "originalLabel", Encode.string box.originalLabel )
        , ( "isEditing", Encode.bool box.isEditing )
        , ( "isDragging", Encode.bool box.isDragging )
        , ( "selectedIndex", Encode.int box.selectedIndex )
        , ( "style", Style.encode box.style )
        ]


apply : Decode.Decoder (a -> b) -> Decode.Decoder a -> Decode.Decoder b
apply func value =
    JsonExt.map2 (<|) func value


decode : Decode.Decoder Model
decode =
    let
        extract property decoder =
            (property := decoder)
    in
        apply
            (apply
                (apply
                    (apply
                        (apply
                            (apply
                                (apply
                                    (apply
                                        (Decode.map mkBox
                                            (extract "position" Geometry.decodePoint)
                                        )
                                        (extract "size" Geometry.decodeSize)
                                    )
                                    (extract "label" Decode.string)
                                )
                                (extract "originalLabel" Decode.string)
                            )
                            (extract "key" Decode.int)
                        )
                        (extract "isEditing" Decode.bool)
                    )
                    (extract "isDragging" Decode.bool)
                )
                (extract "selectedIndex" Decode.int)
            )
            ("style" := Style.decode)


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
