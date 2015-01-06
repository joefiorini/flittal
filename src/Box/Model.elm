module Box.Model where

import Geometry.Types as Geometry
import Geometry.Types (Geometric)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode ((:=))
import Json.Ext as JsonExt
import Result
import Result (andThen)
import List

import Style

type alias BoxKey = Int

type alias Model = Geometric
  { key: BoxKey
  , label: String
  , originalLabel: String
  , isEditing: Bool
  , isDragging: Bool
  , selectedIndex: Int
  , style: Style.Model
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
    [ ("position", Geometry.encodePoint box.position)
    , ("size", Geometry.encodeSize box.size)
    , ("key", Encode.int box.key)
    , ("label", Encode.string box.label)
    , ("originalLabel", Encode.string box.originalLabel)
    , ("isEditing", Encode.bool box.isEditing)
    , ("isDragging", Encode.bool box.isDragging)
    , ("selectedIndex", Encode.int box.selectedIndex)
    , ("style", Style.encode box.style)
    ]


apply : Decode.Decoder (a -> b) -> Decode.Decoder a -> Decode.Decoder b
apply func value =
  JsonExt.map2 (<|) func value

decode : Decode.Decoder Model
decode =
  let property ~= decoder = Decode.decodeValue (property := decoder)
      extract property decoder = (property := decoder)
  in
   Decode.map mkBox
    (extract "position" Geometry.decodePoint)
    `apply`
    (extract "size" Geometry.decodeSize)
    `apply`
    (extract "label" Decode.string)
    `apply`
    (extract "originalLabel" Decode.string)
    `apply`
    (extract "key" Decode.int)
    `apply`
    (extract "isEditing" Decode.bool)
    `apply`
    (extract "isDragging" Decode.bool)
    `apply`
    (extract "selectedIndex" Decode.int)
    `apply`
    ("style" := Style.decode)

isSelected : Model -> Bool
isSelected box =
  box.selectedIndex == -1

filterKey : (Model -> Bool) -> BoxKey -> List Model -> List Model
filterKey pred key boxes =
    List.filter (\b ->
      b.key == key && pred b) boxes
