module Connection.Model exposing (..)

import Box.Model as Box
import Geometry.Types as Geometry
import Json.Decode as Decode
import Json.Decode exposing ((:=))
import Geometry.Types exposing (Point, Size)
import Json.Encode as Encode
import List
import Result


type LineLayout
    = Vertical
    | Horizontal


type PortOrder
    = StartEnd
    | EndStart


type alias ConnectionPort =
    { start : PortLocation
    , end : PortLocation
    , order : PortOrder
    }


type PortLocation
    = Right Point
    | Bottom Point
    | Left Point
    | Top Point


type alias RawPort =
    { location : String
    , point : Geometry.Point
    }


type alias Line =
    { position : Point
    , size : Size
    , layout : LineLayout
    }


type alias Model =
    { segments : List Line
    , startPort : PortLocation
    , endPort : PortLocation
    , startBox : Box.BoxKey
    , endBox : Box.BoxKey
    }


encodePort portLocation =
    let
        ( location, point ) =
            case portLocation of
                Right point ->
                    ( "right", point )

                Left point ->
                    ( "left", point )

                Top point ->
                    ( "top", point )

                Bottom point ->
                    ( "bottom", point )
    in
        Encode.object
            [ ( "location", Encode.string location )
            , ( "point", Geometry.encodePoint point )
            ]


portRawDecoder =
    Decode.map2 RawPort
        ("location" := Decode.string)
        ("point" := Geometry.decodePoint)


decodePort =
    Decode.customDecoder portRawDecoder
        (\{ location, point } ->
            case location of
                "right" ->
                    Result.Ok <| Right point

                "left" ->
                    Result.Ok <| Left point

                "top" ->
                    Result.Ok <| Top point

                "bottom" ->
                    Result.Ok <| Bottom point

                _ ->
                    Result.Err ("port location value of \"" ++ location ++ "\" is invalid.")
        )


encode connection =
    let
        encodedSegments =
            List.map encodeSegment connection.segments
    in
        Encode.object
            [ ( "segments", Encode.list encodedSegments )
            , ( "startPort", encodePort connection.startPort )
            , ( "endPort", encodePort connection.endPort )
            , ( "startBox", Encode.int connection.startBox )
            , ( "endBox", Encode.int connection.endBox )
            ]


encodeLineLayout layout =
    case layout of
        Vertical ->
            Encode.string "Vertical"

        Horizontal ->
            Encode.string "Horizontal"


decodeLineLayout =
    Decode.customDecoder Decode.string
        (\layout ->
            case layout of
                "Vertical" ->
                    Result.Ok Vertical

                "Horizontal" ->
                    Result.Ok Horizontal

                _ ->
                    Result.Err ("layout value of \"" ++ layout ++ "\" is invalid.")
        )


encodeSegment segment =
    Encode.object
        [ ( "position", Geometry.encodePoint segment.position )
        , ( "size", Geometry.encodeSize segment.size )
        , ( "layout", encodeLineLayout segment.layout )
        ]


decodeSegment =
    Decode.object3 Line
        ("position" := Geometry.decodePoint)
        ("size" := Geometry.decodeSize)
        ("layout" := decodeLineLayout)


decode : Decode.Decoder Model
decode =
    Decode.object5 Model
        ("segments" := Decode.list decodeSegment)
        ("startPort" := decodePort)
        ("endPort" := decodePort)
        ("startBox" := Decode.int)
        ("endBox" := Decode.int)
