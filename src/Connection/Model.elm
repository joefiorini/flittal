module Connection.Model exposing (..)

import Box.Types exposing (BoxKey)
import Geometry.Types exposing (Point, Size)
import Json.Decode as Decode exposing (andThen, fail, field, string, succeed)
import Json.Encode as Encode
import List
import Geometry.Types as Geometry


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
    , point : Point
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
    , startBox : BoxKey
    , endBox : BoxKey
    }


encodePort : PortLocation -> Encode.Value
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


portRawDecoder : Decode.Decoder RawPort
portRawDecoder =
    Decode.map2 RawPort
        (field "location" Decode.string)
        (field "point" Geometry.decodePoint)


decodePort : Decode.Decoder PortLocation
decodePort =
    portRawDecoder
        |> andThen
            (\{ location, point } ->
                case location of
                    "right" ->
                        succeed <| Right point

                    "left" ->
                        succeed <| Left point

                    "top" ->
                        succeed <| Top point

                    "bottom" ->
                        succeed <| Bottom point

                    _ ->
                        fail ("port location value of \"" ++ location ++ "\" is invalid.")
            )


encode : Model -> Encode.Value
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


encodeLineLayout : LineLayout -> Encode.Value
encodeLineLayout layout =
    case layout of
        Vertical ->
            Encode.string "Vertical"

        Horizontal ->
            Encode.string "Horizontal"


decodeLineLayout : Decode.Decoder LineLayout
decodeLineLayout =
    Decode.string
        |> andThen
            (\layout ->
                case layout of
                    "Vertical" ->
                        succeed Vertical

                    "Horizontal" ->
                        succeed Horizontal

                    _ ->
                        fail ("layout value of \"" ++ layout ++ "\" is invalid.")
            )


encodeSegment : Line -> Encode.Value
encodeSegment segment =
    Encode.object
        [ ( "position", Geometry.encodePoint segment.position )
        , ( "size", Geometry.encodeSize segment.size )
        , ( "layout", encodeLineLayout segment.layout )
        ]


decodeSegment : Decode.Decoder Line
decodeSegment =
    Decode.map3 Line
        (field "position" Geometry.decodePoint)
        (field "size" Geometry.decodeSize)
        (field "layout" decodeLineLayout)


decode : Decode.Decoder Model
decode =
    Decode.map5 Model
        (field "segments" <| Decode.list decodeSegment)
        (field "startPort" decodePort)
        (field "endPort" decodePort)
        (field "startBox" Decode.int)
        (field "endBox" Decode.int)
