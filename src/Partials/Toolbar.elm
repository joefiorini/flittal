module Partials.Toolbar exposing (..)

import Html exposing (input, div, text, section, button, img, Html)
import Html.Attributes exposing (id, class, placeholder, src, width, type_, readonly, required, title, value)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Navigation exposing (Location)


clearButton : Html Msg
clearButton =
    div
        [ class "clear-board" ]
        [ button
            [ onClick ClearBoard
            , title "Clear the board"
            ]
            [ img
                [ src "/images/icon-clear.svg" ]
                []
            ]
        ]


shareUrl location encodedBoard =
    case encodedBoard of
        Just str ->
            let
                hostAndPath =
                    String.join "/" [ location.host, "#", str ]
            in
                location.protocol ++ "//" ++ hostAndPath

        Nothing ->
            ""


shareButton : Maybe String -> Location -> Html Msg
shareButton encodedBoard location =
    div
        [ class "share" ]
        [ input
            [ class "share__url"
            , id "share-url"
            , placeholder "Share this board"
            , onClick ShareBoard
            , type_ "text"
            , readonly True
            , value <| shareUrl location <| encodedBoard
            ]
            []
        , button
            [ class "button-pseudo" ]
            [ img
                [ src "/images/icon-share.svg"
                , width 25
                ]
                []
            ]
        ]


view : Maybe String -> Location -> Html Msg
view encodedBoard location =
    section
        [ class "l-container" ]
        [ clearButton
        , shareButton encodedBoard location
        ]
