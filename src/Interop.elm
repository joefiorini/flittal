port module Interop exposing (..)

import DomUtils exposing (DragEvent)


port loadedState : (String -> msg) -> Sub msg


port serializeState : String -> Cmd msg


port drop : (DragEvent -> msg) -> Sub msg


port dragstart : (DragEvent -> msg) -> Sub msg


port dragend : (DragEvent -> msg) -> Sub msg
