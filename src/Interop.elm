port module Interop exposing (..)

import Dom.Types exposing (DragEvent)


port drop : (DragEvent -> msg) -> Sub msg


port dragstart : (DragEvent -> msg) -> Sub msg


port dragend : (DragEvent -> msg) -> Sub msg


port selectInputText : String -> Cmd msg
