module Dom.Types exposing (..)


type alias DragEvent =
    { id : String
    , isStart : Bool
    , isEnd : Bool
    , isDrop : Bool
    , isMulti : Bool
    , startX : Int
    , endX : Int
    , startY : Int
    , endY : Int
    }


type alias MouseSelectionEvent =
    { id : String
    , metaKey : Bool
    , altKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    }
