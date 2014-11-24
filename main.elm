module Main where
import Html (Html, toElement)
import Board.Controller (renderBoard, checkFocus)
import Board.Action (Action(..))
import DomUtils (DragEvent, DropPort)
import Window

port drop : Signal DragEvent
port dragstart : Signal DragEvent
port dragend : Signal DragEvent

port focus : Signal String
port focus = checkFocus


main : Signal Element
main = (toElement 900 600) <~ (renderBoard drop)

