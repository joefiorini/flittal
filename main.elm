module Main where
-- import Box.Controller (render as renderBoxes)
import Html (Html, toElement)
import Board.Controller (renderBoard)
import DomUtils (DragEvent, DropPort)
import Window

port drop : Signal DragEvent
port dragstart : Signal DragEvent
port dragend : Signal DragEvent


main : Signal Element
main = (toElement 900 600) <~ (renderBoard drop)

