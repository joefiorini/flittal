import Mouse
import Debug
import Transform2D (translation)
import Window
import Drag
import Time

type Box =
  { x: Float
  , y: Float
  , width: Float
  , height: Float
  , selected: Bool
  }

type Board =
  { cursorX: Int
  , cursorY: Int
  , selectedBox: Maybe Box
  , boxes: [Box]
  }

data Update = Move (Int, Int) | Start (Int, Int)

newBox : (Float, Float) -> Box
newBox (a,b) =
  { x = a
  , y = b
  , width = 100
  , height = 50
  }

defaultBoard : Board
defaultBoard =
  { cursorX = 0
  , cursorY = 0
  , selectedBox = Nothing
  , boxes = [newBox (-100, 5), newBox (200, 30)]
  }

boardState : Signal Board
boardState = foldp updateBoard defaultBoard dragger

findFirst : (a -> Bool) -> [a] -> Maybe a
findFirst p l = let filtered = filter p l in
  if length filtered == 1 then Just head filtered else Nothing

selectBox : Board -> (Int, Int) -> Maybe Box
selectBox board (x,y) = findFirst (\box -> x > box.x && x < box.width && y > box.y && y < box.height) board.boxes


dragger = merge Drag.start <| Drag.lastPosition (40 * Time.millisecond)
main = lift2 displayBoard Window.dimensions boardState

updateBoard locationM board = Debug.watch "board" <| case locationM of
  Move box -> { board | selectedBox <- box }
  Start (x,y) -> { board | cursorX <- x, cursorY <- y }
  _ -> board

displayBoard : (Int, Int) -> Board -> Element
displayBoard (w,h) {cursorX,cursorY,selectedBox,boxes} =
  collage w h <| map boxToRect boxes

placeBox : (Float, Float) -> Form -> Form
placeBox (x,y) form = groupTransform (translation x y) [form]

boxToRect : Box -> Form
boxToRect box = placeBox (box.x, box.y) <| filled clearGrey <| rect box.width box.height

clearGrey : Color
clearGrey = rgba 111 111 111 0.6
