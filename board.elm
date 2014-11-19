import Transform2D (translation)
import Window

type Box =
  { x: Float
  , y: Float
  , width: Float
  , height: Float
  }

type Board =
  { cursorX: Float
  , cursorY: Float
  , selectedBox: Maybe Box
  , width: Int
  , height: Int
  , boxes: [Box]
  }

newBox : (Float, Float) -> Box
newBox (a,b) =
  { x = a
  , y = b
  , width = 100
  , height = 50
  }

defaultBoard : (Int, Int) -> Board
defaultBoard (w,h) =
  { width = w
  , height = h
  , cursorX = 0
  , cursorY = 0
  , selectedBox = Nothing
  , boxes = [newBox (-100, 5), newBox (200, 30)]
  }

main = lift displayBoard (defaultBoard <~ Window.dimensions)

displayBoard {width,height,cursorX,cursorY,selectedBox,boxes} = collage width height <| map boxToRect boxes

placeBox : (Float, Float) -> Form -> Form
placeBox (x,y) form = groupTransform (translation x y) [form]

boxToRect : Box -> Form
boxToRect box = placeBox (box.x, box.y) <| filled clearGrey <| rect box.width box.height

clearGrey : Color
clearGrey = rgba 111 111 111 0.6
