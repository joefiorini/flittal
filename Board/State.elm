module Board.State where

import Box.State (Box, BoxKey)

import Debug

startingState : Board
startingState =
  { boxes = []
  , selectedBoxes = []
  }


type Board =
  { boxes: [Box]
  , selectedBoxes: [Box]
  }

boxForKey : Int -> [Box] -> Box
boxForKey key boxes = head (filter (\b -> b.key == key) boxes)

replaceBox : [Box] -> Box -> [Box]
replaceBox boxes withBox = map (\box ->
      if box.key == withBox.key then withBox else box) boxes


makeBox : BoxKey -> Box
makeBox identifier =
  { position = (0,0)
  , size = (100, 50)
  , label = "New Box"
  , key = identifier
  , isEditing = False
  , isSelected = False
  }
