module Board.State where

import Box.State (Box, BoxKey)
import Connection.State (Connection)

import Debug

data BoardMode = Connect |
                  Normal

type Board =
  { boxes: [Box]
  , connections: [Connection]
  , mode: BoardMode
  , nextIdentifier: BoxKey
  }

startingState : Board
startingState =
  { boxes = []
  , connections = []
  , mode = Normal
  , nextIdentifier = 1
  }

boxForKey : BoxKey -> [Box] -> Box
boxForKey key boxes = head (filter (\b -> b.key == key) boxes)

replaceBox : [Box] -> Box -> [Box]
replaceBox boxes withBox = map (\box ->
      if box.key == withBox.key then withBox else box) boxes


makeBox : BoxKey -> Box
makeBox identifier =
  { position = (0,0)
  , size = (100, 50)
  , label = "New Box"
  , originalLabel = "New Box"
  , key = identifier
  , isEditing = False
  , isSelected = False
  , isDragging = False
  }
