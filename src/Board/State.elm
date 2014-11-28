module Board.State where

import List (head, filter, map)

import Box.State (Box, BoxKey)
import Connection.State (Connection)

import Debug

type BoardMode = Connect |
                  Normal

type alias Board =
  { boxes: List Box
  , connections: List Connection
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

boxForKey : BoxKey -> List Box -> Box
boxForKey key boxes = head (filter (\b -> b.key == key) boxes)

replaceBox : List Box -> Box -> List Box
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
