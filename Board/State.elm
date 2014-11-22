module Board.State where

import Box.State (Box, BoxKey)

import String (split, toInt)
import Debug

startingState : Board
startingState =
  { boxes = []
  }


type Board =
  { boxes: [Box]
  }

selectedBox : String -> [Box] -> Box
selectedBox id boxes = let keyM = toInt << last <| (split "-" id) in
  case keyM of
    Just key -> head (filter (\b -> b.key == key) boxes)
    Nothing -> Debug.crash "expected box key to be a number"

replaceBox : [Box] -> Box -> [Box]
replaceBox boxes withBox = map (\box ->
      if box.key == withBox.key then withBox else box) boxes


makeBox : BoxKey -> Box
makeBox identifier =
  { position = (0,0)
  , size = (100, 50)
  , label = "New Box"
  , key = identifier
  }
