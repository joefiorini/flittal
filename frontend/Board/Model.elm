module Board.Model where

import List (head, filter, map)

import Box.Model
import Connection.Model as Connection

import Debug

type alias BoxKey = Box.Model.BoxKey
type alias Box = Box.Model.Model

type alias Model =
  { boxes: List Box
  , connections: List Connection.Model
  , nextIdentifier: BoxKey
  }
