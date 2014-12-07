{-# LANGUAGE OverloadedStrings #-}

module Diagrammer (diagrammer) where

import Control.Applicative
import Control.Concurrent.STM (newTVarIO)
import Data.Set (Set)
import qualified Data.Set as Set

import ApiTypes (ServerData (..))
import Type.User (User (User))

diagrammer :: IO ServerData
diagrammer = ServerData
              <$> newTVarIO mockUsers

mockUsers :: Set User
mockUsers = Set.fromList
  [ User "joe@joefiorini.com" "abcd1234"
  ]
