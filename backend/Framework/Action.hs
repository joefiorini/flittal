{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Framework.Action ( mkListAction, mkMemberAction, Action ) where

import Framework.App
import Framework.Resource
import Data.Text.Lazy (Text, unpack)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Control.Monad.Trans (liftIO)
import Web.Scotty.Trans (ActionT, json, param, Parsable(..))

import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))

import qualified Data.Maybe                 as Maybe
import qualified Data.UUID                  as UUID
import qualified Database.PostgreSQL.Simple as DB


type Action = ActionT Text ConfigM ()

instance DB.ToRow UUID where
  toRow uuid = [toField uuid]

instance Parsable UUID where
  parseParam t = Maybe.maybe (Left "Cannot parse UUID") Right $ UUID.fromString (unpack t)

runMemberAction :: (DB.FromRow a, DB.ToRow q) => Resource a -> ResourceStore -> q -> IO [a]
runMemberAction r c = DB.query c (member r)

runListAction :: (DB.FromRow a) => Resource a -> ResourceStore -> IO [a]
runListAction r c = DB.query_ c (list r)

mkMemberAction :: (ToJSON a, DB.FromRow a) => Resource a -> ResourceStore -> Action
mkMemberAction resource connection = do
  itemId <- (param "id") :: ActionT Text ConfigM UUID
  item <- liftIO $ runMemberAction resource connection itemId
  json $ (represent $ head item)

mkListAction :: (ToJSON a, DB.FromRow a) => Resource a -> ResourceStore -> Action
mkListAction resource connection = do
  item <- liftIO $ runListAction resource connection
  json item

