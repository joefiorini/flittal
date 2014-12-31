{-# LANGUAGE OverloadedStrings, RankNTypes, DeriveDataTypeable #-}

module Framework.Action ( mkListAction, mkMemberAction, mkCreateAction, Action ) where

import Framework.App
import Framework.Resource
import Control.Exception
import Data.Text.Lazy (Text, unpack, pack)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import Data.Typeable (Typeable)
import Control.Monad.Trans (liftIO)
import Web.Scotty.Trans (ActionT, json, jsonData, param, Parsable(..), addHeader, ScottyError(..))

import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.ToField (ToField(toField))

import qualified Data.Maybe                 as Maybe
import qualified Data.UUID                  as UUID
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Errors as Errors

import Type.UserInfo

type Action = ActionT Text ConfigM ()

data AppError = DuplicateEmailAddressError
  deriving (Typeable, Show, Read)

instance Exception AppError
instance ScottyError AppError where
  stringError = read
  showError = pack . show

instance DB.ToRow UUID where
  toRow uuid = [toField uuid]

instance Parsable UUID where
  parseParam t = Maybe.maybe (Left "Cannot parse UUID") Right $ UUID.fromString (unpack t)

runMemberAction :: (DB.FromRow a, DB.ToRow q) => Resource a b -> ResourceStore -> q -> IO [a]
runMemberAction r c = DB.query c (member r)

runCreateAction :: (DB.FromRow a, DB.FromRow b) => Resource a b -> ResourceStore -> b -> IO [a]
runCreateAction r c o = do
  (q, p) <- (create r o)
  Errors.catchViolation handleViolation $ DB.query c q p
  where
    handleViolation _ (Errors.UniqueViolation "users_email_unique") =
      throw DuplicateEmailAddressError

runListAction :: (DB.FromRow a) => Resource a b -> ResourceStore -> IO [a]
runListAction r c = DB.query_ c (list r)

mkCreateAction :: (ToJSON a, FromJSON b, DB.FromRow a, DB.FromRow b) => Resource a b -> ResourceStore -> Action
mkCreateAction r c =  do
  obj <- jsonData
  item <- liftIO $ runCreateAction r c obj
  addHeader "Access-Control-Allow-Origin" "*"
  json $ (represent $ head item)
  -- json $ (represent $ head item)

mkMemberAction :: (ToJSON a, DB.FromRow a) => Resource a b -> ResourceStore -> Action
mkMemberAction resource connection = do
  itemId <- (param "id") :: ActionT Text ConfigM UUID
  item <- liftIO $ runMemberAction resource connection itemId
  addHeader "Access-Control-Allow-Origin" "*"
  json $ (represent $ head item)

mkListAction :: (ToJSON a, DB.FromRow a) => Resource a b -> ResourceStore -> Action
mkListAction resource connection = do
  item <- liftIO $ runListAction resource connection
  addHeader "Access-Control-Allow-Origin" "*"
  json item

  
