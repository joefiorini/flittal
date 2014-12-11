{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , OverloadedStrings
  #-}
module Type.UserInfo where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.Aeson
import Control.Applicative ((<$>), (<*>))
import GHC.Generics
-- import Generics.Regular
-- import Generics.Regular.XmlPickler
-- import Text.XML.HXT.Arrow.Pickle
import Data.Time.Clock (UTCTime)
import Data.JSON.Schema.Types (JSONSchema(..))

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(ConversionFailed, Incompatible, UnexpectedNull), returnError, typeOid)

import qualified Type.User as User
import qualified Data.ByteString as BS

data UserInfo = UserInfo
  { userId :: UUID
  , email :: Text
  , createdAt :: UTCTime
  } deriving (Generic, Show, Typeable)

data UserEntry = UserEntry
  { signupEmail :: Text
  , signupPassword :: Text
  } deriving (Generic, Show, Typeable)

instance FromRow UserInfo where
  fromRow = UserInfo <$> field <*> field <*> field

instance FromRow UserEntry where
  fromRow = UserEntry <$> field <*> field

instance ToJSON     UserInfo
instance FromJSON   UserInfo

instance FromJSON   UserEntry
