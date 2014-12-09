{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
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

-- instance ToJSON UserUid

-- deriveAll ''UserUid "PFUserUid"
-- type instance PF UserUid = PFUserUid
-- instance FromJSON UserUid
-- instance FromField UserUid where fromField = UUID.fromField


-- deriveAll ''UTCTime "PFUTCTime"
-- type instance PF UTCTime = PFUTCTime

-- instance XmlPickler UTCTime where xpickle = gxpickle

data UserInfo = UserInfo
  { uid :: UUID
  , email :: Text
  , createdAt :: UTCTime
  } deriving (Generic, Show, Typeable)

-- deriveAll ''UserInfo "PFUserInfo"
-- type instance PF UserInfo = PFUserInfo

instance FromRow UserInfo where
  fromRow = UserInfo <$> field <*> field <*> field


-- instance XmlPickler UserInfo where xpickle = gxpickle
-- instance JSONSchema UserInfo where schema = gSchema
instance ToJSON     UserInfo
instance FromJSON   UserInfo
