{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Framework.Resource
( Resource(..)
, ResourceStore
, getResourceList
, represent
)
where

import Data.Text.Lazy (Text)
import Data.Aeson (ToJSON(..), (.=), object)
import qualified Database.PostgreSQL.Simple as DB

getResourceList :: (DB.FromRow a) => DB.Query -> ResourceStore -> IO [a]
getResourceList q c = (DB.query_ c q)

type ResourceStore = DB.Connection

represent item = Representation
  { data_ = item }

instance (ToJSON a) => ToJSON (Representation a) where
  toJSON (Representation data_) = object ["data" .= data_]

data Representation a = Representation
  { data_ :: a
  }

data Resource a = Resource
  { title :: Text
  , list :: DB.Query
  , member :: DB.Query
  }

-- data Resource a = Resource
--   { title :: Text
--   , list :: DB.Query
--   , member :: DB.Query
--   }

