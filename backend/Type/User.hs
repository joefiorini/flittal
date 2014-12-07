{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}

module Type.User where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics

type Name = Text
type Password = Text

data User = User
  { name :: Name
  , password :: Password
  } deriving (Eq, Generic, Ord, Show, Typeable)

