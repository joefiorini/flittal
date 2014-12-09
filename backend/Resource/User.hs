{-# LANGUAGE OverloadedStrings #-}

module Resource.User (user) where

import Framework.Resource (Resource(..), getResourceList)
import Type.UserInfo

import qualified Database.PostgreSQL.Simple as DB

allUsers :: DB.Query
allUsers = "select user_id, email, created_at from users"

user :: Resource UserInfo
user = Resource
  { title = "Users"
  , list = getResourceList allUsers
  }


