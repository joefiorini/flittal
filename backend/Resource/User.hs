{-# LANGUAGE OverloadedStrings #-}

module Resource.User (user) where

import Framework.Resource (Resource(..), getResourceList)
import Type.UserInfo

import qualified Web.Scotty.Trans           as Scotty
import qualified Database.PostgreSQL.Simple as DB

allUsers :: DB.Query
allUsers = "select user_id, email, created_at from users"

singleUserById :: DB.Query
singleUserById  = "select user_id, email, created_at from users where user_id = ?"

user :: Resource UserInfo
user = Resource
  { title = "Users"
  , list = allUsers
  , member = singleUserById
  }


