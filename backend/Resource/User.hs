{-# LANGUAGE OverloadedStrings #-}

module Resource.User (user) where

import Framework.Resource (Resource(..), getResourceList)
import Type.UserInfo
import Control.Applicative ((<$>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Crypto.BCrypt              as Crypt
import qualified Web.Scotty.Trans           as Scotty
import qualified Data.ByteString.Char8      as BS
import qualified Data.Maybe                 as Maybe
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple.ToField (ToField)

allUsers :: DB.Query
allUsers = "select user_id, email, created_at from users"

singleUserById :: DB.Query
singleUserById  = "select user_id, email, created_at from users where user_id = ?"

encryptPassword :: Text -> IO BS.ByteString
encryptPassword plaintext =
  let plaintext' = encodeUtf8 plaintext in
    Maybe.fromJust <$>
      Crypt.hashPasswordUsingPolicy Crypt.fastBcryptHashingPolicy plaintext'

createUser :: UserEntry -> IO (DB.Query, [Text])
createUser obj = do
  cryptedPassword <- encryptPassword $ signupPassword obj
  let query = "insert into users (email, password) values (?, ?) returning user_id, email, created_at"
  return (query, [signupEmail obj, decodeUtf8 cryptedPassword])

user :: Resource UserInfo UserEntry
user = Resource
  { title = "Users"
  , list = allUsers
  , member = singleUserById
  , create = createUser
  }


