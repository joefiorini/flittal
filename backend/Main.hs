{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}

module Main (main) where

import Network.Wai.Handler.Warp
import Control.Monad.Trans (liftIO)

import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (postgreSQLConnectionString, defaultConnectInfo, connectPostgreSQL, ConnectInfo(..), query_, Query)
import Control.Concurrent.STM (newTVarIO, readTVarIO, TVar)
import Control.Monad.Reader (ReaderT (..))

import Network.HTTP.Types.Method (StdMethod(..))
import qualified Network.HTTP.Types.Status as Status

import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Web.Scotty.Trans (scottyT, get, post, json, middleware, notFound)
import Web.Scotty.Trans (jsonData, raw, ActionT, body, stringError, raise, addHeader, addroute, defaultHandler, ScottyError, showError, status)
import Data.Text.Lazy (Text, unpack)
import Type.UserInfo

import System.Posix.Env (getEnvDefault)

import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import qualified Data.Aeson as A
import qualified Data.Maybe as Maybe

import Framework (Resource, ResourceStore)

import Framework.App -- TODO: REMOVE THIS IN FAVOR OF BETTER ENCAPSULATION
import Framework.Action (mkListAction, mkMemberAction, mkCreateAction, Action)
import Resource.User

main :: IO ()
main = do

  -- Set up the server state for the blog and start warp.
  putStrLn "Starting warp server on http://localhost:3000"
  c <- getConfig
  connection <- newTVarIO =<< connectPostgreSQL (BS.pack $ databaseUrl c)
  let runM m = runReaderT (runConfigM m) c
      runActionToIO = runM in

    scottyT (httpPort c) runM runActionToIO (app connection)

getDefaultConnectionString :: BS.ByteString
getDefaultConnectionString =
  postgreSQLConnectionString $
    defaultConnectInfo { connectDatabase = "diagrammer_devel" }

getConfig :: IO ServerConfig
getConfig = do
  databaseUrl <- getEnvDefault "DATABASE_URL" (BS.unpack getDefaultConnectionString)
  httpPortS <- getEnvDefault "PORT" "3000"
  return ServerConfig
    { databaseUrl = databaseUrl
    , httpPort = read httpPortS
    }

app :: TVar DB.Connection -> App
app db = do
  connection <- liftIO $ readTVarIO db
  let memberAction = flip mkMemberAction connection
      listAction = flip mkListAction connection
      createAction = flip mkCreateAction connection
  middleware logStdoutDev
  defaultHandler handleError
  addroute OPTIONS "/users" corsA
  get "/" rootA
  get "/users" $ listAction user
  post "/users" $ createAction user
  get "/users/:id" $ memberAction user

rootA :: Action
rootA = json ("Hello World" :: String)

corsA :: Action
corsA = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Headers" "Content-Type, Accept"

handleError :: (ScottyError e) => e -> Action
handleError error = do
  let msg = (BS.pack . unpack) $ showError error
  addHeader "Access-Control-Allow-Origin" "*"
  status $ Status.mkStatus 500 msg
  json ("{\"error\": \"" ++ (BS.unpack msg) ++ "\"}" :: String)
