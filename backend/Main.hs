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

import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Web.Scotty.Trans (scottyT, get, json, middleware, notFound)

import System.Posix.Env (getEnvDefault)

import qualified Data.ByteString.Char8  as BS

import Framework (mkListAction, mkMemberAction, Action, Resource, ResourceStore)

import Framework.App -- TODO: REMOVE THIS IN FAVOR OF BETTER ENCAPSULATION
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
  httpPortS <- getEnvDefault "PORT" "8000"
  return ServerConfig
    { databaseUrl = databaseUrl
    , httpPort = read httpPortS
    }

app :: TVar DB.Connection -> App
app db = do
  connection <- liftIO $ readTVarIO db
  let memberAction = flip mkMemberAction connection
      listAction = flip mkListAction connection
  middleware logStdoutDev
  get "/" rootA
  get "/users" $ listAction user
  get "/users/:id" $ memberAction user

rootA :: Action
rootA = json ("Hello World" :: String)

