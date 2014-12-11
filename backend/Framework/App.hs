{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Framework.App where

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ScottyT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT (..))

type App = ScottyT Text ConfigM ()

data ServerConfig = ServerConfig
  { httpPort :: Int
  , databaseUrl :: String
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT ServerConfig IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerConfig)

