{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ApiTypes where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)

import Data.Set (Set)

import Type.User (User)

data ServerData = ServerData
  { users       :: TVar (Set User)
  }

newtype Diagrammer a = Diagrammer
  { unDiagrammerApi :: ReaderT ServerData IO a
  }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader ServerData
           )

runApi :: ServerData -> Diagrammer a -> IO a
runApi serverData = flip runReaderT serverData . unDiagrammerApi
