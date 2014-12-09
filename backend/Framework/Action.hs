module Framework.Action ( mkAction, Action ) where

import Framework.App
import Framework.Resource
import Data.Text.Lazy (Text)
import Data.Aeson (ToJSON)
import Control.Monad.Trans (liftIO)
import Web.Scotty.Trans (ActionT, json)


type Action = ActionT Text ConfigM ()

mkAction :: (ToJSON a) => Resource a -> ResourceStore -> Action
mkAction resource connection = do
  item <- liftIO $ list resource connection
  json item

